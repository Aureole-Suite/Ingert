mod ctx;

use crate::scp::{Binop, Value, Label, Op};
use super::{Expr, Place, CallKind};
use ctx::{Ctx, StackVal};
use snafu::OptionExt as _;

#[derive(Debug, Clone, PartialEq)]
pub enum FlatStmt {
	Label(Label),
	Expr(Expr),
	Set(Option<u16>, Place, Expr),
	Return(Option<u16>, Option<Expr>),
	If(Option<u16>, Expr, Label),
	Goto(Label),
	Switch(Option<u16>, Expr, Vec<(i32, Label)>, Label),
	PushVar(Option<u16>),
	Debug(Option<u16>, Vec<Expr>),
}

impl From<Expr> for StackVal {
	fn from(e: Expr) -> Self {
		Self::Expr(e)
	}
}

#[derive(Debug, snafu::Snafu)]
#[snafu(module(error), context(suffix(false)))]
pub enum DecompileError {
	#[snafu(display("attempted to pop from empty stack"))]
	PopEmpty,
	#[snafu(display("attempted to pop a return address"))]
	PopRetAddr,
	#[snafu(display("attempted to read from stack (slot: {slot}, len: {len})"))]
	ReadStack { slot: u32, len: u32 },
	#[snafu(display("stack not empty when finished statement"))]
	NonemptyStack,
	#[snafu(display("return value set unexpectedly"))]
	Temp0Set { temp0: Option<Expr> },
	#[snafu(display("return value not set"))]
	Temp0Unset,
	#[snafu(display("could not parse function call"))]
	BadCall,
	#[snafu(display("could not parse switch statement"))]
	BadSwitch,
	#[snafu(display("unexpected op"))]
	UnexpectedOp,
}

pub fn decompile(code: &[Op]) -> Result<Vec<FlatStmt>, DecompileError> {
	let mut ctx = Ctx::new(code);
	while let Some(op) = ctx.next() {
		match *op {
			Op::Label(l) => {
				ctx.stmt(FlatStmt::Label(l))?;
			}
			Op::Push(ref v) => {
				let line = ctx.pop_line();
				ctx.push(Expr::Value(line, v.clone()))?;
			}
			Op::Pop(_) => {
				// Eh, don't care. If it's wrong, we'll find out when roundtrip fails.
			}
			Op::SetTemp(0) if matches!(ctx.peek(), [Op::GetTemp(0) | Op::Goto(_), ..]) => {
				let line = ctx.pop_stmt_line();
				let v = ctx.pop()?;
				let mut cases = Vec::new();
				let default = loop {
					match ctx.next().context(error::BadSwitch)? {
						Op::GetTemp(0) => {
							if let [
								Some(Op::Push(Value::Int(n))),
								Some(Op::Binop(Binop::Eq)),
								Some(Op::Jnz(l)),
							] = std::array::from_fn(|_| ctx.next()) {
								cases.push((*n, *l));
							} else {
								return error::BadSwitch.fail();
							}
						}
						Op::Goto(l) => break *l,
						_ => return error::BadSwitch.fail(),
					}
				};
				ctx.stmt(FlatStmt::Switch(line, v, cases, default))?;
			}
			Op::PushNull if matches!(ctx.peek(), [Op::SetTemp(0), ..]) => {
				ctx.next();
				ctx.set_temp0(None)?;
			}
			Op::SetTemp(0) => {
				let v = ctx.pop()?;
				ctx.set_temp0(Some(v))?;
			}
			Op::Return => {
				let line = ctx.pop_stmt_line();
				let v = ctx.temp0()?;
				ctx.stmt(FlatStmt::Return(line, v))?;
			}
			Op::PushNull => {
				let line = ctx.pop_stmt_line();
				ctx.stmt(FlatStmt::PushVar(line))?;
			}
			Op::GetVar(s) => {
				let line = ctx.pop_line();
				let p = Place::Var(ctx.var(s)?);
				ctx.push(Expr::Var(line, p))?;
			}
			Op::GetRef(s) => {
				let line = ctx.pop_line();
				let p = Place::Deref(ctx.var(s)?);
				ctx.push(Expr::Var(line, p))?;
			}
			Op::GetGlobal(ref name) => {
				let line = ctx.pop_line();
				let p = Place::Global(name.clone());
				ctx.push(Expr::Var(line, p))?;
			}
			Op::PushRef(s) => {
				let line = ctx.pop_line();
				let n = ctx.var(s)?;
				ctx.push(Expr::Ref(line, n))?;
			}
			Op::SetVar(s) => {
				let line = ctx.pop_stmt_line();
				let v = ctx.pop()?;
				let p = Place::Var(ctx.var(s)?);
				ctx.stmt(FlatStmt::Set(line, p, v))?;
			}
			Op::SetRef(s) => {
				let line = ctx.pop_stmt_line();
				let v = ctx.pop()?;
				let p = Place::Deref(ctx.var(s)?);
				ctx.stmt(FlatStmt::Set(line, p, v))?;
			}
			Op::SetGlobal(ref name) => {
				let line = ctx.pop_stmt_line();
				let v = ctx.pop()?;
				let p = Place::Global(name.clone());
				ctx.stmt(FlatStmt::Set(line, p, v))?;
			}
			Op::Binop(o) => {
				let line = ctx.pop_line();
				let b = ctx.pop()?;
				let a = ctx.pop()?;
				ctx.push(Expr::Binop(line, o, Box::new(a), Box::new(b)))?;
			}
			Op::Unop(o) => {
				let line = ctx.pop_line();
				let a = ctx.pop()?;
				ctx.push(Expr::Unop(line, o, Box::new(a)))?;
			}
			Op::Jz(l) => {
				let line = ctx.pop_stmt_line();
				let cond = ctx.pop()?;
				ctx.stmt(FlatStmt::If(line, cond, l))?;
			}
			Op::Goto(l) => {
				ctx.stmt(FlatStmt::Goto(l))?;
			}
			Op::CallLocal(ref name) => {
				make_call(&mut ctx, 1, name)?;
			}
			Op::CallExtern(ref name, n) => {
				let nargs = make_call(&mut ctx, 4, name)?;
				if nargs != n as usize {
					return error::BadCall.fail();
				}
			}
			Op::CallSystem(a, b, n) => {
				let mut args = Vec::new();
				for _ in 0..n {
					args.push(ctx.pop()?);
				}
				if n != 0 && ctx.next() != Some(&Op::Pop(n)) {
					return error::BadCall.fail();
				}
				push_call(&mut ctx, CallKind::Syscall(a, b), args)?;
			}
			Op::PrepareCallLocal(l) => {
				prepare_call(&mut ctx, 1, l)?;
			}
			Op::PrepareCallExtern(l) => {
				prepare_call(&mut ctx, 4, l)?;
			}
			Op::Line(_) => unreachable!("handled by next()"),
			Op::Debug(n) => {
				let line = ctx.pop_stmt_line();
				let mut args = Vec::new();
				for _ in 0..n {
					args.push(ctx.pop()?);
				}
				args.reverse();
				ctx.stmt(FlatStmt::Debug(line, args))?;
			}
			Op::CallTail(_, _) => return error::UnexpectedOp.fail(),
			Op::Jnz(_) | Op::GetTemp(_) | Op::SetTemp(_) => return error::UnexpectedOp.fail(),
		}
	}
	ctx.finish()
}

fn prepare_call(ctx: &mut Ctx, misc: u32, label: Label) -> Result<(), DecompileError> {
	ctx.delimit_line();
	for _ in 0..misc {
		ctx.push(StackVal::RetMisc)?;
	}
	ctx.push(StackVal::RetAddr(label))?;
	Ok(())
}

fn make_call(ctx: &mut Ctx, misc: u32, name: &str) -> Result<usize, DecompileError> {
	let Some(Op::Label(label)) = ctx.next() else {
		return error::BadCall.fail();
	};
	ctx.undelimit_line();
	let mut args = Vec::new();
	loop {
		match ctx.pop_any()? {
			StackVal::Expr(v) => args.push(v),
			StackVal::RetAddr(l) if l == *label => break,
			_ => return error::BadCall.fail(),
		}
	}
	for _ in 0..misc {
		match ctx.pop_any()? {
			StackVal::RetMisc => {}
			_ => return error::BadCall.fail(),
		}
	}
	let nargs = args.len();
	push_call(ctx, CallKind::Normal(name.to_owned()), args)?;
	if ctx.has_label(*label) {
		ctx.stmt(FlatStmt::Label(*label))?;
	}
	Ok(nargs)
}

fn push_call(ctx: &mut Ctx, kind: CallKind, args: Vec<Expr>) -> Result<(), DecompileError> {
	if matches!(ctx.peek(), [Op::GetTemp(0), ..]) {
		let line = ctx.pop_line();
		ctx.next();
		ctx.push(Expr::Call(line, kind, args))?;
	} else {
		let line = ctx.pop_stmt_line();
		ctx.stmt(FlatStmt::Expr(Expr::Call(line, kind, args)))?;
	}
	Ok(())
}
