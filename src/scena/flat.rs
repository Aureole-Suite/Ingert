mod ctx;

use crate::scp::{Binop, Value, Label, Op};
use super::{Expr, Place, CallKind, FlatStmt};
use ctx::{Ctx, StackVal};
use snafu::{OptionExt as _, ResultExt as _};

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
	#[snafu(display("could not parse function call"))]
	BadCall,
	#[snafu(display("could not parse switch statement"))]
	BadSwitch,
	#[snafu(display("unexpected op"))]
	UnexpectedOp,
	#[snafu(display("invalid labels"))]
	Labels { source: crate::labels::LabelError },
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

			Op::PushNull if matches!(ctx.peek(), [Op::SetTemp(0), ..]) => {
				ctx.next();
				handle_return(&mut ctx, None)?;
			}
			Op::SetTemp(0) => {
				let v = ctx.pop()?;
				handle_return(&mut ctx, Some(v))?;
			}
			Op::Return => {
				return error::UnexpectedOp.fail();
			}

			Op::PushNull => {
				let line = ctx.pop_stmt_line();
				ctx.stmt(FlatStmt::PushVar(line))?;
			}
			Op::Pop(n) => {
				for _ in 0..n {
					ctx.stmt(FlatStmt::PopVar)?;
				}
			}

			Op::CallLocal(ref name) => {
				make_call(&mut ctx, 1, "", name)?;
			}
			Op::CallExtern(ref a, ref b, n) => {
				let nargs = make_call(&mut ctx, 4, a, b)?;
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
			Op::CallTail(_, _, _) => return error::UnexpectedOp.fail(),
			Op::Jnz(_) | Op::GetTemp(_) | Op::SetTemp(_) => return error::UnexpectedOp.fail(),
		}
	}
	let mut out = ctx.finish()?;
	crate::labels::normalize(&mut out, 0).context(error::Labels)?;
	Ok(out)
}

fn handle_return(ctx: &mut Ctx, val: Option<Expr>) -> Result<(), DecompileError> {
	let pop = if let [Op::Pop(n), ..] = ctx.peek() {
		ctx.next();
		*n as usize
	} else {
		0
	};
	if let [Op::Return, ..] = ctx.peek() {
		let line = ctx.pop_stmt_line();
		ctx.next();
		ctx.stmt(FlatStmt::Return(line, val))?;
		for _ in 0..pop {
			ctx.stmt(FlatStmt::PopVar)?;
		}
		Ok(())
	} else {
		error::UnexpectedOp.fail()
	}
}

fn prepare_call(ctx: &mut Ctx, misc: u32, label: Label) -> Result<(), DecompileError> {
	ctx.delimit_line();
	for _ in 0..misc {
		ctx.push(StackVal::RetMisc)?;
	}
	ctx.push(StackVal::RetAddr(label))?;
	Ok(())
}

fn make_call(ctx: &mut Ctx, misc: u32, namea: &str, name: &str) -> Result<usize, DecompileError> {
	ctx.undelimit_line();
	let mut args = Vec::new();
	let label = loop {
		match ctx.pop_any()? {
			StackVal::Expr(v) => args.push(v),
			StackVal::RetAddr(l) => break l,
			_ => return error::BadCall.fail(),
		}
	};
	for _ in 0..misc {
		match ctx.pop_any()? {
			StackVal::RetMisc => {}
			_ => return error::BadCall.fail(),
		}
	}
	let nargs = args.len();
	match ctx.peek() {
		[Op::Label(l), Op::GetTemp(0), ..] if *l == label => {
			ctx.next();
		}
		[Op::Label(l), ..] if *l == label => {}
		_ => return error::BadCall.fail()
	}
	push_call(ctx, CallKind::Normal(namea.to_owned(), name.to_owned()), args)?;
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
