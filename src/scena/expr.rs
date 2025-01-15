mod ctx;

use crate::scp::{Label, Op};
use super::{Expr, Place, CallKind};
use ctx::{Ctx, StackVal};
use snafu::OptionExt as _;

#[derive(Debug, Clone, PartialEq)]
enum Stmt1 {
	Label(Label),
	Expr(Expr),
	Set(Place, Expr),
	Return(Option<Expr>),
	If(Expr, Label),
	Goto(Label),
	Switch(Expr, Vec<(i32, Label)>, Label),
	PushVar,
	Debug(Vec<Expr>),
}

impl From<Expr> for StackVal {
	fn from(e: Expr) -> Self {
		Self::Expr(e)
	}
}

#[derive(Debug, snafu::Snafu)]
#[snafu(module(error), context(suffix(false)))]
pub enum DecompileError {
	BadPop { val: Option<StackVal> },
	StackBounds { slot: u32, len: u32 },
	EmptyStack,
	NotVar { index: u32 },
	NonemptyStack,
	InconsistentLabel { label: Label, expected: usize, actual: usize },
	NoReturn,
}

pub fn build_exprs(nargs: usize, code: &[Op]) -> Result<(), DecompileError> {
	println!();

	let mut ctx = Ctx::new(code, nargs);
	let mut temp0 = None;
	while let Some(op) = ctx.next() {
		match *op {
			Op::Label(l) => {
				ctx.stmt(Stmt1::Label(l))?;
			}
			Op::Push(ref v) => {
				ctx.push(Expr::Value(v.clone()))?;
			}
			Op::Pop(n) => {
				for _ in 0..n {
					match ctx.pop_any()? {
						StackVal::Null => {}
						_ => panic!(),
					}
				}
			}
			Op::PushNull if ctx.peek() == Some(&Op::SetTemp(0)) => {
				ctx.next();
				temp0 = Some(None);
			}
			Op::SetTemp(0) => {
				temp0 = Some(Some(ctx.pop()?));
			}
			Op::Return => {
				ctx.stmt(Stmt1::Return(temp0.take().context(error::NoReturn)?))?;
			}
			Op::PushNull => {
				ctx.push(StackVal::Null)?;
				ctx.stmt(Stmt1::PushVar)?;
			}
			Op::GetVar(s) => {
				let p = Place::Var(ctx.var(s)?);
				ctx.push(Expr::Var(p))?;
			}
			Op::GetRef(s) => {
				let p = Place::Deref(ctx.var(s)?);
				ctx.push(Expr::Var(p))?;
			}
			Op::GetGlobal(ref name) => {
				let p = Place::Global(name.clone());
				ctx.push(Expr::Var(p))?;
			}
			Op::PushRef(s) => {
				let n = ctx.var(s)?;
				ctx.push(Expr::Ref(n))?;
			}
			Op::SetVar(s) => {
				let v = ctx.pop()?;
				let p = Place::Var(ctx.var(s)?);
				ctx.stmt(Stmt1::Set(p, v))?;
			}
			Op::SetRef(s) => {
				let v = ctx.pop()?;
				let p = Place::Deref(ctx.var(s)?);
				ctx.stmt(Stmt1::Set(p, v))?;
			}
			Op::SetGlobal(ref name) => {
				let v = ctx.pop()?;
				let p = Place::Global(name.clone());
				ctx.stmt(Stmt1::Set(p, v))?;
			}
			Op::GetTemp(n) => todo!(),
			Op::SetTemp(n) => todo!(),
			Op::Binop(o) => {
				let b = ctx.pop()?;
				let a = ctx.pop()?;
				ctx.push(Expr::Binop(o, Box::new(a), Box::new(b)))?;
			}
			Op::Unop(o) => {
				let a = ctx.pop()?;
				ctx.push(Expr::Unop(o, Box::new(a)))?;
			}
			Op::Jnz(l) => todo!(),
			Op::Jz(l) => {
				let cond = ctx.pop()?;
				ctx.stmt(Stmt1::If(cond, l))?;
			}
			Op::Goto(l) => {
				ctx.stmt(Stmt1::Label(l))?;
			}
			Op::CallLocal(ref name) => {
				make_call(&mut ctx, 1, name)?;
			}
			Op::CallExtern(ref name, n) => {
				let nargs = make_call(&mut ctx, 4, name)?;
				if nargs != n as usize {
					panic!("{} != {}", nargs, n);
				}
			}
			Op::CallTail(ref name, n) => todo!(),
			Op::CallSystem(a, b, n) => {
				let mut args = Vec::new();
				for _ in 0..n {
					args.push(ctx.pop()?);
				}
				args.reverse();
				if n != 0 && ctx.next() != Some(&Op::Pop(n)) {
					panic!();
				}
				push_call(&mut ctx, Expr::Call(CallKind::Syscall(a, b), args))?;
			}
			Op::PrepareCallLocal(l) => {
				prepare_call(&mut ctx, 1, l)?;
			}
			Op::PrepareCallExtern(l) => {
				prepare_call(&mut ctx, 4, l)?;
			}
			Op::Line(n) => {},
			Op::Debug(n) => todo!(),
		}
	}
	let out = ctx.finish()?;
	dbg!(&out);
	Ok(())
}

fn prepare_call(ctx: &mut Ctx, misc: u32, label: Label) -> Result<(), DecompileError> {
	for _ in 0..misc {
		ctx.push(StackVal::RetMisc)?;
	}
	ctx.push(StackVal::RetAddr(label))?;
	Ok(())
}

fn make_call(ctx: &mut Ctx, misc: u32, name: &str) -> Result<usize, DecompileError> {
	let Some(Op::Label(label)) = ctx.next() else {
		panic!();
	};
	let mut args = Vec::new();
	loop {
		match ctx.pop_any()? {
			StackVal::Expr(v) => args.push(v),
			StackVal::RetAddr(l) if l == *label => break,
			v => panic!("{v:?}"),
		}
	}
	args.reverse();
	for _ in 0..misc {
		match ctx.pop_any()? {
			StackVal::RetMisc => {}
			v => panic!("{v:?}"),
		}
	}
	let nargs = args.len();
	push_call(ctx, Expr::Call(CallKind::Normal(name.to_owned()), args))?;
	if ctx.has_label(*label) {
		ctx.stmt(Stmt1::Label(*label))?;
	}
	Ok(nargs)
}

fn push_call(ctx: &mut Ctx, call: Expr) -> Result<(), DecompileError> {
	if let Some(Op::GetTemp(0)) = ctx.peek() {
		ctx.next();
		ctx.push(call)?;
	} else {
		ctx.stmt(Stmt1::Expr(call))?;
	}
	Ok(())
}
