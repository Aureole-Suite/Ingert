use std::collections::{HashMap, HashSet};
use snafu::OptionExt as _;

mod ctx;

use crate::scp::{Label, Op, Scp, StackSlot};
use super::{Expr, Place, CallKind};
use ctx::{Ctx, StackVal};

#[derive(Debug, Clone, PartialEq)]
enum Stmt1 {
	Label(Label),
	Expr(Expr),
	Set(Place, Expr),
	Return(Option<Expr>),
	If(Expr, Label),
	Switch(Expr, Vec<(i32, Label)>, Label),
	PushVar(u32),
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
}

pub fn build_exprs(nargs: usize, code: &[Op]) -> Result<(), DecompileError> {
	let labels = code.iter().filter_map(|op| match op {
		Op::Jnz(l) | Op::Jz(l) | Op::Goto(l) => Some(*l),
		_ => None,
	}).collect::<HashSet<_>>();

	println!();

	let mut ctx = Ctx::new(code, nargs);
	let mut temp0 = None;
	while let Some(op) = ctx.next() {
		match *op {
			Op::Label(l) => {
				ctx.stmt(Stmt1::Label(l))?;
			}
			Op::Push(ref v) => {
				ctx.push(Expr::Value(v.clone()));
			}
			Op::Pop(n) => {
				for _ in 0..n {
					match ctx.pop()? {
						StackVal::Null => {}
						_ => panic!(),
					}
				}
			}
			Op::PushNull => {
				ctx.push(StackVal::Null);
			}
			Op::GetVar(s) => {
				let p = Place::Var(ctx.var(s)?);
				ctx.push(Expr::Var(p));
			}
			Op::GetRef(s) => {
				let p = Place::Deref(ctx.var(s)?);
				ctx.push(Expr::Var(p));
			}
			Op::GetGlobal(ref name) => {
				let p = Place::Global(name.clone());
				ctx.push(Expr::Var(p));
			}
			Op::PushRef(s) => {
				let n = ctx.var(s)?;
				ctx.push(Expr::Ref(n));
			}
			Op::SetVar(s) => {
				let v = ctx.pop_expr()?;
				let p = Place::Var(ctx.var(s)?);
				ctx.stmt(Stmt1::Set(p, v))?;
			}
			Op::SetRef(s) => {
				let v = ctx.pop_expr()?;
				let p = Place::Deref(ctx.var(s)?);
				ctx.stmt(Stmt1::Set(p, v))?;
			}
			Op::SetGlobal(ref name) => {
				let v = ctx.pop_expr()?;
				let p = Place::Global(name.clone());
				ctx.stmt(Stmt1::Set(p, v))?;
			}
			Op::SetTemp(0) => {
				temp0 = Some(ctx.pop()?);
			}
			Op::GetTemp(n) => todo!(),
			Op::SetTemp(n) => todo!(),
			Op::Binop(o) => {
				let b = ctx.pop_expr()?;
				let a = ctx.pop_expr()?;
				ctx.push(Expr::Binop(o, Box::new(a), Box::new(b)));
			}
			Op::Unop(o) => {
				let a = ctx.pop_expr()?;
				ctx.push(Expr::Unop(o, Box::new(a)));
			}
			Op::Jnz(l) => todo!(),
			Op::Jz(l) => {
				let cond = ctx.pop_expr()?;
				ctx.stmt(Stmt1::If(cond, l))?;
			}
			Op::Goto(l) => {
				ctx.stmt(Stmt1::Label(l))?;
			}
			Op::CallLocal(ref name) => {
				let Some(Op::Label(label)) = ctx.next() else {
					panic!();
				};
				let npop = 1;
				let mut args = Vec::new();
				loop {
					match ctx.pop()? {
						StackVal::Expr(v) => args.push(v),
						StackVal::RetAddr(l) if l == *label => break,
						_ => panic!(),
					}
				}
				for _ in 0..npop {
					match ctx.pop()? {
						StackVal::RetMisc => {}
						_ => panic!(),
					}
				}

				let call = Expr::Call(CallKind::Normal(name.clone()), args);

				if let Some(Op::GetTemp(0)) = ctx.peek() {
					ctx.next();
					ctx.push(call);
				} else {
					ctx.stmt(Stmt1::Expr(call))?;
					if labels.contains(label) {
						ctx.stmt(Stmt1::Label(*label))?;
					}
				}
			}
			Op::CallExtern(ref name, n) => {
				let Some(Op::Label(label)) = ctx.next() else {
					panic!();
				};
				let npop = 4;
				let mut args = Vec::new();
				loop {
					match ctx.pop()? {
						StackVal::Expr(v) => args.push(v),
						StackVal::RetAddr(l) if l == *label => break,
						_ => panic!(),
					}
				}
				for _ in 0..npop {
					match ctx.pop()? {
						StackVal::RetMisc => {}
						_ => panic!(),
					}
				}

				if args.len() != n as usize {
					panic!("{:?} != {}", args, n);
				}

				let call = Expr::Call(CallKind::Normal(name.clone()), args);

				if let Some(Op::GetTemp(0)) = ctx.peek() {
					ctx.next();
					ctx.push(call);
				} else {
					ctx.stmt(Stmt1::Expr(call))?;
					if labels.contains(label) {
						ctx.stmt(Stmt1::Label(*label))?;
					}
				}
			}
			Op::CallTail(ref name, n) => todo!(),
			Op::CallSystem(a, b, n) => {
				let mut args = Vec::new();
				for _ in 0..n {
					args.push(ctx.pop_expr()?);
				}
				if n != 0 && ctx.next() != Some(&Op::Pop(n)) {
					panic!();
				}

				let call = Expr::Call(CallKind::Syscall(a, b), args);
				if let Some(Op::GetTemp(0)) = ctx.peek() {
					ctx.next();
					ctx.push(call);
				} else {
					ctx.stmt(Stmt1::Expr(call))?;
				}
			}
			Op::PrepareCallLocal(l) => {
				ctx.push(StackVal::RetMisc);
				ctx.push(StackVal::RetAddr(l));
			}
			Op::PrepareCallExtern(l) => {
				ctx.push(StackVal::RetMisc);
				ctx.push(StackVal::RetMisc);
				ctx.push(StackVal::RetMisc);
				ctx.push(StackVal::RetMisc);
				ctx.push(StackVal::RetAddr(l));
			}
			Op::Return => {
				let Some(temp0) = temp0.take() else {
					panic!();
				};
				match temp0 {
					StackVal::Expr(e) => ctx.stmt(Stmt1::Return(Some(e)))?,
					StackVal::Null => ctx.stmt(Stmt1::Return(None))?,
					_ => panic!(),
				}
			}
			Op::Line(n) => {},
			Op::Debug(n) => todo!(),
		}
	}
	let out = ctx.finish();
	dbg!(&out);
	Ok(())
}

