use std::collections::{HashMap, HashSet};
use snafu::OptionExt as _;

use crate::scp::{Label, Op, Scp, StackSlot};
use super::{Expr, Place, CallKind};

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

#[derive(Debug, Clone, PartialEq, Default)]
enum StackVal {
	#[default]
	Null,
	RetAddr(Label),
	RetMisc,
	Expr(Expr),
}

impl From<Expr> for StackVal {
	fn from(e: Expr) -> Self {
		Self::Expr(e)
	}
}

#[derive(Debug, snafu::Snafu)]
pub enum DecompileError {
	BadPop { val: Option<StackVal> },
	StackBounds { slot: u32, len: u32 },
	EmptyStack,
	NotVar { index: u32 },
}

pub struct Ctx<'a> {
	code: &'a [Op],
	pos: usize,

	lines: Vec<u16>,
	stack: Vec<StackVal>,
	output: Vec<Stmt1>,
}

impl<'a> Ctx<'a> {
	pub fn new(code: &'a [Op], nargs: usize) -> Self {
		Self {
			code,
			pos: 0,
			lines: Vec::new(),
			stack: vec![StackVal::Null; nargs],
			output: Vec::new(),
		}
	}

	pub fn next(&mut self) -> Option<&'a Op> {
		loop {
			let op = self.code.get(self.pos);
			self.pos += 1;
			if let Some(Op::Line(n)) = op {
				self.lines.push(*n);
			} else {
				break op
			}
		}
	}

	pub fn peek(&self) -> Option<&'a Op> {
		let mut pos = self.pos;
		loop {
			let op = self.code.get(pos);
			pos += 1;
			if let Some(Op::Line(_)) = op {
				continue;
			} else {
				break op
			}
		}
	}

	pub fn var(&self, s: StackSlot) -> Result<u32, DecompileError> {
		let len = self.stack.len() as u32;
		let Some(index) = len.checked_sub(s.0).filter(|v| *v < len) else {
			return StackBoundsSnafu { slot: s.0, len }.fail();
		};
		if self.stack[index as usize] != StackVal::Null {
			return NotVarSnafu { index }.fail();
		}
		Ok(index)
	}

	pub fn push(&mut self, val: impl Into<StackVal>) {
		self.stack.push(val.into());
	}

	pub fn pop(&mut self) -> Result<StackVal, DecompileError> {
		self.stack.pop().context(EmptyStackSnafu)
	}

	pub fn pop_expr(&mut self) -> Result<Expr, DecompileError> {
		match self.pop()? {
			StackVal::Expr(e) => Ok(e),
			val => BadPopSnafu { val: Some(val) }.fail(),
		}
	}

	pub fn stmt(&mut self, stmt: Stmt1) {
		self.output.push(stmt);
	}

	pub fn finish(self) -> Vec<Stmt1> {
		self.output
	}
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
		{
			let r = &ctx.code[ctx.pos..];
			println!("{op:?} {:?}", r.get(..5).unwrap_or(r));
		}
		match *op {
			Op::Label(l) => {
				ctx.stmt(Stmt1::Label(l));
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
				ctx.stmt(Stmt1::Set(p, v));
			}
			Op::SetRef(s) => {
				let v = ctx.pop_expr()?;
				let p = Place::Deref(ctx.var(s)?);
				ctx.stmt(Stmt1::Set(p, v));
			}
			Op::SetGlobal(ref name) => {
				let v = ctx.pop_expr()?;
				let p = Place::Global(name.clone());
				ctx.stmt(Stmt1::Set(p, v));
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
				ctx.stmt(Stmt1::If(cond, l));
			}
			Op::Goto(l) => {
				ctx.stmt(Stmt1::Label(l));
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
					ctx.stmt(Stmt1::Expr(call));
					if labels.contains(label) {
						ctx.stmt(Stmt1::Label(*label));
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
					ctx.stmt(Stmt1::Expr(call));
					if labels.contains(label) {
						ctx.stmt(Stmt1::Label(*label));
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
					ctx.stmt(Stmt1::Expr(call));
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
					StackVal::Expr(e) => ctx.stmt(Stmt1::Return(Some(e))),
					StackVal::Null => ctx.stmt(Stmt1::Return(None)),
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

