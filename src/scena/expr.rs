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

#[derive(Debug, Clone)]
struct Stack {
	stack: Vec<StackVal>,
}

impl Stack {
	fn var(&self, s: StackSlot) -> Result<u32, DecompileError> {
		let len = self.stack.len() as u32;
		let Some(index) = len.checked_sub(s.0).filter(|v| *v < len) else {
			return StackBoundsSnafu { slot: s.0, len }.fail();
		};
		if self.stack[index as usize] != StackVal::Null {
			return NotVarSnafu { index }.fail();
		}
		Ok(index)
	}

	fn push(&mut self, val: impl Into<StackVal>) {
		self.stack.push(val.into());
	}

	fn pop(&mut self) -> Result<StackVal, DecompileError> {
		self.stack.pop().context(EmptyStackSnafu)
	}

	fn pop_expr(&mut self) -> Result<Expr, DecompileError> {
		match self.pop()? {
			StackVal::Expr(e) => Ok(e),
			val => BadPopSnafu { val: Some(val) }.fail(),
		}
	}
}

struct Input<'a> {
	code: &'a [Op],
	pos: usize,
	lines: Vec<u16>,
}

impl<'a> Input<'a> {
	fn new(code: &'a [Op]) -> Self {
		Self {
			code,
			pos: 0,
			lines: Vec::new(),
		}
	}

	fn next(&mut self) -> Option<&'a Op> {
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

	fn peek(&self) -> Option<&'a Op> {
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
}

pub fn build_exprs(nargs: usize, code: &[Op]) -> Result<(), DecompileError> {
	let mut out = Vec::new();
	let mut stack = Stack {
		stack: vec![StackVal::Null; nargs],
	};

	let labels = code.iter().filter_map(|op| match op {
		Op::Jnz(l) | Op::Jz(l) | Op::Goto(l) => Some(*l),
		_ => None,
	}).collect::<HashSet<_>>();

	println!();

	let mut code = Input::new(code);
	let mut temp0 = None;
	while let Some(op) = code.next() {
		{
			let r = &code.code[code.pos..];
			println!("{op:?} {:?}", r.get(..5).unwrap_or(r));
		}
		match *op {
			Op::Label(l) => {
				out.push(Stmt1::Label(l));
			}
			Op::Push(ref v) => {
				stack.push(Expr::Value(v.clone()));
			}
			Op::Pop(n) => {
				for _ in 0..n {
					match stack.pop()? {
						StackVal::Null => {}
						_ => panic!(),
					}
				}
			}
			Op::PushNull => {
				stack.push(StackVal::Null);
			}
			Op::GetVar(s) => {
				let p = Place::Var(stack.var(s)?);
				stack.push(Expr::Var(p));
			}
			Op::GetRef(s) => {
				let p = Place::Deref(stack.var(s)?);
				stack.push(Expr::Var(p));
			}
			Op::GetGlobal(ref name) => {
				let p = Place::Global(name.clone());
				stack.push(Expr::Var(p));
			}
			Op::PushRef(s) => {
				let n = stack.var(s)?;
				stack.push(Expr::Ref(n));
			}
			Op::SetVar(s) => {
				let v = stack.pop_expr()?;
				let p = Place::Var(stack.var(s)?);
				out.push(Stmt1::Set(p, v));
			}
			Op::SetRef(s) => {
				let v = stack.pop_expr()?;
				let p = Place::Deref(stack.var(s)?);
				out.push(Stmt1::Set(p, v));
			}
			Op::SetGlobal(ref name) => {
				let v = stack.pop_expr()?;
				let p = Place::Global(name.clone());
				out.push(Stmt1::Set(p, v));
			}
			Op::SetTemp(0) => {
				temp0 = Some(stack.pop()?);
			}
			Op::GetTemp(n) => todo!(),
			Op::SetTemp(n) => todo!(),
			Op::Binop(o) => {
				let b = stack.pop_expr()?;
				let a = stack.pop_expr()?;
				stack.push(Expr::Binop(o, Box::new(a), Box::new(b)));
			}
			Op::Unop(o) => {
				let a = stack.pop_expr()?;
				stack.push(Expr::Unop(o, Box::new(a)));
			}
			Op::Jnz(l) => todo!(),
			Op::Jz(l) => {
				let cond = stack.pop_expr()?;
				out.push(Stmt1::If(cond, l));
			}
			Op::Goto(l) => {
				out.push(Stmt1::Label(l));
			}
			Op::CallLocal(ref name) => {
				let Some(Op::Label(label)) = code.next() else {
					panic!();
				};
				let npop = 1;
				let mut args = Vec::new();
				loop {
					match stack.pop()? {
						StackVal::Expr(v) => args.push(v),
						StackVal::RetAddr(l) if l == *label => break,
						_ => panic!(),
					}
				}
				for _ in 0..npop {
					match stack.pop()? {
						StackVal::RetMisc => {}
						_ => panic!(),
					}
				}

				let call = Expr::Call(CallKind::Normal(name.clone()), args);

				if let Some(Op::GetTemp(0)) = code.peek() {
					code.next();
					stack.push(call);
				} else {
					out.push(Stmt1::Expr(call));
					if labels.contains(label) {
						out.push(Stmt1::Label(*label));
					}
				}
			}
			Op::CallExtern(ref name, n) => {
				let Some(Op::Label(label)) = code.next() else {
					panic!();
				};
				let npop = 4;
				let mut args = Vec::new();
				loop {
					match stack.pop()? {
						StackVal::Expr(v) => args.push(v),
						StackVal::RetAddr(l) if l == *label => break,
						_ => panic!(),
					}
				}
				for _ in 0..npop {
					match stack.pop()? {
						StackVal::RetMisc => {}
						_ => panic!(),
					}
				}

				if args.len() != n as usize {
					panic!("{:?} != {}", args, n);
				}

				let call = Expr::Call(CallKind::Normal(name.clone()), args);

				if let Some(Op::GetTemp(0)) = code.peek() {
					code.next();
					stack.push(call);
				} else {
					out.push(Stmt1::Expr(call));
					if labels.contains(label) {
						out.push(Stmt1::Label(*label));
					}
				}
			}
			Op::CallTail(ref name, n) => todo!(),
			Op::CallSystem(a, b, n) => {
				let mut args = Vec::new();
				for _ in 0..n {
					args.push(stack.pop_expr()?);
				}
				if n != 0 && code.next() != Some(&Op::Pop(n)) {
					panic!();
				}

				let call = Expr::Call(CallKind::Syscall(a, b), args);
				if let Some(Op::GetTemp(0)) = code.peek() {
					code.next();
					stack.push(call);
				} else {
					out.push(Stmt1::Expr(call));
				}
			}
			Op::PrepareCallLocal(l) => {
				stack.push(StackVal::RetMisc);
				stack.push(StackVal::RetAddr(l));
			}
			Op::PrepareCallExtern(l) => {
				stack.push(StackVal::RetMisc);
				stack.push(StackVal::RetMisc);
				stack.push(StackVal::RetMisc);
				stack.push(StackVal::RetMisc);
				stack.push(StackVal::RetAddr(l));
			}
			Op::Return => {
				let Some(temp0) = temp0.take() else {
					panic!();
				};
				match temp0 {
					StackVal::Expr(e) => out.push(Stmt1::Return(Some(e))),
					StackVal::Null => out.push(Stmt1::Return(None)),
					_ => panic!(),
				}
			}
			Op::Line(n) => {},
			Op::Debug(n) => todo!(),
		}
	}
	dbg!(out);
	Ok(())
}

