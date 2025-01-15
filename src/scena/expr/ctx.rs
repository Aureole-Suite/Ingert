use snafu::OptionExt as _;

use crate::scp::StackSlot;

use super::{DecompileError, error, Op, Label, Expr, Stmt1};

#[derive(Debug, Clone, PartialEq, Default)]
pub enum StackVal {
	#[default]
	Null,
	RetAddr(Label),
	RetMisc,
	Expr(Expr),
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
			return error::StackBounds { slot: s.0, len }.fail();
		};
		if self.stack[index as usize] != StackVal::Null {
			return error::NotVar { index }.fail();
		}
		Ok(index)
	}

	pub fn push(&mut self, val: impl Into<StackVal>) {
		self.stack.push(val.into());
	}

	pub fn pop(&mut self) -> Result<StackVal, DecompileError> {
		self.stack.pop().context(error::EmptyStack)
	}

	pub fn pop_expr(&mut self) -> Result<Expr, DecompileError> {
		match self.pop()? {
			StackVal::Expr(e) => Ok(e),
			val => error::BadPop { val: Some(val) }.fail(),
		}
	}

	pub fn stmt(&mut self, stmt: Stmt1) -> Result<(), DecompileError> {
		snafu::ensure!(self.is_empty(), error::NonemptyStack);
		self.output.push(stmt);
		Ok(())
	}

	pub fn finish(self) -> Vec<Stmt1> {
		self.output
	}

	fn is_empty(&self) -> bool {
		self.stack.iter().all(|v| *v == StackVal::Null)
	}
}

