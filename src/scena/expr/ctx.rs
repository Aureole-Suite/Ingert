use std::collections::HashSet;
use snafu::OptionExt as _;

use crate::scp::{Op, StackSlot, Label};
use super::{DecompileError, error, Expr, Stmt1};

#[derive(Debug, Clone, PartialEq)]
pub enum StackVal {
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
	labels: HashSet<Label>,
	temp0: Option<Option<Expr>>,
}

impl<'a> Ctx<'a> {
	pub fn new(code: &'a [Op]) -> Self {
		Self {
			code,
			pos: 0,
			lines: Vec::new(),
			stack: Vec::new(),
			output: Vec::new(),
			labels: code.iter().filter_map(|op| match op {
				Op::Jnz(l) | Op::Jz(l) | Op::Goto(l) => Some(*l),
				_ => None,
			}).collect(),
			temp0: None,
		}
	}

	pub fn next(&mut self) -> Option<&'a Op> {
		loop {
			let op = self.code.get(self.pos)?;
			self.pos += 1;
			if let Op::Line(n) = op {
				tracing::trace!("line: {n}");
				self.lines.push(*n);
			} else {
				tracing::trace!("op: {op:?}");
				return Some(op)
			}
		}
	}

	pub fn peek(&self) -> &'a [Op] {
		let mut pos = self.pos;
		while let Some(Op::Line(_)) = self.code.get(pos) {
			pos += 1;
		}
		&self.code[pos..]
	}

	pub fn var(&self, s: StackSlot) -> Result<u32, DecompileError> {
		let len = self.stack.len() as u32;
		if s.0 <= len {
			return error::StackBounds { slot: s.0, len }.fail();
		};
		Ok(s.0 - len)
	}

	pub fn push(&mut self, val: impl Into<StackVal>) -> Result<(), DecompileError> {
		self.check_state()?;
		self.stack.push(val.into());
		Ok(())
	}

	pub fn pop_any(&mut self) -> Result<StackVal, DecompileError> {
		self.stack.pop().context(error::EmptyStack)
	}

	pub fn pop(&mut self) -> Result<Expr, DecompileError> {
		match self.pop_any()? {
			StackVal::Expr(e) => Ok(e),
			val => error::BadPop { val: Some(val) }.fail(),
		}
	}

	pub fn stmt(&mut self, stmt: Stmt1) -> Result<(), DecompileError> {
		tracing::trace!("stmt: {stmt:?}");
		self.check_empty()?;
		self.check_state()?;
		if !self.lines.is_empty() {
			tracing::warn!("lines: {:?}", self.lines);
			self.lines.clear();
		}
		self.output.push(stmt);
		Ok(())
	}

	pub fn set_temp0(&mut self, expr: Option<Expr>) -> Result<(), DecompileError> {
		self.check_empty()?;
		self.check_state()?;
		self.temp0 = Some(expr);
		Ok(())
	}

	pub fn temp0(&mut self) -> Result<Option<Expr>, DecompileError> {
		self.temp0.take().context(error::Temp0Unset)
	}

	pub fn finish(self) -> Result<Vec<Stmt1>, DecompileError> {
		Ok(self.output)
	}

	fn check_state(&mut self) -> Result<(), DecompileError> {
		if let Some(temp0) = self.temp0.take() {
			return error::Temp0Set { temp0: temp0.clone() }.fail()
		}
		Ok(())
	}

	fn check_empty(&self) -> Result<(), DecompileError> {
		snafu::ensure!(self.stack.is_empty(), error::NonemptyStack);
		Ok(())
	}

	pub fn has_label(&self, label: Label) -> bool {
		self.labels.contains(&label)
	}
}

