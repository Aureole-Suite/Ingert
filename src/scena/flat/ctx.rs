use snafu::OptionExt as _;

use crate::scp::{Op, StackSlot, Label};
use super::{DecompileError, error, Expr, FlatStmt};

#[derive(Debug, Clone, PartialEq)]
pub enum StackVal {
	RetAddr(Label),
	RetMisc,
	Expr(Expr),
}

pub struct Ctx<'a> {
	code: &'a [Op],
	pos: usize,

	label: Option<Label>,
	lines: Vec<Option<u16>>,
	stack: Vec<StackVal>,
	output: Vec<FlatStmt>,
}

impl<'a> Ctx<'a> {
	pub fn new(code: &'a [Op]) -> Self {
		Self {
			code,
			pos: 0,
			label: None,
			lines: Vec::new(),
			stack: Vec::new(),
			output: Vec::new(),
		}
	}

	pub fn next(&mut self) -> Option<&'a Op> {
		let op = self.code.get(self.pos)?;
		self.pos += 1;
		Some(op)
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
			return error::ReadStack { slot: s.0, len }.fail();
		};
		Ok(s.0 - len)
	}

	pub fn push(&mut self, val: impl Into<StackVal>) -> Result<(), DecompileError> {
		self.stack.push(val.into());
		Ok(())
	}

	pub fn pop_any(&mut self) -> Result<StackVal, DecompileError> {
		self.stack.pop().context(error::PopEmpty)
	}

	pub fn pop(&mut self) -> Result<Expr, DecompileError> {
		match self.pop_any()? {
			StackVal::Expr(e) => Ok(e),
			_ => error::PopRetAddr.fail(),
		}
	}

	pub fn label(&mut self, label: Label) -> Result<(), DecompileError> {
		self.check_empty()?;
		self.label = Some(label);
		self.output.push(FlatStmt::Label(label));
		Ok(())
	}

	pub fn get_label(&self) -> Option<Label> {
		self.label
	}

	pub fn line(&mut self, line: u16) -> Result<(), DecompileError> {
		self.lines.push(Some(line));
		Ok(())
	}

	pub fn pop_line(&mut self) -> Option<u16> {
		if let Some(Some(_)) = self.lines.last() && self.lines.len() > 1 {
			let line = self.lines.pop()??;
			Some(line)
		} else {
			None
		}
	}

	pub fn pop_stmt_line(&mut self) -> Option<u16> {
		if let Some(Some(_)) = self.lines.last() && self.lines.len() <= 1{
			let line = self.lines.pop()??;
			tracing::trace!("pop stmt line: {line}");
			Some(line)
		} else {
			None
		}
	}

	pub fn delimit_line(&mut self) {
		self.lines.push(None);
	}

	pub fn undelimit_line(&mut self) {
		while let Some(Some(_)) = self.lines.pop() {}
	}

	pub fn stmt(&mut self, stmt: FlatStmt) -> Result<(), DecompileError> {
		tracing::trace!("stmt: {stmt:?}");
		self.check_empty()?;
		if !self.lines.is_empty() {
			tracing::warn!("lines: {:?}", self.lines);
			self.lines.clear();
		}

		self.label = None;
		self.output.push(stmt);
		Ok(())
	}

	pub fn finish(self) -> Result<Vec<FlatStmt>, DecompileError> {
		self.check_empty()?;
		snafu::ensure!(self.lines.is_empty(), error::NonemptyStack);
		Ok(self.output)
	}

	fn check_empty(&self) -> Result<(), DecompileError> {
		snafu::ensure!(self.stack.is_empty(), error::NonemptyStack);
		Ok(())
	}
}

