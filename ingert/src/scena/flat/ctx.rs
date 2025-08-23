use snafu::OptionExt as _;

use crate::{scena::FlatVar, scp::{Label, Op, StackSlot}};
use super::{DecompileError, error, Expr, FlatStmt};

#[derive(Debug, PartialEq)]
pub enum StackVal {
	RetAddr(Label),
	RetMisc,
	Expr(Expr),
}

pub struct Ctx<'a> {
	code: &'a [Op],
	pos: usize,

	lines: Vec<u16>,
	stack: Vec<(StackVal, Vec<u16>)>,
	output: Vec<FlatStmt>,
}

impl<'a> Ctx<'a> {
	pub fn new(code: &'a [Op]) -> Self {
		Self {
			code,
			pos: 0,
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

	pub fn var(&self, s: StackSlot) -> Result<FlatVar, DecompileError> {
		let len = self.stack.len() as u32;
		if s.0 <= len {
			return error::ReadStack { slot: s.0, len }.fail();
		};
		Ok(FlatVar(s.0 - len))
	}

	pub fn push(&mut self, val: impl Into<StackVal>) -> Result<(), DecompileError> {
		let mut val = val.into();
		if let StackVal::Expr(e) = &mut val && let Some(l) = e.line_mut() {
			assert!(l.is_none());
			*l = self.lines.pop();
		}
		self.stack.push((val, std::mem::take(&mut self.lines)));
		Ok(())
	}

	pub fn pop_any(&mut self) -> Result<StackVal, DecompileError> {
		let (v, l) = self.stack.pop().context(error::PopEmpty)?;
		if !self.lines.is_empty() {
			tracing::warn!("lines: {:?}", self.lines);
		}
		self.lines = l;
		Ok(v)
	}

	pub fn pop(&mut self) -> Result<Expr, DecompileError> {
		match self.pop_any()? {
			StackVal::Expr(e) => Ok(e),
			_ => error::PopRetAddr.fail(),
		}
	}

	pub fn label(&mut self, label: Label) -> Result<(), DecompileError> {
		self.check_empty()?;
		self.output.push(FlatStmt::Label(label));
		Ok(())
	}

	pub fn line(&mut self, line: u16) -> Result<(), DecompileError> {
		self.lines.push(line);
		Ok(())
	}

	pub fn stmt(&mut self, mut stmt: FlatStmt) -> Result<(), DecompileError> {
		self.check_empty()?;
		if let Some(l) = stmt.line_mut() {
			assert!(l.is_none());
			*l = self.lines.pop();
		}
		super::line::sink(&mut stmt);
		tracing::trace!("stmt: {stmt:?}");
		if !self.lines.is_empty() {
			tracing::warn!("lines: {:?}", self.lines);
			self.lines.clear();
		}

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
