use std::collections::{HashMap, HashSet};
use snafu::OptionExt as _;

use crate::scp::{Op, StackSlot, Label};
use super::{DecompileError, error, Expr, Stmt1};

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
	labels: HashSet<Label>,
	jumps: HashMap<Label, usize>,
}

impl<'a> Ctx<'a> {
	pub fn new(code: &'a [Op], nargs: usize) -> Self {
		Self {
			code,
			pos: 0,
			lines: Vec::new(),
			stack: vec![StackVal::Null; nargs],
			output: Vec::new(),
			jumps: HashMap::new(),
			labels: code.iter().filter_map(|op| match op {
				Op::Jnz(l) | Op::Jz(l) | Op::Goto(l) => Some(*l),
				_ => None,
			}).collect::<HashSet<_>>(),
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
		self.check_empty()?;
		match &stmt {
			Stmt1::Label(l) | Stmt1::Goto(l) | Stmt1::If(_, l) => self.label(*l)?,
			Stmt1::Switch(_, cs, l) => {
				for (_, l) in cs {
					self.label(*l)?;
				}
				self.label(*l)?;
			}
			_ => {}
		}
		self.output.push(stmt);
		Ok(())
	}

	pub fn finish(self) -> Result<Vec<Stmt1>, DecompileError> {
		Ok(self.output)
	}

	fn check_empty(&self) -> Result<(), DecompileError> {
		let empty = self.stack.iter().all(|v| *v == StackVal::Null);
		snafu::ensure!(empty, error::NonemptyStack);
		Ok(())
	}

	fn label(&mut self, label: Label) -> Result<(), DecompileError> {
		assert!(self.has_label(label));
		self.check_empty()?;
		let expected = *self.jumps.entry(label).or_insert(self.stack.len());
		if expected != self.stack.len() {
			return error::InconsistentLabel { label, expected, actual: self.stack.len() }.fail();
		}
		Ok(())
	}

	pub fn has_label(&self, label: Label) -> bool {
		self.labels.contains(&label)
	}
}

