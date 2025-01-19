use std::collections::{HashMap, HashSet};
use snafu::OptionExt as _;

use crate::scp::{Op, StackSlot, Label};
use super::{DecompileError, error, Expr, Stmt1};

#[derive(Debug, Clone, PartialEq)]
pub enum StackVal {
	Null,
	RetAddr(Label),
	RetMisc,
	Expr(Expr),
}

enum State {
	Normal,
	Temp0(Option<Expr>),
	Ghost,
}

pub struct Ctx<'a> {
	code: &'a [Op],
	pos: usize,

	lines: Vec<u16>,
	stack: Vec<StackVal>,
	output: Vec<Stmt1>,
	labels: HashSet<Label>,
	jumps: HashMap<Label, usize>,
	state: State,
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
			state: State::Normal,
		}
	}

	pub fn next(&mut self) -> Option<&'a Op> {
		loop {
			let op = self.code.get(self.pos);
			self.pos += 1;
			if let Some(Op::Line(n)) = op {
				self.lines.push(*n);
			} else {
				tracing::trace!("op: {} {op:?}", self.stack.len());
				break op
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
		let Some(index) = len.checked_sub(s.0).filter(|v| *v < len) else {
			return error::StackBounds { slot: s.0, len }.fail();
		};
		if self.stack[index as usize] != StackVal::Null {
			return error::NotVar { index }.fail();
		}
		Ok(index)
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
		tracing::trace!("stmt: {} {stmt:?}", self.stack.len());
		self.check_empty()?;
		self.check_state()?;
		match &stmt {
			Stmt1::Label(l) | Stmt1::Goto(l) | Stmt1::If(_, l) => self.label(*l)?,
			Stmt1::Switch(_, cs, l) => {
				for (_, l) in cs {
					self.label(*l)?;
				}
				self.label(*l)?;
			}
			Stmt1::Return(_) => {
				self.state = State::Ghost;
			}
			_ => {}
		}
		self.output.push(stmt);
		Ok(())
	}

	pub fn set_temp0(&mut self, expr: Option<Expr>) -> Result<(), DecompileError> {
		self.check_empty()?;
		self.check_state()?;
		self.state = State::Temp0(expr);
		Ok(())
	}

	pub fn temp0(&mut self) -> Result<Option<Expr>, DecompileError> {
		match std::mem::replace(&mut self.state, State::Normal) {
			State::Temp0(expr) => Ok(expr),
			_ => error::Temp0Unset.fail(),
		}
	}

	pub fn finish(self) -> Result<Vec<Stmt1>, DecompileError> {
		Ok(self.output)
	}

	fn check_state(&self) -> Result<(), DecompileError> {
		match &self.state {
			State::Temp0(temp0) => error::Temp0Set { temp0: temp0.clone() }.fail(),
			_ => Ok(())
		}
	}

	fn check_empty(&self) -> Result<(), DecompileError> {
		let empty = self.stack.iter().all(|v| *v == StackVal::Null);
		snafu::ensure!(empty, error::NonemptyStack);
		Ok(())
	}

	fn label(&mut self, label: Label) -> Result<(), DecompileError> {
		assert!(self.has_label(label));
		self.check_empty()?;
		if matches!(self.state, State::Ghost) {
			if let Some(&stack) = self.jumps.get(&label) {
				self.state = State::Normal;
				self.stack.resize_with(stack, || StackVal::Null);
				if let Some(Stmt1::Goto(l)) = self.output.last() {
					self.label(*l)?;
				}
			}
		} else {
			let expected = *self.jumps.entry(label).or_insert(self.stack.len());
			if expected != self.stack.len() {
				return error::InconsistentLabel { label, expected, actual: self.stack.len() }.fail();
			}
		}
		Ok(())
	}

	pub fn has_label(&self, label: Label) -> bool {
		self.labels.contains(&label)
	}
}

