use std::collections::{HashSet, VecDeque};

use crate::scp::{self, Op};
use crate::expr::{Binop, Value};
pub use scp::{Label, StackSlot};

pub type Expr = crate::expr::Expr<StackSlot>;
pub type Lvalue = crate::expr::Lvalue<StackSlot>;
pub use crate::expr::CallKind;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
	Return(Option<Expr>),
	Expr(Expr),
	Set(Lvalue, Expr),
	Label(Label),
	If(Expr, Label),
	Switch(Expr),
	Case(i32, Label),
	Goto(Label),
	PushVar,
	PopVar,
	Line(u16),
	Debug(Vec<Expr>),
}

#[derive(Debug, snafu::Snafu)]
#[snafu(module(e), context(suffix(false)))]
pub enum Error {
	#[snafu(display("unexpected {op:?} when parsing {what}"))]
	Unexpected { op: Op, what: &'static str },
	#[snafu(display("expected {expected:?}, got {got:?}"))]
	Expected { expected: Op, got: Op },
	#[snafu(display("unexpected end of code"))]
	End,
	#[snafu(display("missing label {label:?}"))]
	MissingLabel { label: Label },
}

mod display {
	use super::*;
	use std::fmt::{Display, Formatter, Result};

	impl Display for Label {
		fn fmt(&self, f: &mut Formatter<'_>) -> Result {
			std::fmt::Debug::fmt(self, f)
		}
	}

	impl Display for StackSlot {
		fn fmt(&self, f: &mut Formatter<'_>) -> Result {
			write!(f, "s[{}]", self.0)
		}
	}

	impl Display for Stmt {
		fn fmt(&self, f: &mut Formatter<'_>) -> Result {
			match self {
				Stmt::Return(None) => write!(f, "return")?,
				Stmt::Return(Some(e)) => write!(f, "return {e}")?,
				Stmt::Expr(e) => write!(f, "{e}")?,
				Stmt::Set(v, e) => write!(f, "{v} = {e}")?,
				Stmt::Label(l) => write!(f, "{l}:")?,
				Stmt::If(e, l) => write!(f, "if {e} goto {l}")?,
				Stmt::Switch(e) => write!(f, "switch {e}")?,
				Stmt::Case(n, l) => write!(f, "case {n} goto {l}")?,
				Stmt::Goto(l) => write!(f, "goto {l}")?,
				Stmt::PushVar => write!(f, "push")?,
				Stmt::PopVar => write!(f, "pop")?,
				Stmt::Line(l) => write!(f, "line {l}")?,
				Stmt::Debug(args) => {
					write!(f, "debug")?;
					crate::expr::write_args(f, args)?;
				}
			}
			Ok(())
		}
	}
}

type Result<T, E = Error> = std::result::Result<T, E>;

macro_rules! pat {
	($($pat:pat $(if $cond:expr)? => $expr:expr),*) => {
		|v| match v {
			$($pat $(if $cond)? => Some($expr),)*
			_ => None,
		}
	}
}

pub fn decompile(f: &scp::Function) -> Result<Vec<Stmt>> {
	let _span = tracing::info_span!("function", name = f.name.clone()).entered();

	let labels = f.code.iter()
		.map(|a| &a.1)
		.filter_map(pat!(Op::If(l) | Op::If2(l) | Op::Goto(l) => *l))
		.collect();

	let mut ctx = Ctx {
		function: f,
		labels,
		code: &f.code,
		end: f.code_end,
		index: f.code.len(),
	};

	let mut lines = VecDeque::new();
	ctx.decompile(|l| lines.push_front(l))?;

	if let Some(label) = ctx.labels.into_iter().next() {
		return e::MissingLabel { label }.fail();
	}

	Ok(Vec::from(lines))
}

struct Ctx<'a> {
	function: &'a scp::Function,
	labels: HashSet<Label>,
	code: &'a [(Label, Op)],
	end: Label,
	index: usize,
}

impl<'a> Ctx<'a> {
	fn pos(&self) -> Label {
		if self.index == self.code.len() {
			self.end
		} else {
			self.code[self.index].0
		}
	}

	fn rewind(&mut self) -> &mut Self {
		self.index += 1;
		self
	}

	fn next_if<T>(&mut self, pat: impl Fn(&'a Op) -> Option<T>) -> Result<Option<T>> {
		if self.index == 0 {
			return e::End.fail();
		}
		if let Some(v) = pat(&self.code[self.index - 1].1) {
			self.index -= 1;
			Ok(Some(v))
		} else {
			Ok(None)
		}
	}

	fn next(&mut self) -> Result<&'a Op> {
		self.next_if(Some).map(Option::unwrap)
	}

	fn expect(&mut self, op: &Op) -> Result<()> {
		let next = self.next()?;
		if next != op {
			return e::Expected { expected: op.clone(), got: next.clone() }.fail();
		}
		Ok(())
	}

	fn decompile(&mut self, mut l: impl FnMut(Stmt)) -> Result<()> {
		while self.index > 0 {
			self.line(&mut l)?;
		}
		Ok(())
	}

	#[tracing::instrument(skip(self, push), fields(pos = ?self.pos()))]
	fn line(&mut self, mut push: impl FnMut(Stmt)) -> Result<()> {
		match self.next()? {
			Op::Return => {
				self.do_pop(&mut push);
				self.expect(&Op::SetTemp(0))?;
				if let Some(()) = self.next_if(pat!(Op::PushSpecial(0) => ()))? {
					push(Stmt::Return(None));
				} else {
					let expr = self.expr()?;
					push(Stmt::Return(Some(expr)));
				}
			}
			Op::If(l) => {
				let expr = self.expr()?;
				push(Stmt::If(expr, *l));
			},
			Op::SetTemp(0) => {
				let expr = self.expr()?;
				push(Stmt::Switch(expr));
			}
			Op::If2(l) => {
				self.expect(&Op::Binop(Binop::Eq))?;
				let expr = match self.next()? {
					Op::Push(Value::Int(n)) => *n,
					op => return e::Unexpected { op: op.clone(), what: "switch case" }.fail(),
				};
				self.expect(&Op::GetTemp(0))?;
				push(Stmt::Case(expr, *l));
			}
			Op::Goto(l) => {
				push(Stmt::Goto(*l));
			},
			Op::Call(..) | Op::CallExtern(..) | Op::CallSystem(..) => {
				let expr = self.rewind().call()?;
				push(Stmt::Expr(expr));
			}
			Op::CallTail(n, c) => {
				for i in 1..=*c {
					self.expect(&Op::GetTemp(i))?;
				}
				self.do_pop(&mut push);
				for i in (1..=*c).rev() {
					self.expect(&Op::SetTemp(i))?;
				}
				let mut args = Vec::new();
				for _ in 0..*c {
					args.push(self.expr()?);
				}
				let expr = Expr::Call(CallKind::Tail(n.clone()), args);
				push(Stmt::Expr(expr));
			}
			Op::PushSpecial(0) => {
				push(Stmt::PushVar);
			}
			Op::Pop(..) => self.rewind().do_pop(&mut push),
			Op::SetVar(n) => {
				let expr = self.expr()?;
				push(Stmt::Set(Lvalue::Stack(*n), expr));
			}
			Op::SetGlobal(n) => {
				let expr = self.expr()?;
				push(Stmt::Set(Lvalue::Global(n.clone()), expr));
			}
			Op::SetRef(n) => {
				let expr = self.expr()?;
				push(Stmt::Set(Lvalue::Deref(*n), expr));
			}
			Op::Debug(n) => {
				let mut args = Vec::new();
				for _ in 0..*n {
					args.push(self.expr_line()?);
				}
				push(Stmt::Debug(args));
			}
			Op::Line(n) => {
				push(Stmt::Line(*n));
			}
			op => {
				self.rewind();
				return e::Unexpected { op: op.clone(), what: "statement" }.fail();
			}
		}
		if self.labels.remove(&self.pos()) {
			push(Stmt::Label(self.pos()));
		}
		Ok(())
	}

	fn do_pop(&mut self, mut push: impl FnMut(Stmt)) {
		if let Some(pop) = self.next_if(pat!(Op::Pop(n) => *n)).ok().flatten() {
			for _ in 0..pop/4 {
				push(Stmt::PopVar);
			}
		}
	}

	#[tracing::instrument(skip(self), fields(pos = ?self.pos()))]
	fn expr(&mut self) -> Result<Expr> {
		let expr = match self.next()? {
			Op::Push(value) => Expr::Value(value.clone()),
			Op::PushRef(n) => Expr::Ref(*n),
			Op::GetVar(n) => Expr::Var(Lvalue::Stack(*n)),
			Op::GetGlobal(n) => Expr::Var(Lvalue::Global(n.clone())),
			Op::GetRef(n) => Expr::Var(Lvalue::Deref(*n)),
			Op::Binop(op) => {
				let b = self.expr_line()?;
				let a = self.expr_line()?;
				Expr::Binop(*op, Box::new(a), Box::new(b))
			},
			Op::Unop(op) => {
				let a = self.expr_line()?;
				Expr::Unop(*op, Box::new(a))
			}
			Op::GetTemp(0) => self.call()?,
			op => {
				self.rewind();
				return e::Unexpected { op: op.clone(), what: "expression" }.fail();
			}
		};
		Ok(expr)
	}

	fn expr_line(&mut self) -> Result<Expr> {
		let expr = self.expr()?;
		if let Some(line) = self.maybe_line() {
			Ok(Expr::Line(line, Box::new(expr)))
		} else {
			Ok(expr)
		}
	}

	#[tracing::instrument(skip(self), fields(pos = ?self.pos()))]
	fn call(&mut self) -> Result<Expr> {
		let pos = self.pos();
		let mut args = Vec::new();
		let kind = match self.next()? {
			Op::Call(n) => {
				while self.next_if(pat!(Op::PushSpecial(n) if *n == pos.0 => ()))?.is_none() {
					args.push(self.expr_line()?);
				}
				self.expect(&Op::PushSpecial(self.function.index))?;
				CallKind::Func(n.clone())
			}
			Op::CallSystem(a, b, c) => {
				for _ in 0..*c {
					args.push(self.expr_line()?);
				}
				CallKind::System(*a, *b)
			}
			Op::CallExtern(n, c) => {
				for _ in 0..*c {
					args.push(self.expr()?); // Wonder why no line numbers here
				}
				self.expect(&Op::_25(pos))?;
				CallKind::Func(n.clone())
			}
			op => {
				self.rewind();
				return e::Unexpected { op: op.clone(), what: "call" }.fail();
			}
		};
		Ok(Expr::Call(kind, args))
	}

	fn maybe_line(&mut self) -> Option<u16> {
		if self.labels.contains(&self.pos()) {
			None
		} else {
			self.next_if(pat!(Op::Line(n) => *n)).ok().flatten()
		}
	}
}
