use std::collections::{HashSet, VecDeque};

use super::scp;
use scp::{Label, Op, StackSlot};
pub use scp::{Value, Binop, Unop};

#[derive(Debug, Clone, PartialEq)]
pub enum NStmt {
	Return(Option<Expr<StackSlot>>),
	Expr(Expr<StackSlot>),
	Set(Lvalue<StackSlot>, Expr<StackSlot>),
	Label(Label),
	If(Expr<StackSlot>, Label),
	Switch(Expr<StackSlot>),
	Case(i32, Label),
	Goto(Label),
	PushVar,
	PopVar,
	Line(u16),
	Debug(Vec<Expr<StackSlot>>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<T> {
	Value(Value),
	Var(Lvalue<T>),
	Ref(T),
	Call(CallKind, Vec<Expr<T>>),
	Unop(Unop, Box<Expr<T>>),
	Binop(Binop, Box<Expr<T>>, Box<Expr<T>>),
	Line(u16, Box<Expr<T>>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Lvalue<T> {
	Stack(T),
	Deref(T),
	Global(u32),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CallKind {
	System(u8, u8),
	Func(String, String),
	Become(String, String),
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

type Result<T, E = Error> = std::result::Result<T, E>;

macro_rules! pat {
	($($pat:pat $(if $cond:expr)? => $expr:expr),*) => {
		|v| match v {
			$($pat $(if $cond)? => Some($expr),)*
			_ => None,
		}
	}
}

#[derive(Debug)]
pub struct NestedScp {
	pub functions: Vec<(scp::Function, Vec<NStmt>)>,
}

pub fn decompile(f: &scp::Function) -> Result<Vec<NStmt>> {
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

	fn decompile(&mut self, mut l: impl FnMut(NStmt)) -> Result<()> {
		while self.index > 0 {
			self.line(&mut l)?;
		}
		Ok(())
	}

	#[tracing::instrument(skip(self, push), fields(pos = ?self.pos()))]
	fn line(&mut self, mut push: impl FnMut(NStmt)) -> Result<()> {
		match self.next()? {
			Op::Return => {
				self.do_pop(&mut push);
				self.expect(&Op::SetTemp(0))?;
				if let Some(()) = self.next_if(pat!(Op::Push(Value::Uint(0)) => ()))? {
					push(NStmt::Return(None));
				} else {
					let expr = self.expr()?;
					push(NStmt::Return(Some(expr)));
				}
			}
			Op::If(l) => {
				let expr = self.expr()?;
				push(NStmt::If(expr, *l));
			},
			Op::SetTemp(0) => {
				let expr = self.expr()?;
				push(NStmt::Switch(expr));
			}
			Op::If2(l) => {
				self.expect(&Op::Binop(Binop::Eq))?;
				let expr = match self.next()? {
					Op::Push(Value::Int(n)) => *n,
					op => return e::Unexpected { op: op.clone(), what: "switch case" }.fail(),
				};
				self.expect(&Op::GetTemp(0))?;
				push(NStmt::Case(expr, *l));
			}
			Op::Goto(l) => {
				push(NStmt::Goto(*l));
			},
			Op::Call(..) | Op::CallExtern(..) | Op::CallSystem(..) => {
				let expr = self.rewind().call()?;
				let expr = self.maybe_wrap_line(expr);
				push(NStmt::Expr(expr));
			}
			Op::_23(a, b, c) => {
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
				args.reverse();
				let expr = Expr::Call(CallKind::Become(a.clone(), b.clone()), args);
				let expr = self.maybe_wrap_line(expr);
				push(NStmt::Expr(expr));
			}
			Op::Push(Value::Uint(0)) => {
				push(NStmt::PushVar);
			}
			Op::Pop(..) => self.rewind().do_pop(&mut push),
			Op::SetVar(n) => {
				let expr = self.expr()?;
				push(NStmt::Set(Lvalue::Stack(*n), expr));
			}
			Op::SetGlobal(n) => {
				let expr = self.expr()?;
				push(NStmt::Set(Lvalue::Global(*n), expr));
			}
			Op::SetRef(n) => {
				let expr = self.expr()?;
				push(NStmt::Set(Lvalue::Deref(*n), expr));
			}
			Op::Debug(n) => {
				let mut args = Vec::new();
				for _ in 0..*n {
					args.push(self.expr()?);
				}
				push(NStmt::Debug(args));
			}
			Op::Line(n) => {
				push(NStmt::Line(*n));
			}
			op => {
				self.rewind();
				return e::Unexpected { op: op.clone(), what: "statement" }.fail();
			}
		}
		if self.labels.remove(&self.pos()) {
			push(NStmt::Label(self.pos()));
		}
		Ok(())
	}

	fn do_pop(&mut self, mut push: impl FnMut(NStmt)) {
		if let Some(pop) = self.next_if(pat!(Op::Pop(n) => *n)).ok().flatten() {
			for _ in 0..pop/4 {
				push(NStmt::PopVar);
			}
		}
	}

	#[tracing::instrument(skip(self), fields(pos = ?self.pos()))]
	fn expr(&mut self) -> Result<Expr<StackSlot>> {
		let expr = match self.next()? {
			Op::Push(value) => Expr::Value(value.clone()),
			Op::PushRef(n) => Expr::Ref(*n),
			Op::GetVar(n) => Expr::Var(Lvalue::Stack(*n)),
			Op::GetGlobal(n) => Expr::Var(Lvalue::Global(*n)),
			Op::GetRef(n) => Expr::Var(Lvalue::Deref(*n)),
			Op::Binop(op) => {
				let b = self.expr()?;
				let a = self.expr()?;
				Expr::Binop(*op, Box::new(a), Box::new(b))
			},
			Op::Unop(op) => Expr::Unop(*op, Box::new(self.expr()?)),
			Op::GetTemp(0) => self.call()?,
			op => {
				self.rewind();
				return e::Unexpected { op: op.clone(), what: "expression" }.fail();
			}
		};
		Ok(self.maybe_wrap_line(expr))
	}

	#[tracing::instrument(skip(self), fields(pos = ?self.pos()))]
	fn call(&mut self) -> Result<Expr<StackSlot>> {
		let pos = self.pos();
		let mut args = Vec::new();
		let kind = match self.next()? {
			Op::Call(n) => {
				while self.next_if(pat!(Op::Push(Value::Uint(n)) if *n == pos.0 => ()))?.is_none() {
					args.push(self.expr()?);
				}
				self.expect(&Op::Push(Value::Uint(self.function.index)))?;
				CallKind::Func(String::new(), n.clone())
			}
			Op::CallSystem(a, b, c) => {
				for _ in 0..*c {
					args.push(self.expr()?);
				}
				CallKind::System(*a, *b)
			}
			Op::CallExtern(a, b, c) => {
				for _ in 0..*c {
					args.push(self.expr()?);
				}
				self.expect(&Op::_25(pos))?;
				CallKind::Func(a.clone(), b.clone())
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

	fn maybe_wrap_line(&mut self, expr: Expr<StackSlot>) -> Expr<StackSlot> {
		if let Some(line) = self.maybe_line() {
			Expr::Line(line, Box::new(expr))
		} else {
			expr
		}
	}
}
