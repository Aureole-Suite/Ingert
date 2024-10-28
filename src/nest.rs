use std::collections::HashSet;

use crate::scp::{self, Op};
use crate::expr::{Binop, Value};
pub use scp::{Label, StackSlot};
use snafu::OptionExt as _;

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
		stmts: Vec::new(),
	};

	while ctx.index > 0 {
		ctx.line()?;
	}

	if let Some(label) = ctx.labels.into_iter().next() {
		return e::MissingLabel { label }.fail();
	}
	ctx.stmts.reverse();
	Ok(ctx.stmts)
}

struct Ctx<'a> {
	function: &'a scp::Function,
	labels: HashSet<Label>,
	code: &'a [(Label, Op)],
	end: Label,
	index: usize,
	stmts: Vec<Stmt>,
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

	#[tracing::instrument(skip(self), fields(pos = ?self.pos()))]
	fn line(&mut self) -> Result<()> {
		match self.next()? {
			Op::Return => {
				self.do_pop();
				self.expect(&Op::SetTemp(0))?;
				if let Some(()) = self.next_if(pat!(Op::PushSpecial(0) => ()))? {
					self.stmts.push(Stmt::Return(None));
				} else {
					let expr = self.expr(None)?;
					self.stmts.push(Stmt::Return(Some(expr)));
				}
			}
			Op::If(l) => {
				let expr = self.expr(None)?;
				self.stmts.push(Stmt::If(expr, *l));
			},
			Op::SetTemp(0) => {
				let expr = self.expr(None)?;
				self.stmts.push(Stmt::Switch(expr));
			}
			Op::If2(l) => {
				self.expect(&Op::Binop(Binop::Eq))?;
				let expr = match self.next()? {
					Op::Push(Value::Int(n)) => *n,
					op => return e::Unexpected { op: op.clone(), what: "switch case" }.fail(),
				};
				self.expect(&Op::GetTemp(0))?;
				self.stmts.push(Stmt::Case(expr, *l));
			}
			Op::Goto(l) => {
				self.stmts.push(Stmt::Goto(*l));
			},
			Op::Call(..) | Op::CallExtern(..) | Op::CallSystem(..) => {
				let expr = self.rewind().call()?;
				self.stmts.push(Stmt::Expr(expr));
			}
			Op::CallTail(n, c) => {
				for i in 1..=*c {
					self.expect(&Op::GetTemp(i))?;
				}
				self.do_pop();
				for i in (1..=*c).rev() {
					self.expect(&Op::SetTemp(i))?;
				}
				let mut args = Vec::new();
				for _ in 0..*c {
					args.push(self.expr(None)?);
				}
				let expr = Expr::Call(CallKind::Tail(n.clone()), args);
				self.stmts.push(Stmt::Expr(expr));
			}
			Op::PushSpecial(0) => {
				self.stmts.push(Stmt::PushVar);
			}
			Op::Pop(..) => self.rewind().do_pop(),
			Op::SetVar(n) => {
				let expr = self.expr(None)?;
				self.stmts.push(Stmt::Set(Lvalue::Stack(*n), expr));
			}
			Op::SetGlobal(n) => {
				let expr = self.expr(None)?;
				self.stmts.push(Stmt::Set(Lvalue::Global(n.clone()), expr));
			}
			Op::SetRef(n) => {
				let expr = self.expr(None)?;
				self.stmts.push(Stmt::Set(Lvalue::Deref(*n), expr));
			}
			Op::Debug(n) => {
				let mut args = Vec::new();
				for _ in 0..*n {
					let e = self.expr(args.last_mut())?;
					args.push(e);
				}
				self.stmts.push(Stmt::Debug(args));
			}
			Op::Line(l) => {
				let mut l = *l;
				while self.index > 0 && let Some(n) = self.next_if(pat!(Op::Line(n) => n))? {
					let expr = self.stmts.last_mut()
						.and_then(tail_expr)
						.context(e::Unexpected { op: Op::Line(*n), what: "expression" }).expect("debug todo");
					add_line(expr, l);
					l = *n;
				}
				self.stmts.push(Stmt::Line(l));
			}
			op => {
				self.rewind();
				return e::Unexpected { op: op.clone(), what: "statement" }.fail();
			}
		}
		if self.labels.remove(&self.pos()) {
			self.stmts.push(Stmt::Label(self.pos()));
		}
		Ok(())
	}

	fn do_pop(&mut self) {
		if let Some(pop) = self.next_if(pat!(Op::Pop(n) => *n)).ok().flatten() {
			for _ in 0..pop/4 {
				self.stmts.push(Stmt::PopVar);
			}
		}
	}

	#[tracing::instrument(skip(self), fields(pos = ?self.pos()))]
	fn expr(&mut self, mut last: Option<&mut Expr>) -> Result<Expr> {
		let expr = match self.next()? {
			Op::Push(value) => Expr::Value(value.clone()),
			Op::PushRef(n) => Expr::Ref(*n),
			Op::GetVar(n) => Expr::Var(Lvalue::Stack(*n)),
			Op::GetGlobal(n) => Expr::Var(Lvalue::Global(n.clone())),
			Op::GetRef(n) => Expr::Var(Lvalue::Deref(*n)),
			Op::Binop(op) => {
				let mut b = self.expr(last)?;
				let a = self.expr(Some(&mut b))?;
				Expr::Binop(*op, Box::new(a), Box::new(b))
			},
			Op::Unop(op) => {
				let a = self.expr(last)?;
				Expr::Unop(*op, Box::new(a))
			}
			Op::GetTemp(0) => self.call()?,
			Op::Line(l) => {
				let last0 = last.as_mut().context(e::Unexpected { op: Op::Line(*l), what: "expression" }).expect("debug todo");
				add_line(last0, *l);
				self.expr(last)?
			}
			op => {
				self.rewind();
				return e::Unexpected { op: op.clone(), what: "expression" }.fail();
			}
		};
		Ok(expr)
	}

	#[tracing::instrument(skip(self), fields(pos = ?self.pos()))]
	fn call(&mut self) -> Result<Expr> {
		let pos = self.pos();
		let mut args = Vec::new();
		let kind = match self.next()? {
			Op::Call(n) => {
				while self.next_if(pat!(Op::PushSpecial(n) if *n == pos.0 => ()))?.is_none() {
					args.push(self.expr(None)?);
					self.maybe_line(args.last_mut().unwrap())?;
				}
				self.expect(&Op::PushSpecial(self.function.index))?;
				CallKind::Func(n.clone())
			}
			Op::CallSystem(a, b, c) => {
				for _ in 0..*c {
					let e = self.expr(args.last_mut())?;
					args.push(e);
				}
				CallKind::System(*a, *b)
			}
			Op::CallExtern(n, c) => {
				for _ in 0..*c {
					args.push(self.expr(None)?);
					self.maybe_line(args.last_mut().unwrap())?;
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

	fn maybe_line(&mut self, expr: &mut Expr) -> Result<()> {
		while let Some(l) = self.next_if(pat!(Op::Line(l) => l))? {
			add_line(expr, *l);
		}
		Ok(())
	}
}

fn add_line(expr: &mut Expr, l: u16) {
	let e = std::mem::replace(expr, Expr::Value(Value::Int(0)));
	*expr = Expr::Line(l, Box::new(e));
}

fn tail_expr(stmt: &mut Stmt) -> Option<&mut Expr> {
	match stmt {
		Stmt::Return(e) => e.as_mut(),
		Stmt::Expr(e) => Some(e),
		Stmt::Set(_, e) => Some(e),
		Stmt::Label(_) => None,
		Stmt::If(e, _) => Some(e),
		Stmt::Switch(e) => Some(e),
		Stmt::Case(_, _) => None,
		Stmt::Goto(_) => None,
		Stmt::PushVar => None,
		Stmt::PopVar => None,
		Stmt::Line(_) => None,
		Stmt::Debug(es) => es.last_mut(),
	}
}
