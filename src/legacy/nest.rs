use std::collections::HashSet;

use crate::legacy::scp::{self, Op};
use crate::legacy::expr::{Binop, Value};
pub use scp::{Label, StackSlot};
use snafu::OptionExt as _;

pub type Expr = crate::legacy::expr::Expr<StackSlot>;
pub type Lvalue = crate::legacy::expr::Lvalue<StackSlot>;
pub use crate::legacy::expr::CallKind;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
	Return(Option<u16>, Option<Expr>),
	Expr(Expr),
	Set(Option<u16>, Lvalue, Expr),
	Label(Label),
	If(Option<u16>, Expr, Label),
	Switch(Option<u16>, Expr),
	Case(i32, Label),
	Goto(Label),
	PushVar(Option<u16>),
	PopVar,
	Debug(Option<u16>, Vec<Expr>),
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
					self.stmts.push(Stmt::Return(None, None));
				} else {
					let expr = self.expr()?;
					self.stmts.push(Stmt::Return(None, Some(expr)));
				}
			}
			Op::If(l) => {
				let expr = self.expr()?;
				self.stmts.push(Stmt::If(None, expr, *l));
			},
			Op::SetTemp(0) => {
				let expr = self.expr()?;
				self.stmts.push(Stmt::Switch(None, expr));
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
					if let Some(last) = args.last_mut() {
						self.maybe_line(last)?;
					}
					args.push(self.expr()?);
				}
				let expr = Expr::Call(None, CallKind::Tail(n.clone()), args);
				self.stmts.push(Stmt::Expr(expr));
			}
			Op::PushSpecial(0) => {
				self.stmts.push(Stmt::PushVar(None));
			}
			Op::Pop(..) => self.rewind().do_pop(),
			Op::SetVar(n) => {
				let expr = self.expr()?;
				self.stmts.push(Stmt::Set(None, Lvalue::Stack(*n), expr));
			}
			Op::SetGlobal(n) => {
				let expr = self.expr()?;
				self.stmts.push(Stmt::Set(None, Lvalue::Global(n.clone()), expr));
			}
			Op::SetRef(n) => {
				let expr = self.expr()?;
				self.stmts.push(Stmt::Set(None, Lvalue::Deref(*n), expr));
			}
			Op::Debug(n) => {
				let mut args = Vec::new();
				for _ in 0..*n {
					if let Some(last) = args.last_mut() {
						self.maybe_line(last)?;
					}
					args.push(self.expr()?);
				}
				self.stmts.push(Stmt::Debug(None, args));
			}
			Op::Line(l) => {
				let mut l = *l;
				let mut lines = Vec::new();
				while self.index > 0
					&& !self.labels.contains(&self.pos())
					&& let Some(n) = self.next_if(pat!(Op::Line(n) => n))?
				{
					lines.push(l);
					l = *n;
				}
				let is_loop = self.labels.contains(&self.pos())
					&& matches!(self.code[self.index - 1].1, Op::Line(_));
				let last_stmt = self.stmts.iter_mut().rfind(|a| !matches!(a, Stmt::Label(_)));
				if is_loop || last_stmt.and_then(self::stmt_line).is_none() {
					lines.push(l);
				}
				if !lines.is_empty() {
					let expr = self.stmts.iter_mut()
						.rfind(|a| !matches!(a, Stmt::Label(_)))
						.and_then(tail_expr)
						.context(e::Unexpected { op: Op::Line(l), what: "expression" }).expect("debug todo");
					add_lines(expr, &lines);
				}
				let last_stmt = self.stmts.iter_mut().rfind(|a| !matches!(a, Stmt::Label(_)));
				if !is_loop && let Some(line) = last_stmt.and_then(self::stmt_line) {
					*line = Some(l);
				}
			}
			op => return e::Unexpected { op: op.clone(), what: "statement" }.fail()
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
	fn expr(&mut self) -> Result<Expr> {
		let expr = match self.next()? {
			Op::Push(value) => Expr::Value(None, value.clone()),
			Op::PushRef(n) => Expr::Ref(None, *n),
			Op::GetVar(n) => Expr::Var(None, Lvalue::Stack(*n)),
			Op::GetGlobal(n) => Expr::Var(None, Lvalue::Global(n.clone())),
			Op::GetRef(n) => Expr::Var(None, Lvalue::Deref(*n)),
			Op::Binop(op) => {
				let mut b = self.expr()?;
				self.maybe_line(&mut b)?;
				let a = self.expr()?;
				Expr::Binop(None, *op, Box::new(a), Box::new(b))
			},
			Op::Unop(op) => {
				let a = self.expr()?;
				Expr::Unop(None, *op, Box::new(a))
			}
			Op::GetTemp(0) => self.call()?,
			op => return e::Unexpected { op: op.clone(), what: "expression" }.fail()
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
					args.push(self.expr()?);
					self.maybe_line(args.last_mut().unwrap())?;
				}
				self.expect(&Op::PushSpecial(self.function.index))?;
				CallKind::Func(n.clone())
			}
			Op::CallSystem(a, b, c) => {
				for _ in 0..*c {
					if let Some(last) = args.last_mut() {
						self.maybe_line(last)?;
					}
					let e = self.expr()?;
					args.push(e);
				}
				CallKind::System(*a, *b)
			}
			Op::CallExtern(n, c) => {
				for _ in 0..*c {
					args.push(self.expr()?);
					self.maybe_line(args.last_mut().unwrap())?;
				}
				self.expect(&Op::_25(pos))?;
				CallKind::Func(n.clone())
			}
			op => return e::Unexpected { op: op.clone(), what: "call" }.fail()
		};
		Ok(Expr::Call(None, kind, args))
	}

	fn maybe_line(&mut self, expr: &mut Expr) -> Result<()> {
		let mut lines = Vec::new();
		while let Some(l) = self.next_if(pat!(Op::Line(l) => l))? {
			lines.push(*l);
		}
		if !lines.is_empty() {
			add_lines(expr, &lines);
		}
		Ok(())
	}
}

fn add_lines(expr: &mut Expr, l: &[u16]) {
	fn tail(expr: &mut Expr) -> Option<&mut Expr> {
		match expr {
			Expr::Call(None, _, a) => a.last_mut(),
			Expr::Unop(None, _, a) => Some(a),
			Expr::Binop(None, _, a, _) => Some(a),
			_ => None,
		}
	}
	fn line_of(expr: &mut Expr) -> Option<&mut Option<u16>> {
		match expr {
			Expr::Value(l, ..) => Some(l),
			Expr::Var(l, ..) => Some(l),
			Expr::Ref(l, ..) => Some(l),
			Expr::Call(l, ..) => Some(l),
			Expr::Unop(l, ..) => Some(l),
			Expr::Binop(l, ..) => Some(l),
		}
	}
	if let [rest @ .., l] = l {
		if !rest.is_empty() {
			if let Some(e) = tail(expr) {
				add_lines(e, rest);
			} else {
				println!("tail {:?}", rest);
			}
		}
		if let Some(out) = line_of(expr) {
			*out = Some(*l);
		}
	}
}

fn tail_expr(stmt: &mut Stmt) -> Option<&mut Expr> {
	match stmt {
		Stmt::Return(_, e) => e.as_mut(),
		Stmt::Expr(e) => Some(e),
		Stmt::Set(_, _, e) => Some(e),
		Stmt::Label(_) => None,
		Stmt::If(_, e, _) => Some(e),
		Stmt::Switch(_, e) => Some(e),
		Stmt::Case(_, _) => None,
		Stmt::Goto(_) => None,
		Stmt::PushVar(_) => None,
		Stmt::PopVar => None,
		Stmt::Debug(_, es) => es.last_mut(),
	}
}

fn stmt_line(stmt: &mut Stmt) -> Option<&mut Option<u16>> {
	match stmt {
		Stmt::Return(l, _) => Some(l),
		Stmt::Expr(_) => None,
		Stmt::Set(l, _, _) => Some(l),
		Stmt::Label(_) => None,
		Stmt::If(l, _, _) => Some(l),
		Stmt::Switch(l, _) => Some(l),
		Stmt::Case(_, _) => None,
		Stmt::Goto(_) => None,
		Stmt::PushVar(l) => Some(l),
		Stmt::PopVar => None,
		Stmt::Debug(l, _) => Some(l),
	}
}
