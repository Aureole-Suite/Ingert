use std::collections::{HashSet, VecDeque};

use super::scp;
use scp::{Label, Op, Scp, StackSlot};
pub use scp::{Value, Binop, Unop};

#[derive(Debug, Clone, PartialEq)]
enum Stmt {
	Return(Expr<StackSlot>),
	ReturnVoid(Option<u16>),
	Expr(Expr<StackSlot>),
	Set(Lvalue<StackSlot>, Expr<StackSlot>),
	Label(Label),
	If(Expr<StackSlot>, Label),
	Switch(Expr<StackSlot>),
	Case(i32, Label),
	Goto(Label),
	PushVar(Option<u16>),
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
pub enum StackVar {
	Stack(u32),
	Arg(u32),
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
	_23(String, String),
}

macro_rules! pat {
	($($pat:pat => $expr:expr),*) => {
		|v| match v {
			$($pat => Some($expr),)*
			_ => None,
		}
	}
}

pub fn decompile(out: &mut super::Write, scp: &Scp) {
	let mut functions = scp.functions.iter().collect::<Vec<_>>();
	functions.sort_by_key(|f| f.start);

	let mut start = 0;
	let ends = functions
		.iter()
		.skip(1)
		.map(|f| f.start)
		.chain(Some(scp.code_end));
	for (f, end) in std::iter::zip(&functions, ends) {
		let _span = tracing::info_span!("function", name = f.name.clone()).entered();
		let code = &scp.code[start..];
		let length = if end == scp.code_end {
			code.len()
		} else {
			code.binary_search_by_key(&end, |(l, _)| *l).unwrap()
		};
		let code = &code[..length];
		start += length;

		let labels = code.iter()
			.map(|a| &a.1)
			.filter_map(pat!(Op::If(l) | Op::If2(l) | Op::Goto(l) => *l))
			.collect();

		let mut ctx = Ctx {
			function: f,
			labels,
			code,
			end,
			index: code.len(),
		};

		writeln!(out, "function {f}");

		let mut lines = VecDeque::new();
		while ctx.line(|l| lines.push_front(l)).is_some() {}


		// for (pos, op) in code {
		// 	writeln!(out, "  {pos:?}: {op:?}");
		// }
		// writeln!(out);

		for line in lines.iter() {
			writeln!(out, "  {line:?}");
		}
		writeln!(out);

		if ctx.index != 0 {
			println!("{f}");
			for (pos, line) in code {
				println!("  {pos:?}: {line:?}");
			}
			println!();
			for line in lines.iter() {
				println!("  {line:?}");
			}
		}
	}
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

	fn next(&mut self) -> Option<&'a Op> {
		if self.index == 0 {
			return None;
		}
		self.index -= 1;
		let op = &self.code[self.index].1;
		Some(op)
	}

	fn peek(&mut self) -> Option<&'a Op> {
		let p = self.index;
		self.next().inspect(|_| self.index = p)
	}

	fn expect(&mut self, op: &Op) -> Option<()> {
		let next = self.next()?;
		if next != op {
			tracing::info!("expected {:?}, got {:?}", op, next);
			return None;
		}
		Some(())
	}

	fn next_if<T>(&mut self, pat: impl Fn(&Op) -> Option<T>) -> Option<T> {
		let next = self.peek()?;
		pat(next).inspect(|_| self.index -= 1)
	}

	#[tracing::instrument(skip(self, push), fields(pos = ?self.pos()))]
	fn line(&mut self, mut push: impl FnMut(Stmt)) -> Option<()> {
		match self.next()? {
			Op::Return => {
				self.do_pop(&mut push)?;
				self.expect(&Op::SetTemp(0))?;
				if let Some(()) = self.next_if(pat!(Op::Push(Value::Uint(0)) => ())) {
					let line = self.maybe_line();
					push(Stmt::ReturnVoid(line));
				} else {
					let expr = self.expr()?;
					push(Stmt::Return(expr));
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
				let expr = self.next_if(pat!(Op::Push(Value::Int(n)) => *n))?;
				self.expect(&Op::GetTemp(0))?;
				push(Stmt::Case(expr, *l));
			}
			Op::Goto(l) => {
				self.do_pop(&mut push)?;
				push(Stmt::Goto(*l));
			},
			Op::Call(..) | Op::_23(..) | Op::CallExtern(..) | Op::CallSystem(..) => {
				let expr = self.rewind().call()?;
				let expr = self.maybe_wrap_line(expr);
				push(Stmt::Expr(expr));
			}
			Op::Push(Value::Uint(0)) => {
				let line = self.maybe_line();
				push(Stmt::PushVar(line));
			}
			Op::Pop(..) => self.rewind().do_pop(&mut push)?,
			Op::SetVar(n) => {
				let expr = self.expr()?;
				push(Stmt::Set(Lvalue::Stack(*n), expr));
			}
			Op::SetGlobal(n) => {
				let expr = self.expr()?;
				push(Stmt::Set(Lvalue::Global(*n), expr));
			}
			Op::Debug(n) => {
				let mut args = Vec::new();
				for _ in 0..*n {
					args.push(self.expr()?);
				}
				push(Stmt::Debug(args));
			}
			Op::Line(n) => {
				push(Stmt::Line(*n));
			}
			op => {
				self.rewind();
				tracing::info!("unexpected {:?}", op);
				return None
			}
		}
		if self.labels.remove(&self.pos()) {
			push(Stmt::Label(self.pos()));
		}
		Some(())
	}

	fn do_pop(&mut self, mut push: impl FnMut(Stmt)) -> Option<()> {
		if let Some(pop) = self.next_if(pat!(Op::Pop(n) => *n)) {
			for _ in 0..pop/4 {
				push(Stmt::PopVar);
			}
		}
		Some(())
	}

	#[tracing::instrument(skip(self), fields(pos = ?self.pos()))]
	fn expr(&mut self) -> Option<Expr<StackSlot>> {
		let expr = match self.next()? {
			Op::Push(value) => Expr::Value(value.clone()),
			Op::PushRef(n) => Expr::Ref(*n),
			Op::GetVar(n) => Expr::Var(Lvalue::Stack(*n)),
			Op::GetGlobal(n) => Expr::Var(Lvalue::Global(*n)),
			Op::Binop(op) => {
				let b = self.expr()?;
				let a = self.expr()?;
				Expr::Binop(*op, Box::new(a), Box::new(b))
			},
			Op::Unop(op) => Expr::Unop(*op, Box::new(self.expr()?)),
			Op::GetTemp(0) => self.call()?,
			op => {
				self.rewind();
				tracing::info!("unexpected {:?}", op);
				return None
			}
		};
		Some(self.maybe_wrap_line(expr))
	}

	#[tracing::instrument(skip(self), fields(pos = ?self.pos()))]
	fn call(&mut self) -> Option<Expr<StackSlot>> {
		let pos = self.pos();
		Some(match self.next()? {
			Op::Call(n) => {
				let mut args = Vec::new();
				loop {
					if self.peek() == Some(&Op::Push(Value::Uint(pos.0))) {
						break;
					}
					args.push(self.expr()?);
				}
				self.expect(&Op::Push(Value::Uint(pos.0)))?;
				self.expect(&Op::Push(Value::Uint(self.function.index)))?;
				Expr::Call(CallKind::Func(String::new(), n.clone()), args)
			}
			Op::CallSystem(a, b, c) => {
				let mut args = Vec::new();
				for _ in 0..*c {
					args.push(self.expr()?);
				}
				Expr::Call(CallKind::System(*a, *b), args)
			}
			Op::CallExtern(a, b, c) => {
				let mut args = Vec::new();
				for _ in 0..*c {
					args.push(self.expr()?);
				}
				self.expect(&Op::_25(pos))?;
				Expr::Call(CallKind::Func(a.clone(), b.clone()), args)
			}
			op => {
				self.rewind();
				tracing::info!("unexpected {:?}", op);
				return None
			}
		})
	}

	fn maybe_line(&mut self) -> Option<u16> {
		self.next_if(pat!(Op::Line(n) => *n))
	}

	fn maybe_wrap_line(&mut self, expr: Expr<StackSlot>) -> Expr<StackSlot> {
		if let Some(line) = self.maybe_line() {
			Expr::Line(line, Box::new(expr))
		} else {
			expr
		}
	}
}