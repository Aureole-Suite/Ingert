#![feature(let_chains, is_sorted, is_none_or)]
use std::cell::Cell;
use std::collections::{HashMap, VecDeque};
use std::fmt::Display;

mod scp;

use scp::{Label, Op, Scp};
pub use scp::{Value, Binop, Unop};
pub use scp::parse_scp;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
	Value(Value),
	Var(Lvalue),
	Ref(StackVar),
	Call(CallKind, Vec<Expr>),
	Unop(Unop, Box<Expr>),
	Binop(Binop, Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum StackVar {
	Stack(u32),
	Arg(u32),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Lvalue {
	Stack(StackVar),
	Deref(StackVar),
	Local(u32),
	Global(u8),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CallKind {
	System(u8, u8),
	Func(String, String),
	_23(String, String),
}

struct Args<'a>(&'a [Expr]);
impl Display for Args<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		format_exprs(f, self.0)
	}
}

fn format_exprs(f: &mut std::fmt::Formatter<'_>, args: &[Expr]) -> std::fmt::Result {
	f.write_str("(")?;
	let mut it = args.iter();
	if let Some(a) = it.next() {
		a.fmt(f)?;
		for a in it {
			f.write_str(", ")?;
			a.fmt(f)?;
		}
	}
	f.write_str(")")
}

impl Display for Expr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Expr::Value(v) => v.fmt(f),
			Expr::Var(n) => n.fmt(f),
			Expr::Ref(n) => {
				f.write_str("&")?;
				n.fmt(f)
			}
			Expr::Call(k, a) => {
				k.fmt(f)?;
				format_exprs(f, a)
			}
			Expr::Unop(v, a) => {
				v.fmt(f)?;
				f.write_str("(")?;
				a.fmt(f)?;
				f.write_str(")")
			}
			Expr::Binop(v, a, b) => {
				f.write_str("(")?;
				a.fmt(f)?;
				f.write_str(" ")?;
				v.fmt(f)?;
				f.write_str(" ")?;
				b.fmt(f)?;
				f.write_str(")")
			}
		}
	}
}

impl Display for Lvalue {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Stack(v) => v.fmt(f),
			Self::Deref(v) => {
				f.write_str("*")?;
				v.fmt(f)
			}
			Self::Local(v) => write!(f, "local[{}]", v),
			Self::Global(v) => write!(f, "global[{}]", v),
		}
	}
}

impl Display for StackVar {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Stack(v) => write!(f, "stack[{}]", v),
			Self::Arg(v) => write!(f, "arg[{}]", v),
		}
	}
}

impl Display for CallKind {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			CallKind::System(a, b) => write!(f, "system[{}, {}]", a, b),
			CallKind::Func(a, b) => write!(f, "_22 {}.{}", a, b),
			CallKind::_23(a, b) => write!(f, "_23 {}.{}", a, b),
		}
	}
}

struct Ctx<'a> {
	functions: &'a [scp::Function],
	code: &'a [(Label, Op)],
	code_end: Label,

	stack: VecDeque<Expr>,
	current_func: u32,
	pos: usize,
	indent: Indent,
	cont: Option<Label>,
	brk: Option<Label>,
	popped_last: bool,
	nargs: usize,
}

impl<'a> Ctx<'a> {
	fn pos(&self) -> Label {
		match self.code.get(self.pos) {
			Some(t) => t.0,
			None => self.code_end,
		}
	}

	fn next(&mut self) -> Option<&'a Op> {
		let v = self.peek();
		self.pos += 1;
		v
	}

	fn peek(&self) -> Option<&'a Op> {
		if self.pos == self.code.len() || (self.popped_last && self.pos == self.code.len() - 1) {
			None
		} else {
			Some(&self.code[self.pos].1)
		}
	}

	fn push(&mut self, e: Expr) {
		self.stack.push_front(e);
	}

	#[track_caller]
	fn pop(&mut self) -> Expr {
		self.stack.pop_front().unwrap()
	}

	fn pop_n(&mut self, n: usize) -> Vec<Expr> {
		self.stack.drain(..n).collect()
	}

	fn push_call(&mut self, call: CallKind, args: Vec<Expr>) {
		let call = Expr::Call(call, args);
		if self.peek() == Some(&Op::GetGlobal(0)) {
			self.next();
			self.stack.push_front(call);
		} else {
			println!("{i}{call}", i=self.indent);
		}
	}

	fn sub(&mut self, target: Label) -> Ctx<'a> {
		let index = if target == self.code_end {
			self.code.len()
		} else {
			self.code.binary_search_by_key(&target, |(l, _)| *l).unwrap()
		};
		assert!(index >= self.pos);
		let new = Ctx {
			functions: self.functions,
			code: &self.code[self.pos..index],
			code_end: target,
			stack: self.stack.clone(),
			current_func: self.current_func,
			pos: 0,
			indent: self.indent.inc(),
			cont: self.cont,
			brk: self.brk,
			popped_last: self.popped_last && index == self.code.len(),
			nargs: self.nargs,
		};
		self.pos = index;
		new
	}

	fn last_goto(&mut self, target: impl Fn(Label) -> bool) -> Option<Label> {
		if let Some((_, Op::Goto(t))) = self.code.last()
			&& target(*t)
			&& self.cont != Some(*t)
			&& self.brk != Some(*t)
		{
			self.popped_last = true;
			Some(*t)
		} else {
			None
		}
	}

	fn resolve(&self, n: scp::StackSlot) -> StackVar {
		let v = n.0 + self.stack.len() as i32;
		if v < 0 {
			StackVar::Arg(!v as u32)
		} else {
			StackVar::Stack(v as u32)
		}
	}
}

pub fn stuff(scp: &Scp) {
	let mut ctx = Ctx {
		functions: &scp.functions,
		code: &scp.code,
		code_end: scp.code_end,
		stack: VecDeque::new(),
		current_func: 0,
		pos: 0,
		indent: Indent(0),
		cont: None,
		brk: None,
		popped_last: false,
		nargs: 0,
	};

	let mut functions = scp.functions.iter().collect::<Vec<_>>();
	functions.sort_by_key(|f| f.start);

	let ends = functions
		.iter()
		.skip(1)
		.map(|f| f.start)
		.chain(Some(scp.code_end));
	for (f, end) in std::iter::zip(&functions, ends) {
		assert_eq!(ctx.pos(), f.start);
		ctx.current_func = f.index;
		ctx.nargs = f.args.len();
		let sub = ctx.sub(end);

		// println!("\n{f}");
		// scp::dump_ops(sub.code);

		println!("\n{f}");
		stmts(sub);
	}
}

#[derive(Debug, Clone, Copy, PartialEq)]
struct Indent(usize);
impl Indent {
	fn inc(&self) -> Self {
		Self(self.0 + 1)
	}
}
impl Display for Indent {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		for _ in 0..self.0 {
			write!(f, "  ")?;
		}
		Ok(())
	}
}

fn switch_cases(ctx: &mut Ctx<'_>) -> Vec<(Option<i32>, Label)> {
	let mut cases = Vec::new();
	let default = loop {
		match &ctx.next().unwrap() {
			Op::GetGlobal(0) => {
				let Op::Push(Value::Int(n)) = ctx.next().unwrap() else { unreachable!() };
				let Op::Binop(Binop::Eq) = ctx.next().unwrap() else { unreachable!() };
				let Op::If2(target) = ctx.next().unwrap() else { unreachable!() };
				cases.push((Some(*n), *target));
			}
			Op::Goto(default) => break *default,
			op => unreachable!("unexpected {op:?} in switch"),
		}
	};
	assert!(cases.is_sorted_by_key(|(_, a)| a));
	let default_pos = cases.partition_point(|(_, a)| *a < default);
	cases.insert(default_pos, (None, default));
	cases
}

fn stmts(mut ctx: Ctx<'_>) {
	// let depth = ctx.stack.len();
	while ctx.peek().is_some() {
		stmt(&mut ctx);
	}
	// assert_eq!(ctx.stack.len(), depth);
}

fn stmt(ctx: &mut Ctx<'_>) {
	let i = ctx.indent;
	match ctx.next().unwrap() {
		Op::Return => {
			println!("{i}(end)");
			// assert_eq!(ctx.stack, &[]);
		}

		Op::If(target) => {
			let a = ctx.pop();
			let mut sub = ctx.sub(*target);
			let the_else = sub.last_goto(|l| (l >= *target && l <= ctx.code_end) || l < ctx.pos());
			if let Some(the_cont) = the_else && the_cont < ctx.pos() {
				// This check is not correct really; I need to know where the arg actually started
				println!("{i}while {a} {{");
				sub.brk = Some(*target);
				sub.cont = Some(the_cont);
				stmts(sub);
				println!("{i}}}");
			} else if let Some(the_else) = the_else {
				println!("{i}if {a} {{");
				stmts(sub);
				println!("{i}}} else {{");
				let sub = ctx.sub(the_else);
				stmts(sub);
				println!("{i}}}");
			} else {
				println!("{i}if {a} {{");
				stmts(sub);
				println!("{i}}}");
			}
		}

		Op::SetGlobal(0) if ctx.peek() == Some(&Op::GetGlobal(0)) => {
			let a = ctx.pop();
			let cases = switch_cases(ctx);
			println!("{i}switch {a} {{");
			let the_end = Cell::new(None);
			let ends = cases.iter().skip(1).map(|i| i.1).chain(std::iter::once_with(|| the_end.get()).flatten());
			for ((key, target), end) in std::iter::zip(&cases, ends) {
				assert_eq!(ctx.pos(), *target);
				if Some(ctx.pos()) == the_end.get() && key.is_none() {
					continue
				}
				match key {
					Some(key) => println!("{i}  case {key}:"),
					None => println!("{i}  default:"),
				}
				let mut sub = ctx.sub(end);
				let new_end = sub.last_goto(|l| l >= end);
				if let Some(new_end) = new_end {
					assert!(the_end.get().is_none_or(|e| e == new_end));
					the_end.set(Some(new_end));
				}
				sub.indent.0 += 1;
				sub.brk = the_end.get();
				stmts(sub);
				if new_end.is_some() {
					println!("{i}    break");
				}
			}
			println!("{i}}}");
		}

		Op::SetGlobal(0) => {
			let a = ctx.pop();
			println!("{i}return {a}");
		}
		Op::Goto(t) if ctx.cont == Some(*t) => {
			println!("{i}continue");
		}
		Op::Goto(t) if ctx.brk == Some(*t) => {
			println!("{i}break");
		}
		Op::Goto(t) if ctx.pos() == *t => {
			println!("{i}pass");
		}

		Op::Push(v) => {
			ctx.push(Expr::Value(v.clone()));
		}
		Op::Pop(n) => {
			println!("{i}pop{n}/{} {}", ctx.nargs, Args(ctx.stack.make_contiguous()));
		}
		Op::PushVar(n) => {
			let var = Lvalue::Stack(ctx.resolve(*n));
			ctx.push(Expr::Var(var));
		}
		Op::SetVar(n) => {
			let a = ctx.pop();
			let var = Lvalue::Stack(ctx.resolve(*n));
			println!("{i}{} = {}", Expr::Var(var), a);
		}
		Op::PushRef(n) => {
			ctx.push(Expr::Ref(ctx.resolve(*n)));
		}
		Op::ReadRef(n) => {
			let var = Lvalue::Deref(ctx.resolve(*n));
			ctx.push(Expr::Var(var));
		}
		Op::WriteRef(n) => {
			let a = ctx.pop();
			let var = Lvalue::Deref(ctx.resolve(*n));
			println!("{i}{} = {}", Expr::Var(var), a);
		}
		Op::_07(n) => {
			let var = Lvalue::Local(*n);
			ctx.push(Expr::Var(var));
		}
		Op::_08(n) => {
			let a = ctx.pop();
			let var = Lvalue::Local(*n);
			println!("{i}{} = {}", Expr::Var(var), a);
		}
		Op::GetGlobal(n) => {
			let var = Lvalue::Global(*n);
			ctx.push(Expr::Var(var));
		}
		Op::SetGlobal(n) => {
			let a = ctx.pop();
			let var = Lvalue::Global(*n);
			println!("{i}{} = {}", Expr::Var(var), a);
		}
		Op::Binop(op) => {
			let b = ctx.pop();
			let a = ctx.pop();
			ctx.push(Expr::Binop(*op, a.into(), b.into()));
		}
		Op::Unop(op) => {
			let a = ctx.pop();
			ctx.push(Expr::Unop(*op, a.into()));
		}

		Op::Call(n) => {
			let pos = ctx
				.stack
				.iter()
				.position(|v| v == &Expr::Value(Value::Uint(ctx.pos().0)))
				.unwrap();
			let it = ctx.pop_n(pos);
			assert_eq!(ctx.pop(), Expr::Value(Value::Uint(ctx.pos().0)));
			assert_eq!(ctx.pop(), Expr::Value(Value::Uint(ctx.current_func)));
			let call = CallKind::Func(String::new(), ctx.functions[*n as usize].name.clone());
			ctx.push_call(call, it);
		}
		Op::CallExtern(a, b, n) => {
			let it = ctx.pop_n(*n as usize);
			assert_ne!(a, "");
			let call = CallKind::Func(a.clone(), b.clone());
			ctx.push_call(call, it);
		}
		Op::_23(a, b, c) => {
			let it = ctx.pop_n(*c as usize);
			let call = CallKind::_23(a.clone(), b.clone());
			ctx.push_call(call, it);
		}
		Op::CallSystem(a, b, c) => {
			let it = ctx.pop_n(*c as usize);
			let call = CallKind::System(*a, *b);
			ctx.push_call(call, it);
		}
		Op::_25(target) => {
			// Always wraps a complete CallFunc
			while ctx.pos() < *target {
				stmt(ctx);
			}
			// assert_eq!(ctx.pos(), *target); // assert is currently invalid because of GetGlobal(0)
		}

		Op::Line(_) => {}
		Op::Debug(n) => {
			let a = ctx.pop_n(*n as usize);
			println!("{i}debug {}", Args(&a));
		}
		op => {
			println!("{i}!!{op:?} {:?}", ctx.stack);
			unimplemented!("{op:?} {:?}", ctx.stack);
		}
	}
}
