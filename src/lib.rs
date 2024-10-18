#![feature(let_chains, is_sorted, is_none_or)]
use std::{cell::Cell, collections::{BTreeMap, HashMap, VecDeque}};

mod scp;
mod names;

use scp::{Label, Op, Scp, Value};
pub use scp::parse_scp;

#[derive(Clone, PartialEq)]
enum Expr {
	Value(Value),
	Var(i32),
	VarRef(i32),
	Local(u32),
	Global(u8),
	CallSystem(u8, u8, Vec<Expr>),
	CallFunc(String, String, Vec<Expr>),
	_23(String, String, Vec<Expr>),
	Unop(u8, Box<Expr>),
	Binop(u8, Box<Expr>, Box<Expr>),
	Arg,
}

impl std::fmt::Debug for Expr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Expr::Value(v) => v.fmt(f),
			Expr::Var(n) => f.debug_tuple("Var").field(n).finish(),
			Expr::VarRef(n) => f.debug_tuple("VarRef").field(n).finish(),
			Expr::Local(n) => f.debug_tuple("Local").field(n).finish(),
			Expr::Global(n) => f.debug_tuple("Global").field(n).finish(),
			Expr::CallSystem(a, b, v) => f.debug_tuple("CallSystem").field(a).field(b).field(v).finish(),
			Expr::CallFunc(a, b, v) => f.debug_tuple("CallFunc").field(a).field(b).field(v).finish(),
			Expr::_23(a, b, v) => f.debug_tuple("_23").field(a).field(b).field(v).finish(),
			Expr::Unop(v, a) => f.debug_tuple("Unop").field(v).field(a).finish(),
			Expr::Binop(v, a, b) => f.debug_tuple("Binop").field(v).field(a).field(b).finish(),
			Expr::Arg => f.write_str("Arg"),
		}
	}
}

struct Ctx<'a> {
	functions: &'a [scp::Function],
	code: &'a [(Label, Op)],
	code_end: Label,

	loops: &'a HashMap<Label, Label>,
	stack: VecDeque<Expr>,
	current_func: u32,
	pos: usize,
	indent: Indent,
	cont: Option<Label>,
	brk: Option<Label>,
	popped_last: bool,
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

	fn push_call(&mut self, i: Indent, call: Expr) {
		if self.peek() == Some(&Op::GetGlobal(0)) {
			self.next();
			self.stack.push_front(call);
		} else {
			println!("{i}{call:?}");
		}
	}

	fn sub(&mut self, target: Label) -> Ctx<'a> {
		let index = if target == self.code_end {
			self.code.len()
		} else {
			self.code.iter().position(|(l, _)| *l == target).unwrap()
		};
		assert!(index >= self.pos);
		let new = Ctx {
			functions: self.functions,
			code: &self.code[self.pos..index],
			code_end: target,
			loops: self.loops,
			stack: self.stack.clone(),
			current_func: self.current_func,
			pos: 0,
			indent: self.indent.inc(),
			cont: self.cont,
			brk: self.brk,
			popped_last: self.popped_last && index == self.code.len(),
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
}

pub fn stuff(scp: &Scp) {
	let mut loops = HashMap::new();
	for (i, (_, op)) in scp.code.iter().enumerate() {
		if let Op::Goto(start) = op && *start < scp.code[i + 1].0 {
			loops.insert(*start, scp.code[i + 1].0);
		}
	}
	let mut ctx = Ctx {
		functions: &scp.functions,
		code: &scp.code,
		code_end: scp.code_end,
		loops: &loops,
		stack: VecDeque::new(),
		current_func: 0,
		pos: 0,
		indent: Indent(0),
		cont: None,
		brk: None,
		popped_last: false,
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
		let mut sub = ctx.sub(end);

		// println!("\n{f}");
		// scp::dump_ops(sub.code);

		println!("\n{f}");
		for _ in &f.args {
			sub.stack.push_front(Expr::Arg);
		}
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
impl std::fmt::Display for Indent {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		for _ in 0..self.0 {
			write!(f, "  ")?;
		}
		Ok(())
	}
}

fn switch_cases(ctx: &mut Ctx<'_>) -> BTreeMap<Label, Vec<Option<i32>>> {
	let mut cases = Vec::new();
	let default = loop {
		match &ctx.next().unwrap() {
			Op::GetGlobal(0) => {
				let Op::Push(Value::Int(n)) = ctx.next().unwrap() else { unreachable!() };
				let Op::Op(21) = ctx.next().unwrap() else { unreachable!() };
				let Op::If2(target) = ctx.next().unwrap() else { unreachable!() };
				cases.push((*n, *target));
			}
			Op::Goto(default) => break *default,
			op => unreachable!("unexpected {op:?} in switch"),
		}
	};
	assert!(cases.is_sorted_by_key(|(_, a)| a));
	let mut inv_cases = BTreeMap::<Label, Vec<Option<i32>>>::new();
	for (n, target) in cases {
		inv_cases.entry(target).or_default().push(Some(n));
	}
	inv_cases.entry(default).or_default().push(None);
	inv_cases
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
	if let Some(&brk) = ctx.loops.get(&ctx.pos()) && (ctx.indent == Indent(0) || ctx.pos != 0) {
		let cont = ctx.pos();
		println!("{i}loop {{");
		let mut sub = ctx.sub(brk);
		sub.cont = Some(cont);
		sub.brk = Some(brk);
		stmts(sub);
		println!("{i}}}");
		return;
	}
	match ctx.next().unwrap() {
		Op::Return => {
			println!("{i}(end)");
			assert_eq!(ctx.stack, &[]);
		}

		Op::If(target) => {
			let a = ctx.pop();
			let mut sub = ctx.sub(*target);
			let the_else = sub.last_goto(|l| l >= *target && l <= ctx.code_end);
			if let Some(the_else) = the_else {
				println!("{i}if {a:?} {{");
				stmts(sub);
				println!("{i}}} else {{");
				let sub = ctx.sub(the_else);
				stmts(sub);
				println!("{i}}}");
			} else {
				println!("{i}if {a:?} {{");
				stmts(sub);
				println!("{i}}}");
			}
		}

		Op::SetGlobal(0) if ctx.peek() == Some(&Op::GetGlobal(0)) => {
			let a = ctx.pop();
			let cases = switch_cases(ctx);
			println!("{i}switch {a:?} {{");
			let the_end = Cell::new(None);
			let ends = cases.keys().copied().skip(1).chain(std::iter::once_with(|| the_end.get()).flatten());
			for ((target, n), end) in std::iter::zip(&cases, ends) {
				assert_eq!(ctx.pos(), *target);
				if Some(ctx.pos()) == the_end.get() && n == &[None] {
					continue
				}
				for n in n {
					match n {
						Some(n) => println!("{i}  case {n}:"),
						None => println!("{i}  default:"),
					}
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
			println!("{i}return {a:?}");
		}
		Op::Goto(t) if ctx.cont == Some(*t) => {
			println!("{i}continue");
		}
		Op::Goto(t) if ctx.brk == Some(*t) => {
			println!("{i}break");
		}

		Op::Push(v) => {
			ctx.push(Expr::Value(v.clone()));
		}
		Op::Pop(n) => {
			for _ in 0..*n / 4 {
				ctx.pop(); // TODO must be Local or Arg
			}
		}
		Op::GetVar(n) => {
			let d = 4 * ctx.stack.len() as i32;
			ctx.push(Expr::Var(*n + d));
		}
		Op::SetVar(n) => {
			let a = ctx.pop();
			let d = 4 * ctx.stack.len() as i32;
			println!("{i}{:?} = {:?}", Expr::Var(*n + d), a);
		}
		Op::RefVar(n) => {
			let d = 4 * ctx.stack.len() as i32;
			ctx.push(Expr::VarRef(*n + d));
		}
		Op::_07(n) => {
			ctx.push(Expr::Local(*n));
		}
		Op::_08(n) => {
			let a = ctx.pop();
			println!("{i}{:?} = {:?}", Expr::Local(*n), a);
		}
		Op::GetGlobal(n) => {
			ctx.push(Expr::Global(*n));
		}
		Op::SetGlobal(n) => {
			let a = ctx.pop();
			println!("{i}{:?} = {:?}", Expr::Global(*n), a);
		}
		Op::Op(n @ (16..=31 | 33)) => {
			// 16: + (probably)
			// 21: == (certain)
			// 29: &
			// 30: |
			// 32: !
			let b = ctx.pop();
			let a = ctx.pop();
			ctx.push(Expr::Binop(*n, a.into(), b.into()));
		}
		Op::Op(n @ 32) => {
			let a = ctx.pop();
			ctx.push(Expr::Unop(*n, a.into()));
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
			ctx.push_call(i, Expr::CallFunc(String::new(), ctx.functions[*n as usize].name.clone(), it));
		}
		Op::CallExtern(a, b, n) => {
			let it = ctx.pop_n(*n as usize);
			assert_ne!(a, "");
			ctx.push_call(i, Expr::CallFunc(a.clone(), b.clone(), it));
		}
		Op::_23(a, b, c) => {
			let it = ctx.pop_n(*c as usize);
			assert_ne!(a, "");
			ctx.push_call(i, Expr::_23(a.clone(), b.clone(), it));
		}
		Op::CallSystem(a, b, c) => {
			let it = ctx.pop_n(*c as usize);
			ctx.push_call(i, Expr::CallSystem(*a, *b, it));
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
			println!("{i}debug {a:?}");
		}
		op => {
			println!("{i}!!{op:?} {:?}", ctx.stack);
			unimplemented!("{op:?} {:?}", ctx.stack);
		}
	}
}
