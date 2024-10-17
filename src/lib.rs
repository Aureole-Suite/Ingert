#![feature(is_sorted, is_none_or)]
use std::collections::{BTreeMap, VecDeque};

mod scp;

use scp::{Label, Op, Scp, Value};
pub use scp::parse_scp;

#[derive(Clone, PartialEq)]
enum Expr {
	Value(Value),
	Var(i32),
	Syscall(u16, Vec<Expr>),
	Syscall2(u8, u8, Vec<Expr>),
	CallFunc(String, String, Vec<Expr>),
	Unop(u8, Box<Expr>),
	Binop(u8, Box<Expr>, Box<Expr>),
	Arg,
}

impl std::fmt::Debug for Expr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Expr::Value(v) => v.fmt(f),
			Expr::Var(n) => f.debug_tuple("Var").field(n).finish(),
			Expr::Syscall(n, v) => f.debug_tuple("Syscall").field(n).field(v).finish(),
			Expr::Syscall2(a, b, v) => f.debug_tuple("Syscall2").field(a).field(b).field(v).finish(),
			Expr::CallFunc(a, b, v) => f.debug_tuple("CallFunc").field(a).field(b).field(v).finish(),
			Expr::Unop(v, a) => f.debug_tuple("Unop").field(v).field(a).finish(),
			Expr::Binop(v, a, b) => f.debug_tuple("Binop").field(v).field(a).field(b).finish(),
			Expr::Arg => f.write_str("Arg"),
		}
	}
}

struct Ctx<'a> {
	code: &'a [(Label, Op)],
	code_end: Label,

	stack: VecDeque<Expr>,
	current_func: u32,
	pos: usize,
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
		self.code.get(self.pos).map(|(_, a)| a)
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
}

pub fn stuff(scp: &Scp) {
	let functions = scp
		.functions
		.iter()
		.map(|f| (f.start, f))
		.collect::<BTreeMap<_, _>>();

	let mut ctx = Ctx {
		code: &scp.code,
		code_end: scp.code_end,
		stack: VecDeque::new(),
		current_func: 0,
		pos: 0,
	};

	while ctx.peek().is_some() {
		if let Some(f) = functions.get(&ctx.pos()) {
			ctx.current_func = f.index;
			println!("\nfunction {} {:?} {:?}", f.name, (f.a0, f.a1, &f.a2, f.checksum), f.args);
			assert_eq!(ctx.stack, &[]);
			for _ in &f.args {
				ctx.stack.push_front(Expr::Arg);
			}
		}

		stmt(&mut ctx, Indent(1));

		// match op {
		// 	Op::Push(v) => {
		// 		stack.push_front(Expr::Value(v.clone()));
		// 	}
		// 	Op::Pop(n) => {
		// 		line = format!("01({n})");
		// 		for _ in 0..*n/4 {
		// 			stack.pop_front();
		// 		}
		// 	}
		// 	Op::_07(n) => {
		// 		stack.push_front(Expr::_07(*n));
		// 	}
		// 	Op::_08(n) => {
		// 		let a = stack.pop_front().unwrap();
		// 		line = format!("08({n}) = {a:?}");
		// 	}
		//
		// 	Op::GetGlobal(n) => {
		// 		stack.push_front(Expr::Global(*n));
		// 	}
		// 	Op::SetGlobal(n) => {
		// 		let a = stack.pop_front().unwrap();
		// 		line = format!("Global({n}) = {a:?}");
		// 		stack.push_front(a);
		// 	}
		//
		// 	Op::GetVar(v) => {
		// 		let d = 4 * stack.len() as i32;
		// 		stack.push_front(Expr::Var(*v + d));
		// 	}
		// 	Op::SetVar(v) => {
		// 		let a = stack.pop_front().unwrap();
		// 		let d = 4 * stack.len() as i32;
		// 		line = format!("Var({}) = {:?}", *v + d, a);
		// 	}
		//
		// 	Op::Op(n@(16..=30)) => {
		// 		let a = stack.pop_front().unwrap();
		// 		let b = stack.pop_front().unwrap();
		// 		stack.push_front(Expr::Binop(*n, a.into(), b.into()));
		// 	}
		// 	Op::Op(n@32) => {
		// 		let a = stack.pop_front().unwrap();
		// 		stack.push_front(Expr::Unop(*n, a.into()));
		// 	}
		// 	Op::Op(n) => {
		// 		line = format!("Op({n})");
		// 		stack.push_front(Expr::Op(*n))
		// 	}
		// 	Op::If2(l) => {
		// 		let a = stack.pop_front().unwrap();
		// 		line = format!("if2 {:?} {:?}", a, l);
		// 	}
		// 	Op::If(l) => {
		// 		let a = stack.pop_front().unwrap();
		// 		line = format!("if {:?} {:?}", a, l);
		// 	}
		// 	Op::Goto(l) => {
		// 		line = format!("goto {:?}", l);
		// 	}
		// 	Op::_27(n) => { // something about messages
		// 		let a = stack.pop_front().unwrap();
		// 		line = format!("27({n}) {:?}", a);
		// 	}
		// 	Op::CallFunc(a, b, n) => {
		// 		let it = stack.drain(..*n as usize).collect::<Vec<_>>();
		// 		line = format!("call {:?} {:?} {:?}", a, b, it);
		// 	}
		// 	Op::Syscall(n) => {
		// 		let pos = stack
		// 			.iter()
		// 			.position(|v| v == &Expr::Value(&Value::Uint(end.0)));
		// 		if pos.is_some_and(|pos| {
		// 			stack.get(pos + 1) == Some(&Expr::Value(&Value::Uint(current_func)))
		// 		}) {
		// 			let mut it = stack.drain(..pos.unwrap() + 2).collect::<Vec<_>>();
		// 			it.pop();
		// 			it.pop();
		// 			let call = Expr::Syscall(*n, it);
		// 			if syscall_returns(*n) {
		// 				stack.push_front(call);
		// 			} else {
		// 				line = format!("{call:?}");
		// 			}
		// 		} else {
		// 			line = format!("?syscall {} {:?}", n, stack);
		// 		}
		// 	}
		// 	Op::Syscall2(a, b, c) => {
		// 		let it = stack.drain(..*c as usize).collect::<Vec<_>>();
		// 		let call = Expr::_24(*a, *b, it);
		// 		if _24_returns((*a, *b)) {
		// 			stack.push_front(call);
		// 		} else {
		// 			line = format!("{call:?}");
		// 		}
		// 	}
		// 	Op::Line(_) => {}
		// 	_ => {
		// 		line = format!("{:?}", op);
		// 	}
		// }
		//
		// if !line.is_empty() {
		// 	println!("  {line} {stack:?}");
		// }
	}
}

#[derive(Debug, Clone, PartialEq)]
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

fn stmt(ctx: &mut Ctx<'_>, i: Indent) {
	match ctx.next().unwrap() {
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
			println!("{i}Var({}) = {:?}", *n + d, a);
		}
		Op::SetGlobal(0) if ctx.peek() == Some(&Op::GetGlobal(0)) => {
			let a = ctx.pop();
			let cases = switch_cases(ctx);
			println!("{i}switch {a:?} {{");
			let mut the_end = None;
			for (target, n) in cases {
				assert_eq!(ctx.pos(), target);
				for n in n {
					match n {
						Some(n) => println!("{i}  case {n}:"),
						None => println!("{i}  default:"),
					}
				}
				loop {
					if let Some(Op::Goto(t)) = ctx.peek() {
						ctx.next();
						assert!(the_end.is_none_or(|e| e == *t));
						the_end = Some(*t);
						println!("{i}    break");
						break
					}
					stmt(ctx, i.inc().inc());
				}
			}
			assert_eq!(ctx.pos(), the_end.unwrap());
			println!("{i}}}");
		}
		Op::SetGlobal(0) => {
			let a = ctx.pop();
			println!("{i}return {a:?}");
		}
		Op::Syscall(n) => {
			let pos = ctx
				.stack
				.iter()
				.position(|v| v == &Expr::Value(Value::Uint(ctx.pos().0)))
				.unwrap();
			let it = ctx.pop_n(pos);
			assert_eq!(ctx.pop(), Expr::Value(Value::Uint(ctx.pos().0)));
			assert_eq!(ctx.pop(), Expr::Value(Value::Uint(ctx.current_func)));
			ctx.push_call(i, Expr::Syscall(*n, it));
		}
		Op::Return => {
			println!("{i}(end)");
			assert_eq!(ctx.stack, &[]);
		}
		Op::If(mut target) => {
			let a = ctx.pop();
			let has_else = false;
			println!("{i}if {a:?} {{");
			loop {
				if ctx.pos() >= target {
					assert_eq!(ctx.pos(), target);
					break
				}
				if let Some(Op::Goto(t)) = ctx.peek() {
					if !has_else {
						ctx.next();
						println!("{i}}} else {{");
						target = *t;
						continue
					} else {
						unreachable!();
					}
				}
				stmt(ctx, i.inc());
			}
			println!("{i}}}");
		}
		Op::Op(n @ (16..=30)) => {
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
		Op::CallFunc(a, b, n) => {
			let it = ctx.pop_n(*n as usize);
			ctx.push_call(i, Expr::CallFunc(a.clone(), b.clone(), it));
		}
		Op::Syscall2(a, b, c) => {
			let it = ctx.pop_n(*c as usize);
			ctx.push_call(i, Expr::Syscall2(*a, *b, it));
		}
		&Op::_25(target) => {
			println!("{i}_25 {{");
			loop {
				if ctx.pos() >= target {
					assert_eq!(ctx.pos(), target);
					break
				}
				stmt(ctx, i.inc());
			}
			println!("{i}}}");
		}
		Op::Line(_) => {}
		op => {
			println!("{i}!!{op:?} {:?}", ctx.stack);
			unimplemented!("{op:?} {:?}", ctx.stack);
		}
	}
}
