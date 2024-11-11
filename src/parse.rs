lalrpop_util::lalrpop_mod!(grammar);

mod ast {
	pub use crate::expr::{Type, Value, CallKind, Binop, Unop};
	pub type Lvalue = crate::expr::Lvalue<String>;
	pub type Stmt = crate::Stmt<String>;
	pub type Expr = crate::expr::Expr<String>;

	#[derive(Debug)]
	pub enum Item {
		Global(Global),
		Function(Function),
	}

	#[derive(Debug)]
	pub struct Global {
		pub name: String,
		pub ty: Type,
	}

	#[derive(Debug)]
	pub struct Function {
		pub name: String,
		pub prelude: bool,
		pub dup: bool,
		pub args: Vec<Arg>,
		pub body: Vec<Stmt>,
	}

	#[derive(Debug)]
	pub struct Arg {
		pub name: String,
		pub out: bool,
		pub ty: Type,
		pub default: Option<Value>,
	}

	pub fn unescape(s: &str) -> String {
		let mut out = String::new();
		let mut chars = s[1..s.len()-1].chars();
		while let Some(c) = chars.next() {
			if c == '\\' {
				match chars.next() {
					Some('n') => out.push('\n'),
					Some('r') => out.push('\r'),
					Some('t') => out.push('\t'),
					Some('"') => out.push('\"'),
					Some('\\') => out.push('\\'),
					_ => unreachable!(),
				}
			} else {
				out.push(c);
			}
		}
		out
	}

	pub fn expr(left: Expr, rest: Vec<(Option<u16>, Binop, Expr)>) -> Expr {
		pub struct Prio {
			ops: Vec<(Option<u16>, Binop)>,
			stack: Vec<Expr>,
		}

		impl Prio {
			pub fn new(left: Expr) -> Self {
				Self {
					ops: Vec::new(),
					stack: vec![left],
				}
			}

			fn pop(&mut self) {
				let top = self.ops.pop().unwrap();
				let right = self.stack.pop().unwrap();
				let left = self.stack.pop().unwrap();
				self.stack.push(Expr::Binop(top.0, top.1, Box::new(left), Box::new(right)))
			}

			pub fn push(&mut self, op: (Option<u16>, Binop, Expr)) {
				use crate::expr::op_prio;
				while let Some(top) = self.ops.last() && op_prio(top.1) >= op_prio(op.1) {
					self.pop();
				}
				self.ops.push((op.0, op.1));
				self.stack.push(op.2);
			}

			pub fn finish(&mut self) -> Expr {
				while !self.ops.is_empty() {
					self.pop();
				}
				assert_eq!(self.stack.len(), 1);
				self.stack.pop().unwrap()
			}
		}

		let mut prio = Prio::new(left);
		for next in rest {
			prio.push(next);
		}
		prio.finish()
	}
}

pub fn parse(text: &str) -> Result<(), ()> {
	match grammar::ScenaParser::new().parse(text) {
		Ok(v) => {
			// tracing::info!("{:#?}", v);
		},
		Err(e) => {
			tracing::warn!("{}", e);
		}
	}
	Ok(())
}
