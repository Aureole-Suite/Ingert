lalrpop_util::lalrpop_mod!(grammar);

mod ast {
	pub use crate::expr::{Type, Value, CallKind, Binop, Unop};

	#[derive(Debug)]
	pub enum Error {
		IntParse(std::num::ParseIntError),
		FloatParse(std::num::ParseFloatError),
		ExpectedLiteral(&'static [&'static str]),
	}

	pub type Result<T, E = Error> = std::result::Result<T, E>;
	pub type Unwrap<T> = <T as _Unwrap>::Value;
	pub trait _Unwrap {
		type Value;
	}
	impl<T, E> _Unwrap for std::result::Result<T, E> {
		type Value = T;
	}
	impl<T> _Unwrap for Option<T> {
		type Value = T;
	}

	#[derive(Debug)]
	pub struct SpannedError {
		pub error: Error,
		pub start: usize,
		pub end: usize,
	}

	#[allow(non_snake_case)]
	pub fn SpannedError<L, T, E: Into<Error>>(start: usize, end: usize) -> impl FnOnce(E) -> lalrpop_util::ParseError<L, T, SpannedError> {
		move |e| SpannedError { error: e.into(), start, end }.into()
	}

	impl std::fmt::Display for Error {
		fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
			match self {
				Self::IntParse(e) => write!(f, "failed to parse integer: {}", e),
				Self::FloatParse(e) => write!(f, "failed to parse float: {}", e),
				Self::ExpectedLiteral(lits) => write!(f, "expected one of: {}", lits.join(", ")),
			}
		}
	}

	impl std::fmt::Display for SpannedError {
		fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
			write!(f, "{} at {}:{}", self.error, self.start, self.end)
		}
	}

	#[derive(Debug)]
	pub enum Item {
		Global(Global),
		Function(Function),
	}

	#[derive(Debug)]
	pub struct Global {
		pub line: Option<u16>,
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


	#[derive(Debug, Clone, PartialEq)]
	pub enum Stmt {
		Expr(Expr),
		PushVar(Option<u16>, String, Option<Expr>),
		Set(Option<u16>, Lvalue, Expr),
		Debug(Option<u16>, Vec<Expr>),

		If(Option<u16>, Expr, Vec<Stmt>, Option<Vec<Stmt>>),
		While(Option<u16>, Expr, Vec<Stmt>),
		Switch(Option<u16>, Expr, Vec<(Option<i32>, Vec<Stmt>)>),
		Break,
		Continue,
		Return(Option<u16>, Option<Expr>),
	}

	#[derive(Debug, Clone, PartialEq)]
	pub enum Expr {
		Value(Option<u16>, Value),
		Var(Option<u16>, Lvalue),
		Ref(Option<u16>, String),
		Call(Option<u16>, CallKind, Vec<Expr>),
		Unop(Option<u16>, Unop, Box<Expr>),
		Binop(Option<u16>, Binop, Box<Expr>, Box<Expr>),
	}

	#[derive(Debug, Clone, PartialEq, Eq, Hash)]
	pub enum Lvalue {
		Stack(String),
		Deref(String),
		Global(String),
	}

	pub fn unescape(s: &str, del: char) -> String {
		let mut out = String::new();
		let mut chars = s[1..s.len()-1].chars();
		while let Some(c) = chars.next() {
			if c == '\\' {
				match chars.next() {
					Some('n') => out.push('\n'),
					Some('r') => out.push('\r'),
					Some('t') => out.push('\t'),
					Some(c) if c == del => out.push(del),
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
