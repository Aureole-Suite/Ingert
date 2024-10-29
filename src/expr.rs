use std::fmt::{Display, Formatter, Result};

#[derive(Clone, PartialEq)]
pub enum Value {
	Int(i32),
	Float(f32),
	String(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<T> {
	Value(Option<u16>, Value),
	Var(Option<u16>, Lvalue<T>),
	Ref(Option<u16>, T),
	Call(Option<u16>, CallKind, Vec<Expr<T>>),
	Unop(Option<u16>, Unop, Box<Expr<T>>),
	Binop(Option<u16>, Binop, Box<Expr<T>>, Box<Expr<T>>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Lvalue<T> {
	Stack(T),
	Deref(T),
	Global(String),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CallKind {
	System(u8, u8),
	Func(String),
	Tail(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
	Number,
	String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Arg {
	pub out: bool,
	pub ty: Type,
	pub default: Option<Value>,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, strum::FromRepr)]
pub enum Binop {
	Add = 16,
	Sub = 17,
	Mul = 18,
	Div = 19,
	Mod = 20,
	Eq = 21,
	Ne = 22,
	Gt = 23,
	Ge = 24,
	Lt = 25,
	Le = 26,
	BitAnd = 27,
	BitOr = 28,
	BoolAnd = 29,
	BoolOr = 30,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, strum::FromRepr)]
pub enum Unop {
	Neg = 31,
	BoolNot = 32,
	BitNot = 33,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Global {
	pub name: String,
	pub ty: Type,
	pub line: Option<u16>,
}

impl std::fmt::Display for Value {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		std::fmt::Debug::fmt(self, f)
	}
}

impl std::fmt::Debug for Value {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Int(v) => write!(f, "{v}"),
			Self::Float(v) => std::fmt::Debug::fmt(&v, f),
			Self::String(v) => std::fmt::Debug::fmt(&v, f),
		}
	}
}

impl<T: Display> Display for Expr<T> {
	fn fmt(&self, f: &mut Formatter<'_>) -> Result {
		self.display(f, 0)
	}
}

impl<T: Display> Display for Lvalue<T> {
	fn fmt(&self, f: &mut Formatter<'_>) -> Result {
		match self {
			Lvalue::Stack(s) => write!(f, "{s}"),
			Lvalue::Deref(v) => write!(f, "*{v}"),
			Lvalue::Global(n) => write!(f, ":{n}"),
		}
	}
}

impl<T: Display> Expr<T> {
	fn display(&self, f: &mut Formatter, prio: u32) -> Result {
		match self {
			Expr::Value(l, v) => {
				line(f, l)?;
				write!(f, "{v:?}")?;
			}
			Expr::Var(l, v) => {
				line(f, l)?;
				write!(f, "{v}")?;
			}
			Expr::Ref(l, v) => {
				line(f, l)?;
				write!(f, "&{v}")?;
			}
			Expr::Call(l, c, args) => {
				line(f, l)?;
				write!(f, "{c}")?;
				write_args(f, args)?;
			}
			Expr::Unop(l, o, a) => {
				line(f, l)?;
				write!(f, "{}", o)?;
				a.display(f, 10)?;
			}
			Expr::Binop(l, o, a, b) => {
				let p = op_prio(*o);
				if p < prio {
					write!(f, "(")?;
				}
				a.display(f, p)?;
				write!(f, " ")?;
				line(f, l)?;
				write!(f, "{o}")?;
				write!(f, " ")?;
				b.display(f, p + 1)?;
				if p < prio {
					write!(f, ")")?;
				}
			}
		}
		Ok(())
	}
}

pub fn line(f: &mut Formatter, l: &Option<u16>) -> Result {
	if let Some(l) = l {
		write!(f, "{l}@")?;
	}
	Ok(())
}

impl Display for CallKind {
	fn fmt(&self, f: &mut Formatter<'_>) -> Result {
		match self {
			CallKind::System(a, b) => write!(f, "system[{a},{b}]"),
			CallKind::Func(a) => write!(f, "{a}"),
			CallKind::Tail(a) => write!(f, "tail {a}"),
		}
	}
}

impl Display for Type {
	fn fmt(&self, f: &mut Formatter<'_>) -> Result {
		f.write_str(match self {
			Type::Number => "num",
			Type::String => "str",
		})
	}
}

impl std::fmt::Display for Arg {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		if self.out {
			write!(f, "out ")?;
		}
		write!(f, "{}", self.ty)?;
		if let Some(default) = &self.default {
			write!(f, "={:?}", default)?;
		}
		Ok(())
	}
}

impl std::fmt::Display for Binop {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.write_str(match self {
			Binop::Add => "+",
			Binop::Sub => "-",
			Binop::Mul => "*",
			Binop::Div => "/",
			Binop::Mod => "%",
			Binop::Eq => "==",
			Binop::Ne => "!=",
			Binop::Gt => ">",
			Binop::Ge => ">=",
			Binop::Lt => "<",
			Binop::Le => "<=",
			Binop::BitAnd => "&",
			Binop::BitOr => "|",
			Binop::BoolAnd => "&&",
			Binop::BoolOr => "||",
		})
	}
}

impl std::fmt::Display for Unop {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.write_str(match self {
			Unop::Neg => "-",
			Unop::BoolNot => "!",
			Unop::BitNot => "~",
		})
	}
}

pub fn op_prio(op: Binop) -> u32 {
	use Binop::*;
	match op {
		Mul | Div | Mod => 7,
		Add | Sub => 6,
		BitAnd => 5,
		BitOr => 4,
		Eq | Ne | Gt | Ge | Lt | Le => 3,
		BoolAnd => 2,
		BoolOr => 1,
	}
}

pub fn write_args<T: Display>(f: &mut Formatter, args: impl IntoIterator<Item=T>) -> Result {
	f.write_str("(")?;
	let mut it = args.into_iter();
	if let Some(a) = it.next() {
		write!(f, "{a}")?;
		for a in it {
			f.write_str(", ")?;
			write!(f, "{a}")?;
		}
	}
	f.write_str(")")
}
