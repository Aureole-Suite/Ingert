pub use crate::scp::{Value, Binop, Unop};
use std::fmt::{Display, Formatter, Result};

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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Lvalue<T> {
	Stack(T),
	Deref(T),
	Global(String),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CallKind {
	System(u8, u8),
	Func(String, String),
	Tail(String, String),
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
			Expr::Value(v) => write!(f, "{v:?}")?,
			Expr::Var(v) => write!(f, "{v}")?,
			Expr::Ref(v) => write!(f, "&{v}")?,
			Expr::Call(c, args) => {
				write!(f, "{c}")?;
				write_args(f, args)?;
			}
			Expr::Unop(o, a) => {
				write!(f, "{}", o)?;
				a.display(f, 10)?;
			}
			Expr::Binop(o, a, b) => {
				let p = op_prio(*o);
				if p < prio {
					write!(f, "(")?;
				}
				a.display(f, p)?;
				write!(f, " {o} ")?;
				b.display(f, p)?;
				if p < prio {
					write!(f, ")")?;
				}
			}
			Expr::Line(l, e) => {
				if let Expr::Binop(o, a, b) = &**e {
					let p = op_prio(*o);
					if p < prio {
						write!(f, "(")?;
					}
					a.display(f, p)?;
					write!(f, " {l}@{o} ")?;
					b.display(f, p)?;
					if p < prio {
						write!(f, ")")?;
					}
				} else {
					write!(f, "{l}@")?;
					e.display(f, prio)?;
				}
			}
		}
		Ok(())
	}
}

impl Display for CallKind {
	fn fmt(&self, f: &mut Formatter<'_>) -> Result {
		match self {
			CallKind::System(a, b) => write!(f, "system[{a},{b}]"),
			CallKind::Func(a, b) if a.is_empty() => write!(f, "{b}"),
			CallKind::Func(a, b) => write!(f, "{a}.{b}"),
			CallKind::Tail(a, b) if a.is_empty() => write!(f, "tail {b}"),
			CallKind::Tail(a, b) => write!(f, "tail {a}.{b}"),
		}
	}
}

fn op_prio(op: Binop) -> u32 {
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
