pub mod io;
pub use io::{read, write};

pub use crate::labels::Label;

#[derive(Debug, Clone, PartialEq)]
pub struct Scp {
	pub globals: Vec<Global>,
	pub functions: Vec<Function>,
}

#[derive(Clone, PartialEq)]
pub enum Value {
	Int(i32),
	Float(f32),
	String(String),
}

impl std::fmt::Debug for Value {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Int(v) => v.fmt(f),
			Self::Float(v) => v.fmt(f),
			Self::String(v) => v.fmt(f),
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct Global {
	pub name: String,
	pub ty: GlobalType,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GlobalType {
	Number,
	String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
	pub name: String,
	pub args: Vec<Arg>,
	pub called: Vec<Call>,
	pub is_prelude: bool,
	pub code: Vec<Op>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArgType {
	Number,
	String,
	NumberRef,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Arg {
	pub ty: ArgType,
	pub default: Option<Value>,
}

#[derive(Clone, PartialEq)]
pub struct Call {
	pub kind: CallKind,
	pub args: Vec<CallArg>,
}

impl std::fmt::Debug for Call {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let mut t = f.debug_tuple("Call");
		t.field(&self.kind);
		for arg in &self.args {
			t.field(arg);
		}
		t.finish()
	}
}

#[derive(Clone, PartialEq)]
pub enum CallArg {
	Value(Value),
	Call,
	Var,
	Expr,
}

impl std::fmt::Debug for CallArg {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Value(v) => v.fmt(f),
			Self::Call => write!(f, "Call"),
			Self::Var => write!(f, "Var"),
			Self::Expr => write!(f, "Expr"),
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Name(pub String, pub String);

impl Name {
	pub fn local(name: String) -> Self {
		Self(String::new(), name)
	}

	pub fn is_local(&self) -> bool {
		self.0.is_empty()
	}

	pub fn as_local(&self) -> Option<&String> {
		if self.is_local() {
			Some(&self.1)
		} else {
			None
		}
	}
}

impl std::fmt::Display for Name {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		if self.0.is_empty() {
			write!(f, "{}", self.1)
		} else {
			write!(f, "{}.{}", self.0, self.1)
		}
	}
}

impl std::str::FromStr for Name {
	type Err = std::convert::Infallible;

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		if let Some((a, b)) = s.split_once('.') {
			Ok(Self(a.to_owned(), b.to_owned()))
		} else {
			Ok(Self::local(s.to_owned()))
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum CallKind {
	Normal(Name),
	Tailcall(Name),
	Syscall(u8, u8),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StackSlot(pub u32);

#[derive(Debug, Clone, PartialEq)]
pub enum Op {
	Label(Label),

	Push(Value),
	Pop(u16),
	PushNull,

	GetVar(StackSlot),
	GetRef(StackSlot),
	PushRef(StackSlot),
	SetVar(StackSlot),
	SetRef(StackSlot),
	GetGlobal(String),
	SetGlobal(String),
	GetTemp(u8),
	SetTemp(u8),

	Binop(Binop),
	Unop(Unop),

	Jnz(Label),
	Jz(Label),
	Goto(Label),

	CallLocal(String),
	CallExtern(Name, u8),
	CallTail(Name, u8),
	CallSystem(u8, u8, u8),
	PrepareCallLocal(Label),
	PrepareCallExtern(Label),
	Return,

	Line(u16),
	Debug(u8),
}

impl crate::labels::Labels for Op {
	fn defined(&self) -> Option<&Label> {
		match self {
			Self::Label(label) => Some(label),
			_ => None,
		}
	}

	fn referenced(&self, mut f: impl FnMut(&Label)) {
		match self {
			Self::Jnz(label) | Self::Jz(label) | Self::Goto(label) => f(label),
			Self::PrepareCallLocal(label) | Self::PrepareCallExtern(label) => f(label),
			_ => (),
		}
	}
}

impl crate::labels::LabelsMut for Op {
	fn defined_mut(&mut self) -> Option<&mut Label> {
		match self {
			Self::Label(label) => Some(label),
			_ => None,
		}
	}

	fn referenced_mut(&mut self, mut f: impl FnMut(&mut Label)) {
		match self {
			Self::Jnz(label) | Self::Jz(label) | Self::Goto(label) => f(label),
			Self::PrepareCallLocal(label) | Self::PrepareCallExtern(label) => f(label),
			_ => (),
		}
	}
}
