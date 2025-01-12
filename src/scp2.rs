pub mod io;
pub use io::{read, write};

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

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub enum CallKind {
	Normal(String),
	Tailcall(String),
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
pub struct Label(pub u32);
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StackSlot(pub u32);

#[derive(Debug, Clone, PartialEq)]
pub enum Op {
	Label(Label),

	Push(Value),
	Pop(u8),
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

	If(Label),
	Goto(Label),
	Case(Label),

	CallLocal(String),
	CallExtern(String, u8),
	CallTail(String, u8),
	CallSystem(u8, u8, u8),
	PrepareCallLocal(Label),
	PrepareCallExtern(Label),
	Return,

	Line(u16),
	Debug(u8),
}
