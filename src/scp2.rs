mod parse;
pub use parse::scp as parse;

#[derive(Debug, Clone, PartialEq)]
pub struct Scp {
	pub items: Vec<Item>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
	Global(Global),
	Function(Function),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
	Int(i32),
	Float(f32),
	String(String),
}


#[derive(Debug, Clone, PartialEq)]
pub struct Global {
	pub name: String,
	pub ty: GlobalType,
	pub line: Option<u16>,
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
	pub calls: Vec<Call>,
	pub is_prelude: bool,
	// pub code: Vec<Op>,
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

#[derive(Debug, Clone, PartialEq)]
pub struct Call {
	pub kind: CallKind,
	pub args: Vec<CallArg>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CallArg {
	Value(Value),
	Call,
	Var,
	Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CallKind {
	Normal(String),
	Tailcall(String),
	Syscall(u8, u8),
}
