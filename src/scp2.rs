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
