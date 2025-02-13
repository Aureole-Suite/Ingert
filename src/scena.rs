mod flat;

use crate::scp::{Op, Scp};
pub use crate::scp::{Arg, Binop, CallKind, GlobalType, Unop, Value, Label};

pub fn decompile(scp: &Scp) -> Scena {
	let mut globals = scp.globals.iter().rev();
	let mut items = Vec::new();
	for f in scp.functions.iter().rev() {
		let _span = tracing::info_span!("function", name = f.name).entered();
		let mut code = f.code.as_slice();
		while let Some(Op::Line(n)) = code.last() && let Some(g) = globals.next() {
			code = &code[..code.len() - 1];
			items.push(Item::Global(Global {
				name: g.name.clone(),
				ty: g.ty,
				line: Some(*n),
			}));
		}
		let body = match flat::decompile(code) {
			Ok(body) => Body::Flat(body),
			Err(e) => {
				tracing::error!("decompile error: {e}");
				Body::Asm(code.to_vec())
			},
		};
		items.push(Item::Function(Function {
			name: f.name.clone(),
			args: f.args.clone(),
			body,
		}));
	}
	for g in globals {
		items.push(Item::Global(Global {
			name: g.name.clone(),
			ty: g.ty,
			line: None,
		}));
	}
	items.reverse();
	Scena { items }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Scena {
	pub items: Vec<Item>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
	Global(Global),
	Function(Function),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Global {
	pub name: String,
	pub ty: GlobalType,
	pub line: Option<u16>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
	pub name: String,
	pub args: Vec<Arg>,
	pub body: Body,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Body {
	Asm(Vec<Op>),
	Flat(Vec<FlatStmt>),
	Tree(()),
}

#[derive(Clone, PartialEq)]
pub enum Expr {
	Value(Option<u16>, Value),
	Var(Option<u16>, Place),
	Ref(Option<u16>, u32),
	Call(Option<u16>, CallKind, Vec<Expr>),
	Unop(Option<u16>, Unop, Box<Expr>),
	Binop(Option<u16>, Binop, Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Place {
	Var(u32),
	Deref(u32),
	Global(String),
}

#[derive(Clone, PartialEq)]
pub enum FlatStmt {
	Label(Label),
	Expr(Expr),
	Set(Option<u16>, Place, Expr),
	Return(Option<u16>, Option<Expr>),
	If(Option<u16>, Expr, Label),
	Goto(Label),
	Switch(Option<u16>, Expr, Vec<(i32, Label)>, Label),
	PushVar(Option<u16>),
	Debug(Option<u16>, Vec<Expr>),
}

fn line<'a, 'b>(f: &'a mut std::fmt::Formatter<'b>, l: &Option<u16>) -> Result<&'a mut std::fmt::Formatter<'b>, std::fmt::Error> {
	if let Some(l) = l {
		write!(f, "{l}:")?;
	}
	Ok(f)
}

fn write_args(f: &mut std::fmt::Formatter<'_>, name: &str, args: &[Expr]) -> std::fmt::Result {
	let mut t = f.debug_tuple(name);
	for arg in args {
		t.field(arg);
	}
	t.finish()
}

impl std::fmt::Debug for Expr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Value(l, v) => {
				line(f, l)?;
				v.fmt(f)
			}
			Self::Var(l, v) => line(f, l)?.debug_tuple("Var").field(v).finish(),
			Self::Ref(l, v) => line(f, l)?.debug_tuple("Ref").field(v).finish(),
			Self::Call(l, c, args) => {
				line(f, l)?;
				match c {
					CallKind::Normal(n) => write_args(f, &format!("Call[{n}]"), args),
					CallKind::Tailcall(n) => write_args(f, &format!("Tailcall[{n}]"), args),
					CallKind::Syscall(a, b) => write_args(f, &format!("Syscall[{a},{b}]"), args),
				}
			},
			Self::Unop(l, op, a) => line(f, l)?.debug_tuple(&format!("{op:?}")).field(a).finish(),
			Self::Binop(l, op, a, b) => line(f, l)?.debug_tuple(&format!("{op:?}")).field(a).field(b).finish(),
		}
	}
}

impl std::fmt::Debug for FlatStmt {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Label(label) => f.debug_tuple("Label").field(label).finish(),
			Self::Expr(e) => f.debug_tuple("Expr").field(e).finish(),
			Self::Set(l, v, e) => line(f, l)?.debug_tuple("Set").field(v).field(e).finish(),
			Self::Return(l, e) => line(f, l)?.debug_tuple("Return").field(e).finish(),
			Self::If(l, e, label) => line(f, l)?.debug_tuple("If").field(e).field(label).finish(),
			Self::Goto(label) => f.debug_tuple("Goto").field(label).finish(),
			Self::Switch(l, e, cases, default) => line(f, l)?.debug_tuple("Switch").field(e).field(cases).field(default).finish(),
			Self::PushVar(l) => line(f, l)?.debug_tuple("PushVar").finish(),
			Self::Debug(l, args) => {
				line(f, l)?;
				write_args(f, "Debug", args)
			}
		}
	}
}

impl crate::labels::Labels for FlatStmt {
	fn defined(&self) -> Option<&Label> {
		match self {
			Self::Label(l) => Some(l),
			_ => None,
		}
	}

	fn defined_mut(&mut self) -> Option<&mut Label> {
		match self {
			Self::Label(l) => Some(l),
			_ => None,
		}
	}

	fn referenced_mut(&mut self, mut f: impl FnMut(&mut Label)) {
		match self {
			Self::If(_, _, l) => f(l),
			Self::Goto(l) => f(l),
			Self::Switch(_, _, cases, default) => {
				for (_, l) in cases {
					f(l);
				}
				f(default);
			}
			_ => {},
		}
	}
}
