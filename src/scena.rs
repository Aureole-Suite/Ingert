mod flat;

use crate::scp::{Op, Scp};
pub use crate::scp::{Arg, Binop, GlobalType, Unop, Value, Label};

pub fn decompile(scp: &Scp) -> Scena {
	let mut globals = scp.globals.iter().rev();
	let mut items = Vec::new();
	for f in scp.functions.iter().rev() {
		let _span = tracing::info_span!("function", name = f.name).entered();
		let mut code = f.code.as_slice();

		// // Temporarily remove line numbers so I can roundtrip the rest
		// let mut code = code.to_owned();
		// code.retain_mut(|op| !matches!(op, Op::Line(_)));
		// crate::labels::normalize(&mut code, 0).unwrap();
		// let mut code = code.as_slice();

		while let Some(Op::Line(n)) = code.last() && let Some(g) = globals.next() {
			code = &code[..code.len() - 1];
			items.push(Item::Global(Global {
				name: g.name.clone(),
				ty: g.ty,
				line: Some(*n),
			}));
		}
		let body = match flat::decompile(code) {
			Ok(body) => {
				// #[cfg(debug_assertions)]
				similar_asserts::assert_eq!(code, flat::compile(&body).unwrap());
				Body::Flat(body)
			},
			Err(e) => {
				tracing::error!("decompile error: {e}");
				dbg!(code);
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

pub type Line = Option<u16>;

#[derive(Clone, PartialEq)]
pub enum Expr {
	Value(Line, Value),
	Var(Line, Place),
	Ref(Line, u32),
	Call(Line, String, String, Vec<Expr>),
	Syscall(Line, u8, u8, Vec<Expr>),
	Unop(Line, Unop, Box<Expr>),
	Binop(Line, Binop, Box<Expr>, Box<Expr>),
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
	Set(Line, Place, Expr),
	Return(Line, Option<Expr>, usize),
	If(Line, Expr, Label),
	While(Line, Expr, Label),
	Goto(Label),
	Switch(Line, Expr, Vec<(i32, Label)>, Label),
	PushVar(Line),
	PopVar(usize),
	Debug(Line, Vec<Expr>),
	Tailcall(Line, String, String, Vec<Expr>, usize),
}

mod fmt;

impl crate::labels::Labels for FlatStmt {
	fn defined(&self) -> Option<&Label> {
		match self {
			Self::Label(l) => Some(l),
			_ => None,
		}
	}

	fn referenced(&self, mut f: impl FnMut(&Label)) {
		match self {
			Self::If(_, _, l) => f(l),
			Self::While(_, _, l) => f(l),
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

impl crate::labels::LabelsMut for FlatStmt {
	fn defined_mut(&mut self) -> Option<&mut Label> {
		match self {
			Self::Label(l) => Some(l),
			_ => None,
		}
	}

	fn referenced_mut(&mut self, mut f: impl FnMut(&mut Label)) {
		match self {
			Self::If(_, _, l) => f(l),
			Self::While(_, _, l) => f(l),
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

impl Expr {
	pub fn line(&self) -> Option<Line> {
		match self {
			Self::Value(l, _) => Some(*l),
			Self::Var(l, _) => Some(*l),
			Self::Ref(l, _) => Some(*l),
			Self::Call(l, _, _, _) => Some(*l),
			Self::Syscall(l, _, _, _) => Some(*l),
			Self::Unop(l, _, _) => Some(*l),
			Self::Binop(l, _, _, _) => Some(*l),
		}
	}

	pub fn line_mut(&mut self) -> Option<&mut Line> {
		match self {
			Self::Value(l, _) => Some(l),
			Self::Var(l, _) => Some(l),
			Self::Ref(l, _) => Some(l),
			Self::Call(l, _, _, _) => Some(l),
			Self::Syscall(l, _, _, _) => Some(l),
			Self::Unop(l, _, _) => Some(l),
			Self::Binop(l, _, _, _) => Some(l),
		}
	}
}

impl FlatStmt {
	pub fn line(&self) -> Option<Line> {
		match self {
			Self::Label(_) => None,
			Self::Expr(_) => None, // Not sure about this one
			Self::Set(l, _, _) => Some(*l),
			Self::Return(l, _, _) => Some(*l),
			Self::If(l, _, _) => Some(*l),
			Self::While(l, _, _) => Some(*l),
			Self::Goto(_) => None,
			Self::Switch(l, _, _, _) => Some(*l),
			Self::PushVar(l) => Some(*l),
			Self::PopVar(_) => None,
			Self::Debug(l, _) => Some(*l),
			Self::Tailcall(l, _, _, _, _) => Some(*l),
		}
	}

	pub fn line_mut(&mut self) -> Option<&mut Line> {
		match self {
			Self::Label(_) => None,
			Self::Expr(_) => None, // Not sure about this one
			Self::Set(l, _, _) => Some(l),
			Self::Return(l, _, _) => Some(l),
			Self::If(l, _, _) => Some(l),
			Self::While(l, _, _) => Some(l),
			Self::Goto(_) => None,
			Self::Switch(l, _, _, _) => Some(l),
			Self::PushVar(l) => Some(l),
			Self::PopVar(_) => None,
			Self::Debug(l, _) => Some(l),
			Self::Tailcall(l, _, _, _, _) => Some(l),
		}
	}
}
