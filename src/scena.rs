mod flat;

use crate::scp::{Op, Scp};
pub use crate::scp::{Arg, Binop, CallKind, GlobalType, Unop, Value};
pub use flat::FlatStmt;

pub fn decompile(scp: &Scp) -> Scena {
	let mut globals = scp.globals.iter().rev();
	let mut items = Vec::new();
	for f in scp.functions.iter().rev() {
		let _span = tracing::info_span!("decompile", name = f.name).entered();
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

#[derive(Debug, Clone, PartialEq)]
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
