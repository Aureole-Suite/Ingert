mod expr;

use crate::scp::{Op, Scp};
pub use crate::scp::{Arg, Binop, CallKind, GlobalType, Unop, Value};

pub fn decompile(scp: &Scp) -> Scena {
	let mut globals = scp.globals.iter().rev();
	let mut items = Vec::new();
	for f in scp.functions.iter().rev() {
		let _span = tracing::info_span!("decompile", name = f.name).entered();
		dbg!(f);
		let mut code = f.code.as_slice();
		while let Some(Op::Line(n)) = code.last() && let Some(g) = globals.next() {
			code = &code[..code.len() - 1];
			items.push(Item::Global(Global {
				name: g.name.clone(),
				ty: g.ty,
				line: Some(*n),
			}));
		}
		tracing::info!("{:#?}", expr::build_exprs(f.args.len(), code));
		items.push(Item::Function(Function {
			name: f.name.clone(),
			args: f.args.clone(),
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
	// ...
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
	Value(Value),
	Var(Place),
	Ref(u32),
	Call(CallKind, Vec<Expr>),
	Unop(Unop, Box<Expr>),
	Binop(Binop, Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Place {
	Var(u32),
	Deref(u32),
	Global(String),
}
