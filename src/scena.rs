mod flat;
mod called;

use indexmap::IndexMap;

use crate::scp::{Op, Scp};
pub use crate::scp::{ArgType, Binop, GlobalType, Unop, Value, Label, Name};

pub fn from_scp(scp: Scp) -> Scena {
	let mut globals = scp.globals.into_iter().map(|g| (g.name, Global {
		ty: g.ty,
		line: None,
	})).collect::<IndexMap<_, _>>();
	let mut globals_iter = globals.iter_mut();
	let mut functions = IndexMap::with_capacity(scp.functions.len());
	for mut f in scp.functions {
		let _span = tracing::info_span!("function", name = f.name).entered();

		let mut code = f.code.as_slice();
		while code.last().is_some_and(|op| matches!(op, Op::Line(_))) {
			code = &code[..code.len() - 1];
		}
		for line in f.code.drain(code.len()..) {
			let Op::Line(n) = line else { unreachable!() };
			if let Some(g) = globals_iter.next() {
				g.1.line = Some(n);
			} else {
				tracing::warn!("extra line number: {n}");
			}
		}

		functions.insert(f.name, Function {
			args: f.args.into_iter().map(|a| Arg {
				ty: a.ty,
				default: a.default,
				line: None,
			}).collect(),
			called: Called::Raw(f.called),
			body: Body::Asm(f.code),
		});
	}
	Scena { globals, functions }
}

pub fn decompile(scena: &mut Scena) {
	for i in 0..scena.functions.len() { // need indexes to satisfy borrowck
		let entry = scena.functions.get_index_mut(i).unwrap();
		let mut f = entry.1.clone();
		let _span = tracing::info_span!("function", name = entry.0).entered();

		if let Body::Asm(ops) = &mut f.body {
			match flat::decompile(ops) {
				Ok(stmts) => {
					similar_asserts::assert_eq!(*ops, flat::compile(&stmts).unwrap());
					f.body = Body::Flat(stmts);
				}
				Err(e) => {
					tracing::error!("decompile error: {e} for {ops:#?}");
				}
			}
		}

		if let Called::Raw(called) = &f.called {
			match &mut f.body {
				Body::Asm(_) => {},
				Body::Flat(stmts) => {
					let (stmts2, dup) = called::apply_flat(stmts.clone(), called, &scena.functions).unwrap();
					let (stmts3, called2) = called::infer_flat(stmts2.clone(), dup, &scena.functions).unwrap();
					similar_asserts::assert_eq!(*called, called2);
					similar_asserts::assert_eq!(*stmts, stmts3);
					f.called = Called::Merged(dup);
				}
			}
		}

		*scena.functions.get_index_mut(i).unwrap().1 = f;
	}
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Scena {
	pub globals: IndexMap<String, Global>,
	pub functions: IndexMap<String, Function>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Global {
	pub ty: GlobalType,
	pub line: Option<u16>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
	pub args: Vec<Arg>,
	pub called: Called,
	pub body: Body,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Arg {
	pub ty: ArgType,
	pub default: Option<Value>,
	pub line: Line,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Called {
	Raw(Vec<crate::scp::Call>),
	Merged(bool),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Body {
	Asm(Vec<Op>),
	Flat(Vec<FlatStmt>),
}

pub type Line = Option<u16>;

#[derive(Clone, PartialEq)]
pub enum Expr {
	Value(Line, Value),
	Var(Line, Place),
	Ref(Line, u32),
	Call(Line, Name, Vec<Expr>),
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
	Goto(Label),
	Switch(Line, Expr, Vec<(i32, Label)>, Label),
	PushVar(Line),
	PopVar(usize),
	Debug(Line, Vec<Expr>),
	Tailcall(Line, Name, Vec<Expr>, usize),
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
