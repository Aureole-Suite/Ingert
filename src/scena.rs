mod flat;
mod tree;
mod called;

use std::fmt::Write as _;
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
	let mut funcsig = scena.functions.iter().map(|(name, f)| (name.clone(), f.args.clone())).collect();

	for (name, f) in &mut scena.functions {
		let _span = tracing::info_span!("function", name = name).entered();

		if let Body::Asm(ops) = &f.body {
			match flat::decompile(ops) {
				Ok(stmts) => {
					let recomp = flat::compile(&stmts).unwrap();
					if *ops != recomp {
						let orig = format!("{ops:#?}");
						let recomp = format!("{recomp:#?}");
						let diff = similar_asserts::SimpleDiff::from_str(&orig, &recomp, "original", "recompiled");
						tracing::error!("decompile error on {name}\n{stmts:#?}\n{diff}");
					}
					f.body = Body::Flat(stmts);
				}
				Err(e) => {
					tracing::error!("decompile error: {e} for {ops:#?}");
				}
			}
		}

		if let Body::Flat(fstmts) = &f.body {
			match tree::decompile(fstmts) {
				Ok(stmts) => {
					let recomp = tree::compile(&stmts, f.args.len()).unwrap();
					if *fstmts != recomp {
						let orig = format!("{fstmts:#?}");
						let recomp = format!("{recomp:#?}");
						let diff = similar_asserts::SimpleDiff::from_str(&orig, &recomp, "original", "recompiled");
						tracing::error!("decompile error on {name}\n{stmts:#?}\n{diff}");
					}
					f.body = Body::Tree(stmts);
				}
				Err(e) => {
					tracing::error!("decompile error");
					for error in snafu::ErrorCompat::iter_chain(&e) {
						tracing::error!("- {error}");
					}
					let mut str = "code:".to_string();
					for (i, stmt) in fstmts.iter().enumerate() {
						write!(str, "\n{i}: {stmt:?}");
					}
					tracing::error!("{str}");
				}
			}
		}

		if let Called::Raw(called) = &f.called {
			match &mut f.body {
				Body::Asm(_) => {},
				Body::Flat(stmts) => {
					let mut new = stmts.clone();
					let dup = called::apply_flat(&mut new, called, &mut funcsig).unwrap();

					let mut stmts2 = new.clone();
					let called2 = called::infer_flat(&mut stmts2, dup, &funcsig).unwrap();
					similar_asserts::assert_eq!(*called, called2);
					similar_asserts::assert_eq!(*stmts, stmts2);

					*stmts = new;
					f.called = Called::Merged(dup);
				}
				Body::Tree(stmts) => {
					let mut new = stmts.clone();
					let dup = called::apply_tree(&mut new, called, &mut funcsig).unwrap();

					let mut stmts2 = new.clone();
					let called2 = called::infer_tree(&mut stmts2, dup, &funcsig).unwrap();
					similar_asserts::assert_eq!(*called, called2);
					similar_asserts::assert_eq!(*stmts, stmts2);

					*stmts = new;
					f.called = Called::Merged(dup);
				}
			}
		}
	}

	for (k, v) in funcsig {
		scena.functions[&k].args = v;
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
	Tree(Vec<Stmt>),
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
	Goto(Label, usize),
	Switch(Line, Expr, Vec<(i32, Label)>, Label),
	PushVar(Line),
	PopVar(usize),
	Debug(Line, Vec<Expr>),
	Tailcall(Line, Name, Vec<Expr>, usize),
}

#[derive(Clone, PartialEq)]
pub enum Stmt {
	Expr(Expr),
	Set(Line, Place, Expr),
	Return(Line, Option<Expr>),
	If(Line, Expr, Vec<Stmt>, Option<Vec<Stmt>>),
	While(Line, Expr, Vec<Stmt>),
	Switch(Line, Expr, IndexMap<Option<i32>, Vec<Stmt>>),
	Block(Vec<Stmt>),
	Break,
	Continue,
	PushVar(Line),
	Debug(Line, Vec<Expr>),
	Tailcall(Line, Name, Vec<Expr>),
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
			Self::Goto(l, _) => f(l),
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
			Self::Goto(l, _) => f(l),
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
