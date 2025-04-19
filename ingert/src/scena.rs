mod flat;
mod tree;
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
			is_prelude: f.is_prelude,
			body: Body::Asm(f.code),
		});
	}
	Scena { globals, functions }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Default)]
pub enum DecompileMode {
	Asm,
	Flat,
	#[default]
	Tree,
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct DecompileOptions {
	pub mode: DecompileMode,
	pub called: bool,
}

pub fn decompile(scena: &mut Scena, opts: &DecompileOptions) {
	let mut funcsig = scena.functions.iter().map(|(name, f)| (name.clone(), f.args.clone())).collect();

	for (name, f) in &mut scena.functions {
		let _span = tracing::info_span!("function", name = name).entered();

		if let Body::Asm(ops) = &f.body && opts.mode >= DecompileMode::Flat {
			match flat::decompile(ops) {
				Ok(stmts) => f.body = Body::Flat(stmts),
				Err(e) => tracing::warn!("decompile error: {e}"),
			}
		}

		if let Body::Flat(fstmts) = &f.body && opts.mode >= DecompileMode::Tree {
			match tree::decompile(fstmts, f.args.len()) {
				Ok(stmts) => f.body = Body::Tree(stmts),
				Err(e) => tracing::warn!("decompile error: {e}"),
			}
		}

		if let Called::Raw(called) = &f.called && opts.called {
			match &mut f.body {
				Body::Asm(_) => {},
				Body::Flat(stmts) => {
					let mut stmts2 = stmts.clone();
					match called::apply_flat(&mut stmts2, called, &mut funcsig) {
						Ok(result) => {
							*stmts = stmts2;
							f.called = Called::Merged(result);
						}
						Err(e) => tracing::warn!("called error: {e}"),
					}
				}
				Body::Tree(stmts) => {
					let mut stmts2 = stmts.clone();
					match called::apply_tree(&mut stmts2, called, &mut funcsig) {
						Ok(result) => {
							*stmts = stmts2;
							f.called = Called::Merged(result);
						}
						Err(e) => tracing::warn!("called error: {e}"),
					}
				}
			}
		}
	}

	for (k, v) in funcsig {
		scena.functions[&k].args = v;
	}
}

#[derive(Debug, snafu::Snafu)]
#[snafu(module(compile), context(suffix(false)))]
pub enum CompileError {
	#[snafu(context(false))]
	Flat { source: flat::CompileError },
	#[snafu(context(false))]
	Tree { source: tree::CompileError },
	#[snafu(context(false))]
	Called { source: called::InferError },
	#[snafu(display("can't infer calls for asm function"))]
	CalledAsm,
}

pub fn compile(scena: Scena) -> Result<Scp, CompileError> {
	let mut global_lines = scena.globals.iter().filter_map(|g| g.1.line).peekable();
	let mut functions = Vec::<crate::scp::Function>::with_capacity(scena.functions.len());
	let funcsig = scena.functions.iter().map(|(name, f)| (name.clone(), f.args.clone())).collect::<IndexMap<_, _>>();
	for (name, mut func) in scena.functions {
		if let Some(func_line) = first_line(&func.body) {
			while let Some(global_line) = global_lines.next_if(|l| *l <= func_line) {
				let Some(last_func) = functions.last_mut() else {
					tracing::warn!("global comes before first function");
					continue;
				};
				last_func.code.push(Op::Line(global_line));
			}
		}
		let _span = tracing::info_span!("function", name = name).entered();
		let called = match (func.called, &mut func.body) {
			(Called::Raw(called), _) => called,
			(Called::Merged(dup), Body::Flat(stmts)) => called::infer_flat(stmts, dup, &funcsig)?,
			(Called::Merged(dup), Body::Tree(stmts)) => called::infer_tree(stmts, dup, &funcsig)?,
			(Called::Merged(_), Body::Asm(_)) => return compile::CalledAsm.fail(),
		};
		let code = match func.body {
			Body::Asm(ops) => ops,
			Body::Flat(stmts) => flat::compile(&stmts)?,
			Body::Tree(stmts) => flat::compile(&tree::compile(&stmts, func.args.len())?)?,
		};
		functions.push(crate::scp::Function {
			name,
			args: func.args.into_iter().map(|a| crate::scp::Arg {
				ty: a.ty,
				default: a.default,
			}).collect(),
			called,
			is_prelude: func.is_prelude,
			code,
		})
	}

	let globals = scena.globals.into_iter().map(|(name, g)| crate::scp::Global { name, ty: g.ty }).collect();
	Ok(Scp {
		globals,
		functions,
	})
}

pub fn first_line(body: &Body) -> Line {
	match body {
		Body::Asm(ops) => ops.iter().find_map(|op| match op { Op::Line(n) => Some(*n), _ => None }),
		Body::Flat(stmts) => stmts.iter().find_map(|stmt| stmt.line().flatten()),
		Body::Tree(stmts) => stmts.iter().find_map(|stmt| stmt.line().flatten()),
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
	pub is_prelude: bool,
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
pub enum Expr<V=Var> {
	Value(Line, Value),
	Var(Line, Place<V>),
	Ref(Line, V),
	Call(Line, Name, Vec<Expr<V>>),
	Syscall(Line, u8, u8, Vec<Expr<V>>),
	Unop(Line, Unop, Box<Expr<V>>),
	Binop(Line, Binop, Box<Expr<V>>, Box<Expr<V>>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Place<V=Var> {
	Var(V),
	Deref(V),
	Global(String),
}

#[derive(Clone, PartialEq)]
pub enum FlatStmt {
	Label(Label),
	Expr(Expr<FlatVar>),
	Set(Line, Place<FlatVar>, Expr<FlatVar>),
	Return(Line, Option<Expr<FlatVar>>, usize),
	If(Line, Expr<FlatVar>, Label),
	Goto(Label, usize),
	Switch(Line, Expr<FlatVar>, Vec<(i32, Label)>, Label),
	PushVar(Line),
	PopVar(usize),
	Debug(Line, Vec<Expr<FlatVar>>),
	Tailcall(Line, Name, Vec<Expr<FlatVar>>, usize),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct FlatVar(pub u32);

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
	PushVar(Line, Var, Option<Expr>),
	Debug(Line, Vec<Expr>),
	Tailcall(Line, Name, Vec<Expr>),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Var(pub u32);

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
