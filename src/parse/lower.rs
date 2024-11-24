use std::cell::RefCell;
use std::collections::HashMap;

use super::ast::{self, Span};

#[derive(Debug)]
pub enum Error {
	DuplicateDef {
		what: &'static str,
		name: String,
		first: Span,
		second: Span,
	},
	DuplicateCase {
		case: Option<i32>,
		first: Span,
		second: Span,
	},
	IllegalBreak {
		span: Span,
	},
	IllegalContinue {
		span: Span,
	},
	MissingFunction {
		name: String,
		span: Span,
	},
	MissingGlobal {
		name: String,
		span: Span,
	},
	ReservedFunctionName {
		name: String,
		span: Span,
	},
}

#[derive(Default)]
struct Diag {
	diags: RefCell<Vec<Error>>,
}

impl Diag {
	fn error(&self, error: Error) {
		self.diags.borrow_mut().push(error);
	}

	fn insert<'a, V>(
		&self,
		what: &'static str,
		map: &mut HashMap<&'a str, V>,
		key: &'a str,
		value: V,
		mut span: impl FnMut(&V) -> Span,
	) {
		let value_span = span(&value);
		if let Some(first) = map.insert(key, value) {
			self.error(Error::DuplicateDef {
				what,
				name: key.to_string(),
				first: span(&first),
				second: value_span,
			});
		}
	}
}

struct RootCtx<'a> {
	functions: HashMap<&'a str, &'a ast::Function>,
	globals: HashMap<&'a str, &'a ast::Global>,
	diag: Diag,
}

pub fn lower(items: &[ast::Item]) -> (Vec<crate::Item>, Vec<Error>) {
	let mut ctx = RootCtx {
		functions: HashMap::new(),
		globals: HashMap::new(),
		diag: Diag::default(),
	};
	for item in items {
		match item {
			ast::Item::Global(g) => {
				ctx.diag.insert("global", &mut ctx.globals, &g.name.value, g, |g| g.name.span);
			}
			ast::Item::Function(f) => {
				ctx.diag.insert("fn", &mut ctx.functions, &f.name.value, f, |f| f.name.span);
			}
		}
	}

	let mut out = Vec::new();
	for item in items {
		match item {
			ast::Item::Global(g) => {
				out.push(crate::Item::Global(crate::Global {
					line: g.line,
					name: g.name.value.clone(),
					ty: g.ty,
				}));
			}
			ast::Item::Function(f) => {
				if f.name.value == "debug" {
					ctx.diag.error(Error::ReservedFunctionName {
						name: f.name.value.clone(),
						span: f.name.span,
					});
				}
				out.push(crate::Item::Function(lower_fn(&ctx, f)));
			}
		}
	}

	(out, ctx.diag.diags.into_inner())
}

#[derive(Clone)]
struct Scope<'a> {
	root: &'a RootCtx<'a>,
	locals: HashMap<&'a str, (Span, i32)>,
	nlocals: usize,
	brk: bool,
	cont: bool,
}

fn lower_fn(ctx: &RootCtx, f: &ast::Function) -> crate::Function {
	let mut scope = Scope {
		root: ctx,
		locals: HashMap::new(),
		nlocals: 0,
		brk: false,
		cont: false,
	};
	let mut args = Vec::new();
	for (arg, i) in f.args.iter().zip(0..) {
		args.push(crate::Arg {
			out: arg.out,
			ty: arg.ty,
			default: arg.default.clone(),
		});
		scope.root.diag.insert("arg", &mut scope.locals, &arg.name.value, (arg.name.span, !i), |(s, _)| *s);
	}
	crate::Function {
		name: f.name.value.clone(),
		args,
		body: map_stmts(scope, &f.body),
		is_prelude: f.prelude,
		dup: f.dup,
	}
}

fn map_stmts<'a>(mut scope: Scope<'a>, body: &'a [ast::Stmt]) -> Vec<crate::Stmt> {
	let mut out = Vec::new();
	for stmt in body {
		match stmt {
			ast::Stmt::Expr(e) => {
				if let ast::Expr::Call(l, c, args) = e
					&& let ast::CallKind::Func(name) = c
					&& name.value == "debug"
				{
					let args = args.iter().map(|a| map_expr(&scope, a)).collect();
					out.push(crate::Stmt::Debug(*l, args));
				} else {
					out.push(crate::Stmt::Expr(map_expr(&scope, e)));
				}
			}
			ast::Stmt::PushVar(l, v, e) => {
				scope.root.diag.insert("local", &mut scope.locals, &v.value, (v.span, scope.nlocals as i32), |(s, _)| *s);
				scope.nlocals += 1;
				let v = lookup_local(&scope, v);
				let e = e.as_ref().map(|e| map_expr(&scope, e));
				out.push(crate::Stmt::PushVar(*l, v, e));
			}
			ast::Stmt::Set(l, v, e) => {
				let v = map_lvalue(&scope, v);
				let e = map_expr(&scope, e);
				out.push(crate::Stmt::Set(*l, v, e));
			}
			ast::Stmt::If(l, e, yes, no) => {
				let e = map_expr(&scope, e);
				let yes = map_stmts(scope.clone(), yes);
				let no = no.as_ref().map(|no| map_stmts(scope.clone(), no));
				out.push(crate::Stmt::If(*l, e, yes, no));
			}
			ast::Stmt::While(l, e, body) => {
				let e = map_expr(&scope, e);
				let mut inner = scope.clone();
				inner.brk = true;
				inner.cont = true;
				let body = map_stmts(inner, body);
				out.push(crate::Stmt::While(*l, e, body));
			}
			ast::Stmt::Switch(l, e, cases) => {
				let e = map_expr(&scope, e);
				let mut used_cases = HashMap::new();
				let mut out_cases = Vec::new();
				for (case, body) in cases {
					if let Some(prev) = used_cases.insert(case.value, case.span) {
						scope.root.diag.error(Error::DuplicateCase {
							case: case.value,
							first: prev,
							second: case.span,
						});
					}
					let mut inner = scope.clone();
					inner.brk = true;
					let body = map_stmts(inner, body);
					out_cases.push((case.value, body));
				}
				out.push(crate::Stmt::Switch(*l, e, out_cases));
			}
			ast::Stmt::Break(span) => {
				if !scope.brk {
					scope.root.diag.error(Error::IllegalBreak { span: *span });
				}
				out.push(crate::Stmt::Break);
			}
			ast::Stmt::Continue(span) => {
				if !scope.cont {
					scope.root.diag.error(Error::IllegalContinue { span: *span });
				}
				out.push(crate::Stmt::Continue);
			}
			ast::Stmt::Return(l, e) => {
				let e = e.as_ref().map(|e| map_expr(&scope, e));
				out.push(crate::Stmt::Return(*l, e));
			}
		}
	}
	out
}

fn map_expr(scope: &Scope, e: &ast::Expr) -> crate::Expr {
	match e {
		ast::Expr::Value(l, v) => crate::Expr::Value(*l, v.clone()),
		ast::Expr::Var(l, v) => crate::Expr::Var(*l, map_lvalue(scope, v)),
		ast::Expr::Ref(l, v) => crate::Expr::Ref(*l, lookup_local(scope, v)),
		ast::Expr::Call(l, c, args) => {
			let c = map_call(scope, c);
			let args = args.iter().map(|a| map_expr(scope, a)).collect();
			crate::Expr::Call(*l, c, args)
		}
		ast::Expr::Unop(l, o, a) => {
			let a = map_expr(scope, a);
			crate::Expr::Unop(*l, *o, Box::new(a))
		}
		ast::Expr::Binop(l, o, a, b) => {
			let a = map_expr(scope, a);
			let b = map_expr(scope, b);
			crate::Expr::Binop(*l, *o, Box::new(a), Box::new(b))
		}
	}
}

fn map_lvalue(scope: &Scope, v: &ast::Lvalue) -> crate::Lvalue {
	match v {
		ast::Lvalue::Stack(s) => crate::Lvalue::Stack(lookup_local(scope, s)),
		ast::Lvalue::Deref(d) => crate::Lvalue::Deref(lookup_local(scope, d)),
		ast::Lvalue::Global(g) => crate::Lvalue::Global(lookup_global(scope, g)),
	}
}

fn map_call(scope: &Scope, c: &ast::CallKind) -> crate::CallKind {
	match c {
		ast::CallKind::System(a, b) => crate::CallKind::System(*a, *b),
		ast::CallKind::Func(name) => {
			lookup_fn(scope, name);
			crate::CallKind::Func(name.value.clone())
		}
		ast::CallKind::Tail(name) => {
			lookup_fn(scope, name);
			crate::CallKind::Tail(name.value.clone())
		}
	}
}

fn lookup_local(scope: &Scope, v: &ast::Ident) -> crate::StackVar {
	if let Some((_, i)) = scope.locals.get(v.value.as_str()) {
		crate::StackVar(*i)
	} else {
		scope.root.diag.error(Error::MissingGlobal {
			name: v.value.clone(),
			span: v.span,
		});
		crate::StackVar(i32::MAX)
	}
}

fn lookup_global(scope: &Scope, g: &ast::Ident) -> String {
	if !scope.root.globals.contains_key(g.value.as_str()) {
		scope.root.diag.error(Error::MissingGlobal {
			name: g.value.clone(),
			span: g.span,
		});
	}
	g.value.clone()
}

fn lookup_fn(scope: &Scope, name: &ast::Ident) {
	if !name.value.contains('.') && !scope.root.functions.contains_key(name.value.as_str()) {
		scope.root.diag.error(Error::MissingFunction {
			name: name.value.clone(),
			span: name.span,
		});
	}
}
