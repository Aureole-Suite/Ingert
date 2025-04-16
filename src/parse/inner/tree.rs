use indexmap::IndexMap;

use crate::scena::{Expr, Place, Stmt, Var};
use crate::scp::{Binop, Name, Unop, Value};

use crate::parse::{self, do_parse, Alt, Parser, Result, Scope};

struct Ctx<'a> {
	scope: &'a Scope,
	brk: bool,
	cont: bool,
	vars: Vec<String>,
}

impl Ctx<'_> {
	fn sub(&mut self) -> Ctx<'_> {
		Ctx {
			scope: self.scope,
			brk: self.brk,
			cont: self.cont,
			vars: self.vars.clone(),
		}
	}

	fn with_brk(mut self) -> Self {
		self.brk = true;
		self
	}

	fn with_cont(mut self) -> Self {
		self.cont = true;
		self
	}
}

pub fn parse(parser: Parser, scope: &Scope, vars: Vec<String>) -> Vec<Stmt> {
	parse_tree(parser, Ctx {
		scope,
		brk: false,
		cont: false,
		vars,
	})
}

fn parse_tree(parser: Parser, mut ctx: Ctx) -> Vec<Stmt> {
	do_parse(parser, |parser| {
		let mut stmts = Vec::new();
		while !parser.at_end() {
			stmts.push(parse_stmt(parser, &mut ctx)?);
		}
		Ok(stmts)
	}).unwrap_or_default()
}

fn parse_stmt(parser: &mut Parser, ctx: &mut Ctx) -> Result<Stmt> {
	let l = parser.line();
	Alt::new(parser)
		.test(|parser| {
			parser.keyword("if")?;
			parser.commit();
			let cond = parse_expr(parser, ctx)?;
			let then = parse_tree(parser.delim('{')?, ctx.sub());
			let els = if parser.keyword("else").is_ok() {
				if parser.peek().keyword("if").is_ok() {
					Some(vec![parse_stmt(parser, ctx)?])
				} else {
					Some(parse_tree(parser.delim('{')?, ctx.sub()))
				}
			} else {
				None
			};
			Ok(Stmt::If(l, cond, then, els))
		})
		.test(|parser| {
			parser.keyword("while")?;
			parser.commit();
			let cond = parse_expr(parser, ctx)?;
			let body = parse_tree(parser.delim('{')?, ctx.sub().with_brk().with_cont());
			Ok(Stmt::While(l, cond, body))
		})
		.test(|parser| {
			parser.keyword("switch")?;
			parser.commit();
			let cond = parse_expr(parser, ctx)?;
			let body = do_parse(parser.delim('{')?, |p| parse_switch(p, ctx.sub().with_brk())).unwrap_or_default();
			Ok(Stmt::Switch(l, cond, body))
		})
		.test(|parser| {
			parser.keyword("return")?;
			parser.commit();
			let val = if parser.punct(';').is_ok() {
				None
			} else {
				let expr = parse_expr(parser, ctx)?;
				parser.punct(';')?;
				Some(expr)
			};
			Ok(Stmt::Return(l, val))
		})
		.test(|parser| {
			parser.keyword("break")?;
			let span = parser.prev_span();
			parser.commit();
			if !ctx.brk {
				parser.errors.error("break outside of loop/switch", span);
			}
			parser.punct(';')?;
			Ok(Stmt::Break)
		})
		.test(|parser| {
			parser.keyword("continue")?;
			let span = parser.prev_span();
			parser.commit();
			if !ctx.cont {
				parser.errors.error("continue outside of loop", span);
			}
			parser.punct(';')?;
			Ok(Stmt::Continue)
		})
		.test(|parser| {
			parser.keyword("var")?;
			parser.commit();
			let name = parser.ident()?;
			let var = Var(ctx.vars.len() as u32);
			ctx.vars.push(name.to_owned());
			let expr = if parser.punct('=').is_ok() {
				let expr = parse_expr(parser, ctx)?;
				Some(expr)
			} else {
				None
			};
			parser.punct(';')?;
			Ok(Stmt::PushVar(l, var, expr))
		})
		.test(|parser| {
			let place = parse_place(parser, ctx)?;
			parser.punct('=').inspect_err(|_| {
				parser.reject();
			})?;
			parser.commit();
			let expr = parse_expr(parser, ctx)?;
			parser.punct(';')?;
			Ok(Stmt::Set(l, place, expr))
		})
		.test(|parser| {
			parser.keyword("debug")?;
			let args = parse_args(parser.delim('(')?, ctx).unwrap_or_default();
			parser.commit();
			parser.punct(';')?;
			Ok(Stmt::Debug(l, args))
		})
		.test(|parser| {
			parser.keyword("tailcall")?;
			parser.commit();
			let (name, args) = parse_func_call(parser, ctx, true)?;
			parser.punct(';')?;
			Ok(Stmt::Tailcall(l, name, args))
		})
		.test(|parser| {
			let block = parser.delim('{')?;
			Ok(Stmt::Block(parse_tree(block, ctx.sub())))
		})
		.test(|parser| {
			let expr = parse_call(parser, ctx).inspect_err(|_| {
				parser.reject(); // This might skip some useful suggestions, but it's hard to do it better
			})?;
			parser.punct(';')?;
			Ok(Stmt::Expr(expr))
		})
		.finish()
}

fn parse_switch(parser: &mut Parser, mut ctx: Ctx) -> Result<IndexMap<Option<i32>, Vec<Stmt>>> {
	// TODO error on duplicate cases
	let mut cases = IndexMap::new();
	while !parser.at_end() {
		if parser.keyword("case").is_ok() {
			let num = parser.int()?;
			parser.punct(':')?;
			cases.insert(Some(num), Vec::new());
		} else if parser.keyword("default").is_ok() {
			parser.punct(':')?;
			cases.insert(None, Vec::new());
		} else {
			let span = parser.next_span();
			let stmt = parse_stmt(parser, &mut ctx)?;
			if let Some((_, stmts)) = cases.last_mut() {
				stmts.push(stmt);
			} else {
				parser.errors.error("expected 'case', 'default'", span);
			}
		}
	}
	Ok(cases)
}

struct PrioOp {
	line: Option<u16>,
	op: Binop,
	prio: u32,
}

pub struct Prio<T> {
	ops: Vec<PrioOp>,
	stack: Vec<Expr<T>>,
}

impl<T> Prio<T> {
	pub fn new(left: Expr<T>) -> Self {
		Self {
			ops: Vec::new(),
			stack: vec![left],
		}
	}

	fn pop(&mut self) {
		let top = self.ops.pop().unwrap();
		let right = self.stack.pop().unwrap();
		let left = self.stack.pop().unwrap();
		self.stack.push(Expr::Binop(top.line, top.op, Box::new(left), Box::new(right)))
	}

	fn push(&mut self, op: PrioOp, expr: Expr<T>) {
		while let Some(top) = self.ops.last() && top.prio >= op.prio {
			self.pop();
		}
		self.ops.push(op);
		self.stack.push(expr);
	}

	fn finish(&mut self) -> Expr<T> {
		while !self.ops.is_empty() {
			self.pop();
		}
		assert_eq!(self.stack.len(), 1);
		self.stack.pop().unwrap()
	}
}

fn parse_expr<T: ParseVar>(parser: &mut Parser, ctx: &mut T::Ctx<'_>) -> Result<Expr<T>> {
	let mut prio = Prio::new(parse_atom(parser, ctx)?);
	while let Some(op) = parse_binop(parser)? {
		prio.push(op, parse_atom(parser, ctx)?);
	}
	Ok(prio.finish())
}

fn parse_binop(parser: &mut Parser) -> Result<Option<PrioOp>> {
	let line = parser.line();
	macro_rules! op {
		($($ident:ident => ($op:literal, $prio:literal),)*) => {
			$(if parser.operator($op).is_ok() {
				return Ok(Some(PrioOp {
					line,
					op: Binop::$ident,
					prio: $prio,
				}));
			} else )* { Ok(None) }
		}
	}
	op! {
		BoolOr => ("||", 1),
		BoolAnd => ("&&", 2),
		Le => ("<=", 3),
		Lt => ("<", 3),
		Ge => (">=", 3),
		Gt => (">", 3),
		Ne => ("!=", 3),
		Eq => ("==", 3),
		BitOr => ("|", 4),
		BitAnd => ("&", 5),
		Sub => ("-", 6),
		Add => ("+", 6),
		Mod => ("%", 7),
		Div => ("/", 7),
		Mul => ("*", 7),
	}
}

fn parse_atom<T: ParseVar>(parser: &mut Parser, ctx: &mut T::Ctx<'_>) -> Result<Expr<T>> {
	let l = parser.line();
	Alt::new(parser)
		.test(|parser| {
			let value = parse::parse_value(parser)?;
			Ok(Expr::Value(l, value))
		})
		.test(|parser| {
			let expr = do_parse(parser.delim('(')?, |p| parse_expr(p, ctx));
			Ok(expr.unwrap_or(Expr::Value(l, Value::Int(0))))
		})
		.test(|parser| {
			parser.punct('!')?;
			let expr = parse_atom(parser, ctx)?;
			Ok(Expr::Unop(l, Unop::BoolNot, Box::new(expr)))
		})
		.test(|parser| {
			parser.punct('-')?;
			let expr = parse_atom(parser, ctx)?;
			Ok(Expr::Unop(l, Unop::Neg, Box::new(expr)))
		})
		.test(|parser| {
			parser.punct('~')?;
			let expr = parse_atom(parser, ctx)?;
			Ok(Expr::Unop(l, Unop::BitNot, Box::new(expr)))
		})
		.test(|parser| {
			parser.punct('&')?;
			Ok(Expr::Ref(l, T::parse_var(parser, ctx)?))
		})
		.test(|parser| parse_call(parser, ctx))
		.test(|parser| {
			let place = parse_place(parser, ctx)?;
			Ok(Expr::Var(l, place))
		})
		.finish()
}

fn parse_call<T: ParseVar>(parser: &mut Parser, ctx: &mut T::Ctx<'_>) -> Result<Expr<T>> {
	let l = parser.line();
	Alt::new(parser)
		.test(|parser| {
			let (a, b) = parse::parse_syscall(parser)?;
			let args = parse_args(parser.delim('(')?, ctx).unwrap_or_default();
			Ok(Expr::Syscall(l, a, b, args))
		})
		.test(|parser| {
			let (name, args) = parse_func_call(parser, ctx, false)?;
			Ok(Expr::Call(l, name, args))
		})
		.finish()
}

fn parse_func_call<T: ParseVar>(parser: &mut Parser, ctx: &mut T::Ctx<'_>, missing_ok: bool) -> Result<(Name, Vec<Expr<T>>)> {
	Alt::new(parser)
		.test(|parser| {
			let name = parser.ident()?;
			let span = parser.prev_span();
			let missing = ctx.scope().functions.get(name).is_none();
			let args = if let Some(args) = parse_args(parser.delim('(')?, ctx) {
				if let Some(sig) = ctx.scope().functions.get(name) {
					if !sig.contains(&args.len()) {
						if missing_ok {
							parser.errors.warning(format!("expected {sig:?} args"), span.clone());
						} else {
							parser.errors.error(format!("expected {sig:?} args"), span.clone());
						}
					}
				}
				args
			} else {
				Vec::new()
			};
			if missing && !ctx.scope().error {
				if missing_ok {
					parser.errors.warning("unknown function", span);
				} else {
					parser.errors.error("unknown function", span);
				}
			}
			Ok((Name(String::new(), name.to_owned()), args))
		})
		.test(|parser| {
			let name1 = parser.ident()?;
			parser.punct('.')?;
			parser.commit();
			let name2 = parser.ident()?;
			let args = parse_args(parser.delim('(')?, ctx).unwrap_or_default();
			Ok((Name(name1.to_owned(), name2.to_owned()), args))
		})
		.finish()
}

fn parse_place<T: ParseVar>(parser: &mut Parser, ctx: &mut T::Ctx<'_>) -> Result<Place<T>> {
	Alt::new(parser)
		.test(|parser| {
			parser.punct('*')?;
			Ok(Place::Deref(T::parse_var(parser, ctx)?))
		})
		.test(|parser| T::parse_var_or_global(parser, ctx))
		.finish()
}

trait HasScope {
	fn scope(&self) -> &Scope;
}

trait ParseVar: Sized {
	type Ctx<'a>: HasScope;

	fn parse_var(parser: &mut Parser, ctx: &mut Self::Ctx<'_>) -> Result<Self>;
	fn parse_var_or_global(parser: &mut Parser, ctx: &mut Self::Ctx<'_>) -> Result<Place<Self>>;
}

impl HasScope for Ctx<'_> {
	fn scope(&self) -> &Scope {
		self.scope
	}
}

impl ParseVar for Var {
	type Ctx<'a> = Ctx<'a>;

	fn parse_var(parser: &mut Parser, ctx: &mut Ctx) -> Result<Var> {
		let name = parser.ident()?;
		let span = parser.prev_span();
		let var = if let Some(num) = ctx.vars.iter().rposition(|v| *v == name) {
			Var(num as u32)
		} else if ctx.scope.globals.contains(name) {
			parser.errors.error("cannot dereference globals", span);
			Var(0)
		} else {
			parser.errors.error("unknown variable", span);
			Var(0)
		};
		Ok(var)
	}

	fn parse_var_or_global(parser: &mut Parser, ctx: &mut Ctx) -> Result<Place> {
		let name = parser.ident()?;
		let span = parser.prev_span();
		let var = if let Some(num) = ctx.vars.iter().rposition(|v| *v == name) {
			Place::Var(Var(num as u32))
		} else if ctx.scope.globals.contains(name) {
			Place::Global(name.to_owned())
		} else {
			parser.errors.error("unknown variable", span);
			Place::Var(Var(0))
		};
		Ok(var)
	}
}

fn parse_args<T: ParseVar>(p: Parser, ctx: &mut T::Ctx<'_>) -> Option<Vec<Expr<T>>> {
	do_parse(p, |p| parse::parse_comma_sep(p, |p| parse_expr(p, ctx)))
}
