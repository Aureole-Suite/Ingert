use indexmap::IndexMap;

use ingert::scena::{Place, Stmt, Var};
use crate::parse::{do_parse, Alt, Parser, Result, Scope};
use super::expr;

pub struct Ctx<'a> {
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
			let cond = expr::parse_expr(parser, ctx)?;
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
			let cond = expr::parse_expr(parser, ctx)?;
			let body = parse_tree(parser.delim('{')?, ctx.sub().with_brk().with_cont());
			Ok(Stmt::While(l, cond, body))
		})
		.test(|parser| {
			parser.keyword("switch")?;
			parser.commit();
			let cond = expr::parse_expr(parser, ctx)?;
			let body = do_parse(parser.delim('{')?, |p| parse_switch(p, ctx.sub().with_brk())).unwrap_or_default();
			Ok(Stmt::Switch(l, cond, body))
		})
		.test(|parser| {
			parser.keyword("return")?;
			parser.commit();
			let val = if parser.punct(';').is_ok() {
				None
			} else {
				let expr = expr::parse_expr(parser, ctx)?;
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
				let expr = expr::parse_expr(parser, ctx)?;
				Some(expr)
			} else {
				None
			};
			parser.punct(';')?;
			Ok(Stmt::PushVar(l, var, expr))
		})
		.test(|parser| {
			let place = expr::parse_place(parser, ctx)?;
			parser.punct('=').inspect_err(|_| {
				parser.reject();
			})?;
			parser.commit();
			let expr = expr::parse_expr(parser, ctx)?;
			parser.punct(';')?;
			Ok(Stmt::Set(l, place, expr))
		})
		.test(|parser| {
			parser.keyword("debug")?;
			let args = expr::parse_args(parser.delim('(')?, ctx).unwrap_or_default();
			parser.commit();
			parser.punct(';')?;
			Ok(Stmt::Debug(l, args))
		})
		.test(|parser| {
			parser.keyword("tailcall")?;
			parser.commit();
			let (name, args) = expr::parse_func_call(parser, ctx, true)?;
			parser.punct(';')?;
			Ok(Stmt::Tailcall(l, name, args))
		})
		.test(|parser| {
			let block = parser.delim('{')?;
			Ok(Stmt::Block(parse_tree(block, ctx.sub())))
		})
		.test(|parser| {
			let expr = expr::parse_call(parser, ctx).inspect_err(|_| {
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

impl expr::HasScope for Ctx<'_> {
	fn scope(&self) -> &Scope {
		self.scope
	}
}

impl expr::ParseVar for Var {
	type Ctx<'a> = Ctx<'a>;

	fn parse_var(parser: &mut Parser, ctx: &mut Ctx) -> Result<Self> {
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

	fn parse_var_or_global(parser: &mut Parser, ctx: &mut Ctx) -> Result<Place<Self>> {
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
