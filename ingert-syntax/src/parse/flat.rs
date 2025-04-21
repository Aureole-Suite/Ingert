use ingert::scena::{FlatStmt, FlatVar, Place, Label};
use crate::parse::{do_parse, Alt, Parser, Result, Scope};

use super::expr;
use super::labels;

pub struct Ctx<'a> {
	scope: &'a Scope<'a>,
	labels: labels::Labels,
}

pub fn parse(parser: Parser, scope: &Scope) -> Vec<FlatStmt> {
	let mut ctx = Ctx {
		scope,
		labels: labels::Labels::new(),
	};
	let stmts = do_parse(Parser::new(parser.cursor, parser.errors), |parser| {
		let mut stmts = Vec::new();
		while !parser.at_end() {
			stmts.push(parse_stmt(parser, &mut ctx)?);
		}
		Ok(stmts)
	});
	if stmts.is_none() {
		return Vec::new();
	}

	ctx.labels.finish(stmts.unwrap_or_default(), parser.errors)
}

fn parse_stmt(parser: &mut Parser, ctx: &mut Ctx) -> Result<FlatStmt> {
	let l = parser.line();
	Alt::new(parser)
		.test(|parser| {
			let label = parse_label(parser, ctx, true)?;
			parser.punct(':')?;
			Ok(FlatStmt::Label(label))
		})
		.test(|parser| {
			parser.keyword("if")?;
			parser.commit();
			let cond = expr::parse_expr(parser, ctx)?;
			let label = parse_label(parser, ctx, false)?;
			parser.punct(';')?;
			Ok(FlatStmt::If(l, cond, label))
		})
		.test(|parser| {
			parser.keyword("switch")?;
			parser.commit();
			let cond = expr::parse_expr(parser, ctx)?;
			let block = parse_switch(parser.delim('{')?, ctx);
			let label = parse_label(parser, ctx, false)?;
			parser.punct(';')?;
			Ok(FlatStmt::Switch(l, cond, block, label))
		})
		.test(|parser| {
			parser.keyword("return")?;
			parser.commit();
			let depth = parse_depth(parser)?;
			let val = if parser.punct(';').is_ok() {
				None
			} else {
				let expr = expr::parse_expr(parser, ctx)?;
				parser.punct(';')?;
				Some(expr)
			};
			Ok(FlatStmt::Return(l, val, depth))
		})
		.test(|parser| {
			parser.keyword("goto")?;
			parser.commit();
			let label = parse_label(parser, ctx, false)?;
			parser.punct(';')?;
			Ok(FlatStmt::Goto(label))
		})
		.test(|parser| {
			parser.keyword("push_var")?;
			parser.punct(';')?;
			Ok(FlatStmt::PushVar(l))
		})
		.test(|parser| {
			parser.keyword("pop_var")?;
			let depth = parse_depth(parser)?;
			parser.punct(';')?;
			Ok(FlatStmt::PopVar(depth))
		})
		.test(|parser| {
			let place = expr::parse_place(parser, ctx)?;
			parser.punct('=').inspect_err(|_| {
				parser.reject();
			})?;
			parser.commit();
			let expr = expr::parse_expr(parser, ctx)?;
			parser.punct(';')?;
			Ok(FlatStmt::Set(l, place, expr))
		})
		.test(|parser| {
			parser.keyword("debug")?;
			let args = expr::parse_args(parser.delim('(')?, ctx).unwrap_or_default();
			parser.commit();
			parser.punct(';')?;
			Ok(FlatStmt::Debug(l, args))
		})
		.test(|parser| {
			parser.keyword("tailcall")?;
			parser.commit();
			let depth = parse_depth(parser)?;
			let (name, args) = expr::parse_func_call(parser, ctx, true)?;
			parser.punct(';')?;
			Ok(FlatStmt::Tailcall(l, name, args, depth))
		})
		.test(|parser| {
			let expr = expr::parse_call(parser, ctx)?;
			parser.punct(';')?;
			Ok(FlatStmt::Expr(expr))
		})
		.finish()
}

fn parse_switch(delim: Parser, ctx: &mut Ctx) -> Vec<(i32, Label)> {
	do_parse(delim, |parser| {
		let mut cases = Vec::new();
		while !parser.at_end() {
			let case = parser.int()?;
			parser.operator("=>")?;
			let label = parse_label(parser, ctx, false)?;
			parser.punct(';')?;
			cases.push((case, label));
		}
		Ok(cases)
	}).unwrap_or_default()
}

fn parse_depth(parser: &mut Parser) -> Result<usize> {
	Ok(do_parse(parser.delim('[')?, |parser| {
		let depth = parser.int()?;
		let span = parser.prev_span();
		if depth < 0 {
			parser.errors.error("depth must be non-negative", span);
		}
		Ok(depth as usize)
	}).unwrap_or(0))
}

fn parse_label(parser: &mut Parser, ctx: &mut Ctx, def: bool) -> Result<Label> {
	parser.punct('$')?;
	let ident = parser.ident()?;
	Ok(ctx.labels.add(ident, parser.errors, parser.prev_span(), def))
}

impl expr::HasScope for Ctx<'_> {
	fn scope(&self) -> &Scope {
		self.scope
	}
}

impl expr::ParseVar for FlatVar {
	type Ctx<'a> = Ctx<'a>;

	fn parse_var(parser: &mut Parser, ctx: &mut Ctx) -> Result<Self> {
		Alt::new(parser)
			.test(|parser| {
				parser.punct('#')?;
				let var = parser.int()?;
				let span = parser.prev_span();
				if var < 0 {
					parser.errors.error("depth must be non-negative", span);
				}
				Ok(FlatVar(var as u32))
			})
			.test(|parser| {
				let name = parser.ident()?;
				let span = parser.prev_span();
				if ctx.scope.globals.contains_key(name) {
					parser.errors.error("cannot dereference globals", span);
				} else {
					parser.errors.error("unknown variable", span);
				}
				Ok(FlatVar(0))
			})
			.finish()
	}

	fn parse_var_or_global(parser: &mut Parser, ctx: &mut Ctx) -> Result<Place<Self>> {
		Alt::new(parser)
			.test(|parser| {
				parser.punct('#')?;
				let var = parser.int()?;
				let span = parser.prev_span();
				if var < 0 {
					parser.errors.error("depth must be non-negative", span);
				}
				Ok(Place::Var(FlatVar(var as u32)))
			})
			.test(|parser| {
				let name = parser.ident()?;
				let span = parser.prev_span();
				if ctx.scope.globals.contains_key(name) {
					Ok(Place::Global(name.to_owned()))
				} else {
					parser.errors.error("unknown variable", span);
					Ok(Place::Var(FlatVar(0)))
				}
			})
			.finish()
	}
}
