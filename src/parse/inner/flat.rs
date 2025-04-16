use std::ops::Range;

use indexmap::IndexMap;

use crate::parse::parser::Error;
use crate::scena::{FlatStmt, FlatVar, Place, Label};
use crate::parse::{do_parse, Alt, Parser, Result, Scope};

use super::expr;

struct LabelInfo {
	label: Label,
	defined: Option<Range<usize>>,
	referenced: Option<Range<usize>>,
}

struct Labels {
	labels: IndexMap<String, LabelInfo>,
}

pub struct Ctx<'a> {
	scope: &'a Scope,
	labels: Labels,
}

pub fn parse(parser: Parser, scope: &Scope) -> Vec<FlatStmt> {
	let mut ctx = Ctx {
		scope,
		labels: Labels {
			labels: IndexMap::new(),
		},
	};
	let stmts = do_parse(parser, |parser| {
		let mut stmts = Vec::new();
		while !parser.at_end() {
			stmts.push(parse_stmt(parser, &mut ctx)?);
		}
		Ok(stmts)
	}).unwrap_or_default();
	{} // TODO check for undefined labels
	stmts
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
			let depth = parse_depth(parser).unwrap_or(0);
			let label = parse_label(parser, ctx, false)?;
			parser.punct(';')?;
			Ok(FlatStmt::Goto(label, depth))
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
	let n = ctx.labels.labels.len() as u32;
	let label = ctx.labels.labels.entry(ident.to_owned())
		.or_insert_with(|| LabelInfo {
			label: Label(n),
			defined: None,
			referenced: None,
		});
	if def {
		if let Some(prev) = &label.defined {
			parser.errors.error("label already defined", parser.prev_span())
				.note("previously defined here", prev.clone());
		}
		label.defined = Some(parser.prev_span());
	} else if label.referenced.is_none() {
		label.referenced = Some(parser.prev_span());
	}
	Ok(label.label)
}

impl expr::HasScope for Ctx<'_> {
	fn scope(&self) -> &Scope {
		self.scope
	}
}

impl expr::ParseVar for FlatVar {
	type Ctx<'a> = Ctx<'a>;

	fn parse_var(parser: &mut Parser, _ctx: &mut Ctx) -> Result<Self> {
		if parser.punct('#').is_ok() {
			let var = parser.int()?;
			let span = parser.prev_span();
			if var <= 0 {
				parser.errors.error("variable must be positive", span);
			}
			Ok(FlatVar(var as u32))
		} else {
			Err(Error)
		}
	}

	fn parse_var_or_global(parser: &mut Parser, _ctx: &mut Ctx) -> Result<Place<Self>> {
		if parser.punct('#').is_ok() {
			let var = parser.int()?;
			let span = parser.prev_span();
			if var < 0 {
				parser.errors.error("depth must be non-negative", span);
			}
			Ok(Place::Var(FlatVar(var as u32)))
		} else {
			Err(Error)
		}
	}
}
