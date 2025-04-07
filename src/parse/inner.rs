use indexmap::IndexMap;

use crate::scena::{Arg, Body, Called, Expr, Function, Stmt};

use super::{Alt, Result, Parser};
use super::error::Errors;
use super::{PArg, PBody, PCalled};

type Sig<'a> = IndexMap<&'a str, &'a [PArg]>;

pub fn parse_fn(f: &super::PFunction, signatures: &Sig, errors: &mut Errors) -> Function {
	let mut vars = Vec::new();
	let args = f
		.args
		.iter()
		.map(|arg| {
			vars.push(arg.name.clone().clone());
			Arg { ty: arg.ty, default: arg.default.clone(), line: None }
		})
		.collect();
	
	let called = match &f.called {
		PCalled::Raw(parser) => Called::Raw(parse_called(parser.clone(), signatures, errors)),
		PCalled::Merged(dup) => Called::Merged(*dup),
	};

	let body = match &f.body {
		PBody::Asm(parser) => Body::Asm(parse_asm(parser.clone(), signatures, errors)),
		PBody::Flat(parser) => Body::Flat(parse_flat(parser.clone(), signatures, errors)),
		PBody::Tree(parser) => Body::Tree(parse_tree(parser.clone(), Ctx {
			signatures,
			errors,
			brk: false,
			cont: false,
			vars,
		})),
	};

	Function {
		args,
		called,
		body,
		is_prelude: f.is_prelude,
	}
}

fn parse_called(
	parser: Parser<'_>,
	signatures: &IndexMap<&str, &[PArg]>,
	errors: &mut Errors,
) -> Vec<crate::scp::Call> {
	todo!()
}

fn parse_asm(
	parser: Parser<'_>,
	signatures: &IndexMap<&str, &[PArg]>,
	errors: &mut Errors,
) -> Vec<crate::scp::Op> {
	todo!()
}

fn parse_flat(
	parser: Parser<'_>,
	signatures: &IndexMap<&str, &[PArg]>,
	errors: &mut Errors,
) -> Vec<crate::scena::FlatStmt> {
	todo!()
}

struct Ctx<'a> {
	signatures: &'a IndexMap<&'a str, &'a [PArg]>,
	errors: &'a mut Errors,
	brk: bool,
	cont: bool,
	vars: Vec<String>,
}

impl Ctx<'_> {
	fn sub(&mut self) -> Ctx<'_> {
		Ctx {
			signatures: &self.signatures,
			errors: &mut self.errors,
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

fn parse_tree(mut parser: Parser<'_>, mut ctx: Ctx<'_>) -> Vec<Stmt> {
	let mut stmts = Vec::new();
	while !parser.at_end() {
		match parse_stmt(&mut parser, &mut ctx) {
			Ok(stmt) => stmts.push(stmt),
			Err(_) => {
				let (cursor, err) = parser.report();
				ctx.errors.error(err.to_string(), cursor.next_span());
				break;
			}
		}
	}
	stmts
}

fn parse_stmt(
	parser: &mut Parser<'_>,
	ctx: &mut Ctx<'_>,
) -> Result<Stmt> {
	let l = parser.line();
	Alt::new(parser)
		.test(|parser| {
			parser.keyword("if")?;
			let cond = parse_expr(parser, ctx)?;
			let then = parse_tree(parser.delim('{')?, ctx.sub());
			let els = if parser.keyword("else").is_ok() {
				Some(parse_tree(parser.delim('{')?, ctx.sub()))
			} else {
				None
			};
			Ok(Stmt::If(l, cond, then, els))
		})
		.test(|parser| {
			parser.keyword("while")?;
			let cond = parse_expr(parser, ctx)?;
			let body = parse_tree(parser.delim('{')?, ctx.sub().with_brk().with_cont());
			Ok(Stmt::While(l, cond, body))
		})
		.test(|parser| {
			parser.keyword("return")?;
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
			let expr = parse_expr(parser, ctx)?;
			parser.punct(';')?;
			Ok(Stmt::Expr(expr))
		})
		.finish()
}

fn parse_expr(parser: &mut Parser<'_>, ctx: &mut Ctx<'_>) -> Result<Expr> {
	let l = parser.line();
	Alt::new(parser)
		.test(|parser| {
			parser.keyword("system")?;
			let (a, b) = parse_syscall(parser.delim('[')?, ctx);
			let args = parse_args(parser.delim('(')?, ctx).unwrap_or_default();
			Ok(Expr::Syscall(l, a, b, args))
		})
		.finish()
}

fn parse_syscall(parser: Parser<'_>, ctx: &mut Ctx<'_>) -> (u8, u8) {
	fn inner(mut parser: Parser<'_>, ctx: &mut Ctx<'_>) -> Result<(u8, u8)> {
		let a = parser.int()?;
		if !(0..=255).contains(&a) {
			ctx.errors.error("invalid syscall number", parser.report().0.prev_span());
		}
		parser.punct(',')?;
		let b = parser.int()?;
		if !(0..=255).contains(&b) {
			ctx.errors.error("invalid syscall number", parser.report().0.prev_span());
		}
		if !parser.at_end() {
			let (cursor, err) = parser.report();
			ctx.errors.error(err.to_string(), cursor.next_span());
		}
		Ok((a as u8, b as u8))
	}
	inner(parser, ctx).unwrap_or((0, 0))
}

fn parse_args(mut parser: Parser<'_>, ctx: &mut Ctx<'_>) -> Option<Vec<Expr>> {
	let result = super::parse_comma_sep(&mut parser, |parser| parse_expr(parser, ctx));
	if result.is_err() {
		let (cursor, err) = parser.report();
		ctx.errors.error(err.to_string(), cursor.next_span());
	}
	result.ok()
}
