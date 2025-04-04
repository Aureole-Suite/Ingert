use indexmap::IndexMap;

use crate::scena::{Arg, Body, Called, Expr, Function, Stmt};

use super::cursor::{self, Cursor};
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
		PCalled::Raw(cursor) => Called::Raw(parse_called(cursor.clone(), signatures, errors)),
		PCalled::Merged(dup) => Called::Merged(*dup),
	};

	let body = match &f.body {
		PBody::Asm(cursor) => Body::Asm(parse_asm(cursor.clone(), signatures, errors)),
		PBody::Flat(cursor) => Body::Flat(parse_flat(cursor.clone(), signatures, errors)),
		PBody::Tree(cursor) => Body::Tree(parse_tree(cursor.clone(), Ctx {
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
	cursor: Cursor<'_>,
	signatures: &IndexMap<&str, &[PArg]>,
	errors: &mut Errors,
) -> Vec<crate::scp::Call> {
    todo!()
}

fn parse_asm(
	cursor: Cursor<'_>,
	signatures: &IndexMap<&str, &[PArg]>,
	errors: &mut Errors,
) -> Vec<crate::scp::Op> {
    todo!()
}

fn parse_flat(
	cursor: Cursor<'_>,
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

fn parse_tree(mut cursor: Cursor<'_>, mut ctx: Ctx<'_>) -> Vec<Stmt> {
	let mut stmts = Vec::new();
	while !cursor.at_end() {
		match parse_stmt(&mut cursor, &mut ctx) {
			Ok(stmt) => stmts.push(stmt),
			Err(err) => {
				ctx.errors.error(err.to_string(), cursor.next_span());
				break;
			}
		}
	}
	stmts
}

fn parse_stmt(
	cursor: &mut Cursor<'_>,
	ctx: &mut Ctx<'_>,
) -> cursor::Result<Stmt> {
	let l = cursor.line();

	if let Some(v) = cursor.test(|cursor| {
		cursor.keyword("if")?;
		let cond = parse_expr(cursor, ctx)?;
		let then = parse_tree(cursor.delim('{')?, ctx.sub());
		let els = if cursor.keyword("else").is_ok() {
			Some(parse_tree(cursor.delim('{')?, ctx.sub()))
		} else {
			None
		};
		Ok(Stmt::If(l, cond, then, els))
	}) { return v; }

	if let Some(v) = cursor.test(|cursor| {
		cursor.keyword("while")?;
		let cond = parse_expr(cursor, ctx)?;
		let body = parse_tree(cursor.delim('{')?, ctx.sub().with_brk().with_cont());
		Ok(Stmt::While(l, cond, body))
	}) { return v; }

	if let Some(v) = cursor.test(|cursor| {
		cursor.keyword("return")?;
		let val = if cursor.punct(';').is_ok() {
			None
		} else {
			let expr = parse_expr(cursor, ctx)?;
			cursor.punct(';')?;
			Some(expr)
		};
		Ok(Stmt::Return(l, val))
	}) { return v; }

	if let Some(v) = cursor.test(|cursor| {
		let expr = parse_expr(cursor, ctx)?;
		cursor.punct(';')?;
		Ok(Stmt::Expr(expr))
	}) { return v; }

	cursor.fail()?
}

fn parse_expr(cursor: &mut Cursor<'_>, ctx: &mut Ctx<'_>) -> cursor::Result<Expr> {
	let l = cursor.line();

	if let Some(v) = cursor.test(|cursor| {
		cursor.keyword("system")?;
		let (a, b) = parse_syscall(cursor.delim('[')?, ctx);
		let args = parse_args(cursor.delim('(')?, ctx).unwrap_or_default();
		Ok(Expr::Syscall(l, a, b, args))
	}) { return v; }

	dbg!(&cursor);
	cursor.keyword("<expr>")?;
	cursor.fail()?
}

fn parse_syscall(cursor: Cursor<'_>, ctx: &mut Ctx<'_>) -> (u8, u8) {
	fn inner(mut cursor: Cursor<'_>, ctx: &mut Ctx<'_>) -> cursor::Result<(u8, u8)> {
		let a = cursor.int()?;
		if !(0..=255).contains(&a) {
			ctx.errors.error("invalid syscall number", cursor.prev_span());
		}
		cursor.punct(',')?;
		let b = cursor.int()?;
		if !(0..=255).contains(&b) {
			ctx.errors.error("invalid syscall number", cursor.prev_span());
		}
		if !cursor.at_end() {
			ctx.errors.error("unexpected token", cursor.next_span());
		}
		Ok((a as u8, b as u8))
	}
	inner(cursor, ctx).unwrap_or((0, 0))
}

fn parse_args(mut cursor: Cursor<'_>, ctx: &mut Ctx<'_>) -> Option<Vec<Expr>> {
	match super::parse_comma_sep(&mut cursor, |cursor| parse_expr(cursor, ctx)) {
		Ok(args) => Some(args),
		Err(e) => {
			ctx.errors.error(e.to_string(), cursor.next_span());
			None
		}
	}
}
