use std::ops::Range;

use indexmap::IndexMap;

use crate::scena::{Arg, Body, Called, Expr, Function, Place, Stmt, Var};
use crate::scp::{Binop, Name, Unop, Value};

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
	vars.reverse();
	
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
			signatures: self.signatures,
			errors: self.errors,
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
			parser.commit();
			let cond = parse_expr(parser, ctx)?;
			let then = parse_tree(parser.delim('{')?, ctx.sub());
			let els = if parser.keyword("else").is_ok() {
				if parser.clone().keyword("if").is_ok() {
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
			let body = do_parse(parser.delim('{')?, &mut ctx.sub().with_brk(), parse_switch).unwrap_or_default();
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
			parser.commit();
			if !ctx.brk {
				ctx.errors.error("break outside of loop/switch", parser.prev_span());
			}
			parser.punct(';')?;
			Ok(Stmt::Break)
		})
		.test(|parser| {
			parser.keyword("continue")?;
			parser.commit();
			if !ctx.cont {
				ctx.errors.error("continue outside of loop", parser.prev_span());
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
			parser.punct('=')?;
			parser.commit();
			let expr = parse_expr(parser, ctx)?;
			parser.punct(';')?;
			Ok(Stmt::Set(l, place.lookup(ctx), expr))
		})
		.test(|parser| {
			parser.keyword("debug")?;
			let args = parse_args(parser.delim('(')?, ctx).unwrap_or_default();
			parser.commit();
			parser.punct(';')?;
			Ok(Stmt::Debug(l, args))
		})
		.test(|parser| {
			let expr = parse_call(parser, ctx)?;
			parser.punct(';')?;
			Ok(Stmt::Expr(expr))
		})
		.finish()
}

fn parse_switch(parser: &mut Parser<'_>, ctx: &mut Ctx<'_>) -> Result<IndexMap<Option<i32>, Vec<Stmt>>> {
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
			let stmt = parse_stmt(parser, ctx)?;
			if let Some((_, stmts)) = cases.last_mut() {
				stmts.push(stmt);
			} else {
				ctx.errors.error("expected 'case', 'default'", span);
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

pub struct Prio {
	ops: Vec<PrioOp>,
	stack: Vec<Expr>,
}

impl Prio {
	pub fn new(left: Expr) -> Self {
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

	fn push(&mut self, op: PrioOp, expr: Expr) {
		while let Some(top) = self.ops.last() && top.prio >= op.prio {
			self.pop();
		}
		self.ops.push(op);
		self.stack.push(expr);
	}

	fn finish(&mut self) -> Expr {
		while !self.ops.is_empty() {
			self.pop();
		}
		assert_eq!(self.stack.len(), 1);
		self.stack.pop().unwrap()
	}
}

fn parse_expr(parser: &mut Parser<'_>, ctx: &mut Ctx<'_>) -> Result<Expr> {
	let mut prio = Prio::new(parse_atom(parser, ctx)?);
	while let Some(op) = parse_binop(parser, ctx)? {
		prio.push(op, parse_atom(parser, ctx)?);
	}
	Ok(prio.finish())
}

fn parse_binop(parser: &mut Parser<'_>, _ctx: &mut Ctx<'_>) -> Result<Option<PrioOp>> {
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

fn parse_atom(parser: &mut Parser<'_>, ctx: &mut Ctx<'_>) -> Result<Expr> {
	let l = parser.line();
	Alt::new(parser)
		.test(|parser| {
			let value = super::parse_value(parser)?;
			Ok(Expr::Value(l, value))
		})
		.test(|parser| {
			let paren = parser.delim('(')?;
			Ok(do_parse(paren, ctx, parse_expr).unwrap_or_else(|| Expr::Value(l, Value::Int(0))))
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
			let name = parser.ident()?;
			parser.commit();
			Ok(Expr::Ref(l, lookup_var(ctx, name, parser.prev_span())))
		})
		.test(|parser| parse_call(parser, ctx))
		.test(|parser| {
			let place = parse_place(parser, ctx)?;
			Ok(Expr::Var(l, place.lookup(ctx)))
		})
		.finish()
}

fn parse_call(parser: &mut Parser<'_>, ctx: &mut Ctx<'_>) -> Result<Expr> {
	let l = parser.line();
	Alt::new(parser)
		.test(|parser| {
			parser.keyword("system")?;
			let (a, b) = parse_syscall(parser.delim('[')?, ctx);
			parser.commit();
			let args = parse_args(parser.delim('(')?, ctx).unwrap_or_default();
			Ok(Expr::Syscall(l, a, b, args))
		})
		.test(|parser| {
			let name = parser.ident()?;
			let args = if let Some(args) = parse_args(parser.delim('(')?, ctx) {
				// TODO verify args
				args
			} else {
				Vec::new()
			};
			Ok(Expr::Call(l, Name(String::new(), name.to_owned()), args))
		})
		.test(|parser| {
			let name1 = parser.ident()?;
			parser.punct('.')?;
			parser.commit();
			let name2 = parser.ident()?;
			let args = parse_args(parser.delim('(')?, ctx).unwrap_or_default();
			Ok(Expr::Call(l, Name(name1.to_owned(), name2.to_owned()), args))
		})
		.finish()
}

enum PPlace {
	Var(Range<usize>, String),
}

fn parse_place(parser: &mut Parser<'_>, _ctx: &mut Ctx<'_>) -> Result<PPlace> {
	Alt::new(parser)
		.test(|parser| {
			let name = parser.ident()?;
			Ok(PPlace::Var(parser.prev_span(), name.to_owned()))
		})
		.finish()
}

impl PPlace {
	fn lookup(self, ctx: &mut Ctx<'_>) -> Place {
		match self {
			PPlace::Var(span, name) => {
				Place::Var(lookup_var(ctx, &name, span))
			}
		}
	}
}

fn lookup_var(ctx: &mut Ctx<'_>, name: &str, span: Range<usize>) -> Var {
	if let Some(num) = ctx.vars.iter().rposition(|v| *v == name) {
		Var(num as u32)
	} else {
		ctx.errors.error("unknown variable", span);
		Var(0)
	}
}

fn parse_syscall(parser: Parser<'_>, ctx: &mut Ctx<'_>) -> (u8, u8) {
	fn inner(mut parser: Parser<'_>, ctx: &mut Ctx<'_>) -> Result<(u8, u8)> {
		let a = parser.int()?;
		if !(0..=255).contains(&a) {
			ctx.errors.error("invalid syscall number", parser.prev_span());
		}
		parser.punct(',')?;
		let b = parser.int()?;
		if !(0..=255).contains(&b) {
			ctx.errors.error("invalid syscall number", parser.prev_span());
		}
		if !parser.at_end() {
			let (cursor, err) = parser.report();
			ctx.errors.error(err.to_string(), cursor.next_span());
		}
		Ok((a as u8, b as u8))
	}
	inner(parser, ctx).unwrap_or((0, 0))
}

fn parse_args(parser: Parser<'_>, ctx: &mut Ctx<'_>) -> Option<Vec<Expr>> {
	do_parse(parser, ctx, |p, c| super::parse_comma_sep(p, |p| parse_expr(p, c)))
}

fn do_parse<T>(
	mut parser: Parser<'_>,
	ctx: &mut Ctx<'_>,
	f: impl FnOnce(&mut Parser<'_>, &mut Ctx<'_>) -> Result<T>,
) -> Option<T> {
	let result = f(&mut parser, ctx);
	if result.is_err() {
		let (cursor, err) = parser.report();
		ctx.errors.error(err.to_string(), cursor.next_span());
	}
	result.ok()
}
