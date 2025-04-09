use indexmap::IndexMap;

use crate::scena::{Arg, Body, Called, Expr, Function, Place, Stmt, Var};
use crate::scp::{Binop, Name, Unop};

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
			let expr = parse_call(parser, ctx)?;
			parser.punct(';')?;
			Ok(Stmt::Expr(expr))
		})
		.finish()
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
		.test(|parser| parse_call(parser, ctx))
		.test(|parser| {
			let place = parse_place(parser, ctx)?;
			Ok(Expr::Var(l, place))
		})
		.finish()
}

fn parse_call(parser: &mut Parser<'_>, ctx: &mut Ctx<'_>) -> Result<Expr> {
	let l = parser.line();
	Alt::new(parser)
		.test(|parser| {
			parser.keyword("system")?;
			let (a, b) = parse_syscall(parser.delim('[')?, ctx);
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
			let name2 = parser.ident()?;
			let args = parse_args(parser.delim('(')?, ctx).unwrap_or_default();
			Ok(Expr::Call(l, Name(name1.to_owned(), name2.to_owned()), args))
		})
		.finish()
}

fn parse_place(parser: &mut Parser<'_>, ctx: &mut Ctx<'_>) -> Result<Place> {
	Alt::new(parser)
		.test(|parser| {
			let var = parse_var(parser, ctx)?;
			Ok(Place::Var(var))
		})
		.finish()
}

fn parse_var(parser: &mut Parser<'_>, ctx: &mut Ctx<'_>) -> Result<Var> {
	let ident = parser.ident()?;
	if let Some(num) = ctx.vars.iter().position(|v| v == ident) {
		Ok(Var(num as u32))
	} else {
		ctx.errors.error("unknown variable", parser.prev_span());
		Ok(Var(0))
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

fn parse_args(mut parser: Parser<'_>, ctx: &mut Ctx<'_>) -> Option<Vec<Expr>> {
	let result = super::parse_comma_sep(&mut parser, |parser| parse_expr(parser, ctx));
	if result.is_err() {
		let (cursor, err) = parser.report();
		ctx.errors.error(err.to_string(), cursor.next_span());
	}
	result.ok()
}
