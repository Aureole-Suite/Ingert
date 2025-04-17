use ingert::scena::{Expr, Place};
use ingert::scp::{Binop, Name, Unop, Value};

use crate::parse::{self, do_parse, Alt, Parser, Result, Scope};

pub trait HasScope {
	fn scope(&self) -> &Scope;
}

pub trait ParseVar: Sized {
	type Ctx<'a>: HasScope;

	fn parse_var(parser: &mut Parser, ctx: &mut Self::Ctx<'_>) -> Result<Self>;
	fn parse_var_or_global(parser: &mut Parser, ctx: &mut Self::Ctx<'_>) -> Result<Place<Self>>;
}

pub struct PrioOp {
	pub line: Option<u16>,
	pub op: Binop,
	pub prio: u32,
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

pub fn parse_expr<T: ParseVar>(parser: &mut Parser, ctx: &mut T::Ctx<'_>) -> Result<Expr<T>> {
	let mut prio = Prio::new(parse_atom(parser, ctx)?);
	while let Ok(op) = parse_binop(parser) {
		prio.push(op, parse_atom(parser, ctx)?);
	}
	Ok(prio.finish())
}

pub fn parse_binop(parser: &mut Parser) -> Result<PrioOp> {
	let line = parser.line();
	macro_rules! op {
		($($ident:ident => ($op:literal, $prio:literal),)*) => {
			$(if parser.operator($op).is_ok() {
				Ok(PrioOp {
					line,
					op: Binop::$ident,
					prio: $prio,
				})
			} else )* { Err(crate::parse::parser::Error) }
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

pub fn parse_call<T: ParseVar>(parser: &mut Parser, ctx: &mut T::Ctx<'_>) -> Result<Expr<T>> {
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

pub fn parse_func_call<T: ParseVar>(parser: &mut Parser, ctx: &mut T::Ctx<'_>, missing_ok: bool) -> Result<(Name, Vec<Expr<T>>)> {
	Alt::new(parser)
		.test(|parser| {
			let name = parser.ident()?;
			let span = parser.prev_span();
			let missing = ctx.scope().functions.get(name).is_none();
			let args = if let Some(args) = parse_args(parser.delim('(')?, ctx) {
				if let Some(func) = ctx.scope().functions.get(name) {
					if let Some(sig) = &func.arg_count && !sig.contains(&args.len()) {
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

pub fn parse_place<T: ParseVar>(parser: &mut Parser, ctx: &mut T::Ctx<'_>) -> Result<Place<T>> {
	Alt::new(parser)
		.test(|parser| {
			parser.punct('*')?;
			Ok(Place::Deref(T::parse_var(parser, ctx)?))
		})
		.test(|parser| T::parse_var_or_global(parser, ctx))
		.finish()
}


pub fn parse_args<T: ParseVar>(p: Parser, ctx: &mut T::Ctx<'_>) -> Option<Vec<Expr<T>>> {
	do_parse(p, |p| parse::parse_comma_sep(p, |p| parse_expr(p, ctx)))
}
