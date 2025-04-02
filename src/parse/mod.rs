use error::Errors;
use cursor::Cursor;

use crate::{scena::Arg, scp::{ArgType, Value}};

pub mod lex;
pub mod error;
mod cursor;

#[derive(Debug, Clone)]
struct PFunction<'a> {
	name: String,
	args: Vec<PArg>,
	called: PCalled<'a>,
	is_prelude: bool,
	body: PBody<'a>,
}

#[derive(Debug, Clone)]
struct PArg {
	name: String,
	ty: ArgType,
	default: Option<Value>,
}

#[derive(Debug, Clone)]
enum PCalled<'a> {
	Raw(Cursor<'a>),
	Merged(bool),
}

#[derive(Debug, Clone)]
enum PBody<'a> {
	Asm(Cursor<'a>),
	Flat(Cursor<'a>),
	Tree(Cursor<'a>),
}

pub fn parse(tokens: &lex::Tokens) -> ((), Errors) {
	let mut cursor = tokens.cursor();
	let mut errors = Errors::new();

	let mut functions = Vec::new();

	while !cursor.at_end() {
		let mut clone = cursor.clone();
		clone.keyword("prelude");
		if clone.keyword("fn").is_some() {
			if let Some(func) = parse_fn(&mut cursor) {
				functions.push(func);
			} else {
				errors.error("expected function", cursor.next_span());
				cursor.skip_any();
			}
		} else {
			errors.error("expected function or global", cursor.next_span());
			cursor.skip_any();
		}
	}

	dbg!(&errors);
	dbg!(&functions);

	((), errors)
}

fn parse_fn<'a>(cursor: &mut Cursor<'a>) -> Option<PFunction<'a>> {
	let is_prelude = cursor.keyword("prelude").is_some();
	cursor.keyword("fn")?;
	let name = cursor.ident()?;
	let args = parse_args(cursor.delim('(')?)?;

	let called = if cursor.keyword("calls").is_some() {
		PCalled::Raw(cursor.delim('{')?)
	} else if cursor.keyword("dup").is_some() {
		PCalled::Merged(true)
	} else {
		PCalled::Merged(false)
	};

	let body = if cursor.keyword("asm").is_some() {
		PBody::Asm(cursor.delim('{')?)
	} else if cursor.keyword("flat").is_some() {
		PBody::Flat(cursor.delim('{')?)
	} else {
		PBody::Tree(cursor.delim('{')?)
	};

	Some(PFunction { name, args, called, is_prelude, body })
}

fn parse_args(mut cursor: Cursor) -> Option<Vec<PArg>> {
	let mut args = Vec::new();
	loop {
		if cursor.at_end() { break }
		let name = cursor.ident()?;
		cursor.punct(':')?;
		let ty = if cursor.keyword("num").is_some() {
			ArgType::Number
		} else if cursor.keyword("str").is_some() {
			ArgType::String
		} else if cursor.punct('&').is_some() && cursor.keyword("num").is_some() {
			ArgType::NumberRef
		} else {
			return None;
		};
		let default = if cursor.punct('=').is_some() {
			Some(parse_value(&mut cursor)?)
		} else {
			None
		};
		args.push(PArg { name, ty, default });
		if cursor.at_end() { break }
		cursor.punct(',')?;
	}
	Some(args)
}

fn parse_value(cursor: &mut Cursor<'_>) -> Option<Value> {
	None
		.or_else(|| cursor.int().map(Value::Int))
		.or_else(|| cursor.float().map(Value::Float))
		.or_else(|| cursor.string().map(Value::String))
}
