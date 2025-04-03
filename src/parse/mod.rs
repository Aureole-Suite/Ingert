use error::Errors;
use cursor::Cursor;
use indexmap::IndexMap;

use crate::scena::{ArgType, Value};

pub mod lex;
pub mod error;
mod cursor;
mod inner;

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
		let _ = clone.keyword("prelude");
		if clone.keyword("fn").is_ok() {
			match parse_fn(&mut cursor) {
				Ok(func) => {
					functions.push(func);
				}
				Err(e) => {
					errors.error(e.to_string(), cursor.next_span());
				}
			}
		} else {
			let e: cursor::Error = clone.fail().unwrap_err().into();
			errors.error(e.to_string(), clone.next_span());
			cursor.skip_any();
		}
	}

	dbg!(&errors);

	let signatures = functions.iter().map(|f| (f.name.as_str(), f.args.as_slice())).collect::<IndexMap<_, _>>();

	let functions = functions.iter()
		.take(4)
		.map(|f| (f.name.clone(), inner::parse_fn(f, &signatures, &mut errors)))
		.collect::<IndexMap<_, _>>();

	dbg!(&functions);

	((), errors)
}

fn parse_fn<'a>(cursor: &mut Cursor<'a>) -> cursor::Result<PFunction<'a>> {
	let is_prelude = cursor.keyword("prelude").is_ok();
	cursor.keyword("fn")?;
	let name = cursor.ident()?;
	let args = parse_args(cursor.delim('(')?)?;

	let called = if cursor.keyword("calls").is_ok() {
		PCalled::Raw(cursor.delim('{')?)
	} else if cursor.keyword("dup").is_ok() {
		PCalled::Merged(true)
	} else {
		PCalled::Merged(false)
	};

	let body = if cursor.keyword("asm").is_ok() {
		PBody::Asm(cursor.delim('{')?)
	} else if cursor.keyword("flat").is_ok() {
		PBody::Flat(cursor.delim('{')?)
	} else {
		PBody::Tree(cursor.delim('{')?)
	};

	Ok(PFunction { name, args, called, is_prelude, body })
}

fn parse_args(mut cursor: Cursor) -> cursor::Result<Vec<PArg>> {
	let mut args = Vec::new();
	loop {
		if cursor.at_end() { break }
		let name = cursor.ident()?;
		cursor.punct(':')?;
		let ty = if cursor.keyword("num").is_ok() {
			ArgType::Number
		} else if cursor.keyword("str").is_ok() {
			ArgType::String
		} else if cursor.punct('&').is_ok() && cursor.keyword("num").is_ok() {
			ArgType::NumberRef
		} else {
			cursor.fail()?;
		};
		let default = if cursor.punct('=').is_ok() {
			Some(parse_value(&mut cursor)?)
		} else {
			None
		};
		args.push(PArg { name, ty, default });
		if cursor.at_end() { break }
		cursor.punct(',')?;
	}
	Ok(args)
}

fn parse_value(cursor: &mut Cursor<'_>) -> cursor::Result<Value> {
	if let Ok(int) = cursor.int() {
		return Ok(Value::Int(int));
	}
	if let Ok(float) = cursor.float() {
		return Ok(Value::Float(float));
	}
	if let Ok(string) = cursor.string() {
		return Ok(Value::String(string));
	}
	cursor.fail()?;
}
