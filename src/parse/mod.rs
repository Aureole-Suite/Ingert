use indexmap::IndexMap;

use crate::scena::{ArgType, Value};

pub mod lex;
pub mod error;
mod cursor;
mod inner;
mod parser;
mod alt;

use alt::Alt;
use error::Errors;
use parser::{Error, Parser, Result};

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
	Raw(Parser<'a>),
	Merged(bool),
}

#[derive(Debug, Clone)]
enum PBody<'a> {
	Asm(Parser<'a>),
	Flat(Parser<'a>),
	Tree(Parser<'a>),
}

pub fn parse(tokens: &lex::Tokens) -> ((), Errors) {
	let mut parser = Parser::new(tokens.cursor());
	let mut errors = Errors::new();

	let mut functions = Vec::new();

	while !parser.at_end() {
		let result = Alt::new(&mut parser)
			.test(|p| {
				let func = parse_fn(p)?;
				functions.push(func);
				Ok(())
			})
			.finish();

		if let Err(parser::Error) = result {
			let (cursor, err) = parser.report();
			errors.error(err.to_string(), cursor.next_span());
			while !(cursor.at_end() || cursor.keyword("fn").is_ok()) {
				cursor.skip_any();
			}
		}
	}

	dbg!(&errors);

	let signatures = functions.iter().map(|f| (f.name.as_str(), f.args.as_slice())).collect::<IndexMap<_, _>>();

	let functions = functions.iter()
		.map(|f| (f.name.clone(), inner::parse_fn(f, &signatures, &mut errors)))
		.collect::<IndexMap<_, _>>();

	dbg!(&errors);

	((), errors)
}

fn parse_fn<'a>(parser: &mut Parser<'a>) -> Result<PFunction<'a>> {
	let is_prelude = parser.keyword("prelude").is_ok();
	parser.keyword("fn")?;
	let name = parser.ident()?;
	let args = parse_args(parser.delim('(')?)?;

	let called = if parser.keyword("calls").is_ok() {
		PCalled::Raw(parser.delim('{')?)
	} else if parser.keyword("dup").is_ok() {
		PCalled::Merged(true)
	} else {
		PCalled::Merged(false)
	};

	let body = if parser.keyword("asm").is_ok() {
		PBody::Asm(parser.delim('{')?)
	} else if parser.keyword("flat").is_ok() {
		PBody::Flat(parser.delim('{')?)
	} else {
		PBody::Tree(parser.delim('{')?)
	};

	Ok(PFunction {
		name: name.to_owned(),
		args,
		called,
		is_prelude,
		body,
	})
}

fn parse_args(mut parser: Parser) -> Result<Vec<PArg>> {
	parse_comma_sep(&mut parser, |parser| {
		let name = parser.ident()?;
		parser.punct(':')?;
		let ty = parse_ty(parser)?;
		let default = if parser.punct('=').is_ok() {
			Some(parse_value(parser)?)
		} else {
			None
		};
		Ok(PArg {
			name: name.to_owned(),
			ty,
			default,
		})
	})
}

fn parse_ty(parser: &mut Parser) -> Result<ArgType> {
	Alt::new(parser)
		.test(|p| {
			p.keyword("num")?;
			Ok(ArgType::Number)
		})
		.test(|p| {
			p.keyword("str")?;
			Ok(ArgType::String)
		})
		.test(|p| {
			p.punct('&')?;
			p.keyword("num")?;
			Ok(ArgType::NumberRef)
		})
		.finish()
}

fn parse_comma_sep<T>(parser: &mut Parser, mut f: impl FnMut(&mut Parser) -> Result<T>) -> Result<Vec<T>> {
	let mut args = Vec::new();
	loop {
		if parser.at_end() { break }
		args.push(f(parser)?);
		if parser.at_end() { break }
		parser.punct(',')?;
	}
	Ok(args)
}

fn parse_value(parser: &mut Parser<'_>) -> Result<Value> {
	Alt::new(parser)
		.test(|p| p.int().map(Value::Int))
		.test(|p| p.float().map(Value::Float))
		.test(|p| p.string().map(|s| Value::String(s.to_owned())))
		.finish()
}
