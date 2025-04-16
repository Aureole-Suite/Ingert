use std::ops::RangeInclusive;

use indexmap::{IndexMap, IndexSet};

use crate::{scena::{ArgType, Global, Line, Scena, Value}, scp::GlobalType};

pub mod lex;
pub mod error;
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
	line: Line,
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

struct Scope {
	error: bool,
	functions: IndexMap<String, RangeInclusive<usize>>,
	globals: IndexSet<String>,
}

pub fn parse(tokens: &lex::Tokens) -> (Scena, Errors) {
	let mut parser = Parser::new(tokens.cursor());
	let mut errors = Errors::new();

	let mut error = false;
	let mut functions = Vec::new();
	let mut globals = IndexMap::new();

	while !parser.at_end() {
		let result = Alt::new(&mut parser)
			.test(|p| {
				let func = parse_fn(p)?;
				functions.push(func);
				Ok(())
			})
			.test(|p| {
				let (name, glob) = parse_global(p)?;
				globals.insert(name, glob);
				Ok(())
			})
			.finish();

		if let Err(parser::Error) = result {
			parser.report(|cursor, err| {
				error = true;
				errors.error(err.to_string(), cursor.next_span());
				while !(
					cursor.at_end()
					|| cursor.clone().keyword("fn").is_ok()
					|| cursor.clone().keyword("global").is_ok()
				) {
					cursor.skip_any();
				}
			});
		}
	}
	
	let scope = Scope {
		error,
		functions: functions.iter()
			.map(|f| (
				f.name.clone(),
				f.args.iter().filter(|a| a.default.is_none()).count() ..= f.args.len(),
			))
			.collect(),
		globals: globals.keys().cloned().collect(),
	};

	let functions = functions.iter()
		.map(|f| (f.name.clone(), inner::parse_fn(f, &scope, &mut errors)))
		.collect::<IndexMap<_, _>>();

	(Scena {
		globals,
		functions,
	}, errors)
}

fn parse_global(parser: &mut alt::TryParser<'_>) -> Result<(String, Global)> {
	let line = parser.line();
	parser.keyword("global")?;
	parser.commit();
	let name = parser.ident()?;
	parser.punct(':')?;
	let ty = parse_gty(parser)?;
	parser.punct(';')?;
	Ok((name.to_owned(), Global { ty, line }))
}

fn parse_fn<'a>(parser: &mut alt::TryParser<'a>) -> Result<PFunction<'a>> {
	let is_prelude = parser.keyword("prelude").is_ok();
	parser.keyword("fn")?;
	parser.commit();
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
		let line = parser.line();
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
			line,
		})
	})
}

fn parse_gty(parser: &mut Parser) -> Result<GlobalType> {
	Alt::new(parser)
		.test(|p| {
			p.keyword("num")?;
			Ok(GlobalType::Number)
		})
		.test(|p| {
			p.keyword("str")?;
			Ok(GlobalType::String)
		})
		.finish()
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
