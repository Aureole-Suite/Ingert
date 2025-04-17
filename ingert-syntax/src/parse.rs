use std::ops::RangeInclusive;

use indexmap::{IndexMap, IndexSet};
use lex::Cursor;

use crate::print::SyscallWrapper;
use ingert::scena::{ArgType, Global, Line, Scena, Value, GlobalType};

pub mod lex;
pub mod error;
mod inner;
mod parser;
mod alt;

use alt::Alt;
use error::Errors;
use parser::{Parser, Result};

#[derive(Debug, Clone)]
struct PFunction<'a> {
	name: String,
	args: Option<Vec<PArg>>,
	called: PCalled<'a>,
	is_prelude: bool,
	body: PBody<'a>,
}

#[derive(Debug, Clone)]
struct PArg {
	// empty except on Tree
	name: String,
	ty: ArgType,
	default: Option<Value>,
	line: Line,
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
	Wrapper(SyscallWrapper),
}

struct Scope {
	error: bool,
	functions: IndexMap<String, Option<RangeInclusive<usize>>>,
	globals: IndexSet<String>,
}

pub fn parse(tokens: &lex::Tokens) -> (Scena, Errors) {
	let mut errors = Errors::new();
	let mut parser = Parser::new(tokens.cursor(), &mut errors);

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
			parser.report(|cursor| {
				error = true;
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

	drop(parser);
	
	let scope = Scope {
		error,
		functions: functions.iter()
			.map(|f| (
				f.name.clone(),
				f.args.as_ref().map(|a| a.iter().filter(|a| a.default.is_none()).count() ..= a.len()),
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

fn parse_global(parser: &mut alt::TryParser) -> Result<(String, Global)> {
	let line = parser.line();
	parser.keyword("global")?;
	parser.commit();
	let name = parser.ident()?;
	parser.punct(':')?;
	let ty = parse_gty(parser)?;
	parser.punct(';')?;
	Ok((name.to_owned(), Global { ty, line }))
}

fn parse_fn<'a>(parser: &mut alt::TryParser<'a, '_>) -> Result<PFunction<'a>> {
	let is_prelude = parser.keyword("prelude").is_ok();
	parser.keyword("fn")?;
	parser.commit();
	let name = parser.ident()?;

	if parser.punct('=').is_ok() {
		let ret = parser.keyword("return").is_ok();
		let (a, b) = parse_syscall(parser)?;

		let args = parser.delim_later('(')?;
		let called = parse_called(parser)?;

		parser.punct(';')?;

		let args = parse_args(Parser::new(args, parser.errors), false);

		return Ok(PFunction {
			name: name.to_owned(),
			args,
			called,
			is_prelude,
			body: PBody::Wrapper(SyscallWrapper { a, b, ret }),
		});
	}

	let args = parser.delim_later('(')?;
	let called = parse_called(parser)?;

	let body = if parser.keyword("asm").is_ok() {
		PBody::Asm(parser.delim_later('{')?)
	} else if parser.keyword("flat").is_ok() {
		PBody::Flat(parser.delim_later('{')?)
	} else {
		PBody::Tree(parser.delim_later('{')?)
	};

	let args = parse_args(Parser::new(args, parser.errors), matches!(body, PBody::Tree(_)));

	Ok(PFunction {
		name: name.to_owned(),
		args,
		called,
		is_prelude,
		body,
	})
}

fn parse_args(parser: Parser, has_name: bool) -> Option<Vec<PArg>> {
	do_parse(parser, |p| parse_comma_sep(p, |parser| {
		let line = parser.line();
		let name = if has_name {
			let name = parser.ident()?;
			parser.punct(':')?;
			name
		} else {
			""
		};
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
	}))
}

fn parse_called<'a>(parser: &mut Parser<'a, '_>) -> Result<PCalled<'a>> {
	if parser.keyword("calls").is_ok() {
		Ok(PCalled::Raw(parser.delim_later('{')?))
	} else if parser.keyword("dup").is_ok() {
		Ok(PCalled::Merged(true))
	} else {
		Ok(PCalled::Merged(false))
	}
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

fn parse_value(parser: &mut Parser) -> Result<Value> {
	Alt::new(parser)
		.test(|p| p.int().map(Value::Int))
		.test(|p| p.float().map(Value::Float))
		.test(|p| p.string().map(|s| Value::String(s.to_owned())))
		.finish()
}

fn parse_syscall(parser: &mut Parser) -> Result<(u8, u8)> {
	fn inner(mut parser: Parser) -> Result<(u8, u8)> {
		let a = parser.int()?;
		if !(0..=255).contains(&a) {
			parser.errors.error("invalid syscall number", parser.prev_span());
		}
		parser.punct(',')?;
		let b = parser.int()?;
		if !(0..=255).contains(&b) {
			parser.errors.error("invalid syscall number", parser.prev_span());
		}
		Ok((a as u8, b as u8))
	}
	parser.keyword("system")?;
	Ok(inner(parser.delim('[')?).unwrap_or((0, 0)))
}

fn do_parse<'a, 'e, T>(
	mut parser: Parser<'a, 'e>,
	f: impl FnOnce(&mut Parser<'a, 'e>) -> Result<T>,
) -> Option<T> {
	let result = f(&mut parser);
	if result.is_err() {
		parser.report(|_cursor| { });
	}
	result.ok()
}
