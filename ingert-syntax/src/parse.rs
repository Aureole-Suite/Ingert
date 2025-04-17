use std::ops::RangeInclusive;

use indexmap::IndexMap;
use crate::lex::Cursor;

use crate::print::SyscallWrapper;
use ingert::scena::{ArgType, Global, Line, Scena, Value};

mod parser;
mod alt;

mod inner;
mod global;

use alt::Alt;
use crate::diag::Errors;
use parser::{Parser, Result};

#[derive(Debug, Clone)]
struct PFunction<'a> {
	name: String,
	args: Option<Vec<PArg>>,
	arg_count: Option<RangeInclusive<usize>>,
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

#[derive(Debug, Clone)]
struct Scope<'a> {
	error: bool,
	functions: IndexMap<String, PFunction<'a>>,
	globals: IndexMap<String, Global>,
}

pub fn parse(tokens: &crate::lex::Tokens, errors: &mut Errors) -> Scena {
	let scope = global::parse(Parser::new(tokens.cursor(), errors));

	let functions = scope.functions.iter()
		.map(|(n, f)| (n.clone(), inner::parse_fn(f, &scope, errors)))
		.collect::<IndexMap<_, _>>();

	Scena {
		globals: scope.globals,
		functions,
	}
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
