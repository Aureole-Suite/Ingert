use indexmap::IndexMap;
use ingert::scena::Global;
use ingert::scp::{ArgType, GlobalType};

use crate::SyscallWrapper;
use super::parser::{Parser, Result};
use super::alt::{Alt, TryParser};
use super::{do_parse, parse_comma_sep, parse_value, PArg, PBody, PCalled, PFunction};

pub fn parse<'a>(mut parser: Parser<'a, '_>) -> super::Scope<'a> {
	let mut error = false;
	let mut functions = IndexMap::new();
	let mut globals = IndexMap::new();

	while !parser.at_end() {
		let result = Alt::new(&mut parser)
			.test(|p| {
				let func = parse_fn(p)?;
				functions.insert(func.name.clone(), func);
				Ok(())
			})
			.test(|p| {
				let (name, glob) = parse_global(p)?;
				globals.insert(name, glob);
				Ok(())
			})
			.finish();

		if result.is_err() {
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
	
	super::Scope {
		error,
		functions,
		globals,
	}
}

fn parse_global(parser: &mut TryParser) -> Result<(String, Global)> {
	let line = parser.line();
	parser.keyword("global")?;
	parser.commit();
	let name = parser.ident()?;
	parser.punct(':')?;
	let ty = parse_gty(parser)?;
	parser.punct(';')?;
	Ok((name.to_owned(), Global { ty, line }))
}

fn parse_fn<'a>(parser: &mut TryParser<'a, '_>) -> Result<PFunction<'a>> {
	let is_prelude = parser.keyword("prelude").is_ok();
	parser.keyword("fn")?;
	parser.commit();
	let name = parser.ident()?;

	let (args, called, body);

	if parser.punct('=').is_ok() {
		let ret = parser.keyword("return").is_ok();
		let (a, b) = super::parse_syscall(parser)?;

		args = parser.delim_later('(')?;
		called = parse_called(parser)?;

		parser.punct(';')?;

		body = PBody::Wrapper(SyscallWrapper { a, b, ret });
	} else {
		args = parser.delim_later('(')?;
		called = parse_called(parser)?;
		body = if parser.keyword("asm").is_ok() {
			PBody::Asm(parser.delim_later('{')?)
		} else if parser.keyword("flat").is_ok() {
			PBody::Flat(parser.delim_later('{')?)
		} else {
			PBody::Tree(parser.delim_later('{')?)
		};
	}

	let args = parse_args(Parser::new(args, parser.errors), matches!(body, PBody::Tree(_)));

	Ok(PFunction {
		name: name.to_owned(),
		arg_count: args.as_ref().map(|a| a.iter().filter(|a| a.default.is_none()).count() ..= a.len()),
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

