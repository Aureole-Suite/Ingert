use crate::parse::parser::Error;
use crate::parse::{do_parse, parse_comma_sep, parse_value, Alt, Parser, Result, Scope};
use crate::scp::{Call, CallArg, CallKind, Name};

pub fn parse(parser: Parser, scope: &Scope) -> Vec<Call> {
	let stmts = do_parse(Parser::new(parser.cursor, parser.errors), |parser| {
		let mut stmts = Vec::new();
		while !parser.at_end() {
			stmts.push(parse_call(parser, scope)?);
		}
		Ok(stmts)
	});
	stmts.unwrap_or_default()
}

fn parse_call(parser: &mut Parser, scope: &Scope) -> Result<Call> {
	let kind = Alt::new(parser)
		.test(|parser| {
			let (a, b) = super::super::parse_syscall(parser)?;
			Ok(CallKind::Syscall(a, b))
		})
		.test(|parser| {
			let tailcall = parser.keyword("tailcall").is_ok();
			let a = parser.ident()?;
			let name = if parser.punct('.').is_ok() {
				let b = parser.ident()?;
				Name(a.to_owned(), b.to_owned())
			} else {
				Name::local(a.to_owned())
			};
			if tailcall {
				Ok(CallKind::Tailcall(name))
			} else {
				Ok(CallKind::Normal(name))
			}
		})
		.test(|parser| {
			let ident = parser.ident()?;
			Ok(CallKind::Normal(Name::local(ident.to_owned())))
		})
		.finish()?;

	let args = do_parse(parser.delim('(')?, |p| parse_comma_sep(p, parse_arg)).unwrap_or_default();
	parser.punct(';')?;
	Ok(Call { kind, args })
}

fn parse_arg(parser: &mut Parser) -> Result<CallArg> {
	Alt::new(parser)
		.test(|parser| {
			parser.keyword("call")?;
			Ok(CallArg::Call)
		})
		.test(|parser| {
			parser.keyword("var")?;
			Ok(CallArg::Var)
		})
		.test(|parser| {
			parser.keyword("expr")?;
			Ok(CallArg::Expr)
		})
		.test(|parser| {
			let value = parse_value(parser)?;
			Ok(CallArg::Value(value))
		})
		.finish()
}
