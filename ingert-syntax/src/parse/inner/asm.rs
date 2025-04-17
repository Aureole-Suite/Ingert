use ingert::scp::{Label, Name, Op, StackSlot, Unop};
use crate::parse::parser::Error;
use crate::parse::{do_parse, Alt, Parser, Result, Scope};

use super::labels;

pub struct Ctx<'a> {
	scope: &'a Scope<'a>,
	labels: labels::Labels,
}

pub fn parse(parser: Parser, scope: &Scope) -> Vec<Op> {
	let mut ctx = Ctx {
		scope,
		labels: labels::Labels::new(),
	};
	let stmts = do_parse(Parser::new(parser.cursor, parser.errors), |parser| {
		let mut stmts = Vec::new();
		while !parser.at_end() {
			stmts.push(parse_op(parser, &mut ctx)?);
		}
		Ok(stmts)
	});
	if stmts.is_none() {
		return Vec::new();
	}

	ctx.labels.finish(stmts.unwrap_or_default(), parser.errors)
}

fn parse_op(parser: &mut Parser, ctx: &mut Ctx) -> Result<Op> {
	Alt::new(parser)
		.test(|parser| {
			let label = parse_label(parser, ctx, true)?;
			parser.punct(':')?;
			Ok(Op::Label(label))
		})
		.test(|parser| {
			parser.keyword("push")?;
			let push = super::super::parse_value(parser)?;
			parser.punct(';')?;
			Ok(Op::Push(push))
		})
		.test(|parser| {
			parser.keyword("push")?;
			parser.keyword("null")?;
			parser.punct(';')?;
			Ok(Op::PushNull)
		})
		.test(|parser| {
			parser.keyword("pop")?;
			let pop = parse_u8(parser)?;
			parser.punct(';')?;
			Ok(Op::Pop(pop))
		})
		.test(|parser| {
			parser.keyword("return")?;
			parser.punct(';')?;
			Ok(Op::Return)
		})

		.test(|parser| {
			parser.keyword("get_var")?;
			let slot = parse_slot(parser)?;
			parser.punct(';')?;
			Ok(Op::GetVar(slot))
		})
		.test(|parser| {
			parser.keyword("set_var")?;
			let slot = parse_slot(parser)?;
			parser.punct(';')?;
			Ok(Op::SetVar(slot))
		})
		.test(|parser| {
			parser.keyword("push_ref")?;
			let slot = parse_slot(parser)?;
			parser.punct(';')?;
			Ok(Op::PushRef(slot))
		})
		.test(|parser| {
			parser.keyword("get_ref")?;
			let slot = parse_slot(parser)?;
			parser.punct(';')?;
			Ok(Op::PushRef(slot))
		})
		.test(|parser| {
			parser.keyword("set_ref")?;
			let slot = parse_slot(parser)?;
			parser.punct(';')?;
			Ok(Op::PushRef(slot))
		})

		.test(|parser| {
			parser.keyword("get_global")?;
			let var = parser.ident()?;
			parser.punct(';')?;
			Ok(Op::GetGlobal(var.to_owned()))
		})
		.test(|parser| {
			parser.keyword("set_global")?;
			let var = parser.ident()?;
			parser.punct(';')?;
			Ok(Op::SetGlobal(var.to_owned()))
		})
		.test(|parser| {
			parser.keyword("get_temp")?;
			let var = parse_u8(parser)?;
			parser.punct(';')?;
			Ok(Op::GetTemp(var))
		})
		.test(|parser| {
			parser.keyword("set_temp")?;
			let var = parse_u8(parser)?;
			parser.punct(';')?;
			Ok(Op::SetTemp(var))
		})

		.test(|parser| {
			parser.keyword("call")?;
			let (a, b) = super::super::parse_syscall(parser)?;
			let pop = parse_u8(parser)?;
			parser.punct(';')?;
			Ok(Op::CallSystem(a, b, pop))
		})
		.test(|parser| {
			parser.keyword("prepare_call_local")?;
			let label = parse_label(parser, ctx, false)?;
			parser.punct(';')?;
			Ok(Op::PrepareCallLocal(label))
		})
		.test(|parser| {
			parser.keyword("call")?;
			let ident = parser.ident()?;
			parser.punct(';')?;
			Ok(Op::CallLocal(ident.to_owned()))
		})
		.test(|parser| {
			parser.keyword("prepare_call_extern")?;
			let label = parse_label(parser, ctx, false)?;
			parser.punct(';')?;
			Ok(Op::PrepareCallExtern(label))
		})
		.test(|parser| {
			parser.keyword("call")?;
			let a = parser.ident()?;
			parser.punct('.')?;
			let b = parser.ident()?;
			let pop = parse_u8(parser)?;
			parser.punct(';')?;
			Ok(Op::CallExtern(Name(a.to_owned(), b.to_owned()), pop))
		})

		.test(|parser| {
			parser.keyword("call_tail")?;
			let a = parser.ident()?;
			let (a, b) = if parser.punct('.').is_ok() {
				let b = parser.ident()?;
				(a, b)
			} else {
				("", a)
			};
			let pop = parse_u8(parser)?;
			parser.punct(';')?;
			Ok(Op::CallTail(Name(a.to_owned(), b.to_owned()), pop))
		})

		.test(|parser| {
			parser.keyword("binop")?;
			let op = super::expr::parse_binop(parser)?;
			parser.punct(';')?;
			Ok(Op::Binop(op.op))
		})
		.test(|parser| {
			parser.keyword("unop")?;
			let op = if parser.punct('-').is_ok() {
				Unop::Neg
			} else if parser.punct('!').is_ok() {
				Unop::BoolNot
			} else if parser.punct('~').is_ok() {
				Unop::BitNot
			} else {
				Err(Error)?
			};
			parser.punct(';')?;
			Ok(Op::Unop(op))
		})

		.test(|parser| {
			parser.keyword("jz")?;
			let label = parse_label(parser, ctx, false)?;
			parser.punct(';')?;
			Ok(Op::Jz(label))
		})
		.test(|parser| {
			parser.keyword("jnz")?;
			let label = parse_label(parser, ctx, false)?;
			parser.punct(';')?;
			Ok(Op::Jnz(label))
		})
		.test(|parser| {
			parser.keyword("goto")?;
			let label = parse_label(parser, ctx, false)?;
			parser.punct(';')?;
			Ok(Op::Goto(label))
		})

		.test(|parser| {
			parser.keyword("line")?;
			let line = parse_u16(parser)?;
			parser.punct(';')?;
			Ok(Op::Line(line))
		})
		.test(|parser| {
			parser.keyword("debug")?;
			let line = parse_u8(parser)?;
			parser.punct(';')?;
			Ok(Op::Debug(line))
		})
		.finish()
}

fn parse_u8(parser: &mut Parser) -> Result<u8> {
	let var = parser.int()?;
	let span = parser.prev_span();
	if let Ok(v) = var.try_into() {
		Ok(v)
	} else {
		parser.errors.error("must be u8", span);
		Ok(0)
	}
}

fn parse_u16(parser: &mut Parser) -> Result<u16> {
	let var = parser.int()?;
	let span = parser.prev_span();
	if let Ok(v) = var.try_into() {
		Ok(v)
	} else {
		parser.errors.error("must be u16", span);
		Ok(0)
	}
}

fn parse_slot(parser: &mut Parser) -> Result<StackSlot> {
	let var = parser.int()?;
	let span = parser.prev_span();
	if var <= 0 {
		parser.errors.error("variable must be positive", span);
	}
	Ok(StackSlot(var as u32))
}

fn parse_label(parser: &mut Parser, ctx: &mut Ctx, def: bool) -> Result<Label> {
	parser.punct('$')?;
	let ident = parser.ident()?;
	Ok(ctx.labels.add(ident, parser.errors, parser.prev_span(), def))
}
