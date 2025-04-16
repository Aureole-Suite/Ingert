mod expr;
mod tree;

use crate::scena::{Arg, Body, Called, Expr, Function, Place, Stmt, Var};

use super::parser::Parser;
use super::Scope;
use super::error::Errors;
use super::{PBody, PCalled};

pub fn parse_fn(f: &super::PFunction, scope: &Scope, errors: &mut Errors) -> Function {
	let mut vars = Vec::new();
	let args = f
		.args
		.iter()
		.map(|arg| {
			vars.push(arg.name.clone().clone());
			Arg { ty: arg.ty, default: arg.default.clone(), line: arg.line }
		})
		.collect();
	vars.reverse();

	let called = match &f.called {
		PCalled::Raw(cursor) => Called::Raw(parse_called(Parser::new(cursor.clone(), errors), scope)),
		PCalled::Merged(dup) => Called::Merged(*dup),
	};

	let body = match &f.body {
		PBody::Asm(cursor) => Body::Asm(parse_asm(Parser::new(cursor.clone(), errors), scope)),
		PBody::Flat(cursor) => Body::Flat(parse_flat(Parser::new(cursor.clone(), errors), scope)),
		PBody::Tree(cursor) => Body::Tree(tree::parse(Parser::new(cursor.clone(), errors), scope, vars)),
		PBody::Wrapper(wr) => {
			let args = (0..f.args.len())
				.rev()
				.map(|i| Expr::Var(None, Place::Var(Var(i as u32))))
				.collect();
			let syscall = Expr::Syscall(None, wr.a, wr.b, args);
			let body = if wr.ret {
				vec![Stmt::Return(None, Some(syscall))]
			} else {
				vec![Stmt::Expr(syscall), Stmt::Return(None, None)]
			};
			Body::Tree(body)
		}
	};

	Function {
		args,
		called,
		body,
		is_prelude: f.is_prelude,
	}
}

fn parse_called(_parser: Parser, scope: &Scope) -> Vec<crate::scp::Call> {
	todo!()
}

fn parse_asm(_parser: Parser, scope: &Scope) -> Vec<crate::scp::Op> {
	todo!()
}

fn parse_flat(_parser: Parser, scope: &Scope) -> Vec<crate::scena::FlatStmt> {
	todo!()
}
