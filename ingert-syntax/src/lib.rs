#![feature(if_let_guard, never_type, iter_order_by)]

use ingert::scena::{Expr, Place, Stmt, Var};

pub mod print;
pub mod lex;
pub mod diag;
pub mod parse;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct SyscallWrapper {
	pub ret: bool,
	pub a: u8,
	pub b: u8,
}

impl SyscallWrapper {
	fn args(nargs: usize) -> impl Iterator<Item = Expr> {
		(0..nargs)
			.rev()
			.map(|i| Expr::Var(None, Place::Var(Var(i as u32))))
	}

	pub fn from_tree(body: &[Stmt], nargs: usize) -> Option<SyscallWrapper> {
		let (ret, a, b, args) = match body {
			[Stmt::Return(None, Some(Expr::Syscall(None, a, b, args)))] => (true, *a, *b, args),
			[Stmt::Expr(Expr::Syscall(None, a, b, args)), Stmt::Return(None, None)] => (false, *a, *b, args),
			_ => return None,
		};
		Self::args(nargs)
			.eq_by(args, |a, b| a == *b)
			.then_some(SyscallWrapper { ret, a, b })
	}

	pub fn as_tree(&self, nargs: usize) -> Vec<Stmt> {
		let args = Self::args(nargs).collect();
		let syscall = Expr::Syscall(None, self.a, self.b, args);
		if self.ret {
			vec![Stmt::Return(None, Some(syscall))]
		} else {
			vec![Stmt::Expr(syscall), Stmt::Return(None, None)]
		}
	}
}
