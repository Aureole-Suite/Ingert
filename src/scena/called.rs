#![allow(clippy::result_large_err)]
use std::collections::HashMap;

use snafu::OptionExt as _;

use crate::scp::{self, Call, CallArg, CallKind};
use super::{FlatStmt, Expr};

#[derive(Debug, snafu::Snafu)]
#[snafu(module(apply), context(suffix(false)))]
pub enum ApplyError {
	#[snafu(display("called table has {called:?} but code has {code:?}"))]
	Mismatch {
		called: Call,
		code: Call,
	},
	#[snafu(display("call to missing function {name}"))]
	MissingFunction {
		name: String,
	},
	#[snafu(display("signature mismatch for function {name}: expected {signature:?}, got {args:?}"))]
	SignatureMismatch {
		name: String,
		signature: Vec<scp::Arg>,
		args: Vec<CallArg>,
	},
	#[snafu(display("called table has {called:?} calls but code has {code:?}"))]
	WrongNumber {
		called: usize,
		code: usize,
	}
}

struct Apply<'a> {
	called: &'a [Call],
	pos: usize,
	functable: HashMap<&'a str, &'a scp::Function>,
}

impl Apply<'_> {
	pub fn new<'a>(called: &'a [Call], functions: &'a [scp::Function]) -> Apply<'a> {
		let functable = functions.iter().map(|f| (f.name.as_str(), f)).collect();
		Apply { called, pos: 0, functable }
	}

	pub fn flat_stmt(&mut self, stmt: &mut FlatStmt) -> Result<(), ApplyError> {
		match stmt {
			FlatStmt::Label(_) => {}
			FlatStmt::Expr(expr) => {
				self.expr(expr)?;
			},
			FlatStmt::Set(_, _, expr) => {
				self.expr(expr)?;
			}
			FlatStmt::Return(_, expr, _) => {
				if let Some(expr) = expr {
					self.expr(expr)?;
				}
			}
			FlatStmt::If(_, expr, _) => {
				self.expr(expr)?;
			}
			FlatStmt::Goto(_) => {},
			FlatStmt::Switch(_, expr, _, _) => {
				self.expr(expr)?;
			}
			FlatStmt::PushVar(_) => {}
			FlatStmt::PopVar(_) => {}
			FlatStmt::Debug(_, exprs) => {
				for expr in exprs {
					self.expr(expr)?;
				}
			},
			FlatStmt::Tailcall(_, name, exprs, _) => {
				self.call(CallKind::Tailcall(name.clone()), exprs)?;
			}
		}
		Ok(())
	}

	pub fn expr(&mut self, expr: &mut Expr) -> Result<(), ApplyError> {
		match expr {
			Expr::Value(_, _) => {}
			Expr::Var(_, _) => {}
			Expr::Ref(_, _) => {}
			Expr::Call(_, name, exprs) => {
				self.call(CallKind::Normal(name.clone()), exprs)?;
			}
			Expr::Syscall(_, a, b, exprs) => {
				self.call(CallKind::Syscall(*a, *b), exprs)?;
			}
			Expr::Unop(_, _, v) => {
				self.expr(v)?;
			}
			Expr::Binop(_, _, a, b) => {
				self.expr(a)?;
				self.expr(b)?;
			}
		}
		Ok(())
	}

	fn call(&mut self, kind: CallKind, args: &mut Vec<Expr>) -> Result<(), ApplyError> {
		self.pos += 1;
		let Some(called) = self.called.get(self.pos - 1) else { return Ok(()) }; // it'll error on finish

		let code_args = args.iter().map(|e| match e {
			Expr::Value(_, value) => CallArg::Value(value.clone()),
			Expr::Var(..) => CallArg::Var,
			Expr::Ref(..) => CallArg::Var,
			Expr::Call(..) => CallArg::Call,
			Expr::Syscall(..) => CallArg::Call,
			Expr::Unop(..) => CallArg::Expr,
			Expr::Binop(..) => CallArg::Expr,
		}).collect::<Vec<_>>();

		if let CallKind::Normal(name) = &kind && let Some(name) = name.as_local().map(|s| s.as_str()) {
			snafu::ensure!(kind == called.kind && code_args.starts_with(&called.args), apply::Mismatch {
				called: called.clone(),
				code: Call { kind, args: code_args },
			});
			let func = self.functable.get(name).context(apply::MissingFunction { name })?;
			let mismatch_error = apply::SignatureMismatch {
				name,
				signature: func.args.as_slice(),
				args: code_args.as_slice(),
			};
			snafu::ensure!(func.args.len() == code_args.len(), mismatch_error);

			let extra_args = &code_args[called.args.len()..];
			let extra_sig = &func.args[called.args.len()..];
			for (arg, sig) in extra_args.iter().zip(extra_sig) {
				match (arg, sig.default.as_ref()) {
					(CallArg::Value(v), Some(default)) if v == default => {}
					_ => return mismatch_error.fail()
				}
			}
			args.truncate(called.args.len());
		} else {
			snafu::ensure!(kind == called.kind && code_args == called.args, apply::Mismatch {
				called: called.clone(),
				code: Call { kind, args: code_args },
			});
		}

		for expr in args {
			self.expr(expr)?;
		}
		Ok(())
	}

	pub fn finish(self) -> Result<bool, ApplyError> {
		if self.called.len() == self.pos * 2 {
			let (first, second) = self.called.split_at(self.pos);
			snafu::ensure!(first == second, apply::WrongNumber {
				called: self.called.len(),
				code: self.pos,
			});
			Ok(true)
		} else {
			snafu::ensure!(self.pos == self.called.len(), apply::WrongNumber {
				called: self.called.len(),
				code: self.pos,
			});
			Ok(false)
		}
	}
}

pub fn apply_flat(
	mut body: Vec<FlatStmt>,
	called: &[Call],
	functions: &[scp::Function],
) -> Result<(Vec<FlatStmt>, bool), ApplyError> {
	let mut ctx = Apply::new(called, functions);
	for stmt in &mut body {
		ctx.flat_stmt(stmt)?;
	}
	Ok((body, ctx.finish()?))
}

#[derive(Debug, snafu::Snafu)]
#[snafu(module(infer), context(suffix(false)))]
pub enum InferError {
	#[snafu(display("call to missing function {name}"))]
	MissingFunction {
		name: String,
	},
	#[snafu(display("signature mismatch for function {name}: expected {signature:?}, got {args:?}"))]
	SignatureMismatch {
		name: String,
		signature: Vec<scp::Arg>,
		args: Vec<CallArg>,
	},
}

struct Infer<'a> {
	called: Vec<Call>,
	functable: HashMap<&'a str, &'a scp::Function>,
}

impl Infer<'_> {
	pub fn new(functions: &[scp::Function]) -> Infer<'_> {
		let functable = functions.iter().map(|f| (f.name.as_str(), f)).collect();
		Infer { called: Vec::new(), functable }
	}

	pub fn flat_stmt(&mut self, stmt: &mut FlatStmt) -> Result<(), InferError> {
		match stmt {
			FlatStmt::Label(_) => {}
			FlatStmt::Expr(expr) => {
				self.expr(expr)?;
			},
			FlatStmt::Set(_, _, expr) => {
				self.expr(expr)?;
			}
			FlatStmt::Return(_, expr, _) => {
				if let Some(expr) = expr {
					self.expr(expr)?;
				}
			}
			FlatStmt::If(_, expr, _) => {
				self.expr(expr)?;
			}
			FlatStmt::Goto(_) => {},
			FlatStmt::Switch(_, expr, _, _) => {
				self.expr(expr)?;
			}
			FlatStmt::PushVar(_) => {}
			FlatStmt::PopVar(_) => {}
			FlatStmt::Debug(_, exprs) => {
				for expr in exprs {
					self.expr(expr)?;
				}
			},
			FlatStmt::Tailcall(_, name, exprs, _) => {
				self.call(CallKind::Tailcall(name.clone()), exprs)?;
			}
		}
		Ok(())
	}

	pub fn expr(&mut self, expr: &mut Expr) -> Result<(), InferError> {
		match expr {
			Expr::Value(_, _) => {}
			Expr::Var(_, _) => {}
			Expr::Ref(_, _) => {}
			Expr::Call(_, name, exprs) => {
				self.call(CallKind::Normal(name.clone()), exprs)?;
			}
			Expr::Syscall(_, a, b, exprs) => {
				self.call(CallKind::Syscall(*a, *b), exprs)?;
			}
			Expr::Unop(_, _, v) => {
				self.expr(v)?;
			}
			Expr::Binop(_, _, a, b) => {
				self.expr(a)?;
				self.expr(b)?;
			}
		}
		Ok(())
	}

	fn call(&mut self, kind: CallKind, args: &mut Vec<Expr>) -> Result<(), InferError> {
		let code_args = args.iter().map(|e| match e {
			Expr::Value(_, value) => CallArg::Value(value.clone()),
			Expr::Var(..) => CallArg::Var,
			Expr::Ref(..) => CallArg::Var,
			Expr::Call(..) => CallArg::Call,
			Expr::Syscall(..) => CallArg::Call,
			Expr::Unop(..) => CallArg::Expr,
			Expr::Binop(..) => CallArg::Expr,
		}).collect::<Vec<_>>();

		if let CallKind::Normal(name) = &kind && let Some(name) = name.as_local().map(|s| s.as_str()) {
			let func = self.functable.get(name).context(infer::MissingFunction { name })?;

			let mismatch_error = infer::SignatureMismatch {
				name,
				signature: func.args.as_slice(),
				args: code_args.as_slice(),
			};

			for default in func.args.get(code_args.len()..).context(mismatch_error)? {
				let default = default.default.as_ref().context(mismatch_error)?;
				args.push(Expr::Value(None, default.clone()));
			}
		}

		self.called.push(Call { kind, args: code_args });

		for expr in args {
			self.expr(expr)?;
		}
		Ok(())
	}

	pub fn finish(self, dup: bool) -> Result<Vec<Call>, InferError> {
		let mut called = self.called;
		if dup {
			called.extend_from_within(..);
		}
		Ok(called)
	}
}

pub fn infer_flat(
	mut body: Vec<FlatStmt>,
	dup: bool,
	functions: &[scp::Function],
) -> Result<(Vec<FlatStmt>, Vec<Call>), InferError> {
	let mut ctx = Infer::new(functions);
	for stmt in &mut body {
		ctx.flat_stmt(stmt)?;
	}
	Ok((body, ctx.finish(dup)?))
}
