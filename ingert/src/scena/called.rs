#![allow(clippy::result_large_err)]
use snafu::OptionExt as _;

use crate::scp::{Call, CallArg, CallKind};
use super::{Arg, FlatStmt, Expr, Stmt};

type Functions = indexmap::IndexMap<String, Vec<Arg>>;

trait Visit {
	type Error;
	fn call<V>(&mut self, kind: CallKind, args: &mut Vec<Expr<V>>) -> Result<(), Self::Error>;
}

struct Visitor<F>(F);

impl<F: Visit> Visitor<F> {
	fn flat_stmt(&mut self, stmt: &mut FlatStmt) -> Result<(), F::Error> {
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

	fn stmt(&mut self, stmt: &mut Stmt) -> Result<(), F::Error> {
		match stmt {
			Stmt::Expr(expr) => {
				self.expr(expr)?;
			}
			Stmt::Set(_, _, expr) => {
				self.expr(expr)?;
			}
			Stmt::Return(_, expr) => {
				if let Some(expr) = expr {
					self.expr(expr)?;
				}
			}
			Stmt::If(_, expr, a, b) => {
				self.expr(expr)?;
				for stmt in a {
					self.stmt(stmt)?;
				}
				if let Some(b) = b {
					for stmt in b {
						self.stmt(stmt)?;
					}
				}
			}
			Stmt::While(_, expr, a) => {
				self.expr(expr)?;
				for stmt in a {
					self.stmt(stmt)?;
				}
			}
			Stmt::Switch(_, expr, cases) => {
				self.expr(expr)?;
				for (_, a) in cases {
					for stmt in a {
						self.stmt(stmt)?;
					}
				}
			}
			Stmt::Block(stmts) => {
				for stmt in stmts {
					self.stmt(stmt)?;
				}
			}
			Stmt::Break => {}
			Stmt::Continue => {}
			Stmt::PushVar(_, _, e) => {
				if let Some(e) = e {
					self.expr(e)?;
				}
			}
			Stmt::Debug(_, exprs) => {
				for expr in exprs {
					self.expr(expr)?;
				}
			}
			Stmt::Tailcall(_, name, exprs) => {
				self.call(CallKind::Tailcall(name.clone()), exprs)?;
			}
		}
		Ok(())
	}

	fn expr<V>(&mut self, expr: &mut Expr<V>) -> Result<(), F::Error> {
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

	fn call<V>(&mut self, kind: CallKind, args: &mut Vec<Expr<V>>) -> Result<(), F::Error> {
		self.0.call(kind, args)?;
		for expr in args {
			self.expr(expr)?;
		}
		Ok(())
	}
}

fn code_args<'a, V>(kind: &'a CallKind, args: &[Expr<V>]) -> (Option<&'a str>, Vec<CallArg>) {
	let name = if let CallKind::Normal(name) = kind  {
		name.as_local().map(|s| s.as_str())
	} else {
		None
	};
	let args = args.iter().map(|e| match e {
		Expr::Value(_, value) => CallArg::Value(value.clone()),
		Expr::Var(..) => CallArg::Var,
		Expr::Ref(..) => CallArg::Var,
		Expr::Call(..) => CallArg::Call,
		Expr::Syscall(..) => CallArg::Call,
		Expr::Unop(..) => CallArg::Expr,
		Expr::Binop(..) => CallArg::Expr,
	}).collect::<Vec<_>>();
	(name, args)
}

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
		signature: Vec<Arg>,
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
	functions: &'a mut Functions,
}

impl Apply<'_> {
	pub fn new<'a>(called: &'a [Call], functions: &'a mut Functions) -> Apply<'a> {
		Apply { called, pos: 0, functions }
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

impl Visit for Apply<'_> {
	type Error = ApplyError;

	fn call<V>(&mut self, kind: CallKind, args: &mut Vec<Expr<V>>) -> Result<(), ApplyError> {
		self.pos += 1;
		let Some(called) = self.called.get(self.pos - 1) else { return Ok(()) }; // it'll error on finish

		let (name, code_args) = code_args(&kind, args);

		if let Some(name) = name {
			snafu::ensure!(kind == called.kind && code_args.starts_with(&called.args), apply::Mismatch {
				called: called.clone(),
				code: Call { kind, args: code_args },
			});
			let sig = self.functions.get_mut(name).context(apply::MissingFunction { name })?;
			snafu::ensure!(sig.len() == code_args.len(), apply::SignatureMismatch {
				name,
				signature: sig.as_slice(),
				args: code_args.as_slice(),
			});

			let extra_args = &args[called.args.len()..];
			let extra_sig = &mut sig[called.args.len()..];
			for (arg, sigarg) in extra_args.iter().zip(extra_sig) {
				match (arg, sigarg) {
					(Expr::Value(expr_line, v), Arg { default: Some(default), line, ty: _ }) if v == default => {
						if let Some(expr_line) = expr_line {
							if let Some(line) = line {
								if expr_line != line {
									tracing::warn!(expr_line, line, "default value has different line");
								}
							} else {
								*line = Some(*expr_line);
							}
						}
					}
					_ => return apply::SignatureMismatch {
						name,
						signature: sig.as_slice(),
						args: code_args.as_slice(),
					}.fail()
				}
			}
			args.truncate(called.args.len());
		} else {
			snafu::ensure!(kind == called.kind && code_args == called.args, apply::Mismatch {
				called: called.clone(),
				code: Call { kind, args: code_args },
			});
		}

		Ok(())
	}
}

pub fn apply_flat(body: &mut [FlatStmt], called: &[Call], functions: &mut Functions) -> Result<bool, ApplyError> {
	let mut ctx = Visitor(Apply::new(called, functions));
	for stmt in body {
		ctx.flat_stmt(stmt)?;
	}
	ctx.0.finish()
}

pub fn apply_tree(body: &mut [Stmt], called: &[Call], functions: &mut Functions) -> Result<bool, ApplyError> {
	let mut ctx = Visitor(Apply::new(called, functions));
	for stmt in body {
		ctx.stmt(stmt)?;
	}
	ctx.0.finish()
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
		signature: Vec<Arg>,
		args: Vec<CallArg>,
	},
}

struct Infer<'a> {
	called: Vec<Call>,
	functions: &'a Functions,
}

impl Infer<'_> {
	pub fn new(functions: &Functions) -> Infer<'_> {
		Infer { called: Vec::new(), functions }
	}

	pub fn finish(self, dup: bool) -> Result<Vec<Call>, InferError> {
		let mut called = self.called;
		if dup {
			called.extend_from_within(..);
		}
		Ok(called)
	}
}

impl Visit for Infer<'_> {
	type Error = InferError;

	fn call<V>(&mut self, kind: CallKind, args: &mut Vec<Expr<V>>) -> Result<(), InferError> {
		let (name, code_args) = code_args(&kind, args);

		if let Some(name) = name {
			let sig = self.functions.get(name).context(infer::MissingFunction { name })?;

			let mismatch_error = infer::SignatureMismatch {
				name,
				signature: sig.as_slice(),
				args: code_args.as_slice(),
			};

			for sigarg in sig.get(code_args.len()..).context(mismatch_error)? {
				let default = sigarg.default.as_ref().context(mismatch_error)?;
				args.push(Expr::Value(sigarg.line, default.clone()));
			}
		}

		self.called.push(Call { kind, args: code_args });

		Ok(())
	}
}

pub fn infer_flat(body: &mut [FlatStmt], dup: bool, functions: &Functions) -> Result<Vec<Call>, InferError> {
	let mut ctx = Visitor(Infer::new(functions));
	for stmt in body {
		ctx.flat_stmt(stmt)?;
	}
	ctx.0.finish(dup)
}

pub fn infer_tree(body: &mut [Stmt], dup: bool, functions: &Functions) -> Result<Vec<Call>, InferError> {
	let mut ctx = Visitor(Infer::new(functions));
	for stmt in body {
		ctx.stmt(stmt)?;
	}
	ctx.0.finish(dup)
}
