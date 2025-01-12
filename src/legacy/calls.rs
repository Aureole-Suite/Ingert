use std::collections::HashMap;

use crate::legacy::expr::Arg;
use crate::legacy::scp::{Call, CallArg, Item};
use crate::legacy::decompile::{Expr, Stmt, CallKind};
// It might be tempting to do this on nest rather than decompile, but that might screw up stack usage.

#[derive(Debug, snafu::Snafu)]
pub enum Error {
	DifferentCall { call: CallKind, exp_call: CallKind },
	DifferentArgs { call: CallKind, args: Vec<CallArg>, exp_args: Vec<CallArg> },
	NonLocalDefault { call: CallKind, args: Vec<CallArg>, exp_args: Vec<CallArg> },
	TooManyCalls,
	BadArgs { call: CallKind, args: Vec<CallArg>, sig: Vec<Arg> },
}

type Result<T, E = Error> = std::result::Result<T, E>;

// Returns whether the calls table is mysteriously duplicated. Seriously, I have no idea why this is a thing.
pub fn infer_calls(funcs: &[Item], called: &[Call], stmts: &mut [Stmt]) -> Result<bool> {
	let mut functable = HashMap::new();
	for f in funcs {
		if let Item::Function(f) = f {
			functable.insert(f.name.as_str(), f.args.as_slice());
		}
	}
	let mut called = Calls { called, pos: 0, functable };
	stmts.infer(&mut called)?;
	if called.pos != called.called.len() {
		let (a, b) = called.called.split_at(called.pos);
		snafu::ensure!(a == b, TooManyCallsSnafu);
	}
	Ok(called.pos != called.called.len())
}

struct Calls<'a> {
	called: &'a [Call],
	pos: usize,
	functable: HashMap<&'a str, &'a [Arg]>,
}

trait Infer {
	fn infer(&mut self, called: &mut Calls) -> Result<()>;
}

impl<T: Infer> Infer for [T] {
	fn infer(&mut self, called: &mut Calls) -> Result<()> {
		for stmt in self {
			T::infer(stmt, called)?;
		}
		Ok(())
	}
}

impl<T: Infer> Infer for Vec<T> {
	fn infer(&mut self, called: &mut Calls) -> Result<()> {
		self.as_mut_slice().infer(called)
	}
}

impl<T: Infer> Infer for Option<T> {
	fn infer(&mut self, called: &mut Calls) -> Result<()> {
		if let Some(stmt) = self {
			T::infer(stmt, called)?;
		}
		Ok(())
	}
}

impl<T: Infer + ?Sized> Infer for Box<T> {
	fn infer(&mut self, called: &mut Calls) -> Result<()> {
		T::infer(self, called)
	}
}

impl<T: Infer + ?Sized> Infer for &mut T {
	fn infer(&mut self, called: &mut Calls) -> Result<()> {
		T::infer(self, called)
	}
}

impl Infer for Stmt {
	fn infer(&mut self, called: &mut Calls) -> Result<()> {
		match self {
			Stmt::Expr(e) => e.infer(called)?,
			Stmt::PushVar(_, _, e) => e.infer(called)?,
			Stmt::Set(_, _, e) => e.infer(called)?,
			Stmt::Debug(_, a) => a.infer(called)?,
			Stmt::If(_, a, b, c) => {
				a.infer(called)?;
				b.infer(called)?;
				c.infer(called)?;
			}
			Stmt::While(_, a, b) => {
				a.infer(called)?;
				b.infer(called)?;
			}
			Stmt::Switch(_, a, b) => {
				a.infer(called)?;
				for (_, v) in b {
					v.infer(called)?;
				}
			}
			Stmt::Break => {}
			Stmt::Continue => {}
			Stmt::Return(_, v) => v.infer(called)?,
		}
		Ok(())
	}
}

impl Infer for Expr {
	fn infer(&mut self, called: &mut Calls) -> Result<()> {
		match self {
			Expr::Value(_, _) => {},
			Expr::Var(_, _) => {},
			Expr::Ref(_, _) => {},
			Expr::Call(_, c, a) => {
				infer_call(called, c, a)?;
				a.infer(called)?;
			}
			Expr::Unop(_, _, a) => a.infer(called)?,
			Expr::Binop(_, _, a, b) => {
				a.infer(called)?;
				b.infer(called)?;
			},
		}
		Ok(())
	}
}

fn infer_call(called: &mut Calls, call: &mut CallKind, a: &mut Vec<Expr>) -> Result<()> {
	let args = a.iter().map(to_call_arg).collect::<Vec<_>>();
	let (exp_call, exp_args) = &called.called[called.pos];
	called.pos += 1;

	snafu::ensure!(call == exp_call, DifferentCallSnafu { call: call.clone(), exp_call: exp_call.clone() });
	snafu::ensure!(args.starts_with(exp_args), DifferentArgsSnafu { call: call.clone(), args: args.clone(), exp_args: exp_args.clone() });
	if let CallKind::Func(name) = call && !name.contains('.') {
		let sig = *called.functable.get(name.as_str()).expect("name was looked up from table in the first place");
		snafu::ensure!(args.len() == sig.len(), BadArgsSnafu { call: call.clone(), args: args.clone(), sig });
		for i in a.len()..exp_args.len() {
			let Some(default) = &sig[i].default else {
				return BadArgsSnafu { call: call.clone(), args: args.clone(), sig }.fail();
			};
			if !matches!(&a[i], Expr::Value(_, v) if v == default) {
				return BadArgsSnafu {
					call: call.clone(),
					args: args.clone(),
					sig,
				}.fail();
			};
		}
		a.truncate(exp_args.len());
	} else {
		snafu::ensure!(args.len() == exp_args.len(), NonLocalDefaultSnafu { call: call.clone(), args: args.clone(), exp_args: exp_args.clone() });
	}
	Ok(())
}

fn to_call_arg(expr: &Expr) -> CallArg {
	match expr {
		Expr::Value(_, v) => CallArg::Value(v.clone()),
		Expr::Var(..) => CallArg::Var,
		Expr::Ref(..) => CallArg::Var,
		Expr::Call(..) => CallArg::Call,
		Expr::Unop(..) => CallArg::Expr,
		Expr::Binop(..) => CallArg::Expr,
	}
}
