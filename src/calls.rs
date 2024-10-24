use std::collections::HashMap;

use crate::scp::{Arg, Call, CallArg};
use crate::decompile::{Expr, Stmt, CallKind};
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
pub fn infer_calls(funcs: &[crate::scp::Function], called: &[Call], stmts: &mut [Stmt]) -> Result<bool> {
	let functable = funcs.iter().map(|f| (f.name.as_str(), f.args.as_slice())).collect::<HashMap<_, _>>();
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
			Stmt::PushVar(_, e) => e.infer(called)?,
			Stmt::Set(_, e) => e.infer(called)?,
			Stmt::Line(_) => {}
			Stmt::Debug(a) => a.infer(called)?,
			Stmt::If(a, b, c) => {
				a.infer(called)?;
				b.infer(called)?;
				c.infer(called)?;
			}
			Stmt::While(a, b) => {
				a.infer(called)?;
				b.infer(called)?;
			}
			Stmt::Switch(a, b) => {
				a.infer(called)?;
				for (_, v) in b {
					v.infer(called)?;
				}
			}
			Stmt::Break => {}
			Stmt::Continue => {}
			Stmt::Return(v) => v.infer(called)?,
		}
		Ok(())
	}
}

impl Infer for Expr {
	fn infer(&mut self, called: &mut Calls) -> Result<()> {
		match self {
			crate::expr::Expr::Value(_) => {},
			crate::expr::Expr::Var(_) => {},
			crate::expr::Expr::Ref(_) => {},
			crate::expr::Expr::Call(c, a) => {
				infer_call(called, c, a)?;
				a.infer(called)?;
			}
			crate::expr::Expr::Unop(_, a) => a.infer(called)?,
			crate::expr::Expr::Binop(_, a, b) => {
				a.infer(called)?;
				b.infer(called)?;
			},
			crate::expr::Expr::Line(_, a) => a.infer(called)?,
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
		let func = called.functable.get(name.as_str()).expect("name was looked up from table in the first place");
		snafu::ensure!(args.len() == func.len(), BadArgsSnafu { call: call.clone(), args: args.clone(), sig: func.to_vec() });
		for i in a.len()..exp_args.len() {
			let Some(default) = &func[i].default else {
				return BadArgsSnafu { call: call.clone(), args: args.clone(), sig: func.to_vec() }.fail();
			};
			snafu::ensure!(a[i] == Expr::Value(default.clone()), BadArgsSnafu { call: call.clone(), args: args.clone(), sig: func.to_vec() });
		}
		a.truncate(exp_args.len());
	} else {
		snafu::ensure!(args.len() == exp_args.len(), NonLocalDefaultSnafu { call: call.clone(), args: args.clone(), exp_args: exp_args.clone() });
	}
	Ok(())
}

fn to_call_arg(expr: &Expr) -> CallArg {
	match expr {
		crate::expr::Expr::Value(v) => CallArg::Value(v.clone()),
		crate::expr::Expr::Var(_) => CallArg::Var,
		crate::expr::Expr::Ref(_) => CallArg::Var,
		crate::expr::Expr::Call(_, _) => CallArg::Call,
		crate::expr::Expr::Unop(_, _) => CallArg::Expr,
		crate::expr::Expr::Binop(_, _, _) => CallArg::Expr,
		crate::expr::Expr::Line(_, l) => to_call_arg(l),
	}
}
