use super::*;
use std::fmt::{Formatter, Result, Debug};

fn line<'a, 'b>(f: &'a mut Formatter<'b>, l: &Option<u16>) -> std::result::Result<&'a mut Formatter<'b>, std::fmt::Error> {
	if let Some(l) = l {
		write!(f, "{l}:")?;
	}
	Ok(f)
}

fn write_args<V: Debug>(f: &mut Formatter<'_>, name: &str, args: &[Expr<V>]) -> Result {
	let mut t = f.debug_tuple(name);
	for arg in args {
		t.field(arg);
	}
	t.finish()
}

impl<V: Debug> Debug for Expr<V> {
	fn fmt(&self, f: &mut Formatter<'_>) -> Result {
		match self {
			Self::Value(l, v) => {
				line(f, l)?;
				v.fmt(f)
			}
			Self::Var(l, v) => line(f, l)?.debug_tuple("Var").field(v).finish(),
			Self::Ref(l, v) => line(f, l)?.debug_tuple("Ref").field(v).finish(),
			Self::Call(l, name, args) => {
				line(f, l)?;
				write_args(f, &format!("Call[{name}]"), args)
			}
			Self::Syscall(l, a, b, args) => {
				line(f, l)?;
				write_args(f, &format!("Syscall[{a},{b}]"), args)
			}
			Self::Unop(l, op, a) => line(f, l)?.debug_tuple(&format!("{op:?}")).field(a).finish(),
			Self::Binop(l, op, a, b) => line(f, l)?.debug_tuple(&format!("{op:?}")).field(a).field(b).finish(),
		}
	}
}

impl Debug for FlatStmt {
	fn fmt(&self, f: &mut Formatter<'_>) -> Result {
		match self {
			Self::Label(label) => f.debug_tuple("Label").field(label).finish(),
			Self::Expr(e) => f.debug_tuple("Expr").field(e).finish(),
			Self::Set(l, v, e) => line(f, l)?.debug_tuple("Set").field(v).field(e).finish(),
			Self::Return(l, e, pop) => line(f, l)?.debug_tuple(&format!("Return[{pop}]")).field(e).finish(),
			Self::If(l, e, label) => line(f, l)?.debug_tuple("If").field(e).field(label).finish(),
			Self::Goto(label, pop) => f.debug_tuple(&format!("Goto[{pop}]")).field(label).finish(),
			Self::Switch(l, e, cases, default) => line(f, l)?.debug_tuple("Switch").field(e).field(cases).field(default).finish(),
			Self::PushVar(l) => line(f, l)?.debug_tuple("PushVar").finish(),
			Self::PopVar(pop) => f.debug_tuple(&format!("PopVar[{pop}]")).finish(),
			Self::Debug(l, args) => {
				line(f, l)?;
				write_args(f, "Debug", args)
			}
			Self::Tailcall(l, name, args, pop) => {
				line(f, l)?;
				write_args(f, &format!("Tailcall[{name};{pop}]"), args)
			}
		}
	}
}

impl Debug for Stmt {
	fn fmt(&self, f: &mut Formatter<'_>) -> Result {
		match self {
			Self::Expr(e) => f.debug_tuple("Expr").field(e).finish(),
			Self::Set(l, v, e) => line(f, l)?.debug_tuple("Set").field(v).field(e).finish(),
			Self::Return(l, e) => line(f, l)?.debug_tuple("Return").field(e).finish(),
			Self::If(l, e, then, els) => {
				line(f, l)?;
				let mut tup = f.debug_tuple("If");
				tup.field(e);
				tup.field(then);
				if let Some(els) = els {
					if let [stmt@Stmt::If(..)] = els.as_slice() {
						tup.field(stmt);
					} else {
						tup.field(els);
					}
				}
				tup.finish()
			},
			Self::While(l, e, body) => line(f, l)?.debug_tuple("While").field(e).field(body).finish(),
			Self::Switch(l, e, cases) => line(f, l)?.debug_tuple("Switch").field(e).field(cases).finish(),
			Self::Block(stmts) => f.debug_tuple("Block").field(stmts).finish(),
			Self::Break => f.debug_tuple("Break").finish(),
			Self::Continue => f.debug_tuple("Continue").finish(),
			Self::PushVar(l, depth, expr) => line(f, l)?.debug_tuple("PushVar").field(depth).field(expr).finish(),
			Self::Debug(l, args) => {
				line(f, l)?;
				write_args(f, "Debug", args)
			}
			Self::Tailcall(l, name, args) => {
				line(f, l)?;
				write_args(f, &format!("Tailcall[{name}]"), args)
			}
		}
	}
}

impl Debug for FlatVar {
	fn fmt(&self, f: &mut Formatter<'_>) -> Result {
		f.write_str("#")?;
		self.0.fmt(f)
	}
}

impl Debug for Var {
	fn fmt(&self, f: &mut Formatter<'_>) -> Result {
		self.0.fmt(f)
	}
}

