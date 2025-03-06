use super::*;
use std::fmt::{Formatter, Result, Debug};

fn line<'a, 'b>(f: &'a mut Formatter<'b>, l: &Option<u16>) -> std::result::Result<&'a mut Formatter<'b>, std::fmt::Error> {
	if let Some(l) = l {
		write!(f, "{l}:")?;
	}
	Ok(f)
}

fn write_args(f: &mut Formatter<'_>, name: &str, args: &[Expr]) -> Result {
	let mut t = f.debug_tuple(name);
	for arg in args {
		t.field(arg);
	}
	t.finish()
}

impl Debug for Expr {
	fn fmt(&self, f: &mut Formatter<'_>) -> Result {
		match self {
			Self::Value(l, v) => {
				line(f, l)?;
				v.fmt(f)
			}
			Self::Var(l, v) => line(f, l)?.debug_tuple("Var").field(v).finish(),
			Self::Ref(l, v) => line(f, l)?.debug_tuple("Ref").field(v).finish(),
			Self::Call(l, a, b, args) => {
				line(f, l)?;
				if a.is_empty() {
					write_args(f, &format!("Call[{b}]"), args)
				} else {
					write_args(f, &format!("Call[{a}.{b}]"), args)
				}
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
			Self::While(l, e, label) => line(f, l)?.debug_tuple("While").field(e).field(label).finish(),
			Self::Goto(label) => f.debug_tuple("Goto").field(label).finish(),
			Self::Switch(l, e, cases, default) => line(f, l)?.debug_tuple("Switch").field(e).field(cases).field(default).finish(),
			Self::PushVar(l) => line(f, l)?.debug_tuple("PushVar").finish(),
			Self::PopVar(pop) => f.debug_tuple(&format!("PopVar[{pop}]")).finish(),
			Self::Debug(l, args) => {
				line(f, l)?;
				write_args(f, "Debug", args)
			}
			Self::Tailcall(l, a, b, args, pop) => {
				line(f, l)?;
				if a.is_empty() {
					write_args(f, &format!("Tailcall[{b};{pop}]"), args)
				} else {
					write_args(f, &format!("Tailcall[{a}.{b};{pop}]"), args)
				}
			}
		}
	}
}
