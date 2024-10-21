use std::collections::HashMap;

use crate::nest::{self, NStmt, Label};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StackVar(pub i32);
// #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
// pub enum StackVar {
// 	Stack(u32),
// 	Arg(u32),
// }

pub use nest::CallKind;
use snafu::OptionExt as _;
pub type Expr = nest::Expr<StackVar>;
pub type Lvalue = nest::Lvalue<StackVar>;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
	Expr(Expr),
	PushVar(Option<Expr>),
	Set(Lvalue, Expr),
	Line(u16),
	Debug(Vec<Expr>),

	If(Expr, Vec<Stmt>, Option<Vec<Stmt>>),
	While(Expr, Vec<Stmt>),
	Switch(Expr, Vec<(Option<i32>, Vec<Stmt>)>),
	Break,
	Continue,
	Return(Option<Expr>),
}

mod display {
	use super::*;
	use std::fmt::{Display, Formatter, Result};

	impl Display for StackVar {
		fn fmt(&self, f: &mut Formatter<'_>) -> Result {
			write!(f, "v[{}]", self.0)
		}
	}

	impl Display for Stmt {
		fn fmt(&self, f: &mut Formatter<'_>) -> Result {
			self.display(f, 0)
		}
	}

	impl Stmt {
		pub fn display(&self, f: &mut Formatter, indent: usize) -> Result {
			let i = "  ".repeat(indent);
			match self {
				Stmt::Expr(e) => writeln!(f, "{i}{e}"),
				Stmt::PushVar(None) => writeln!(f, "{i}push"),
				Stmt::PushVar(Some(e)) => writeln!(f, "{i}push {e}"),
				Stmt::Set(v, e) => writeln!(f, "{i}{v} = {e}"),
				Stmt::Line(l) => writeln!(f, "{i}line {l}"),
				Stmt::Debug(args) => {
					write!(f, "{i}debug(")?;
					let mut it = args.iter();
					if let Some(a) = it.next() {
						a.fmt(f)?;
						for a in it {
							write!(f, ", ")?;
							a.fmt(f)?;
						}
					}
					writeln!(f, ")")
				}
				Stmt::If(e, yes, no) => {
					writeln!(f, "{i}if {e} {{")?;
					for stmt in yes {
						stmt.display(f, indent + 1)?;
					}
					if let Some(no) = no {
						if let [s@Stmt::If(..)] = no.as_slice() {
							write!(f, "}} else ")?;
							s.display(f, indent)?;
						} else {
							writeln!(f, "}} else {{")?;
							for stmt in no {
								stmt.display(f, indent + 1)?;
							}
						}
					}
					writeln!(f, "{i}}}")?;
					Ok(())
				}
				Stmt::While(e, body) => {
					writeln!(f, "{i}while {e} {{")?;
					for stmt in body {
						stmt.display(f, indent + 1)?;
					}
					writeln!(f, "{i}}}")
				}
				Stmt::Switch(e, cases) => {
					writeln!(f, "{i}switch {e} {{")?;
					for (c, body) in cases {
						if let Some(c) = c {
							writeln!(f, "{i}  case {c}:")?;
						} else {
							writeln!(f, "{i}  default:")?;
						}
						for stmt in body {
							stmt.display(f, indent + 2)?;
						}
					}
					writeln!(f, "{i}}}")
				}
				Stmt::Break => writeln!(f, "{i}break"),
				Stmt::Continue => writeln!(f, "{i}continue"),
				Stmt::Return(None) => writeln!(f, "{i}return"),
				Stmt::Return(Some(e)) => writeln!(f, "{i}return {e}"),
			}
		}
	}
}

#[derive(Debug, snafu::Snafu)]
pub enum Error {
	#[snafu(display("unknown label: {}", label))]
	Label { label: Label },
	#[snafu(display("unexpected stmt when parsing switch: {}", stmt))]
	Switch { stmt: NStmt },
}

type Result<T, E = Error> = std::result::Result<T, E>;

// TODO do I really need to remove labels?
pub fn decompile(nargs: usize, stmts: &[NStmt]) -> Result<Vec<Stmt>> {
	let mut stmts_no_labels = Vec::new();
	let mut labels = HashMap::new();
	for stmt in stmts {
		match stmt {
			NStmt::Label(l) => {
				labels.insert(*l, stmts_no_labels.len());
			}
			stmt => {
				stmts_no_labels.push(stmt);
			}
		}
	}

	let gctx = Gctx {
		stmts: &stmts_no_labels,
		labels,
	};

	block(Ctx::new(&gctx, nargs))
}

struct Gctx<'a> {
	stmts: &'a [&'a NStmt],
	labels: HashMap<Label, usize>,
}

impl Gctx<'_> {
    fn lookup(&self, label: Label) -> Result<usize> {
		self.labels.get(&label).copied().context(LabelSnafu { label })
	}
}

struct Ctx<'a> {
	gctx: &'a Gctx<'a>,
	pos: usize,
	end: usize,
	stack: usize,
	brk: Option<Label>,
	cont: Option<Label>,
	has_goto: bool
}

impl<'a> Ctx<'a> {
	fn new(gctx: &'a Gctx, stack: usize) -> Self {
		Self {
			gctx,
			pos: 0,
			end: gctx.stmts.len(),
			stack,
			brk: None,
			cont: None,
			has_goto: false,
		}
	}

	fn next(&mut self) -> Option<&'a NStmt> {
		if self.pos == self.end || (self.has_goto && self.pos == self.end - 1) {
			None
		} else {
			let stmt = self.gctx.stmts[self.pos];
			self.pos += 1;
			Some(stmt)
		}
	}

	fn sub(&mut self, label: Label) -> Result<Ctx<'a>> {
		let pos = self.gctx.lookup(label)?;
		snafu::ensure!((self.pos..=self.end).contains(&pos), LabelSnafu { label });
		let sub = Self {
			end: pos,
			has_goto: self.has_goto && pos == self.end,
			..*self
		};
		self.pos = pos;
		Ok(sub)
	}

	fn last_goto(&mut self, pos: impl Fn(usize) -> bool) -> Result<Option<Label>> {
		if self.pos == self.end {
			return Ok(None);
		}

		if let NStmt::Goto(l) = *self.gctx.stmts[self.end - 1] && pos(self.gctx.lookup(l)?) {
			self.has_goto = true;
			Ok(Some(l))
		} else {
			Ok(None)
		}
	}
}

fn block(mut ctx: Ctx) -> Result<Vec<Stmt>> {
	let mut stmts = Vec::new();
	while let Some(stmt) = ctx.next() {
		match stmt {
			NStmt::Return(None) => stmts.push(Stmt::Return(None)),
			NStmt::Return(Some(e)) => stmts.push(Stmt::Return(Some(expr(ctx.stack, e)?))),
			NStmt::Expr(e) => stmts.push(Stmt::Expr(expr(ctx.stack, e)?)),
			NStmt::Set(l, e) => stmts.push(Stmt::Set(lvalue(ctx.stack, l)?, expr(ctx.stack, e)?)),
			NStmt::Label(_) => {}
			NStmt::If(e, l) => {
				let e = expr(ctx.stack, e)?;
				let mut sub = ctx.sub(*l)?;
				if let Some(cont) = sub.last_goto(|i| i == ctx.pos - 1)? {
					sub.brk = Some(*l);
					sub.cont = Some(cont);
					let body = block(sub)?;
					stmts.push(Stmt::While(e, body));
				} else if let Some(els) = sub.last_goto(|i| i >= ctx.pos && i <= ctx.end)? {
					let yes = block(sub)?;
					let sub = ctx.sub(els)?;
					let no = block(sub)?;
					stmts.push(Stmt::If(e, yes, Some(no)));
				} else {
					let yes = block(sub)?;
					stmts.push(Stmt::If(e, yes, None));
				}
			}
			NStmt::Switch(_) => todo!(),
			NStmt::Case(_, _) => return SwitchSnafu { stmt: stmt.clone() }.fail(),
			NStmt::Goto(_) => todo!(),
			NStmt::PushVar => todo!(),
			NStmt::PopVar => {}, // only used at end of block
			NStmt::Line(l) => stmts.push(Stmt::Line(*l)),
			NStmt::Debug(args) => stmts.push(Stmt::Debug(do_args(ctx.stack, args, 0..args.len())?)),
		}
	}
	Ok(stmts)
}

fn expr(stack: usize, e: &nest::Expr<crate::scp::StackSlot>) -> Result<Expr> {
	Ok(match e {
		nest::Expr::Value(v) => Expr::Value(v.clone()),
		nest::Expr::Var(v) => Expr::Var(lvalue(stack, v)?),
		nest::Expr::Ref(v) => Expr::Ref(stack_slot(stack, v)?),
		nest::Expr::Call(call, args) => {
			let args = match call {
				CallKind::System(..) => do_args(stack, args, (0..args.len()).rev())?,
				CallKind::Func(a, _) => do_args(stack + if a.is_empty() { 2 } else { 5 }, args, (0..args.len()).rev())?,
				CallKind::Become(..) => do_args(stack, args, 0..args.len())?,
			};
			Expr::Call(call.clone(), args)
		}
		nest::Expr::Unop(o, a) => Expr::Unop(*o, expr(stack, a)?.into()),
		nest::Expr::Binop(o, a, b) => Expr::Binop(*o, expr(stack, a)?.into(), expr(stack + 1, b)?.into()),
		nest::Expr::Line(l, a) => Expr::Line(*l, expr(stack, a)?.into()),
	})
}

fn do_args(stack: usize, args: &[nest::Expr<crate::scp::StackSlot>], plus: impl IntoIterator<Item=usize>) -> Result<Vec<Expr>> {
	let mut out = Vec::with_capacity(args.len());
	for (a, plus) in args.iter().zip(plus) {
		out.push(expr(stack + plus, a)?);
	}
	Ok(out)
}

fn lvalue(stack: usize, l: &nest::Lvalue<crate::scp::StackSlot>) -> Result<Lvalue> {
	Ok(match l {
		nest::Lvalue::Stack(s) => Lvalue::Stack(stack_slot(stack, s)?),
		nest::Lvalue::Deref(s) => Lvalue::Deref(stack_slot(stack, s)?),
		nest::Lvalue::Global(n) => Lvalue::Global(n.clone())
	})
}

fn stack_slot(stack: usize, v: &crate::scp::StackSlot) -> Result<StackVar> {
	Ok(StackVar(v.0 + stack as i32))
}

