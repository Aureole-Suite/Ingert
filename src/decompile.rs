use std::{cell::Cell, collections::HashMap};

use crate::nest::{self, Label};
pub use nest::CallKind;
use snafu::OptionExt as _;

use crate::expr;
pub type Expr = expr::Expr<StackVar>;
pub type Lvalue = expr::Lvalue<StackVar>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StackVar(pub i32);
// #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
// pub enum StackVar {
// 	Stack(u32),
// 	Arg(u32),
// }

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
	Expr(Expr),
	PushVar(StackVar, Option<Expr>),
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
		pub fn display_block(stmts: &[Stmt], f: &mut Formatter, indent: usize) -> Result {
			let mut was_line = false;
			for stmt in stmts {
				if let Stmt::Line(n) = stmt {
					if !was_line {
						write!(f, "{}", "  ".repeat(indent))?;
					}
					was_line = true;
					write!(f, "line {n} ")?;
				} else {
					if was_line {
						stmt.display_inline(f, indent)?;
					} else {
						stmt.display(f, indent)?;
					}
					was_line = false;
				}
			}
			Ok(())
		}

		pub fn display(&self, f: &mut Formatter, indent: usize) -> Result {
			let i = "  ".repeat(indent);
			write!(f, "{i}")?;
			self.display_inline(f, indent)
		}

		fn display_inline(&self, f: &mut Formatter, indent: usize) -> Result {
			let i = "  ".repeat(indent);
			match self {
				Stmt::Expr(e) => writeln!(f, "{e}"),
				Stmt::PushVar(v, None) => writeln!(f, "let {v}"),
				Stmt::PushVar(v, Some(e)) => writeln!(f, "let {v} = {e}"),
				Stmt::Set(v, e) => writeln!(f, "{v} = {e}"),
				Stmt::Line(l) => writeln!(f, "line {l}"),
				Stmt::Debug(args) => {
					write!(f, "debug")?;
					expr::write_args(f, args)?;
					writeln!(f)
				}
				Stmt::If(e, yes, no) => {
					writeln!(f, "if {e} {{")?;
					Self::display_block(yes, f, indent + 1)?;
					match no.as_ref().map(Vec::as_slice) {
						Some([s@Stmt::If(..)]) => {
							write!(f, "{i}}} else ")?;
							s.display_inline(f, indent)?;
						}
						Some([Stmt::Line(n), s@Stmt::If(..)]) => {
							write!(f, "{i}}} else line {n} ")?;
							s.display_inline(f, indent)?;
						}
						Some(no) => {
							writeln!(f, "{i}}} else {{")?;
							Self::display_block(no, f, indent + 1)?;
							writeln!(f, "{i}}}")?;
						}
						None => {
							writeln!(f, "{i}}}")?;
						}
					}
					Ok(())
				}
				Stmt::While(e, body) => {
					writeln!(f, "while {e} {{")?;
					Self::display_block(body, f, indent + 1)?;
					writeln!(f, "{i}}}")
				}
				Stmt::Switch(e, cases) => {
					writeln!(f, "switch {e} {{")?;
					for (c, body) in cases {
						if let Some(c) = c {
							writeln!(f, "{i}  case {c}:")?;
						} else {
							writeln!(f, "{i}  default:")?;
						}
						Self::display_block(body, f, indent + 2)?;
					}
					writeln!(f, "{i}}}")
				}
				Stmt::Break => writeln!(f, "break"),
				Stmt::Continue => writeln!(f, "continue"),
				Stmt::Return(None) => writeln!(f, "return"),
				Stmt::Return(Some(e)) => writeln!(f, "return {e}"),
			}
		}
	}
}

#[derive(Debug, snafu::Snafu)]
pub enum Error {
	#[snafu(display("unknown label: {label}"))]
	Label { label: Label },
	#[snafu(display("unexpected {stmt} when parsing switch: {why}"))]
	Switch { why: &'static str, stmt: nest::Stmt },
	#[snafu(display("Unexpected jump to {label}"))]
	Jump { label: Label },
}

type Result<T, E = Error> = std::result::Result<T, E>;

// TODO do I really need to remove labels?
pub fn decompile(nargs: usize, stmts: &[nest::Stmt]) -> Result<Vec<Stmt>> {
	let mut stmts_no_labels = Vec::new();
	let mut labels = HashMap::new();
	for stmt in stmts {
		match stmt {
			nest::Stmt::Label(l) => {
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
	stmts: &'a [&'a nest::Stmt],
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

	fn next(&mut self) -> Option<&'a nest::Stmt> {
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

		if let nest::Stmt::Goto(l) = *self.gctx.stmts[self.end - 1] && pos(self.gctx.lookup(l)?) {
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
			nest::Stmt::Return(None) => stmts.push(Stmt::Return(None)),
			nest::Stmt::Return(Some(e)) => stmts.push(Stmt::Return(Some(expr(ctx.stack, e)?))),
			nest::Stmt::Expr(e) => stmts.push(Stmt::Expr(expr(ctx.stack, e)?)),
			nest::Stmt::Set(l, e) => stmts.push(Stmt::Set(lvalue(ctx.stack, l)?, expr(ctx.stack, e)?)),
			nest::Stmt::Label(_) => {}
			nest::Stmt::If(e, l) => {
				let start = ctx.pos;
				let e = expr(ctx.stack, e)?;
				let mut sub = ctx.sub(*l)?;
				if let Some(cont) = sub.last_goto(|i| i == start - 1)? {
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
			nest::Stmt::Switch(e) => {
				let e = expr(ctx.stack, e)?;
				let cases = parse_switch(&mut ctx, stmt)?;
				stmts.push(Stmt::Switch(e, cases));
			}
			nest::Stmt::Case(_, _) => return SwitchSnafu { why: "stray case", stmt: stmt.clone() }.fail(),
			nest::Stmt::Goto(l) if Some(*l) == ctx.brk => stmts.push(Stmt::Break),
			nest::Stmt::Goto(l) if Some(*l) == ctx.cont => stmts.push(Stmt::Continue),
			nest::Stmt::Goto(l) => return JumpSnafu { label: *l }.fail(),
			nest::Stmt::PushVar => {
				const LAST: nest::StackSlot = nest::StackSlot(-1);
				ctx.stack += 1;
				let var = stack_slot(ctx.stack, &LAST)?;
				if ctx.pos < ctx.end
					&& let nest::Stmt::Set(nest::Lvalue::Stack(LAST), e) = ctx.gctx.stmts[ctx.pos]
				{
					ctx.pos += 1;
					stmts.push(Stmt::PushVar(var, Some(expr(ctx.stack, e)?)));
				} else {
					stmts.push(Stmt::PushVar(var, None));
				}
			}
			nest::Stmt::PopVar => {}, // only used at end of block
			nest::Stmt::Line(l) => stmts.push(Stmt::Line(*l)),
			nest::Stmt::Debug(args) => stmts.push(Stmt::Debug(do_args(ctx.stack, args)?)),
		}
	}
	Ok(stmts)
}

fn parse_switch(ctx: &mut Ctx, stmt: &nest::Stmt) -> Result<Vec<(Option<i32>, Vec<Stmt>)>> {
	let mut cases = Vec::new();
	let default = loop {
		match ctx.next().with_context(|| SwitchSnafu { why: "unterminated", stmt: stmt.clone() })? {
			nest::Stmt::Case(v, l) => cases.push((Some(*v), *l)),
			nest::Stmt::Goto(l) => break *l,
			stmt => return SwitchSnafu { why: "unexpected", stmt: stmt.clone() }.fail(),
		}
	};
	snafu::ensure!(cases.is_sorted_by_key(|(_, a)| a), SwitchSnafu { why: "unsorted", stmt: stmt.clone() });
	let default_pos = cases.partition_point(|(_, a)| *a < default);
	cases.insert(default_pos, (None, default));

	let mut cases2 = Vec::with_capacity(cases.len());
	let the_end = Cell::new(None);
	let ends = cases.iter().skip(1).map(|i| i.1).chain(std::iter::once_with(|| the_end.get()).flatten());
	for (&(key, target), end) in std::iter::zip(&cases, ends) {
		let target = ctx.gctx.lookup(target)?;
		let end_pos = ctx.gctx.lookup(end)?;
		assert_eq!(ctx.pos, target);
		let mut sub = ctx.sub(end)?;
		let new_end = sub.last_goto(|l| l >= end_pos)?;
		if let Some(new_end) = new_end {
			if let Some(the_end) = the_end.get() {
				snafu::ensure!(the_end == new_end, SwitchSnafu { why: "wrong end", stmt: stmt.clone() });
			}
			the_end.set(Some(new_end));
		}
		sub.brk = the_end.get();
		let mut body = block(sub)?;
		if new_end.is_some() {
			body.push(Stmt::Break);
		}
		cases2.push((key, body));
	}
	Ok(cases2)
}

fn expr(stack: usize, e: &nest::Expr) -> Result<Expr> {
	Ok(match e {
		nest::Expr::Value(v) => Expr::Value(v.clone()),
		nest::Expr::Var(v) => Expr::Var(lvalue(stack, v)?),
		nest::Expr::Ref(v) => Expr::Ref(stack_slot(stack, v)?),
		nest::Expr::Call(call, args) => {
			let add = match call {
				CallKind::System(..) => 0,
				CallKind::Func(a, _) => if a.is_empty() { 2 } else { 5 },
				CallKind::Tail(..) => 0,
			};
			Expr::Call(call.clone(), do_args(stack + add, args)?)
		}
		nest::Expr::Unop(o, a) => Expr::Unop(*o, expr(stack, a)?.into()),
		nest::Expr::Binop(o, a, b) => Expr::Binop(*o, expr(stack, a)?.into(), expr(stack + 1, b)?.into()),
		nest::Expr::Line(l, a) => Expr::Line(*l, expr(stack, a)?.into()),
	})
}

fn do_args(stack: usize, args: &[nest::Expr]) -> Result<Vec<Expr>> {
	let mut out = Vec::with_capacity(args.len());
	for (a, plus) in args.iter().zip((0..args.len()).rev()) {
		out.push(expr(stack + plus, a)?);
	}
	Ok(out)
}

fn lvalue(stack: usize, l: &nest::Lvalue) -> Result<Lvalue> {
	Ok(match l {
		nest::Lvalue::Stack(s) => Lvalue::Stack(stack_slot(stack, s)?),
		nest::Lvalue::Deref(s) => Lvalue::Deref(stack_slot(stack, s)?),
		nest::Lvalue::Global(n) => Lvalue::Global(n.clone())
	})
}

fn stack_slot(stack: usize, v: &nest::StackSlot) -> Result<StackVar> {
	Ok(StackVar(v.0 + stack as i32))
}

