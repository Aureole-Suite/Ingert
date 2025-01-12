use std::collections::HashMap;

use crate::legacy::nest::{self, Label};
pub use nest::CallKind;
use snafu::OptionExt as _;

use crate::legacy::expr;
pub type Expr<T = StackVar> = expr::Expr<T>;
pub type Lvalue<T = StackVar> = expr::Lvalue<T>;

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
	PushVar(Option<u16>, StackVar, Option<Expr>),
	Set(Option<u16>, Lvalue, Expr),
	Debug(Option<u16>, Vec<Expr>),

	If(Option<u16>, Expr, Vec<Stmt>, Option<Vec<Stmt>>),
	While(Option<u16>, Expr, Vec<Stmt>),
	Switch(Option<u16>, Expr, Vec<(Option<i32>, Vec<Stmt>)>),
	Break,
	Continue,
	Return(Option<u16>, Option<Expr>),
}

#[derive(Debug, snafu::Snafu)]
pub enum Error {
	#[snafu(display("unknown label: {label:?}"))]
	Label { label: Label },
	#[snafu(display("unexpected {stmt:?} when parsing switch: {why}"))]
	Switch { why: &'static str, stmt: nest::Stmt },
	#[snafu(display("Unexpected jump to {label:?}"))]
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

	let (body, _) = block(&mut Ctx::new(&gctx, 0), GotoAllowed::No)?;
	Ok(body)
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
		}
	}

	fn next(&mut self) -> Option<&'a nest::Stmt> {
		if self.pos == self.end {
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
			..*self
		};
		self.pos = pos;
		Ok(sub)
	}

	fn goto_before(&self, label: Label) -> Result<Option<Label>> {
		let pos = self.gctx.lookup(label)?;
		snafu::ensure!((self.pos..=self.end).contains(&pos), LabelSnafu { label });
		if pos > 0 && let nest::Stmt::Goto(cont) = self.gctx.stmts[pos - 1] {
			Ok(Some(*cont))
		} else {
			Ok(None)
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum GotoAllowed {
	Anywhere,
	Yes,
	No,
}

fn block(ctx: &mut Ctx, goto_allowed: GotoAllowed) -> Result<(Vec<Stmt>, Option<Label>)> {
	let mut stmts = Vec::new();
	while let Some(stmt) = ctx.next() {
		match stmt {
			nest::Stmt::Return(l, None) => stmts.push(Stmt::Return(*l, None)),
			nest::Stmt::Return(l, Some(e)) => stmts.push(Stmt::Return(*l, Some(expr(ctx.stack, e)?))),
			nest::Stmt::Expr(e) => stmts.push(Stmt::Expr(expr(ctx.stack, e)?)),
			nest::Stmt::Set(l, lv, e) => stmts.push(Stmt::Set(*l, lvalue(ctx.stack, lv)?, expr(ctx.stack, e)?)),
			nest::Stmt::Label(_) => {}

			nest::Stmt::If(l, e, label) => {
				let e = expr(ctx.stack, e)?;

				if let Some(cont) = ctx.goto_before(*label)?
						&& ctx.gctx.lookup(cont)? == ctx.pos - 1 {
					let mut sub = ctx.sub(*label)?;
					sub.brk = Some(*label);
					sub.cont = Some(cont);
					let (mut body, _) = block(&mut sub, GotoAllowed::No)?;
					assert_eq!(body.pop(), Some(Stmt::Continue));
					stmts.push(Stmt::While(*l, e, body));
					continue
				}

				let (body, goto) = block(&mut ctx.sub(*label)?, GotoAllowed::Yes)?;

				if let Some(goto) = goto {
					let end = ctx.gctx.lookup(goto)?;
					snafu::ensure!(end <= ctx.end, LabelSnafu { label: goto });
					let (no, _) = block(&mut ctx.sub(goto)?, GotoAllowed::No)?;
					stmts.push(Stmt::If(*l, e, body, Some(no)));
				} else {
					stmts.push(Stmt::If(*l, e, body, None));
				}
			}

			nest::Stmt::Switch(l, e) => {
				let e = expr(ctx.stack, e)?;

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

				let mut brk = None;
				for (_, l) in &cases {
					if let Some(goto) = ctx.goto_before(*l)? && goto > *l {
						brk = brk.max(Some(goto));
					}
				}

				let mut cases2 = Vec::with_capacity(cases.len());
				let ends = cases.iter().skip(1).map(|i| i.1).chain(brk);
				for (&(key, target), end) in std::iter::zip(&cases, ends) {
					let target = ctx.gctx.lookup(target)?;
					assert_eq!(ctx.pos, target);
					let mut sub = ctx.sub(end)?;
					sub.brk = brk;
					let (body, _) = block(&mut sub, GotoAllowed::No)?;
					cases2.push((key, body));
				}

				if brk.is_some() {
					// we know where the break is, so no need for that bullshit
					if cases2.last().is_some_and(|(k, v)| k.is_none() && v.is_empty()) {
						cases2.pop();
					}
					stmts.push(Stmt::Switch(*l, e, cases2));
					continue;
				}

				let last = cases.last().unwrap();
				assert!(ctx.gctx.lookup(last.1)? == ctx.pos);
				let prev_brk = ctx.brk.take();
				let (mut body, goto) = block(ctx, GotoAllowed::Anywhere)?;
				ctx.brk = prev_brk;
				if let Some(goto) = goto {
					if ctx.gctx.lookup(goto)? == ctx.pos {
						// finally found a break, but it's useless
						body.push(Stmt::Break);
						cases2.push((last.0, body));
						stmts.push(Stmt::Switch(*l, e, cases2));
					} else if Some(goto) == ctx.brk {
						// found a break from the parent element
						if last.0.is_some() {
							cases2.push((last.0, Vec::new()));
						}
						stmts.push(Stmt::Switch(*l, e, cases2));
						stmts.extend(body);
						stmts.push(Stmt::Break);
					} else {
						// found weirdo goto
						return JumpSnafu { label: goto }.fail();
					}
				} else {
					// got to end of block without a break, so assume the last case is empty
					if last.0.is_some() {
						cases2.push((last.0, Vec::new()));
					}
					stmts.push(Stmt::Switch(*l, e, cases2));
					stmts.extend(body);
				}
			}
			nest::Stmt::Case(_, _) => return SwitchSnafu { why: "stray case", stmt: stmt.clone() }.fail(),

			nest::Stmt::Goto(l) if Some(*l) == ctx.brk => stmts.push(Stmt::Break),
			nest::Stmt::Goto(l) if Some(*l) == ctx.cont => stmts.push(Stmt::Continue),
			nest::Stmt::Goto(l) => {
				let ok = match goto_allowed {
					GotoAllowed::Anywhere => true,
					GotoAllowed::Yes => ctx.pos == ctx.end,
					GotoAllowed::No => false,
				};
				return if ok {
					Ok((stmts, Some(*l)))
				} else {
					JumpSnafu { label: *l }.fail()
				}
			}

			nest::Stmt::PushVar(l) => {
				const LAST: nest::StackSlot = nest::StackSlot(-1);
				ctx.stack += 1;
				let var = stack_slot(ctx.stack, &LAST)?;
				if ctx.pos < ctx.end
					&& let nest::Stmt::Set(None, nest::Lvalue::Stack(LAST), e) = ctx.gctx.stmts[ctx.pos]
				{
					ctx.pos += 1;
					stmts.push(Stmt::PushVar(*l, var, Some(expr(ctx.stack, e)?)));
				} else {
					stmts.push(Stmt::PushVar(*l, var, None));
				}
			}
			nest::Stmt::PopVar => {}, // only used at end of block
			nest::Stmt::Debug(l, args) => stmts.push(Stmt::Debug(*l, do_args(ctx.stack, args)?)),
		}
	}
	Ok((stmts, None))
}

fn expr(stack: usize, e: &nest::Expr) -> Result<Expr> {
	Ok(match e {
		nest::Expr::Value(l, v) => Expr::Value(*l, v.clone()),
		nest::Expr::Var(l, v) => Expr::Var(*l, lvalue(stack, v)?),
		nest::Expr::Ref(l, v) => Expr::Ref(*l, stack_slot(stack, v)?),
		nest::Expr::Call(l, call, args) => {
			let add = match call {
				CallKind::System(..) => 0,
				CallKind::Func(n) => if n.contains('.') { 5 } else { 2 },
				CallKind::Tail(..) => 0,
			};
			Expr::Call(*l, call.clone(), do_args(stack + add, args)?)
		}
		nest::Expr::Unop(l, o, a) => Expr::Unop(*l, *o, expr(stack, a)?.into()),
		nest::Expr::Binop(l, o, a, b) => Expr::Binop(*l, *o, expr(stack, a)?.into(), expr(stack + 1, b)?.into()),
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

