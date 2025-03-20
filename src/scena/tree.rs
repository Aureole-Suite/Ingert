use indexmap::IndexMap;
use snafu::OptionExt as _;

use super::{FlatStmt, Label, Stmt};

#[derive(Debug, snafu::Snafu)]
#[snafu(module(decompile), context(suffix(false)))]
pub enum DecompileError {
	#[snafu(display("unknown label: {label:?}"))]
	MissingLabel { label: Label },
	#[snafu(display("label out of bounds: {label:?} at {pos} should be in {start}..={end}"))]
	WrongLabel { label: Label, pos: usize, start: usize, end: usize },
	#[snafu(display("unsorted switch: {:?}", stmt))]
	UnsortedSwitch { stmt: FlatStmt },
	#[snafu(display("unexpected jump to {label:?}"))]
	UnexpectedJump { label: Label },
}

pub fn decompile(stmts: &[FlatStmt]) -> Result<Vec<Stmt>, DecompileError> {
	let mut labels = IndexMap::new();
	for (i, stmt) in stmts.iter().enumerate() {
		if let FlatStmt::Label(l) = stmt {
			labels.insert(*l, i);
		}
	}

	let (body, _) = Ctx::new(&Gctx { stmts, labels }).block(GotoAllowed::No)?;
	Ok(body)
}

struct Gctx<'a> {
	stmts: &'a [FlatStmt],
	labels: IndexMap<Label, usize>,
}

impl Gctx<'_> {
    fn lookup(&self, label: Label) -> Result<usize, DecompileError> {
		self.labels.get(&label).copied().context(decompile::MissingLabel { label })
	}
}

struct Ctx<'a> {
	gctx: &'a Gctx<'a>,
	pos: usize,
	end: usize,
	brk: Option<Label>,
	cont: Option<Label>,
}

impl<'a> Ctx<'a> {
	fn new(gctx: &'a Gctx) -> Self {
		Self {
			gctx,
			pos: 0,
			end: gctx.stmts.len(),
			brk: None,
			cont: None,
		}
	}

	fn next(&mut self) -> Option<&'a FlatStmt> {
		if self.pos == self.end {
			None
		} else {
			let stmt = &self.gctx.stmts[self.pos];
			self.pos += 1;
			Some(stmt)
		}
	}

	fn lookup(&self, label: Label) -> Result<usize, DecompileError> {
		let pos = self.gctx.lookup(label)?;
		snafu::ensure!((self.pos..=self.end).contains(&pos), decompile::WrongLabel {
			label,
			pos,
			start: self.pos,
			end: self.end,
		});
		Ok(pos)
	}

	fn sub(&mut self, label: Label) -> Result<Ctx<'a>, DecompileError> {
		let pos = self.lookup(label)?;
		let sub = Self {
			end: pos,
			..*self
		};
		self.pos = pos;
		Ok(sub)
	}

	fn goto_before(&self, label: Label) -> Result<Option<Label>, DecompileError> {
		let pos = self.lookup(label)?;
		if pos > 0 && let FlatStmt::Goto(cont) = self.gctx.stmts[pos - 1] {
			Ok(Some(cont))
		} else {
			Ok(None)
		}
	}

	fn block(&mut self, goto_allowed: GotoAllowed) -> Result<(Vec<Stmt>, Option<Label>), DecompileError> {
		block(self, goto_allowed)
	}
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum GotoAllowed {
	Anywhere,
	Yes,
	No,
}

fn block(ctx: &mut Ctx, goto_allowed: GotoAllowed) -> Result<(Vec<Stmt>, Option<Label>), DecompileError> {
	let mut stmts = Vec::new();
	while let Some(stmt) = ctx.next() {
		match stmt {
			FlatStmt::Label(_) => {}
			FlatStmt::Expr(expr) => stmts.push(Stmt::Expr(expr.clone())),
			FlatStmt::Set(l, v, e) => stmts.push(Stmt::Set(*l, v.clone(), e.clone())),
			FlatStmt::Return(l, e, _) => stmts.push(Stmt::Return(*l, e.clone())),
			FlatStmt::Debug(l, e) => stmts.push(Stmt::Debug(*l, e.clone())),
			FlatStmt::Tailcall(l, n, e, _) => stmts.push(Stmt::Tailcall(*l, n.clone(), e.clone())),

			FlatStmt::If(l, e, label) => {
				let e = e.clone();

				if let Some(cont) = ctx.goto_before(*label)?
						&& ctx.pos > 0
						&& ctx.gctx.lookup(cont)? == ctx.pos - 1 {
					let mut sub = ctx.sub(*label)?;
					sub.brk = Some(*label);
					sub.cont = Some(cont);
					let (mut body, _) = sub.block(GotoAllowed::No)?;
					assert_eq!(body.pop(), Some(Stmt::Continue));
					stmts.push(Stmt::While(*l, e, body));
					continue
				}

				let (body, goto) = ctx.sub(*label)?.block(GotoAllowed::Yes)?;

				if let Some(goto) = goto {
					let (no, _) = ctx.sub(goto)?.block(GotoAllowed::No)?;
					stmts.push(Stmt::If(*l, e, body, Some(no)));
				} else {
					stmts.push(Stmt::If(*l, e, body, None));
				}
			}

			FlatStmt::Switch(l, e, cases, default) => {
				let e = e.clone();

				snafu::ensure!(cases.is_sorted_by_key(|(_, a)| a), decompile::UnsortedSwitch { stmt: stmt.clone() });
				let default_pos = cases.partition_point(|(_, a)| *a < *default);
				let mut cases = cases.iter().map(|(k, l)| (Some(*k), *l)).collect::<Vec<_>>();
				cases.insert(default_pos, (None, *default));

				let mut brk = None;
				for (_, l) in &cases {
					if let Some(goto) = ctx.goto_before(*l)? && goto > *l {
						brk = brk.max(Some(goto));
					}
				}

				let mut cases2 = IndexMap::with_capacity(cases.len());
				let ends = cases.iter().skip(1).map(|i| i.1).chain(brk);
				for (&(key, target), end) in std::iter::zip(&cases, ends) {
					let target = ctx.gctx.lookup(target)?;
					assert_eq!(ctx.pos, target);
					let mut sub = ctx.sub(end)?;
					sub.brk = brk;
					let (body, _) = sub.block(GotoAllowed::No)?;
					cases2.insert(key, body);
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
				let (mut body, goto) = ctx.block(GotoAllowed::Anywhere)?;
				ctx.brk = prev_brk;
				if let Some(goto) = goto {
					if ctx.gctx.lookup(goto)? == ctx.pos {
						// finally found a break, but it's useless
						body.push(Stmt::Break);
						cases2.insert(last.0, body);
						stmts.push(Stmt::Switch(*l, e, cases2));
					} else if Some(goto) == ctx.brk {
						// found a break from the parent element
						if last.0.is_some() {
							cases2.insert(last.0, Vec::new());
						}
						stmts.push(Stmt::Switch(*l, e, cases2));
						stmts.extend(body);
						stmts.push(Stmt::Break);
					} else {
						// found weirdo goto
						return decompile::UnexpectedJump { label: goto }.fail();
					}
				} else {
					// got to end of block without a break, so assume the last case is empty
					if last.0.is_some() {
						cases2.insert(last.0, Vec::new());
					}
					stmts.push(Stmt::Switch(*l, e, cases2));
					stmts.extend(body);
				}
			}

			FlatStmt::Goto(l) if Some(*l) == ctx.brk => stmts.push(Stmt::Break),
			FlatStmt::Goto(l) if Some(*l) == ctx.cont => stmts.push(Stmt::Continue),
			FlatStmt::Goto(l) => {
				let ok = match goto_allowed {
					GotoAllowed::Anywhere => true,
					GotoAllowed::Yes => ctx.pos == ctx.end,
					GotoAllowed::No => false,
				};
				if ok {
					return Ok((stmts, Some(*l)))
				} else {
					return decompile::UnexpectedJump { label: *l }.fail()
				}
			}

			FlatStmt::PushVar(l) => stmts.push(Stmt::PushVar(*l)),
			FlatStmt::PopVar(_) => {}, // not necessary for decompliation, I hope
		}
	}
	Ok((stmts, None))
}
