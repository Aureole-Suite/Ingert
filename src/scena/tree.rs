use indexmap::IndexMap;
use snafu::{OptionExt as _, ResultExt as _};

use super::{Expr, FlatStmt, Label, Line, Stmt};

#[derive(Debug, snafu::Snafu)]
#[snafu(module(decompile), context(suffix(false)))]
pub enum DecompileError {
	#[snafu(display("unknown label: {label:?} ({location})"))]
	MissingLabel {
		label: Label,
		#[snafu(implicit)]
		location: snafu::Location,
	},
	#[snafu(display("label out of bounds: {label:?} at {pos} should be in {start}..={end} ({location})"))]
	WrongLabel {
		label: Label,
		pos: usize,
		start: usize,
		end: usize,
		#[snafu(implicit)]
		location: snafu::Location,
	},
	#[snafu(display("unsorted switch"))]
	UnsortedSwitch,
	#[snafu(display("unexpected jump to {label:?}"))]
	UnexpectedJump { label: Label },

	#[snafu(display("while parsing {what} at {start}..{end}"))]
	Block {
		what: &'static str,
		start: usize,
		end: usize,
		#[snafu(source(from(DecompileError, Box::new)))]
		source: Box<DecompileError>,
	},
}

pub fn decompile(stmts: &[FlatStmt]) -> Result<Vec<Stmt>, DecompileError> {
	let mut labels = IndexMap::new();
	for (i, stmt) in stmts.iter().enumerate() {
		if let FlatStmt::Label(l) = stmt {
			labels.insert(*l, i);
		}
	}

	let (body, _) = Ctx::new(&Gctx { stmts, labels }).block("body", GotoAllowed::No)?;
	Ok(body)
}

struct Gctx<'a> {
	stmts: &'a [FlatStmt],
	labels: IndexMap<Label, usize>,
}

impl Gctx<'_> {
	#[track_caller]
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

	#[track_caller]
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

	#[track_caller]
	fn sub(&mut self, label: Label) -> Result<Ctx<'a>, DecompileError> {
		let pos = self.lookup(label)?;
		let sub = Self {
			end: pos,
			..*self
		};
		self.pos = pos;
		Ok(sub)
	}

	#[track_caller]
	fn goto_before(&self, label: Label) -> Result<Option<Label>, DecompileError> {
		let pos = self.lookup(label)?;
		if pos > 0 && let FlatStmt::Goto(cont) = self.gctx.stmts[pos - 1] {
			Ok(Some(cont))
		} else {
			Ok(None)
		}
	}

	fn block(&mut self, what: &'static str, goto_allowed: GotoAllowed) -> Result<(Vec<Stmt>, Option<Label>), DecompileError> {
		let start = self.pos;
		let end = self.end;
		block(self, goto_allowed).context(decompile::Block { what, start, end })
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
				let start = ctx.pos - 1;
				let end = ctx.end;
				parse_if(&mut stmts, ctx, *l, e.clone(), *label)
					.context(decompile::Block { what: "if", start, end })?
			},
			FlatStmt::Switch(l, e, cases, default) => {
				let start = ctx.pos - 1;
				let end = ctx.end;
				parse_switch(&mut stmts, ctx, *l, e.clone(), cases, *default)
					.context(decompile::Block { what: "switch", start, end })?
			},

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

fn parse_if(stmts: &mut Vec<Stmt>, ctx: &mut Ctx, l: Line, e: Expr, label: Label) -> Result<(), DecompileError> {
	if let Some(cont) = ctx.goto_before(label)?
			&& ctx.pos >= 2
			&& ctx.gctx.lookup(cont)? == ctx.pos - 2 {
		let mut sub = ctx.sub(label)?;
		sub.brk = Some(label);
		sub.cont = Some(cont);
		let (mut body, _) = sub.block("while body", GotoAllowed::No)?;
		assert_eq!(body.pop(), Some(Stmt::Continue));
		stmts.push(Stmt::While(l, e, body));
		return Ok(())
	}

	let (body, goto) = ctx.sub(label)?.block("if body", GotoAllowed::Yes)?;

	if let Some(goto) = goto {
		let (no, _) = ctx.sub(goto)?.block("else body", GotoAllowed::No)?;
		stmts.push(Stmt::If(l, e, body, Some(no)));
	} else {
		stmts.push(Stmt::If(l, e, body, None));
	};
	Ok(())
}

fn parse_switch(stmts: &mut Vec<Stmt>, ctx: &mut Ctx, l: Line, e: Expr, cases: &[(i32, Label)], default: Label) -> Result<(), DecompileError> {
	let pos = cases.iter().map(|(_, l)| ctx.lookup(*l)).collect::<Result<Vec<_>, _>>()?;
	snafu::ensure!(pos.is_sorted(), decompile::UnsortedSwitch);
	let default_pos = ctx.lookup(default)?;
	let default_index = pos.partition_point(|&p| p < default_pos);
	let mut cases = cases.iter().map(|(k, l)| (Some(*k), *l)).collect::<Vec<_>>();
	cases.insert(default_index, (None, default));

	let mut brk = None;
	let mut brk_pos = 0;
	for (_, l) in &cases {
		if let Some(goto) = ctx.goto_before(*l)?
			&& let goto_pos = ctx.lookup(goto)?
			&& goto_pos >= ctx.lookup(*l)?
			&& goto_pos > brk_pos
		{
			brk = Some(goto);
			brk_pos = goto_pos;
		}
	}

	let mut cases2 = IndexMap::with_capacity(cases.len());
	let ends = cases.iter().skip(1).map(|i| i.1).chain(brk);
	for (&(key, target), end) in std::iter::zip(&cases, ends) {
		let target = ctx.gctx.lookup(target)?;
		assert_eq!(ctx.pos, target);
		let mut sub = ctx.sub(end)?;
		sub.brk = brk;
		let (body, _) = sub.block("switch body", GotoAllowed::No)?;
		cases2.insert(key, body);
	}

	if brk.is_some() {
		// we know where the break is, so no need for that bullshit
		if cases2.last().is_some_and(|(k, v)| k.is_none() && v.is_empty()) {
			cases2.pop();
		}
		stmts.push(Stmt::Switch(l, e, cases2));
		return Ok(());
	}

	let last = cases.last().unwrap();
	assert_eq!(ctx.gctx.lookup(last.1)?, ctx.pos);
	let prev_brk = ctx.brk.take();
	let (mut body, goto) = ctx.block("switch last body", GotoAllowed::Anywhere)?;
	ctx.brk = prev_brk;
	if let Some(goto) = goto {
		if ctx.gctx.lookup(goto)? == ctx.pos {
			// finally found a break, but it's useless
			body.push(Stmt::Break);
			cases2.insert(last.0, body);
			stmts.push(Stmt::Switch(l, e, cases2));
			return Ok(());
		} else {
			// this goto probably belongs to a parent scope. Rewind and let the parent handle it.
			ctx.pos -= 1;
		}
	}
	// got to end of block without a break, so assume the last case is empty
	if last.0.is_some() {
		cases2.insert(last.0, Vec::new());
	}
	stmts.push(Stmt::Switch(l, e, cases2));
	stmts.extend(body);

	Ok(())
}

#[derive(Debug, snafu::Snafu)]
#[snafu(module(compile), context(suffix(false)))]
pub enum CompileError {
	#[snafu(display("break outside while/switch"))]
	BadBreak,
	#[snafu(display("continue outside while"))]
	BadContinue,
	#[snafu(display("could not normalize labels"), context(false))]
	Labels { source: crate::labels::LabelError },
}

struct OutCtx {
	out: Vec<FlatStmt>,
	label: u32,
}

impl OutCtx {
	fn label(&mut self) -> Label {
		let l = self.label;
		self.label += 1;
		Label(l)
	}

	fn push(&mut self, stmt: FlatStmt) {
		self.out.push(stmt);
	}
}

pub fn compile(stmts: &[Stmt], nargs: usize) -> Result<Vec<FlatStmt>, CompileError> {
	let mut ctx = OutCtx { out: Vec::new(), label: 0 };
	compile_block(&mut ctx, stmts, None, None, nargs)?;
	crate::labels::normalize(&mut ctx.out, 0)?;
	Ok(ctx.out)
}

fn compile_block(
	ctx: &mut OutCtx,
	stmts: &[Stmt],
	brk: Option<(usize, Label)>,
	cont: Option<(usize, Label)>,
	mut depth: usize,
) -> Result<(), CompileError> {
	let depth0 = depth;
	for stmt in stmts {
		match stmt {
			Stmt::Expr(expr) => ctx.push(FlatStmt::Expr(expr.clone())),
			Stmt::Set(l, place, expr) => ctx.push(FlatStmt::Set(*l, place.clone(), expr.clone())),
			Stmt::Return(l, expr) => ctx.push(FlatStmt::Return(*l, expr.clone(), std::mem::take(&mut depth))),
			Stmt::Debug(l, exprs) => ctx.push(FlatStmt::Debug(*l, exprs.clone())),
			Stmt::Tailcall(l, name, exprs) => ctx.push(FlatStmt::Tailcall(*l, name.clone(), exprs.clone(), std::mem::take(&mut depth))),
			Stmt::If(l, expr, stmts, stmts1) => {
				let label = ctx.label();
				ctx.push(FlatStmt::If(*l, expr.clone(), label));
				compile_block(ctx, stmts, brk, cont, depth)?;
				if let Some(stmts1) = stmts1 {
					let label2 = ctx.label();
					ctx.push(FlatStmt::Goto(label2));
					ctx.push(FlatStmt::Label(label));
					compile_block(ctx, stmts1, brk, cont, depth)?;
					ctx.push(FlatStmt::Label(label2));
				} else {
					ctx.push(FlatStmt::Label(label));
				}
			}
			Stmt::While(l, expr, stmts) => {
				let brk = (depth, ctx.label());
				let cont = (depth, ctx.label());
				ctx.push(FlatStmt::Label(cont.1));
				ctx.push(FlatStmt::If(*l, expr.clone(), brk.1));
				compile_block(ctx, stmts, Some(brk), Some(cont), depth)?;
				ctx.push(FlatStmt::Goto(cont.1));
				ctx.push(FlatStmt::Label(brk.1));
			}
			Stmt::Switch(l, expr, cases) => {
				let brk = (depth, ctx.label());
				let labels = cases.iter().map(|(k, _)| (*k, ctx.label())).collect::<IndexMap<_, _>>();
				ctx.push(FlatStmt::Switch(
					*l,
					expr.clone(),
					labels.iter().filter_map(|(&k, &v)| Some((k?, v))).collect(),
					labels.get(&None).copied().unwrap_or(brk.1),
				));
				for (k, l) in labels {
					ctx.push(FlatStmt::Label(l));
					compile_block(ctx, &cases[&k], Some(brk), cont, depth)?;
				}
				ctx.push(FlatStmt::Label(brk.1));
			}
			Stmt::Break => {
				if let Some(brk) = brk {
					if depth > brk.0 {
						ctx.push(FlatStmt::PopVar(depth - brk.0));
					}
					depth = brk.0;
					ctx.push(FlatStmt::Goto(brk.1));
				} else {
					return compile::BadBreak.fail()
				}
			}
			Stmt::Continue => {
				if let Some(cont) = cont {
					if depth > cont.0 {
						ctx.push(FlatStmt::PopVar(depth - cont.0));
					}
					depth = cont.0;
					ctx.push(FlatStmt::Goto(cont.1));
				} else {
					return compile::BadContinue.fail()
				}
			}
			Stmt::PushVar(l) => {
				ctx.push(FlatStmt::PushVar(*l));
				depth += 1;
			}
		}
	}
	if depth > depth0 {
		ctx.push(FlatStmt::PopVar(depth - depth0));
	}
	Ok(())
}
