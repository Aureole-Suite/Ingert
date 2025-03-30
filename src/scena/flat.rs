mod ctx;
mod line;

use std::collections::HashMap;

use crate::scp::{Binop, Label, Op, StackSlot, Value};
use super::{FlatStmt, Name};
use ctx::{Ctx, StackVal};
use snafu::OptionExt as _;

type Expr = super::Expr<super::FlatVar>;
type Place = super::Place<super::FlatVar>;

impl From<Expr> for StackVal {
	fn from(e: Expr) -> Self {
		Self::Expr(e)
	}
}

#[derive(Debug, snafu::Snafu)]
#[snafu(module(error), context(suffix(false)))]
pub enum DecompileError {
	#[snafu(display("attempted to pop from empty stack"))]
	PopEmpty,
	#[snafu(display("attempted to pop a return address"))]
	PopRetAddr,
	#[snafu(display("attempted to read from stack (slot: {slot}, len: {len})"))]
	ReadStack { slot: u32, len: u32 },
	#[snafu(display("stack not empty when finished statement"))]
	NonemptyStack,
	#[snafu(display("could not parse function call"))]
	BadCall,
	#[snafu(display("could not parse switch statement"))]
	BadSwitch,
	#[snafu(display("could not parse tailcall statement"))]
	BadTailcall,
	#[snafu(display("unexpected op"))]
	UnexpectedOp,
	#[snafu(display("could not normalize labels"), context(false))]
	Labels { source: crate::labels::LabelError },
}

pub fn decompile(code: &[Op]) -> Result<Vec<FlatStmt>, DecompileError> {
	let mut ctx = Ctx::new(code);
	while let Some(op) = ctx.next() {
		match *op {
			Op::Label(l) => {
				ctx.label(l)?;
			}
			Op::Line(l) => {
				ctx.line(l)?;
			}
			Op::Push(ref v) => {
				ctx.push(Expr::Value(None, v.clone()))?;
			}
			Op::SetTemp(0) if matches!(ctx.peek(), [Op::GetTemp(0) | Op::Goto(_), ..]) => {
				let v = ctx.pop()?;
				let mut cases = Vec::new();
				let default = loop {
					match ctx.next().context(error::BadSwitch)? {
						Op::GetTemp(0) => {
							if let [
								Some(Op::Push(Value::Int(n))),
								Some(Op::Binop(Binop::Eq)),
								Some(Op::Jnz(l)),
							] = std::array::from_fn(|_| ctx.next()) {
								cases.push((*n, *l));
							} else {
								return error::BadSwitch.fail();
							}
						}
						Op::Goto(l) => break *l,
						_ => return error::BadSwitch.fail(),
					}
				};
				ctx.stmt(FlatStmt::Switch(None, v, cases, default))?;
			}
			Op::GetVar(s) => {
				let p = Place::Var(ctx.var(s)?);
				ctx.push(Expr::Var(None, p))?;
			}
			Op::GetRef(s) => {
				let p = Place::Deref(ctx.var(s)?);
				ctx.push(Expr::Var(None, p))?;
			}
			Op::GetGlobal(ref name) => {
				let p = Place::Global(name.clone());
				ctx.push(Expr::Var(None, p))?;
			}
			Op::PushRef(s) => {
				let n = ctx.var(s)?;
				ctx.push(Expr::Ref(None, n))?;
			}
			Op::SetVar(s) => {
				let v = ctx.pop()?;
				let p = Place::Var(ctx.var(s)?);
				ctx.stmt(FlatStmt::Set(None, p, v))?;
			}
			Op::SetRef(s) => {
				let v = ctx.pop()?;
				let p = Place::Deref(ctx.var(s)?);
				ctx.stmt(FlatStmt::Set(None, p, v))?;
			}
			Op::SetGlobal(ref name) => {
				let v = ctx.pop()?;
				let p = Place::Global(name.clone());
				ctx.stmt(FlatStmt::Set(None, p, v))?;
			}
			Op::Binop(o) => {
				let b = ctx.pop()?;
				let a = ctx.pop()?;
				ctx.push(Expr::Binop(None, o, Box::new(a), Box::new(b)))?;
			}
			Op::Unop(o) => {
				let a = ctx.pop()?;
				ctx.push(Expr::Unop(None, o, Box::new(a)))?;
			}
			Op::Jz(l) => {
				let cond = ctx.pop()?;
				ctx.stmt(FlatStmt::If(None, cond, l))?;
			}
			Op::Goto(l) => {
				ctx.stmt(FlatStmt::Goto(l, 0))?;
			}
			Op::Pop(n) if let [Op::Goto(l), ..] = ctx.peek() => {
				ctx.next();
				ctx.stmt(FlatStmt::Goto(*l, n as usize))?;
			}

			Op::Pop(n) if let [Op::CallTail(name, 0), ..] = ctx.peek() => {
				ctx.next();
				ctx.stmt(FlatStmt::Tailcall(None, name.clone(), vec![], n as usize))?;
			}
			Op::SetTemp(1) => {
				let mut args = vec![ctx.pop()?];
				let pop = loop {
					match ctx.next().context(error::BadTailcall)? {
						Op::SetTemp(n) if *n == args.len() as u8 + 1 => {
							args.push(ctx.pop()?);
						}
						Op::Pop(n) => break *n,
						_ => return error::BadTailcall.fail(),
					}
				};
				for i in (1..=args.len()).rev() {
					let a = ctx.next();
					if a != Some(&Op::GetTemp(i as u8)) {
						return error::BadTailcall.fail();
					}
				}
				let Some(Op::CallTail(name, n)) = ctx.next() else {
					return error::BadTailcall.fail();
				};
				if *n != args.len() as u8 {
					return error::BadTailcall.fail();
				}
				ctx.stmt(FlatStmt::Tailcall(None, name.clone(), args, pop as usize))?;
			}

			Op::PushNull if matches!(ctx.peek(), [Op::SetTemp(0), ..]) => {
				ctx.next();
				handle_return(&mut ctx, None)?;
			}
			Op::SetTemp(0) => {
				let v = ctx.pop()?;
				handle_return(&mut ctx, Some(v))?;
			}
			Op::Return => {
				return error::UnexpectedOp.fail();
			}

			Op::PushNull => {
				ctx.stmt(FlatStmt::PushVar(None))?;
			}
			Op::Pop(n) => {
				ctx.stmt(FlatStmt::PopVar(n as usize))?;
			}

			Op::CallLocal(ref name) => {
				make_call(&mut ctx, 1, Name::local(name.clone()))?;
			}
			Op::CallExtern(ref name, n) => {
				let nargs = make_call(&mut ctx, 4, name.clone())?;
				if nargs != n as usize {
					return error::BadCall.fail();
				}
			}
			Op::CallSystem(a, b, n) => {
				let mut args = Vec::new();
				for _ in 0..n {
					args.push(ctx.pop()?);
				}
				if n != 0 && ctx.next() != Some(&Op::Pop(n)) {
					return error::BadCall.fail();
				}
				ctx.push(Expr::Syscall(None, a, b, args))?;
				end_call(&mut ctx)?;
			}
			Op::PrepareCallLocal(l) => {
				prepare_call(&mut ctx, 1, l)?;
			}
			Op::PrepareCallExtern(l) => {
				prepare_call(&mut ctx, 4, l)?;
			}
			Op::Debug(n) => {
				let mut args = Vec::new();
				for _ in 0..n {
					args.push(ctx.pop()?);
				}
				ctx.stmt(FlatStmt::Debug(None, args))?;
			}
			Op::CallTail(_, _) => return error::UnexpectedOp.fail(),
			Op::Jnz(_) | Op::GetTemp(_) | Op::SetTemp(_) => return error::UnexpectedOp.fail(),
		}
	}
	let mut out = ctx.finish()?;
	crate::labels::normalize(&mut out, 0)?;
	Ok(out)
}

fn handle_return(ctx: &mut Ctx, val: Option<Expr>) -> Result<(), DecompileError> {
	let pop = if let [Op::Pop(n), ..] = ctx.peek() {
		ctx.next();
		*n as usize
	} else {
		0
	};
	if let [Op::Return, ..] = ctx.peek() {
		ctx.next();
		ctx.stmt(FlatStmt::Return(None, val, pop))?;
		Ok(())
	} else {
		error::UnexpectedOp.fail()
	}
}

fn prepare_call(ctx: &mut Ctx, misc: u32, label: Label) -> Result<(), DecompileError> {
	for _ in 0..misc {
		ctx.push(StackVal::RetMisc)?;
	}
	ctx.push(StackVal::RetAddr(label))?;
	Ok(())
}

fn make_call(ctx: &mut Ctx, misc: u32, name: Name) -> Result<usize, DecompileError> {
	let mut args = Vec::new();
	let label = loop {
		match ctx.pop_any()? {
			StackVal::Expr(v) => args.push(v),
			StackVal::RetAddr(l) => break l,
			_ => return error::BadCall.fail(),
		}
	};
	for _ in 0..misc {
		match ctx.pop_any()? {
			StackVal::RetMisc => {}
			_ => return error::BadCall.fail(),
		}
	}
	let nargs = args.len();
	match ctx.peek() {
		[Op::Label(l), Op::GetTemp(0), ..] if *l == label => {
			ctx.next();
		}
		[Op::Label(l), ..] if *l == label => {}
		_ => return error::BadCall.fail()
	}
	ctx.push(Expr::Call(None, name, args))?;
	end_call(ctx)?;
	Ok(nargs)
}

fn end_call(ctx: &mut Ctx) -> Result<(), DecompileError> {
	if matches!(ctx.peek(), [Op::GetTemp(0), ..]) {
		ctx.next();
	} else {
		let e = ctx.pop()?;
		ctx.stmt(FlatStmt::Expr(e))?;
	}
	Ok(())
}

#[derive(Debug, snafu::Snafu)]
pub enum CompileError {
	#[snafu(display("could not normalize labels"), context(false))]
	Labels { source: crate::labels::LabelError },
}

struct OutCtx {
	out: Vec<Op>,
	label: u32,
}

impl OutCtx {
	fn line(&mut self, l: Option<u16>) {
		if let Some(l) = l {
			self.out.push(Op::Line(l));
		}
	}

	fn label(&mut self) -> Label {
		let l = self.label;
		self.label += 1;
		Label(l)
	}
}

pub fn compile(stmts: &[FlatStmt]) -> Result<Vec<Op>, CompileError> {
	let mut ctx = OutCtx {
		out: Vec::new(),
		label: crate::labels::max_label(stmts),
	};
	let mut backrefs = HashMap::new();
	for stmt in stmts {
		match stmt {
			FlatStmt::Label(l) => {
				ctx.out.push(Op::Label(*l));
			}
			FlatStmt::Expr(expr) => {
				compile_expr(&mut ctx, expr, 0);
				if matches!(ctx.out.last(), Some(Op::GetTemp(0))) {
					ctx.out.pop();
				} else {
					// This doesn't happen in any known script, but better than returning an error
					ctx.out.push(Op::Pop(1));
				}
			},
			FlatStmt::Set(l, place, expr) => {
				ctx.line(*l);
				compile_expr(&mut ctx, expr, 0);
				match place {
					Place::Var(n) => ctx.out.push(Op::SetVar(StackSlot(n.0))),
					Place::Deref(n) => ctx.out.push(Op::SetRef(StackSlot(n.0))),
					Place::Global(n) => ctx.out.push(Op::SetGlobal(n.clone())),
				}
			}
			FlatStmt::Return(l, expr, pop) => {
				ctx.line(*l);
				if let Some(expr) = expr {
					compile_expr(&mut ctx, expr, 0);
				} else {
					ctx.out.push(Op::PushNull);
				}
				ctx.out.push(Op::SetTemp(0));
				if *pop != 0 {
					ctx.out.push(Op::Pop(*pop as u8));
				}
				ctx.out.push(Op::Return);
			}
			FlatStmt::If(l, expr, label) => {
				if let Some(&Op::Label(prelabel)) = ctx.out.last() && l.is_some() {
					ctx.line(*l);
					let newlabel = ctx.label();
					ctx.out.push(Op::Label(newlabel));
					backrefs.insert(prelabel, newlabel);
				} else {
					ctx.line(*l);
				}
				compile_expr(&mut ctx, expr, 0);
				ctx.out.push(Op::Jz(*label));
			}
			FlatStmt::Goto(label, pop) => {
				if *pop != 0 {
					ctx.out.push(Op::Pop(*pop as u8));
				}
				ctx.out.push(Op::Goto(*backrefs.get(label).unwrap_or(label)));
			}
			FlatStmt::Switch(l, expr, items, label) => {
				ctx.line(*l);
				compile_expr(&mut ctx, expr, 0);
				ctx.out.push(Op::SetTemp(0));
				for (n, l) in items {
					ctx.out.push(Op::GetTemp(0));
					ctx.out.push(Op::Push(Value::Int(*n)));
					ctx.out.push(Op::Binop(Binop::Eq));
					ctx.out.push(Op::Jnz(*l));
				}
				ctx.out.push(Op::Goto(*label));
			}
			FlatStmt::PushVar(l) => {
				ctx.line(*l);
				ctx.out.push(Op::PushNull);
			}
			FlatStmt::PopVar(pop) => {
				ctx.out.push(Op::Pop(*pop as u8));
			}
			FlatStmt::Debug(l, exprs) => {
				ctx.line(*l);
				for (expr, i) in exprs.iter().rev().zip(0..) {
					compile_expr(&mut ctx, expr, i);
				}
				ctx.out.push(Op::Debug(exprs.len() as u8));
			}
			FlatStmt::Tailcall(l, name, exprs, pop) => {
				ctx.line(*l);
				for (expr, i) in exprs.iter().rev().zip(0..) {
					compile_expr(&mut ctx, expr, i);
				}
				let n = exprs.len() as u8;
				for i in 1..=n {
					ctx.out.push(Op::SetTemp(i));
				}
				if *pop != 0 {
					ctx.out.push(Op::Pop(*pop as u8));
				}
				for i in (1..=n).rev() {
					ctx.out.push(Op::GetTemp(i));
				}
				ctx.out.push(Op::CallTail(name.clone(), n));
			}
		}
	}
	crate::labels::normalize(&mut ctx.out, 0)?;
	Ok(ctx.out)
}

fn compile_expr(ctx: &mut OutCtx, expr: &Expr, depth: u32) {
	match expr {
		Expr::Value(l, value) => {
			ctx.line(*l);
			ctx.out.push(Op::Push(value.clone()));
		}
		Expr::Var(l, place) => {
			ctx.line(*l);
			match place {
				Place::Var(n) => ctx.out.push(Op::GetVar(StackSlot(n.0 + depth))),
				Place::Deref(n) => ctx.out.push(Op::GetRef(StackSlot(n.0 + depth))),
				Place::Global(n) => ctx.out.push(Op::GetGlobal(n.clone())),
			}
		}
		Expr::Ref(l, n) => {
			ctx.line(*l);
			ctx.out.push(Op::PushRef(StackSlot(n.0 + depth)));
		}
		Expr::Call(l, name, exprs) => {
			ctx.line(*l);
			let l = ctx.label();
			if let Some(name) = name.as_local() {
				ctx.out.push(Op::PrepareCallLocal(l));
				for (expr, i) in exprs.iter().rev().zip(2..) {
					compile_expr(ctx, expr, depth + i);
				}
				ctx.out.push(Op::CallLocal(name.clone()));
			} else {
				ctx.out.push(Op::PrepareCallExtern(l));
				for (expr, i) in exprs.iter().rev().zip(5..) {
					compile_expr(ctx, expr, depth + i);
				}
				ctx.out.push(Op::CallExtern(name.clone(), exprs.len() as u8));
			}
			ctx.out.push(Op::Label(l));
			ctx.out.push(Op::GetTemp(0));
		}
		Expr::Syscall(l, a, b, exprs) => {
			ctx.line(*l);
			for (expr, i) in exprs.iter().rev().zip(0..) {
				compile_expr(ctx, expr, depth + i);
			}
			ctx.out.push(Op::CallSystem(*a, *b, exprs.len() as u8));
			if !exprs.is_empty() {
				ctx.out.push(Op::Pop(exprs.len() as u8));
			}
			ctx.out.push(Op::GetTemp(0));
		}
		Expr::Unop(l, unop, expr) => {
			ctx.line(*l);
			compile_expr(ctx, expr, depth);
			ctx.out.push(Op::Unop(*unop));
		}
		Expr::Binop(l, binop, a, b) => {
			ctx.line(*l);
			compile_expr(ctx, a, depth);
			compile_expr(ctx, b, depth + 1);
			ctx.out.push(Op::Binop(*binop));
		}
	}
}
