use std::collections::{HashMap, HashSet};
use std::cell::Cell;

use gospel::read::{Le as _, Reader};
use snafu::{OptionExt as _, ResultExt};

use super::super::{Op, Label, StackSlot, Binop, Unop};
use super::value::{ValueError, value, string_value};

#[derive(Debug, snafu::Snafu)]
pub enum CodeError {
	#[snafu(display("invalid read (at {location})"), context(false))]
	Read {
		source: gospel::read::Error,
		#[snafu(implicit)]
		location: snafu::Location,
	},
	#[snafu(display("could not read value"))]
	Value { source: ValueError },
	#[snafu(display("could not called function name"))]
	FuncName { source: ValueError },
	#[snafu(display("invalid opcode {op} at {pos}"))]
	Op { op: u8, pos: usize },
	#[snafu(display("invalid stack slot {value}"))]
	StackSlot { value: i32 },
	#[snafu(display("missing label(s) {labels:x?}"))]
	MissingLabel { labels: Vec<usize> },
	#[snafu(display("invalid global id {id}"))]
	Global { id: usize },
	#[snafu(display("invalid function id {id}"))]
	Function { id: usize },
}

pub fn parse(
	f: &mut Reader,
	end: Option<usize>,
	func_id: usize,
	funcs: &[String],
	globals: &[String],
) -> Result<Vec<Op>, CodeError> {
	let extent = Cell::new(f.pos());
	let mut labels = HashSet::new();
	let mut label = |v: u32| -> Label {
		let l = Label(v);
		let v = v as usize;
		if v > extent.get() {
			extent.set(v);
		}
		labels.insert(v);
		l
	};

	let mut ops = Vec::new();
	loop {
		let pos = f.pos();
		let op = match f.u8()? {
			0 => 'a: {
				f.check_u8(4)?; // number of bytes to push, but it always pushes one value
				let p = f.pos();
				// I don't like this check.
				if f.check_u32(func_id as u32).is_ok()
					&& f.check_u8(0).is_ok()
					&& f.check_u8(4).is_ok()
					&& f.clone().u32().is_ok_and(|v| v >> 30 == 0)
				{
					break 'a Op::PushForCallLocal(label(f.u32()?))
				}
				f.seek(p)?;
				if f.check_u32(0).is_ok() {
					break 'a Op::PushNull
				}
				f.seek(p)?;
				Op::Push(value(f).context(ValueSnafu)?)
			}
			1 => Op::Pop(f.u8()?),
			2 => Op::GetVar(stack_slot(f)?),
			3 => Op::GetRef(stack_slot(f)?),
			4 => Op::PushRef(stack_slot(f)?),
			5 => Op::SetVar(stack_slot(f)?),
			6 => Op::SetRef(stack_slot(f)?),
			7 => {
				let id = f.u32()? as usize;
				Op::GetGlobal(globals.get(id).context(GlobalSnafu { id })?.clone())
			}
			8 => {
				let id = f.u32()? as usize;
				Op::SetGlobal(globals.get(id).context(GlobalSnafu { id })?.clone())
			}
			9 => Op::GetTemp(f.u8()?),
			10 => Op::SetTemp(f.u8()?),
			11 => Op::Goto(label(f.u32()?)),
			12 => {
				let id = f.u16()? as usize;
				Op::CallLocal(funcs.get(id).context(FunctionSnafu { id })?.clone())
			}
			13 => Op::Return,
			14 => Op::If2(label(f.u32()?)),
			15 => Op::If(label(f.u32()?)),
			op@16..=30 => Op::Binop(Binop::from_repr(op).unwrap()),
			op@31..=33 => Op::Unop(Unop::from_repr(op).unwrap()),
			34 => {
				let a = string_value(f).context(FuncNameSnafu)?;
				let b = string_value(f).context(FuncNameSnafu)?;
				let c = f.u8()?;
				let name = format!("{a}.{b}");
				Op::CallExtern(name, c)
			}
			35 => {
				let a = string_value(f).context(FuncNameSnafu)?;
				let b = string_value(f).context(FuncNameSnafu)?;
				let c = f.u8()?;
				let name = if a.is_empty() { b } else { format!("{a}.{b}") };
				Op::CallTail(name, c)
			}
			36 => {
				let a = f.u8()?;
				let b = f.u8()?;
				let c = f.u8()?;
				Op::CallSystem(a, b, c)
			}
			37 => Op::PushForCallExtern(label(f.u32()?)),
			38 => Op::Line(f.u16()?),
			39 => Op::Debug(f.u8()?),
			op@40.. => return OpSnafu { op, pos }.fail(),
		};
		tracing::trace!("{pos} {op:?}");
		let is_end = if let Some(end) = end {
			f.pos() >= end
		} else {
			op == Op::Return && pos >= extent.get()
		};
		ops.push((pos, op));
		if is_end {
			break;
		}
	}

	let mut ops2 = Vec::new();
	for (l, op) in ops {
		if labels.remove(&l) {
			let l = Label(l as u32);
			ops2.push(Op::Label(l));
		}
		ops2.push(op);
	}
	if !labels.is_empty() {
		let mut labels = labels.into_iter().collect::<Vec<_>>();
		labels.sort();
		return MissingLabelSnafu { labels }.fail();
	}

	reorder_labels(&mut ops2);
	Ok(ops2)
}

fn stack_slot(f: &mut Reader) -> Result<StackSlot, CodeError> {
	let value = f.i32()?;
	if value % 4 != 0 || value >= 0 {
		return StackSlotSnafu { value }.fail();
	}
	Ok(StackSlot((-value / 4) as u32))
}

fn reorder_labels(ops: &mut Vec<Op>) {
	let mut labels = HashMap::new();
	for op in ops.iter() {
		if let Op::Label(l) = op {
			labels.insert(*l, Label(labels.len() as u32));
		}
	}
	for op in ops {
		match op {
			Op::Label(l)
			| Op::Goto(l)
			| Op::If(l)
			| Op::If2(l)
			| Op::PushForCallLocal(l)
			| Op::PushForCallExtern(l) => {
				*l = labels[l];
			}
			_ => continue,
		}
	}
}
