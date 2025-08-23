use std::collections::{HashMap, HashSet};
use std::cell::Cell;

use gospel::read::{Le as _, Reader};
use gospel::write::Le as _;
use snafu::{OptionExt as _, ResultExt as _};

use crate::scp::{Name, Value};

use super::super::{Op, Label, StackSlot, Binop, Unop};
use super::value::{self, string_value, value, write_string_value, ValueError};

#[derive(Debug, snafu::Snafu)]
pub enum ReadError {
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
	#[snafu(display("invalid pop count {value}"))]
	PopCount { value: u16 },
	#[snafu(display("missing label(s) {labels:x?}"))]
	MissingLabel { labels: Vec<usize> },
	#[snafu(display("invalid global id {id}"))]
	Global { id: usize },
	#[snafu(display("invalid function id {id}"))]
	Function { id: usize },
	#[snafu(display("invalid labels"))]
	Labels { source: crate::labels::LabelError },
}

pub fn read(
	f: &mut Reader,
	end: Option<usize>,
	func_id: usize,
	funcs: &[String],
	globals: &[String],
) -> Result<Vec<Op>, ReadError> {
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
					&& f.clone().u32().is_ok_and(|v| v >> 30 == 0 && v != 0)
				{
					break 'a Op::PrepareCallLocal(label(f.u32()?))
				}
				f.seek(p)?;
				if f.check_u32(0).is_ok() {
					break 'a Op::PushNull
				}
				f.seek(p)?;
				Op::Push(value(f).context(ValueSnafu)?)
			}
			1 => {
				let mut value = 0;
				loop {
					let v = f.u8()?;
					value += v as u16;
					if v != 255 {
						break;
					}
					f.check_u8(1)?;
				}
				if value % 4 != 0 {
					return PopCountSnafu { value }.fail();
				}
				Op::Pop(value / 4)
			}
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
			14 => Op::Jnz(label(f.u32()?)),
			15 => Op::Jz(label(f.u32()?)),
			op@16..=30 => Op::Binop(Binop::from_repr(op).unwrap()),
			op@31..=33 => Op::Unop(Unop::from_repr(op).unwrap()),
			34 => {
				let a = string_value(f).context(FuncNameSnafu)?;
				let b = string_value(f).context(FuncNameSnafu)?;
				let c = f.u8()?;
				Op::CallExtern(Name(a, b), c)
			}
			35 => {
				let a = string_value(f).context(FuncNameSnafu)?;
				let b = string_value(f).context(FuncNameSnafu)?;
				let c = f.u8()?;
				Op::CallTail(Name(a, b), c)
			}
			36 => {
				let a = f.u8()?;
				let b = f.u8()?;
				let c = f.u8()?;
				Op::CallSystem(a, b, c)
			}
			37 => Op::PrepareCallExtern(label(f.u32()?)),
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
	if labels.remove(&f.pos()) {
		ops2.push(Op::Label(Label(f.pos() as u32)));
	}
	if !labels.is_empty() {
		let mut labels = labels.into_iter().collect::<Vec<_>>();
		labels.sort();
		return MissingLabelSnafu { labels }.fail();
	}

	crate::labels::normalize(&mut ops2, 0).context(LabelsSnafu)?;
	Ok(ops2)
}

fn stack_slot(f: &mut Reader) -> Result<StackSlot, ReadError> {
	let value = f.i32()?;
	if value % 4 != 0 || value >= 0 {
		return StackSlotSnafu { value }.fail();
	}
	Ok(StackSlot((-value / 4) as u32))
}

#[derive(Debug, snafu::Snafu)]
#[snafu(module(write), context(suffix(false)))]
pub enum WriteError {
	#[snafu(display("missing label {label:?}"))]
	MissingLabel { label: Label },
	#[snafu(display("unknown global {name}"))]
	Global { name: String },
	#[snafu(display("unknown function {name}"))]
	Function { name: String },
}

pub fn write(code: &[Op], number: usize, w: &mut super::WCtx) -> Result<(), WriteError> {
	let labels = code.iter().filter_map(|op| match op {
		Op::Label(l) => Some((l, gospel::write::Label::new())),
		_ => None,
	}).collect::<HashMap<_, _>>();
	let f = &mut w.f.code;
	for op in code {
		match *op {
			Op::Label(label) => {
				f.place(labels[&label]);
			},
			Op::Push(ref v) => {
				f.u8(0);
				f.u8(4);
				match v {
					Value::Int(v) => value::write_int_value(f, *v),
					Value::Float(v) => value::write_float_value(f, *v),
					Value::String(v) => {
						let pos = w.strings.add(v, &mut w.f.code_strings);
						value::write_string_label(f, pos);
					}
				}
			}
			Op::Pop(n) => {
				let mut m = n * 4;
				while m > 255 {
					f.u8(1);
					f.u8(255);
					m -= 255;
				}
				f.u8(1);
				f.u8(m as u8);
			}
			Op::PushNull => {
				f.u8(0);
				f.u8(4);
				f.u32(0);
			}
			Op::GetVar(s) => {
				f.u8(2);
				f.i32(s.encode());
			}
			Op::GetRef(s) => {
				f.u8(3);
				f.i32(s.encode());
			}
			Op::PushRef(s) => {
				f.u8(4);
				f.i32(s.encode());
			}
			Op::SetVar(s) => {
				f.u8(5);
				f.i32(s.encode());
			}
			Op::SetRef(s) => {
				f.u8(6);
				f.i32(s.encode());
			}
			Op::GetGlobal(ref name) => {
				f.u8(7);
				f.u32(*w.global_names.get(&name).context(write::Global { name })? as u32);
			}
			Op::SetGlobal(ref name) => {
				f.u8(8);
				f.u32(*w.global_names.get(&name).context(write::Global { name })? as u32);
			}
			Op::GetTemp(v) => {
				f.u8(9);
				f.u8(v);
			}
			Op::SetTemp(v) => {
				f.u8(10);
				f.u8(v);
			}
			Op::Binop(o) => {
				f.u8(o as u8);
			}
			Op::Unop(o) => {
				f.u8(o as u8);
			}
			Op::Jnz(label) => {
				f.u8(14);
				f.label32(*labels.get(&label).context(write::MissingLabel { label })?);
			}
			Op::Jz(label) => {
				f.u8(15);
				f.label32(*labels.get(&label).context(write::MissingLabel { label })?);
			}
			Op::Goto(label) => {
				f.u8(11);
				f.label32(*labels.get(&label).context(write::MissingLabel { label })?);
			}
			Op::CallLocal(ref name) => {
				f.u8(12);
				f.u16(*w.function_names.get(&name).context(write::Function { name })? as u16);
			}
			Op::CallExtern(ref name, n) => {
				f.u8(34);
				// These are stored in a different form in called, and thus we don't cache them
				write_string_value(f, &mut w.f.code_strings, &name.0);
				write_string_value(f, &mut w.f.code_strings, &name.1);
				f.u8(n);
			}
			Op::CallTail(ref name, n) => {
				f.u8(35);
				// Same here.
				write_string_value(f, &mut w.f.code_strings, &name.0);
				write_string_value(f, &mut w.f.code_strings, &name.1);
				f.u8(n);
			}
			Op::CallSystem(a, b, n) => {
				f.u8(36);
				f.u8(a);
				f.u8(b);
				f.u8(n);
			}
			Op::PrepareCallLocal(label) => {
				f.u8(0);
				f.u8(4);
				f.u32(number as u32);
				f.u8(0);
				f.u8(4);
				f.label32(*labels.get(&label).context(write::MissingLabel { label })?);
			}
			Op::PrepareCallExtern(l) => {
				f.u8(37);
				f.label32(*labels.get(&l).context(write::MissingLabel { label: l })?);
			}
			Op::Return => {
				f.u8(13);
			}
			Op::Line(n) => {
				f.u8(38);
				f.u16(n);
			}
			Op::Debug(n) => {
				f.u8(39);
				f.u8(n);
			}
		}
	}
	Ok(())
}

impl StackSlot {
	fn encode(&self) -> i32 {
		-(self.0 as i32 * 4)
	}
}
