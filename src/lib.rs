use std::{cell::Cell, collections::HashSet, num::NonZeroU8};

use gospel::read::{Le as _, Reader};
use snafu::ResultExt as _;

#[extend::ext]
impl Reader<'_> {
	fn vec3(&mut self) -> Result<glam::Vec3, gospel::read::Error> {
		Ok(glam::Vec3::new(self.f32()?, self.f32()?, self.f32()?))
	}
}

#[derive(Debug, snafu::Snafu)]
#[snafu(module(scp), context(suffix(false)))]
pub enum ScpError {
	#[snafu(display("invalid read (at {location})"), context(false))]
	Read {
		source: gospel::read::Error,
		#[snafu(implicit)]
		location: snafu::Location,
	},
	#[snafu(display("invalid string: {lossy_string:?}"))]
	Utf8 {
		source: std::str::Utf8Error,
		lossy_string: String,
	},
	#[snafu(display("invalid opcode {op:02X} at {pos}"))]
	Op { op: u8, pos: usize },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
	pub start: Label,
	pub count1: u8,
	pub count2: u8,
	pub a1: Vec<Value>,
	pub a2: Vec<Value>,
	pub a4: Vec<(i32, u16, Vec<TaggedValue>)>,
	pub a5: u32,
	pub a6: Value,
}

fn multi<T>(
	f: &mut Reader,
	n: usize,
	g: impl Fn(&mut Reader) -> Result<T, ScpError>,
) -> Result<Vec<T>, ScpError> {
	(0..n).map(|_| g(f)).collect()
}

fn parse_functions(f: &mut Reader<'_>, n_entries: u32) -> Result<Vec<Function>, ScpError> {
	let mut entries = Vec::with_capacity(n_entries as usize);
	for _ in 0..n_entries {
		let start = Label(f.u32()?);
		let count0 = f.u8()? as usize;
		let count1 = f.u8()?;
		let count2 = f.u8()?;
		let count3 = f.u8()? as usize;
		let a1 = f.u32()? as usize;
		let a2 = f.u32()? as usize;
		let a3 = f.u32()? as usize;
		let a4 = f.u32()? as usize;
		let a5 = f.u32()?;
		let a6 = value(f)?;

		let a1 = multi(&mut f.at(a1)?, count3, value)?;
		let a2 = multi(&mut f.at(a2)?, count0, value)?;
		let a4 = multi(&mut f.at(a4)?, a3, |f| {
			let x = f.i32()?;
			let y = f.u16()?;
			let z = f.u16()? as usize;
			let w = f.u32()? as usize;
			let z = multi(&mut f.at(w)?, z, tagged_value)?;
			Ok((x, y, z))
		})?;
		entries.push(Function {
			start,
			count1,
			count2,
			a1,
			a2,
			a4,
			a5,
			a6,
		});
	}
	Ok(entries)
}

#[derive(Clone, PartialEq)]
pub enum Value {
	Uint(u32),
	Int(i32),
	Float(f32),
	String(String),
}

impl std::fmt::Debug for Value {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Uint(v) => write!(f, "{:#X}", v),
			Self::Int(v) => write!(f, "{v}"),
			Self::Float(v) => v.fmt(f),
			Self::String(v) => v.fmt(f),
		}
	}
}

#[derive(Clone, PartialEq)]
pub struct TaggedValue(Value, u32);

impl std::fmt::Debug for TaggedValue {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		if self.1 == 0 {
			self.0.fmt(f)
		} else {
			write!(f, "{:?}:{}", self.0, self.1)
		}
	}
}

fn f30(v: u32) -> f32 {
	fn float_len(v: &f32) -> usize {
		use std::io::Write;
		struct Counter(usize);
		impl Write for Counter {
			fn write(&mut self, v: &[u8]) -> std::io::Result<usize> {
				self.0 += v.len();
				Ok(v.len())
			}
			fn flush(&mut self) -> std::io::Result<()> {
				Ok(())
			}
		}
		let mut c = Counter(0);
		write!(&mut c, "{:?}", v).unwrap();
		c.0
	}
	(0..4)
		.map(|n| f32::from_bits(v << 2 | n))
		.min_by_key(float_len)
		.unwrap()
}

fn value(f: &mut Reader) -> Result<Value, ScpError> {
	let v = f.u32()?;
	let hi = v >> 30;
	let lo = v & 0x3FFFFFFF;
	match hi {
		0 => Ok(Value::Uint(lo)),
		1 => Ok(Value::Int((lo as i32) << 2 >> 2)),
		2 => Ok(Value::Float(f30(lo))),
		3 => {
			let zs = f.at(lo as usize)?.cstr()?.to_bytes();
			let s = std::str::from_utf8(zs).with_context(|_| scp::Utf8 {
				lossy_string: String::from_utf8_lossy(zs).into_owned(),
			})?;
			Ok(Value::String(s.to_string()))
		}
		_ => unreachable!(),
	}
}

fn tagged_value(f: &mut Reader) -> Result<TaggedValue, ScpError> {
	Ok(TaggedValue(value(f)?, f.u32()?))
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Label(u32);

impl std::fmt::Debug for Label {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "l{:X}", self.0)
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum Op {
	Push(Value),
	_01(u8),
	_02(i32),
	_03(i32),
	_04(i32),
	_05(i32),
	_06(i32),
	_07(u32),
	_08(u32),
	_09(u8),
	_0A(u8),
	_0B(Label),
	Syscall(u16),
	Return,
	_0E(u32),
	_0F(Label),
	Op(u8),
	CallFunc(Value, Value, u8),
	_23(Value, Value, u8),
	_24((u8, u8), Option<(NonZeroU8, u8, u8)>),
	_25(Label),
	Line(u16),
	_27(u8),
}

pub struct Scp {
	pub functions: Vec<Function>,
	pub extras: Vec<TaggedValue>,
	pub ops: Vec<(Label, Op, Label)>,
}

pub fn parse_da(data: &[u8]) -> Result<Scp, ScpError> {
	tracing::info!("reading");
	let mut f = Reader::new(data);
	f.check(b"#scp")?;
	f.check_u32(24)?;
	let n_entries = f.u32()?;
	let code_start = f.u32()?;
	let n3 = f.u32()?;
	f.check_u32(0)?;

	let functions = parse_functions(&mut f, n_entries)?;
	f.seek(code_start as usize)?;
	let extras = multi(&mut f, n3 as usize, tagged_value)?;

	let last_offset = functions
		.iter()
		.map(|e| e.start.0)
		.max()
		.unwrap_or(code_start);
	let last_offset = Cell::new(last_offset);

	let label = |f: &mut Reader| -> Result<Label, ScpError> {
		let l = Label(f.u32()?);
		if l.0 > last_offset.get() {
			last_offset.set(l.0);
		}
		Ok(l)
	};

	let mut ops = Vec::new();
	loop {
		let start = f.pos();
		let op = f.u8()?;
		let op = match op {
			0x00 => {
				f.check_u8(4)?;
				Op::Push(value(&mut f)?)
			}
			0x01 => Op::_01(f.u8()?),
			0x02 => Op::_02(f.i32()?),
			0x03 => Op::_03(f.i32()?),
			0x04 => Op::_04(f.i32()?),
			0x05 => Op::_05(f.i32()?),
			0x06 => Op::_06(f.i32()?),
			0x07 => Op::_07(f.u32()?),
			0x08 => Op::_08(f.u32()?),
			0x09 => Op::_09(f.u8()?),
			0x0A => Op::_0A(f.u8()?),
			0x0B => Op::_0B(label(&mut f)?),
			0x0C => Op::Syscall(f.u16()?),
			0x0D => Op::Return,
			0x0E => Op::_0E(f.u32()?),
			0x0F => Op::_0F(label(&mut f)?),
			0x10..=0x21 => Op::Op(op),
			0x22 => Op::CallFunc(value(&mut f)?, value(&mut f)?, f.u8()?),
			0x23 => Op::_23(value(&mut f)?, value(&mut f)?, f.u8()?),
			0x24 => {
				let a = (f.u8()?, f.u8()?);
				if let Some(v) = NonZeroU8::new(f.u8()?) {
					let b = (f.u8()?, f.u8()?);
					Op::_24((a.0, a.1), Some((v, b.0, b.1)))
				} else {
					Op::_24((a.0, a.1), None)
				}
			}
			0x25 => Op::_25(label(&mut f)?),
			0x26 => Op::Line(f.u16()?),
			0x27 => Op::_27(f.u8()?),
			0x28.. => return scp::Op { op, pos: start }.fail(),
		};
		let end = op == Op::Return && f.pos() > last_offset.get() as usize;
		ops.push((Label(start as u32), op, Label(f.pos() as u32)));
		if end {
			break;
		}
	}

	Ok(Scp {
		functions,
		extras,
		ops,
	})
}

pub fn stuff(scp: &Scp) {
	let labels = {
		let mut labels = HashSet::new();
		for (_, op, _) in &scp.ops {
			match op {
				Op::_0B(l) => labels.insert(*l),
				Op::_0F(l) => labels.insert(*l),
				Op::_25(l) => labels.insert(*l),
				_ => false,
			};
		}
		labels
	};

	let functions = scp
		.functions
		.iter()
		.enumerate()
		.map(|f| (f.1.start, f))
		.collect::<std::collections::HashMap<_, _>>();

	for (start, op, end) in &scp.ops {
		if let Some((i, f)) = functions.get(start) {
			println!("function {:?}, {:?} {:?}", f.a6, f.a1, f.a2);
			for v in &f.a4 {
				println!("  :{:?}", v);
			}
		}
		if labels.contains(start) {
			println!("{:?}:", start);
		}
		println!("  {:?}", op);
	}
}
