use gospel::read::{Le as _, Reader};
use std::cell::Cell;
use snafu::{OptionExt as _, ResultExt as _};

#[derive(Debug, snafu::Snafu)]
#[snafu(module(scp), context(suffix(false)))]
pub enum ScpError {
	#[snafu(display("invalid read (at {location})"), context(false))]
	Read {
		source: gospel::read::Error,
		#[snafu(implicit)]
		location: snafu::Location,
	},
	#[snafu(display("invalid string (at {location})"), context(false))]
	String {
		source: StringError,
		#[snafu(implicit)]
		location: snafu::Location,
	},
	#[snafu(display("invalid opcode {op:02X} at {pos}"))]
	Op { op: u8, pos: usize },
	#[snafu(display("invalid value {value:?}"))]
	BadValue {
		value: Value,
	},
	#[snafu(display("invalid checksum for function {name}: expected {expected:#X}, got {actual:#X}"))]
	Checksum {
		name: String,
		expected: u32,
		actual: u32,
	},
	BadDefaults,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Arg {
	pub ty: u8,
	pub default: Option<Value>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
	pub start: Label,
	pub a0: u8,
	pub a1: u8,
	pub args: Vec<Arg>,
	pub called: Vec<(i32, u16, Vec<TaggedValue>)>,
	pub name: String,
	pub index: u32,
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
	for index in 0..n_entries {
		let start = Label(f.u32()?);
		let argc = f.u8()? as usize;
		let a0 = f.u8()?;
		let a1 = f.u8()?;
		let a2c = f.u8()? as usize;
		let a2p = f.u32()? as usize;
		let argp = f.u32()? as usize;
		let calledc = f.u32()? as usize;
		let calledp = f.u32()? as usize;
		let checksum = f.u32()?;
		let name = string_value(f)?;

		let name_checksum = !crc32fast::hash(name.as_bytes());
		snafu::ensure!(checksum == name_checksum, scp::Checksum {
			name,
			expected: checksum,
			actual: name_checksum,
		});

		let defaults = multi(&mut f.at(a2p)?, a2c, value)?;
		let types = multi(&mut f.at(argp)?, argc, |f| Ok(f.u32()?))?;
		let mut defaults = defaults.into_iter();
		let mut args = Vec::with_capacity(types.len());
		for ty in types {
			let default = if ty & 8 != 0 {
				Some(defaults.next().context(scp::BadDefaults)?)
			} else {
				None
			};
			
			args.push(Arg { ty: (ty & 7) as u8, default });
		}
		snafu::ensure!(defaults.next().is_none(), scp::BadDefaults);

		let called = multi(&mut f.at(calledp)?, calledc, |f| {
			let x = f.i32()?;
			let y = f.u16()?;
			let z = f.u16()? as usize;
			let w = f.u32()? as usize;
			let z = multi(&mut f.at(w)?, z, tagged_value)?;
			Ok((x, y, z))
		})?;
		entries.push(Function {
			start,
			a0,
			a1,
			args,
			called,
			name,
			index,
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

#[derive(Debug, snafu::Snafu)]
#[snafu(module(string), context(suffix(false)))]
pub enum StringError {
	#[snafu(display("invalid read"), context(false))]
	Read {
		source: gospel::read::Error,
	},
	#[snafu(display("invalid string: {lossy_string:?}"))]
	Utf8 {
		source: std::str::Utf8Error,
		lossy_string: String,
	},
}

fn string(f: &mut Reader) -> Result<String, StringError> {
	let zs = f.cstr()?.to_bytes();
	let s = std::str::from_utf8(zs).with_context(|_| string::Utf8 {
		lossy_string: String::from_utf8_lossy(zs).into_owned(),
	})?;
	Ok(s.to_string())
}

fn value(f: &mut Reader) -> Result<Value, ScpError> {
	let v = f.u32()?;
	let hi = v >> 30;
	let lo = v & 0x3FFFFFFF;
	match hi {
		0 => Ok(Value::Uint(lo)),
		1 => Ok(Value::Int((lo as i32) << 2 >> 2)),
		2 => Ok(Value::Float(f30(lo))),
		3 => Ok(Value::String(string(&mut f.at(lo as usize)?)?)),
		_ => unreachable!(),
	}
}

fn string_value(f: &mut Reader) -> Result<String, ScpError> {
	match value(f)? {
		Value::String(s) => Ok(s),
		value => scp::BadValue { value }.fail(),
	}
}

fn tagged_value(f: &mut Reader) -> Result<TaggedValue, ScpError> {
	Ok(TaggedValue(value(f)?, f.u32()?))
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Label(pub u32);

impl std::fmt::Debug for Label {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "l{:X}", self.0)
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum Op {
	Push(Value),
	Pop(u8),
	GetVar(i32),
	_03(i32),
	_04(i32),
	SetVar(i32),
	_06(i32),
	_07(u32),
	_08(u32),
	GetGlobal(u8),
	SetGlobal(u8),
	Goto(Label),
	Call(u16),
	Return,
	If2(Label),
	If(Label),
	Op(u8),
	CallExtern(String, String, u8),
	_23(Value, Value, u8),
	CallSystem(u8, u8, u8),
	_25(Label),
	Line(u16),
	Debug(u8),
}

pub struct Scp {
	pub functions: Vec<Function>,
	pub extras: Vec<TaggedValue>,
	pub code: Vec<(Label, Op)>,
	pub code_end: Label,
}

pub fn parse_scp(data: &[u8]) -> Result<Scp, ScpError> {
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
			0 => {
				f.check_u8(4)?; // number of bytes to push?
				Op::Push(value(&mut f)?)
			}
			1 => Op::Pop(f.u8()?),
			2 => Op::GetVar(f.i32()?),
			3 => Op::_03(f.i32()?),
			4 => Op::_04(f.i32()?),
			5 => Op::SetVar(f.i32()?),
			6 => Op::_06(f.i32()?),
			7 => Op::_07(f.u32()?),
			8 => Op::_08(f.u32()?),
			9 => Op::GetGlobal(f.u8()?),
			10 => Op::SetGlobal(f.u8()?),
			11 => Op::Goto(label(&mut f)?),
			12 => Op::Call(f.u16()?),
			13 => Op::Return,
			14 => Op::If2(label(&mut f)?),
			15 => Op::If(label(&mut f)?),
			16..=33 => Op::Op(op),
			34 => Op::CallExtern(string_value(&mut f)?, string_value(&mut f)?, f.u8()?),
			35 => Op::_23(value(&mut f)?, value(&mut f)?, f.u8()?),
			36 => {
				let a = f.u8()?;
				let b = f.u8()?;
				let c = f.u8()?;
				if c != 0 {
					f.check_u8(1)?;
					f.check_u8(4 * c)?;
				}
				Op::CallSystem(a, b, c)
			}
			37 => Op::_25(label(&mut f)?),
			38 => Op::Line(f.u16()?),
			39 => Op::Debug(f.u8()?),
			40.. => return scp::Op { op, pos: start }.fail(),
		};
		let end = op == Op::Return && f.pos() > last_offset.get() as usize;
		ops.push((Label(start as u32), op));
		if end {
			break;
		}
	}

	Ok(Scp {
		functions,
		extras,
		code: ops,
		code_end: Label(f.pos() as u32)
	})
}

