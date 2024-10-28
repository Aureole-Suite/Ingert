use gospel::read::{Le as _, Reader};
use std::cell::Cell;
use snafu::{OptionExt as _, ResultExt as _};
use crate::expr::{Arg, Binop, CallKind, Global, Type, Unop};

#[derive(Debug, snafu::Snafu)]
#[snafu(module(scp), context(suffix(false)))]
pub enum Error {
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
	#[snafu(display("invalid value {value:?}: {reason}"))]
	BadValue {
		reason: &'static str,
		value: RawValue,
	},
	#[snafu(display("stack slot not aligned {value:?}"))]
	StackSlot { value: i32 },
	#[snafu(display("invalid checksum for function {name}: expected {expected:#X}, got {actual:#X}"))]
	Checksum {
		name: String,
		expected: u32,
		actual: u32,
	},
	#[snafu(display("invalid type {ty}"))]
	BadType {
		ty: u32,
	},
	BadDefaults,
	#[snafu(display("invalid {what} index {index}"))]
	Id { what: &'static str, index: usize },
	#[snafu(display("invalid call: {why}"))]
	BadCall { why: &'static str },
	#[snafu(display("invalid call argument {value:?} with kind {kind}"))]
	BadCallArg {
		value: RawValue,
		kind: u32,
	},
	#[snafu(display("bad function flags on function {name}: {flags:04X}"))]
	BadFlags { name: String, flags: u16 },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
	pub name: String,
	pub index: u32,
	pub is_prelude: bool,
	pub args: Vec<Arg>,
	pub called: Vec<Call>,
	pub start: Label,
	pub code: Vec<(Label, Op)>,
	pub code_end: Label,
}

#[derive(Clone, PartialEq)]
pub enum Value {
	Int(i32),
	Float(f32),
	String(String),
}

#[derive(Clone, PartialEq)]
pub enum RawValue {
	Special(u32),
	Value(Value)
}

impl std::fmt::Display for Value {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		std::fmt::Debug::fmt(self, f)
	}
}

impl std::fmt::Debug for Value {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Int(v) => write!(f, "{v}"),
			Self::Float(v) => v.fmt(f),
			Self::String(v) => v.fmt(f),
		}
	}
}

impl std::fmt::Debug for RawValue {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Special(v) => write!(f, "{:#X}", v),
			Self::Value(v) => v.fmt(f),
		}
	}
}

#[derive(Clone, PartialEq)]
pub enum CallArg {
	Value(Value),
	Call,
	Var,
	Expr,
}

pub type Call = (CallKind, Vec<CallArg>);

impl std::fmt::Debug for CallArg {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			CallArg::Value(v) => v.fmt(f),
			CallArg::Call => f.debug_tuple("Call").finish(),
			CallArg::Var => f.debug_tuple("Var").finish(),
			CallArg::Expr => f.debug_tuple("Expr").finish(),
		}
	}
}

fn multi<T>(
	f: &mut Reader,
	n: usize,
	g: impl Fn(&mut Reader) -> Result<T, Error>,
) -> Result<Vec<T>, Error> {
	(0..n).map(|_| g(f)).collect()
}

fn parse_functions(f: &mut Reader<'_>, n_entries: u32) -> Result<Vec<Function>, Error> {
	let mut entries = Vec::with_capacity(n_entries as usize);
	let mut call_specs = Vec::with_capacity(n_entries as usize);
	for index in 0..n_entries {
		let start = Label(f.u32()?);
		let argc = f.u8()? as usize;
		let flags = f.u16()?;
		let a2c = f.u8()? as usize;
		let a2p = f.u32()? as usize;
		let argp = f.u32()? as usize;
		let calledc = f.u32()? as usize;
		let calledp = f.u32()? as usize;
		let checksum = f.u32()?;
		let name = string_value(f)?;

		let is_prelude = flags & 1 != 0;
		snafu::ensure!(flags & !0x0001 == 0, scp::BadFlags { name: &name, flags });

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
			let out = ty & 4 != 0;
			let default = if ty & 8 != 0 {
				let Some(RawValue::Value(default)) = defaults.next() else {
					return scp::BadDefaults.fail();
				};
				Some(default)
			} else {
				None
			};

			let ty = match ty & !(4 | 8) {
				1 => Type::Number,
				2 => Type::String,
				_ => return scp::BadType { ty }.fail(),
			};
			
			args.push(Arg { out, ty, default });
		}
		snafu::ensure!(defaults.next().is_none(), scp::BadDefaults);

		call_specs.push((calledp, calledc));

		entries.push(Function {
			name,
			index,
			is_prelude,
			args,
			called: Vec::new(),
			start,
			code: Vec::new(),
			code_end: Label(0),
		});
	}

	let mut calls = Vec::with_capacity(call_specs.len());
	for (calledp, calledc) in call_specs {
		let c = multi(&mut f.at(calledp)?, calledc, |f| parse_call(f, &entries))?;
		calls.push(c);
	}

	for (f, c) in std::iter::zip(&mut entries, calls) {
		f.called = c;
	}

	Ok(entries)
}

fn parse_call(f: &mut Reader, entries: &[Function]) -> Result<Call, Error> {
	let name = match f.i32()? {
		-1 => None,
		n => Some(index(n as usize, "function", entries)?.name.clone()),
	};
	let kind = f.u16()?;
	let argc = f.u16()? as usize;
	let argp = f.u32()? as usize;
	let mut args = multi(&mut f.at(argp)?, argc, |f| match (value(f)?, f.u32()?) {
		(RawValue::Value(v), 0) => Ok(CallArg::Value(v)),
		(RawValue::Special(0), 1) => Ok(CallArg::Call),
		(RawValue::Special(0), 2) => Ok(CallArg::Var),
		(RawValue::Special(0), 3) => Ok(CallArg::Expr),
		(value, kind) => scp::BadCallArg { value, kind }.fail(),
	})?;
	let kind = match kind {
		0 => {
			snafu::ensure!(name.is_some(), scp::BadCall { why: "no name on local call" });
			CallKind::Func(name.unwrap())
		}
		1 => {
			snafu::ensure!(name.is_none(), scp::BadCall { why: "name on extern call" });
			snafu::ensure!(args.len() >= 1, scp::BadCall { why: "insufficient args on extern call" });
			let CallArg::Value(Value::String(namearg)) = args.remove(0) else {
				return scp::BadCall { why: "invalid name on extern call" }.fail();
			};
			CallKind::Func(namearg)
		}
		2 => {
			snafu::ensure!(args.len() >= 1, scp::BadCall { why: "insufficient args on tail call" });
			let CallArg::Value(Value::String(namearg)) = args.remove(0) else {
				return scp::BadCall { why: "invalid name on tail call" }.fail();
			};
			if let Some(name) = name {
				snafu::ensure!(name == namearg, scp::BadCall { why: "mismatched names on tail call" });
			} else if !namearg.contains('.') {
				tracing::warn!("tail call to missing function {namearg}");
			}
			CallKind::Tail(namearg)
		}
		3 => {
			snafu::ensure!(name.is_none(), scp::BadCall { why: "name on system call" });
			snafu::ensure!(args.len() >= 2, scp::BadCall { why: "insufficient args on system call" });
			let CallArg::Value(Value::Int(a)) = args.remove(0) else {
				return scp::BadCall { why: "invalid group on system call" }.fail();
			};
			let CallArg::Value(Value::Int(b)) = args.remove(0) else {
				return scp::BadCall { why: "invalid number on system call" }.fail();
			};
			let a = a.try_into().ok().context(scp::BadCall { why: "invalid group on system call" })?;
			let b = b.try_into().ok().context(scp::BadCall { why: "invalid number on system call" })?;
			CallKind::System(a, b)
		}
		_ => return scp::BadCall { why: "invalid call kind" }.fail(),
	};
	Ok((kind, args))
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

fn value(f: &mut Reader) -> Result<RawValue, Error> {
	let v = f.u32()?;
	let hi = v >> 30;
	let lo = v & 0x3FFFFFFF;
	match hi {
		0 => Ok(RawValue::Special(lo)),
		1 => Ok(RawValue::Value(Value::Int((lo as i32) << 2 >> 2))),
		2 => Ok(RawValue::Value(Value::Float(f30(lo)))),
		3 => Ok(RawValue::Value(Value::String(string(&mut f.at(lo as usize)?)?))),
		_ => unreachable!(),
	}
}

fn string_value(f: &mut Reader) -> Result<String, Error> {
	match value(f)? {
		RawValue::Value(Value::String(s)) => Ok(s),
		value => scp::BadValue {
			reason: "expected string",
			value
		}.fail(),
	}
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Label(pub u32);

impl std::fmt::Debug for Label {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "l{:X}", self.0)
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StackSlot(pub i32);

fn stack_slot(f: &mut Reader) -> Result<StackSlot, Error> {
	let value = f.i32()?;
	if value % 4 != 0 {
		return scp::StackSlot { value }.fail();
	}
	Ok(StackSlot(value / 4))
}

#[derive(Debug, Clone, PartialEq)]
pub enum Op {
	Push(Value),
	PushSpecial(u32),
	Pop(u8),
	GetVar(StackSlot),
	GetRef(StackSlot),
	PushRef(StackSlot),
	SetVar(StackSlot),
	SetRef(StackSlot),
	GetGlobal(String),
	SetGlobal(String),
	GetTemp(u8),
	SetTemp(u8),
	Goto(Label),
	Call(String),
	Return,
	If2(Label),
	If(Label),
	Binop(Binop),
	Unop(Unop),
	CallExtern(String, u8),
	CallTail(String, u8),
	CallSystem(u8, u8, u8),
	_25(Label),
	Line(u16),
	Debug(u8),
}

pub struct Scp {
	pub items: Vec<Item>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
	Function(Function),
	Global(Global),
}

fn parse_global(f: &mut Reader) -> Result<Global, Error> {
	let name = string_value(f)?;
	let ty = match f.u32()? {
		0 => Type::Number,
		1 => Type::String,
		ty => return scp::BadType { ty }.fail(),
	};
	Ok(Global { name, ty, line: None })
}

pub fn parse_scp(data: &[u8]) -> Result<Scp, Error> {
	tracing::info!("reading");
	let mut f = Reader::new(data);
	f.check(b"#scp")?;
	f.check_u32(24)?;
	let n_entries = f.u32()?;
	let code_start = f.u32()?;
	let n3 = f.u32()?;
	f.check_u32(0)?;

	let mut functions = parse_functions(&mut f, n_entries)?;
	f.seek(code_start as usize)?;
	let mut globals = multi(&mut f, n3 as usize, parse_global)?;

	let last_offset = functions
		.iter()
		.map(|e| e.start.0)
		.max()
		.unwrap_or(code_start);
	let last_offset = Cell::new(last_offset);

	let label = |f: &mut Reader| -> Result<Label, Error> {
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
				match value(&mut f)? {
					RawValue::Value(v) => Op::Push(v),
					RawValue::Special(v) => Op::PushSpecial(v),
				}
			}
			1 => Op::Pop(f.u8()?),
			2 => Op::GetVar(stack_slot(&mut f)?),
			3 => Op::GetRef(stack_slot(&mut f)?),
			4 => Op::PushRef(stack_slot(&mut f)?),
			5 => Op::SetVar(stack_slot(&mut f)?),
			6 => Op::SetRef(stack_slot(&mut f)?),
			7 => Op::GetGlobal(index(f.u32()? as usize, "global", &globals)?.name.clone()),
			8 => Op::SetGlobal(index(f.u32()? as usize, "global", &globals)?.name.clone()),
			9 => Op::GetTemp(f.u8()?),
			10 => Op::SetTemp(f.u8()?),
			11 => Op::Goto(label(&mut f)?),
			12 => Op::Call(index(f.u16()? as usize, "function", &functions)?.name.clone()),
			13 => Op::Return,
			14 => Op::If2(label(&mut f)?),
			15 => Op::If(label(&mut f)?),
			16..=30 => Op::Binop(Binop::from_repr(op).unwrap()),
			31..=33 => Op::Unop(Unop::from_repr(op).unwrap()),
			34 => {
				let a = string_value(&mut f)?;
				let b = string_value(&mut f)?;
				let c = f.u8()?;
				let name = format!("{a}.{b}");
				Op::CallExtern(name, c)
			}
			35 => {
				let a = string_value(&mut f)?;
				let b = string_value(&mut f)?;
				let c = f.u8()?;
				let name = if a.is_empty() { b } else { format!("{a}.{b}") };
				Op::CallTail(name, c)
			}
			36 => {
				let a = f.u8()?;
				let b = f.u8()?;
				let c = f.u8()?;
				if c != 0 {
					// This is really a Pop instruction, but handling it here makes subsequent stages easier
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

	let mut items = Vec::new();

	functions.sort_by_key(|f| f.start);
	let mut end = Label(f.pos() as u32);
	for mut func in functions.into_iter().rev() {
		let pos = ops.binary_search_by_key(&func.start, |(l, _)| *l).unwrap();
		func.code = ops.split_off(pos);
		func.code_end = end;
		end = func.start;
		while let Some(&(_, Op::Line(line))) = func.code.last() && !globals.is_empty() {
			func.code.pop();
			let mut glob = globals.pop().unwrap();
			glob.line = Some(line);
			items.push(Item::Global(glob));
		}
		items.push(Item::Function(func));
	}
	items.extend(globals.into_iter().rev().map(Item::Global));
	items.reverse();

	Ok(Scp { items })
}

fn index<'a, T>(index: usize, what: &'static str, values: &'a [T]) -> Result<&'a T, Error> {
	values.get(index).context(scp::Id { what, index })
}
