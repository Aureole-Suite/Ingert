use std::collections::HashMap;

use gospel::read::{Le as _, Reader};
use snafu::{ResultExt as _, ensure};
use super::{Arg, ArgType, CallArg, Scp, Value};

fn check_pos(what: &str, pos: usize, expected: usize) {
	if pos != expected {
		tracing::warn!("unexpected {what} pointer: expected {expected:#x}, got {pos:#x}");
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
	Function { number: usize, start: usize, source: FunctionError },
}

pub fn scp(data: &[u8]) -> Result<Scp, ScpError> {
	let mut f = Reader::new(data);
	f.check(b"#scp")?;
	let func_start = f.u32()? as usize;
	let func_count = f.u32()? as usize;
	let global_start = f.u32()? as usize;
	let global_count = f.u32()? as usize;
	f.check_u32(0)?;

	check_pos("function table start", func_start, f.pos());
	f.seek(func_start)?;
	let mut functions = Vec::with_capacity(func_count);
	let mut func_names = HashMap::with_capacity(func_count);
	if func_count > 0 {
		let mut ptrs = Pointers::default();
		for number in 0..func_count {
			let _span = tracing::info_span!("function", number = number);
			let start = f.pos();
			let func = function(&mut f, &mut ptrs).context(scp::Function { number, start })?;
			func_names.insert(func.name.clone(), number);
			functions.push(func);
		}
		let pos = f.pos();
		let pos = ptrs.def.check_start("default values", pos);
		let pos = ptrs.arg.check_start("argument types", pos);
		let pos = ptrs.called.check_start("call table", pos);
		let pos = ptrs.call_arg.check_start("call arguments", pos);
		f.seek(pos)?;
	}

	if !functions.is_sorted_by_key(|f| &f.name) {
		tracing::warn!("function names are not sorted");
	}

	functions.sort_by_key(|f| f.code_start);

	check_pos("globals start", global_start, f.pos());
	f.seek(global_start)?;

	todo!();
}

#[derive(Debug, snafu::Snafu)]
#[snafu(module(value), context(suffix(false)))]
pub enum ValueError {
	#[snafu(display("invalid read (at {location})"), context(false))]
	Read {
		source: gospel::read::Error,
		#[snafu(implicit)]
		location: snafu::Location,
	},
	#[snafu(display("unexpected special value {v}"))]
	Special { v: u32 },
	#[snafu(display("invalid string: {lossy_string:?}"))]
	Utf8 {
		source: std::str::Utf8Error,
		lossy_string: String,
	},
	#[snafu(display("expected string, got {value:?}"))]
	NeedString { value: Value }
}

fn value(f: &mut Reader) -> Result<Value, ValueError> {
	let v = f.u32()?;
	let hi = v >> 30;
	let lo = v & 0x3FFFFFFF;
	match hi {
		0 => value::Special { v }.fail()?,
		1 => Ok(Value::Int((lo as i32) << 2 >> 2)),
		2 => Ok(Value::Float(f30(lo))),
		3 => Ok(Value::String(string(&mut f.at(lo as usize)?)?)),
		_ => unreachable!(),
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

fn string(f: &mut Reader) -> Result<String, ValueError> {
	let zs = f.cstr()?.to_bytes();
	let s = std::str::from_utf8(zs).with_context(|_| value::Utf8 {
		lossy_string: String::from_utf8_lossy(zs).into_owned(),
	})?;
	Ok(s.to_string())
}

fn string_value(f: &mut Reader) -> Result<String, ValueError> {
	match value(f)? {
		Value::String(s) => Ok(s),
		value => value::NeedString { value }.fail(),
	}
}

#[derive(Debug, Clone, PartialEq, Default)]
struct Pointers {
	def: Pointer,
	arg: Pointer,
	called: Pointer,
	call_arg: Pointer,
}

#[derive(Debug, Clone, PartialEq, Default)]
struct Pointer {
	value: Option<(usize, usize)>,
}

impl Pointer {
	fn check(&mut self, what: &str, pos: usize) {
		if let Some((_, end)) = self.value {
			check_pos(what, end, pos);
		} else {
			self.value = Some((pos, pos));
		}
	}

	fn set(&mut self, pos: usize) {
		self.value.as_mut().unwrap().1 = pos;
	}

	fn check_start(&self, arg: &str, pos: usize) -> usize {
		if let Some((start, end)) = self.value {
			check_pos(arg, start, pos);
			end
		} else {
			pos
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
struct RawFunction {
	name: String,
	args: Vec<Arg>,
	calls: Vec<RawCall>,
	is_prelude: bool,
	code_start: usize,
}

#[derive(Debug, snafu::Snafu)]
#[snafu(module(function), context(suffix(false)))]
pub enum FunctionError {
	#[snafu(display("invalid read (at {location})"), context(false))]
	Read {
		source: gospel::read::Error,
		#[snafu(implicit)]
		location: snafu::Location,
	},
	#[snafu(display("unknown flags: {flags:04x}"))]
	Flags { flags: u16 },
	#[snafu(display("parsing name"))]
	Name { source: ValueError },
	#[snafu(display("name checksum mismatch: expected {expected:08x}, got {actual:08x}"))]
	Checksum { actual: u32, expected: u32 },
	#[snafu(display("parsing default value {number}"))]
	Default { number: usize, source: ValueError },
	#[snafu(display("wrong number of defaults: {actual} defaults, but {expected} args with defaults"))]
	DefaultCount { actual: usize, expected: usize },
	#[snafu(display("unknown argument type {ty} for argument {number}"))]
	ArgType { number: usize, ty: u32 },
	#[snafu(display("parsing call table entry {number}"))]
	Call { number: usize, source: CallError },
}

fn function(f: &mut Reader, ptrs: &mut Pointers) -> Result<RawFunction, FunctionError> {
	let code_start = f.u32()? as usize;
	let arg_count = f.u8()? as usize;
	let flags = f.u16()?;
	let def_count = f.u8()? as usize;
	let def_start = f.u32()? as usize;
	let arg_start = f.u32()? as usize;
	let called_count = f.u32()? as usize;
	let called_start = f.u32()? as usize;
	let expected_crc32 = f.u32()?;
	let name = string_value(f).context(function::Name)?;

	let is_prelude = flags & 1 != 0;
	ensure!(flags & !0x0001 == 0, function::Flags { flags });

	let actual_crc32 = !crc32fast::hash(name.as_bytes());
	ensure!(expected_crc32 == actual_crc32, function::Checksum {
		actual: actual_crc32,
		expected: expected_crc32,
	});

	ptrs.def.check("default values", def_start);
	let mut g = f.at(def_start)?;
	let mut defaults = Vec::with_capacity(def_count);
	for number in 0..def_count {
		defaults.push(value(&mut g).context(function::Default { number })?);
	}
	ptrs.def.set(g.pos());

	ptrs.arg.check("argument types", arg_start);
	let mut g = f.at(arg_start)?;
	let mut arg_types = Vec::with_capacity(arg_count);
	for _ in 0..arg_count {
		arg_types.push(g.u32()?);
	}
	ptrs.arg.set(g.pos());

	let num_defaults = arg_types.iter().filter(|&&t| t & 8 != 0).count();
	ensure!(num_defaults == def_count, function::DefaultCount {
		actual: num_defaults,
		expected: def_count,
	});

	let mut defaults = defaults.into_iter();
	let mut args = Vec::with_capacity(arg_count);
	for (number, bits) in arg_types.into_iter().enumerate() {
		let ty = match bits & !8 {
			1 => ArgType::Number,
			2 => ArgType::String,
			5 => ArgType::NumberRef,
			_ => function::ArgType { number, ty: bits }.fail()?
		};
		let default = if bits & 8 != 0 {
			Some(defaults.next().expect("default count mismatch"))
		} else {
			None
		};
		args.push(Arg { ty, default });
	}

	ptrs.called.check("call table", called_start);
	let mut g = f.at(called_start)?;
	let mut calls = Vec::with_capacity(called_count);
	for number in 0..called_count {
		let call = call(&mut g, &mut ptrs.call_arg)
			.context(function::Call { number })?;
		calls.push(call);
	}
	ptrs.called.set(g.pos());

	Ok(RawFunction {
		name,
		args,
		calls,
		is_prelude,
		code_start,
	})
}

#[derive(Debug, Clone, PartialEq)]
enum RawCallKind {
	// args are raw
	Local(u32),
	// first arg is dotted name
	Extern,
	// first arg is either dotted or not, if non-dotted the number must match
	Tailcall(Option<u32>),
	// first two args are syscall number
	Syscall,
}

#[derive(Debug, Clone, PartialEq)]
struct RawCall {
	kind: RawCallKind,
	args: Vec<CallArg>,
}

#[derive(Debug, snafu::Snafu)]
#[snafu(module(call), context(suffix(false)))]
pub enum CallError {
	#[snafu(display("invalid read (at {location})"), context(false))]
	Read {
		source: gospel::read::Error,
		#[snafu(implicit)]
		location: snafu::Location,
	},
	#[snafu(display("unknown argument type {ty}"))]
	BadArgType { ty: u32 },
	#[snafu(display("unknown call kind {kind}"))]
	BadKind { kind: u16 },
	#[snafu(display("bad function id {func_id} for call kind {kind}"))]
	BadFuncId { kind: u16, func_id: i32 },
	#[snafu(display("parsing call argument {number}"))]
	Value { number: usize, source: ValueError },
}

fn call(f: &mut Reader, ptr: &mut Pointer) -> Result<RawCall, CallError> {
	let func_id = f.i32()?;
	let kind = f.u16()?;
	let arg_count = f.u16()? as usize;
	let arg_start = f.u32()? as usize;

	ptr.check("call arguments", arg_start);
	let mut g = f.at(arg_start)?;
	let mut args = Vec::with_capacity(arg_count);
	for number in 0..arg_count {
		let mut h = g.clone();
		g.slice(4)?;
		let val = match g.u32()? {
			0 => {
				let val = value(&mut h).context(call::Value { number })?;
				CallArg::Value(val)
			}
			1 => {
				h.check_u32(0)?;
				CallArg::Call
			}
			2 => {
				h.check_u32(0)?;
				CallArg::Var
			}
			3 => {
				h.check_u32(0)?;
				CallArg::Expr
			}
			ty => call::BadArgType { ty }.fail()?
		};
		args.push(val);
	}
	ptr.set(g.pos());

	let kind = match kind {
		0 => {
			ensure!(func_id >= 0, call::BadFuncId { kind, func_id });
			RawCallKind::Local(func_id as u32)
		}
		1 => {
			ensure!(func_id == -1, call::BadFuncId { kind, func_id });
			RawCallKind::Extern
		}
		2 => {
			ensure!(func_id >= -1, call::BadFuncId { kind, func_id });
			RawCallKind::Tailcall(if func_id == -1 { None } else { Some(func_id as u32) })
		}
		3 => {
			ensure!(func_id == -1, call::BadFuncId { kind, func_id });
			RawCallKind::Syscall
		}
		kind => call::BadKind { kind }.fail()?
	};

	Ok(RawCall { kind, args })
}

