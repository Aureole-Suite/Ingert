use gospel::read::{Le as _, Reader};
use snafu::{ResultExt as _, ensure};
use crate::scp2::{Arg, ArgType};

use super::value::{string_value, value, ValueError};

mod call;

#[derive(Debug, Clone, PartialEq)]
pub struct RawFunction {
	pub name: String,
	pub args: Vec<Arg>,
	pub number: usize,
	calls: Vec<call::RawCall>,
	pub is_prelude: bool,
	pub code_start: usize,
}

#[derive(Debug, snafu::Snafu)]
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
	Call { number: usize, source: call::CallError },
}

pub fn functions(f: &mut Reader, func_count: usize) -> Result<Vec<RawFunction>, super::ScpError> {
	let mut functions = Vec::with_capacity(func_count);
	if func_count > 0 {
		let mut ptrs = Pointers::default();
		for number in 0..func_count {
			let _span = tracing::info_span!("function", number = number).entered();
			let start = f.pos();
			let func = function(number, f, &mut ptrs).context(super::FunctionSnafu { number, start })?;
			functions.push(func);
		}
		let pos = f.pos();
		let pos = ptrs.def.check_start("default values", pos);
		let pos = ptrs.arg.check_start("argument types", pos);
		let pos = ptrs.called.check_start("call table", pos);
		let pos = ptrs.call_arg.check_start("call arguments", pos);
		f.seek(pos)?;
	}

	Ok(functions)
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
			super::check_pos(what, end, pos);
		} else {
			self.value = Some((pos, pos));
		}
	}

	fn set(&mut self, pos: usize) {
		self.value.as_mut().unwrap().1 = pos;
	}

	fn check_start(&self, arg: &str, pos: usize) -> usize {
		if let Some((start, end)) = self.value {
			super::check_pos(arg, start, pos);
			end
		} else {
			pos
		}
	}
}

fn function(number: usize, f: &mut Reader, ptrs: &mut Pointers) -> Result<RawFunction, FunctionError> {
	let code_start = f.u32()? as usize;
	let arg_count = f.u8()? as usize;
	let flags = f.u16()?;
	let def_count = f.u8()? as usize;
	let def_start = f.u32()? as usize;
	let arg_start = f.u32()? as usize;
	let called_count = f.u32()? as usize;
	let called_start = f.u32()? as usize;
	let expected_crc32 = f.u32()?;
	let name = string_value(f).context(NameSnafu)?;

	let is_prelude = flags & 1 != 0;
	ensure!(flags & !0x0001 == 0, FlagsSnafu { flags });

	let actual_crc32 = !crc32fast::hash(name.as_bytes());
	ensure!(expected_crc32 == actual_crc32, ChecksumSnafu {
		actual: actual_crc32,
		expected: expected_crc32,
	});

	ptrs.def.check("default values", def_start);
	let mut g = f.at(def_start)?;
	let mut defaults = Vec::with_capacity(def_count);
	for number in 0..def_count {
		defaults.push(value(&mut g).context(DefaultSnafu { number })?);
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
	ensure!(num_defaults == def_count, DefaultCountSnafu {
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
			_ => ArgTypeSnafu { number, ty: bits }.fail()?
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
	let calls = call::calls(&mut g, called_count, &mut ptrs.call_arg)?;
	ptrs.called.set(g.pos());

	Ok(RawFunction {
		name,
		number,
		args,
		calls,
		is_prelude,
		code_start,
	})
}

