use gospel::read::{Le as _, Reader};
use snafu::{ResultExt as _, ensure};
use crate::scp2::{Arg, ArgType};

use super::value::{string_value, value, ValueError};
use super::Pointers;

mod call;
use call::{call, RawCall, CallError};

#[derive(Debug, Clone, PartialEq)]
pub struct RawFunction {
	pub name: String,
	pub args: Vec<Arg>,
	calls: Vec<RawCall>,
	pub is_prelude: bool,
	pub code_start: usize,
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

pub fn function(f: &mut Reader, ptrs: &mut Pointers) -> Result<RawFunction, FunctionError> {
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

