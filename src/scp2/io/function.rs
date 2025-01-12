use gospel::read::{Le as _, Reader};
use gospel::write::{Le as _, Label};
use snafu::{ResultExt as _, ensure};
use crate::scp2::{Arg, ArgType};

use super::value::{string_value, value, write_value, write_string_value, ValueError};

#[derive(Debug, Clone, PartialEq)]
pub struct RawFunction {
	pub name: String,
	pub args: Vec<Arg>,
	pub number: usize,
	pub is_prelude: bool,
	pub code_start: usize,
	pub called_start: usize,
	pub called_count: usize,
}

#[derive(Debug, snafu::Snafu)]
pub enum ReadError {
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
}

pub fn read(number: usize, f: &mut Reader) -> Result<RawFunction, ReadError> {
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

	let mut g = f.at(def_start)?;
	let mut defaults = Vec::with_capacity(def_count);
	for number in 0..def_count {
		defaults.push(value(&mut g).context(DefaultSnafu { number })?);
	}

	let mut g = f.at(arg_start)?;
	let mut arg_types = Vec::with_capacity(arg_count);
	for _ in 0..arg_count {
		arg_types.push(g.u32()?);
	}

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

	Ok(RawFunction {
		name,
		number,
		args,
		is_prelude,
		code_start,
		called_start,
		called_count,
	})
}

#[derive(Debug, snafu::Snafu)]
pub enum WriteError {}

pub fn write(func: &crate::scp2::Function, code: Label, w: &mut super::WCtx) -> Result<(), WriteError> {
	let f = &mut w.f_functions;
	f.label32(code);
	f.u8(func.args.len() as u8);
	f.u16(if func.is_prelude { 1 } else { 0 });
	f.u8(func.args.iter().filter(|a| a.default.is_some()).count() as u8);
	f.label32(w.f_defaults.here());
	f.label32(w.f_args.here());
	f.u32(func.called.len() as u32);
	f.label32(w.f_called.here());
	f.u32(!crc32fast::hash(func.name.as_bytes()));
	write_string_value(f, &mut w.f_functions_strings, &func.name);

	let f = &mut w.f_defaults;
	for arg in &func.args {
		if let Some(v) = &arg.default {
			write_value(f, &mut w.f_defaults_strings, v);
		}
	}

	let f = &mut w.f_args;
	for arg in &func.args {
		let ty = match arg.ty {
			ArgType::Number => 1,
			ArgType::String => 2,
			ArgType::NumberRef => 5,
		} | if arg.default.is_some() { 8 } else { 0 };
		f.u32(ty);
	}

	Ok(())
}
