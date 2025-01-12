use gospel::read::{Le as _, Reader};
use gospel::write::Le as _;
use snafu::ResultExt as _;
use crate::scp::{Global, GlobalType};
use super::value::{string_value, write_string_value, ValueError};

#[derive(Debug, snafu::Snafu)]
pub enum ReadError {
	#[snafu(display("invalid read (at {location})"), context(false))]
	Read {
		source: gospel::read::Error,
		#[snafu(implicit)]
		location: snafu::Location,
	},
	#[snafu(display("parsing name"))]
	Name { source: ValueError },
	#[snafu(display("unknown global type {ty}"))]
	Type { ty: u32 },
}

pub fn read(f: &mut Reader) -> Result<Global, ReadError> {
	let name = string_value(f).context(NameSnafu)?;
	let ty = match f.u32()? {
		0 => GlobalType::Number,
		1 => GlobalType::String,
		ty => TypeSnafu { ty }.fail()?
	};
	Ok(Global { name, ty })
}

#[derive(Debug, snafu::Snafu)]
pub enum WriteError {}

pub fn write(global: &Global, w: &mut super::WCtx) -> Result<(), WriteError> {
	let f = &mut w.f_globals;
	write_string_value(f, &mut w.f_globals_strings, &global.name);
	match global.ty {
		GlobalType::Number => f.u32(0),
		GlobalType::String => f.u32(1),
	}
	Ok(())
}
