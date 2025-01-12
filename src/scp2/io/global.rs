use gospel::read::{Le as _, Reader};
use snafu::ResultExt as _;
use crate::scp2::{Global, GlobalType};
use super::value::{string_value, ValueError};

#[derive(Debug, snafu::Snafu)]
pub enum GlobalError {
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

pub fn read(f: &mut Reader) -> Result<Global, GlobalError> {
	let name = string_value(f).context(NameSnafu)?;
	let ty = match f.u32()? {
		0 => GlobalType::Number,
		1 => GlobalType::String,
		ty => TypeSnafu { ty }.fail()?
	};
	Ok(Global { name, ty, line: None })
}
