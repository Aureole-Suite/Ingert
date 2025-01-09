mod value;
mod function;
mod global;

use gospel::read::{Le as _, Reader};
use super::Scp;

#[derive(Debug, snafu::Snafu)]
pub enum ScpError {
	#[snafu(display("invalid read (at {location})"), context(false))]
	Read {
		source: gospel::read::Error,
		#[snafu(implicit)]
		location: snafu::Location,
	},
	Function { number: usize, start: usize, source: function::FunctionError },
	Global { number: usize, start: usize, source: global::GlobalError },
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
	let mut functions = function::functions(&mut f, func_count)?;

	check_pos("globals start", global_start, f.pos());
	f.seek(global_start)?;
	let globals = global::globals(&mut f, global_count)?;

	let func_names = functions.iter().map(|f| &f.name).collect::<Vec<_>>();
	if !func_names.is_sorted() {
		tracing::warn!("function names are not sorted");
	}
	functions.sort_by_key(|f| f.code_start);

	todo!();
}

fn check_pos(what: &str, pos: usize, expected: usize) {
	if pos != expected {
		tracing::warn!("unexpected {what} pointer: expected {expected:#x}, got {pos:#x}");
	}
}
