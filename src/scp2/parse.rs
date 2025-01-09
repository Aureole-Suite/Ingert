mod value;
mod function;
mod global;

use gospel::read::{Le as _, Reader};
use snafu::ResultExt as _;
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

	f.seek(func_start)?;
	let mut raw_functions = Vec::with_capacity(func_count);
	for number in 0..func_count {
		let _span = tracing::info_span!("function", number = number).entered();
		let start = f.pos();
		let func = function::function(number, &mut f).context(FunctionSnafu { number, start })?;
		raw_functions.push(func);
	}

	f.seek(global_start)?;
	let mut globals = Vec::with_capacity(global_count);
	if global_count > 0 {
		for number in 0..global_count {
			let _span = tracing::info_span!("global", number = number).entered();
			let start = f.pos();
			let global = global::global(&mut f).context(GlobalSnafu { number, start })?;
			globals.push(global);
		}
	}

	let func_names = raw_functions.iter().map(|f| f.name.clone()).collect::<Vec<_>>();
	let global_names = globals.iter().map(|g| g.name.clone()).collect::<Vec<_>>();
	if !func_names.is_sorted() {
		tracing::warn!("function names are not sorted");
	}
	raw_functions.sort_by_key(|f| f.code_start);

	todo!();
}
