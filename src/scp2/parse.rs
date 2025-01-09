use std::collections::HashMap;

mod value;
mod function;
mod global;

use gospel::read::{Le as _, Reader};
use snafu::ResultExt as _;
use super::Scp;

use global::{global, GlobalError};
use function::{function, FunctionError};

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
	Global { number: usize, start: usize, source: GlobalError },
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

	check_pos("globals start", global_start, f.pos());
	f.seek(global_start)?;
	let mut globals = Vec::with_capacity(global_count);
	if global_count > 0 {
		for number in 0..global_count {
			let _span = tracing::info_span!("global", number = number);
			let start = f.pos();
			let global = global(&mut f).context(scp::Global { number, start })?;
			globals.push(global);
		}
	}

	if !functions.is_sorted_by_key(|f| &f.name) {
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
