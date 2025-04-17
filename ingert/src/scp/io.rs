mod value;
mod function;
mod called;
mod global;
mod code;

use std::collections::HashMap;

use gospel::read::{Le as _, Reader};
use gospel::write::{Le as _, Writer, Label};
use snafu::ResultExt as _;
use crate::scp::Function;

use super::Scp;

#[derive(Debug, snafu::Snafu)]
pub enum ReadError {
	#[snafu(display("invalid read (at {location})"), context(false))]
	Read {
		source: gospel::read::Error,
		#[snafu(implicit)]
		location: snafu::Location,
	},
	Function { number: usize, start: usize, source: function::ReadError },
	Global { number: usize, start: usize, source: global::ReadError },
	Called { name: String, number: usize, start: usize, source: called::ReadError },
	Code { name: String, number: usize, start: usize, source: code::ReadError },
}

pub fn read(data: &[u8]) -> Result<Scp, ReadError> {
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
		let func = function::read(number, &mut f).context(FunctionSnafu { number, start })?;
		raw_functions.push(func);
	}

	f.seek(global_start)?;
	let mut globals = Vec::with_capacity(global_count);
	if global_count > 0 {
		for number in 0..global_count {
			let _span = tracing::info_span!("global", number = number).entered();
			let start = f.pos();
			let global = global::read(&mut f).context(GlobalSnafu { number, start })?;
			globals.push(global);
		}
	}

	let func_names = raw_functions.iter().map(|f| f.name.clone()).collect::<Vec<_>>();
	let global_names = globals.iter().map(|g| g.name.clone()).collect::<Vec<_>>();
	if !func_names.is_sorted() {
		tracing::warn!("function names are not sorted");
	}
	raw_functions.sort_by_key(|f| f.code_start);

	let func_ends = raw_functions.iter().skip(1).map(|f| Some(f.code_start)).chain([None]).collect::<Vec<_>>();
	let mut functions = Vec::new();
	for (func, end) in std::iter::zip(raw_functions, func_ends) {
		let _span = tracing::info_span!("function", name = func.name, number = func.number).entered();
		f.seek(func.called_start)?;
		let mut called = Vec::with_capacity(func.called_count);
		for number in 0..func.called_count {
			let _span = tracing::info_span!("call", name = &func.name, number = number).entered();
			let start = f.pos();
			let call = called::read(&mut f, &func_names)
				.context(CalledSnafu { start, name: &func.name, number })?;
			called.push(call);
		}

		f.seek(func.code_start)?;
		let code = code::read(&mut f, end, func.number, &func_names, &global_names)
			.context(CodeSnafu { name: &func.name, number: func.number, start: func.code_start })?;
		functions.push(Function {
			name: func.name,
			args: func.args,
			called,
			is_prelude: func.is_prelude,
			code,
		});
	}

	Ok(Scp { globals, functions })
}

#[derive(Debug, snafu::Snafu)]
#[snafu(module(write), context(suffix(false)))]
pub enum WriteError {
	#[snafu(display("invalid write (at {location})"), context(false))]
	Write {
		source: gospel::write::Error,
		#[snafu(implicit)]
		location: snafu::Location,
	},
	#[snafu(display("failed to write function {name}"))]
	Function { name: String, source: function::WriteError },
	#[snafu(display("failed to write global {name}"))]
	Global { name: String, source: global::WriteError },
	#[snafu(display("failed to write calleds of {name}"))]
	Called { name: String, source: called::WriteError },
	#[snafu(display("failed to write code for {name}"))]
	Code { name: String, source: code::WriteError },
}

struct WCtx<'a> {
	function_names: HashMap<&'a String, usize>,
	global_names: HashMap<&'a String, usize>,
	f_functions: Writer,
	f_args: Writer,
	f_defaults: Writer,
	f_globals: Writer,
	f_called: Writer,
	f_called_arg: Writer,
	f_code: Writer,

	f_code_strings: Writer,
	f_functions_strings: Writer,
	f_defaults_strings: Writer,
	f_called_strings: Writer,
	f_globals_strings: Writer,
}

pub fn write(scp: &Scp) -> Result<Vec<u8>, WriteError> {
	let mut sorted_funcs = scp.functions.iter().collect::<Vec<_>>();
	sorted_funcs.sort_by_key(|f| f.name.as_str());

	let function_names = sorted_funcs.iter().enumerate().map(|(i, f)| (&f.name, i)).collect::<HashMap<_, _>>();
	let global_names = scp.globals.iter().enumerate().map(|(i, g)| (&g.name, i)).collect::<HashMap<_, _>>();

	let mut w = WCtx {
		function_names,
		global_names,
		f_functions: Writer::new(),
		f_args: Writer::new(),
		f_defaults: Writer::new(),
		f_globals: Writer::new(),
		f_called: Writer::new(),
		f_called_arg: Writer::new(),
		f_code: Writer::new(),
		f_code_strings: Writer::new(),
		f_functions_strings: Writer::new(),
		f_defaults_strings: Writer::new(),
		f_called_strings: Writer::new(),
		f_globals_strings: Writer::new(),
	};

	let func_start = w.f_functions.here();
	let global_start = w.f_globals.here();

	let func_labels = sorted_funcs.iter().enumerate().map(|(i, f)| (f.name.as_str(), (Label::new(), i))).collect::<HashMap<_, _>>();

	for func in sorted_funcs {
		let _span = tracing::info_span!("function", name = &func.name).entered();
		let label = func_labels[func.name.as_str()].0;
		function::write(func, label, &mut w).context(write::Function { name: &func.name })?;
		for call in &func.called {
			called::write(call, &mut w).context(write::Called { name: &func.name })?;
		}
	}

	for global in &scp.globals {
		let _span = tracing::info_span!("global", name = &global.name).entered();
		global::write(global, &mut w).context(write::Global { name: &global.name })?;
	}

	for func in &scp.functions {
		let _span = tracing::info_span!("function", name = &func.name).entered();
		let (label, i) = func_labels[func.name.as_str()];
		w.f_code.place(label);
		code::write(&func.code, i, &mut w).context(write::Code { name: &func.name })?;
	}

	let mut f = Writer::new();
	f.slice(b"#scp");
	f.label32(func_start);
	f.u32(scp.functions.len() as u32);
	f.label32(global_start);
	f.u32(scp.globals.len() as u32);
	f.u32(0);

	f.append(w.f_functions);
	f.append(w.f_defaults);
	f.append(w.f_args);
	f.append(w.f_called);
	f.append(w.f_called_arg);
	f.append(w.f_globals);
	f.append(w.f_code);
	f.append(w.f_code_strings);
	f.append(w.f_functions_strings);
	f.append(w.f_defaults_strings);
	f.append(w.f_called_strings);
	f.append(w.f_globals_strings);

	Ok(f.finish().unwrap())
}
