use gospel::read::{Le as _, Reader};
use gospel::write::Le as _;
use snafu::{ensure, ResultExt as _};
use crate::scp2::{CallArg, CallKind, Call};

use super::value::{value, write_string_value, write_value, Value, ValueError};

#[derive(Debug, snafu::Snafu)]
pub enum ReadError {
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
	#[snafu(display("function id {id} out of range (0..{max})"))]
	FuncId { id: u32, max: u32 },
	#[snafu(display("parsing call argument {number}"))]
	Value { number: usize, source: ValueError },
	#[snafu(display(
		"bad call {kind:?} for {} with args {args:?}",
		match name { Some(name) => format!("name {name}"), None => "unnamed".to_string() }
	))]
	BadCall { kind: RawCallKind, name: Option<String>, args: Vec<CallArg> },
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum RawCallKind {
	Local,
	Extern,
	Tailcall,
	Syscall,
}

pub fn read(f: &mut Reader, func_names: &[String]) -> Result<Call, ReadError> {
	let start = f.pos();
	let (name, kind, args) = parse(f, func_names)?;
	if let Some(call) = make_sense(name, kind, args) {
		Ok(call)
	} else {
		f.seek(start)?;
		let (name, kind, args) = parse(f, func_names)?;
		BadCallSnafu { kind, name, args }.fail()
	}
}

fn parse(f: &mut Reader, func_names: &[String]) -> Result<(Option<String>, RawCallKind, Vec<CallArg>), ReadError> {
	let name = match f.u32()? {
		0xFFFFFFFF => None,
		id => {
			ensure!(id < func_names.len() as u32, FuncIdSnafu { id, max: func_names.len() as u32 });
			Some(func_names[id as usize].to_owned())
		}
	};
	let kind = match f.u16()? {
		0 => RawCallKind::Local,
		1 => RawCallKind::Extern,
		2 => RawCallKind::Tailcall,
		3 => RawCallKind::Syscall,
		kind => BadKindSnafu { kind }.fail()?
	};
	let arg_count = f.u16()? as usize;
	let arg_start = f.u32()? as usize;

	let mut g = f.at(arg_start)?;
	let mut args = Vec::with_capacity(arg_count);
	for number in 0..arg_count {
		let mut h = g.clone();
		g.slice(4)?;
		let val = match g.u32()? {
			0 => {
				let val = value(&mut h).context(ValueSnafu { number })?;
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
			ty => BadArgTypeSnafu { ty }.fail()?
		};
		args.push(val);
	}

	Ok((name, kind, args))
}

fn make_sense(name: Option<String>, kind: RawCallKind, args: Vec<CallArg>) -> Option<Call> {
	let mut args = args.into_iter();
	let kind = match kind {
		RawCallKind::Local => {
			let name = name?;
			if name.contains('.') { return None; }
			CallKind::Normal(name)
		}
		RawCallKind::Extern => {
			if name.is_some() { return None; }
			let Some(CallArg::Value(Value::String(name2))) = args.next() else { return None; };
			if !name2.contains('.') { return None; }
			CallKind::Normal(name2)
		}
		RawCallKind::Tailcall => {
			let Some(CallArg::Value(Value::String(name2))) = args.next() else { return None; };
			if let Some(name) = name {
				if name != name2 { return None; }
			} else if !name2.contains('.') {
				tracing::warn!("tail call to missing function {name2}");
			}
			CallKind::Tailcall(name2)
		}
		RawCallKind::Syscall => {
			if name.is_some() { return None; }
			let Some(CallArg::Value(Value::Int(a@0..=255))) = args.next() else { return None; };
			let Some(CallArg::Value(Value::Int(b@0..=255))) = args.next() else { return None; };
			CallKind::Syscall(a as u8, b as u8)
		}
	};
	Some(Call { kind, args: args.collect() })
}

#[derive(Debug, snafu::Snafu)]
#[snafu(module(write), context(suffix(false)))]
pub enum WriteError {
	#[snafu(display("missing function {name}"))]
	MissingFunction { name: String },
}

pub fn write(call: &Call, w: &mut super::WCtx) -> Result<(), WriteError> {
	let f = &mut w.f_called;
	let g = &mut w.f_called_arg;
	let nargs = call.args.len() as u16;
	let arg_start = g.here();
	match &call.kind {
		CallKind::Normal(name) if !name.contains('.') => {
			let Some(id) = w.function_names.get(name).copied() else {
				return write::MissingFunction { name }.fail();
			};
			f.u32(id as u32);
			f.u16(0);
			f.u16(nargs);
		}
		CallKind::Normal(name) => {
			f.u32(0xFFFFFFFF);
			f.u16(1);
			f.u16(1 + nargs);
			write_string_value(g, &mut w.f_strings, name);
			g.u32(0);
		}
		CallKind::Tailcall(name) => {
			if name.contains('.') {
				f.u32(0xFFFFFFFF);
			} else if let Some(id) = w.function_names.get(name).copied() {
				f.u32(id as u32);
			} else {
				tracing::warn!("tail call to missing function {name}");
				f.u32(0xFFFFFFFF);
			}
			f.u16(2);
			f.u16(1 + nargs);
			write_string_value(g, &mut w.f_strings, name);
			g.u32(0);
		}
		CallKind::Syscall(a, b) => {
			f.u32(0xFFFFFFFF);
			f.u16(3);
			f.u16(2 + nargs);
			write_value(g, &mut w.f_strings, &Value::Int(*a as i32));
			g.u32(0);
			write_value(g, &mut w.f_strings, &Value::Int(*b as i32));
			g.u32(0);
		}
	}
	f.label32(arg_start);
	for value in &call.args {
		match value {
			CallArg::Value(v) => {
				write_value(g, &mut w.f_strings, v);
				g.u32(0);
			}
			CallArg::Call => {
				g.u32(0);
				g.u32(1);
			}
			CallArg::Var => {
				g.u32(0);
				g.u32(2);
			}
			CallArg::Expr => {
				g.u32(0);
				g.u32(3);
			}
		}
	}
	Ok(())
}
