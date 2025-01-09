use gospel::read::{Le as _, Reader};
use snafu::{ResultExt as _, ensure};
use crate::scp2::CallArg;

use super::{ValueError, Pointer, value};

#[derive(Debug, Clone, PartialEq)]
enum RawCallKind {
	// args are raw
	Local(u32),
	// first arg is dotted name
	Extern,
	// first arg is either dotted or not, if non-dotted the number must match
	Tailcall(Option<u32>),
	// first two args are syscall number
	Syscall,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RawCall {
	kind: RawCallKind,
	args: Vec<CallArg>,
}

#[derive(Debug, snafu::Snafu)]
pub enum CallError {
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
	#[snafu(display("bad function id {func_id} for call kind {kind}"))]
	BadFuncId { kind: u16, func_id: i32 },
	#[snafu(display("parsing call argument {number}"))]
	Value { number: usize, source: ValueError },
}

pub fn calls(f: &mut Reader, count: usize, ptr: &mut Pointer) -> Result<Vec<RawCall>, super::FunctionError> {
	let mut calls = Vec::with_capacity(count);
	for number in 0..count {
		let _span = tracing::info_span!("call", number = number).entered();
		let call = call(f, ptr).context(super::CallSnafu { number })?;
		calls.push(call);
	}
	Ok(calls)
}

fn call(f: &mut Reader, ptr: &mut Pointer) -> Result<RawCall, CallError> {
	let func_id = f.i32()?;
	let kind = f.u16()?;
	let arg_count = f.u16()? as usize;
	let arg_start = f.u32()? as usize;

	ptr.check("call arguments", arg_start);
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
	ptr.set(g.pos());

	let kind = match kind {
		0 => {
			ensure!(func_id >= 0, BadFuncIdSnafu { kind, func_id });
			RawCallKind::Local(func_id as u32)
		}
		1 => {
			ensure!(func_id == -1, BadFuncIdSnafu { kind, func_id });
			RawCallKind::Extern
		}
		2 => {
			ensure!(func_id >= -1, BadFuncIdSnafu { kind, func_id });
			RawCallKind::Tailcall(if func_id == -1 { None } else { Some(func_id as u32) })
		}
		3 => {
			ensure!(func_id == -1, BadFuncIdSnafu { kind, func_id });
			RawCallKind::Syscall
		}
		kind => BadKindSnafu { kind }.fail()?
	};

	Ok(RawCall { kind, args })
}
