#![feature(let_chains, if_let_guard, never_type)]

pub mod print;
pub mod lex;
pub mod diag;
pub mod parse;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct SyscallWrapper {
	pub ret: bool,
	pub a: u8,
	pub b: u8,
}
