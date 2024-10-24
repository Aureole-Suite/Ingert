#![feature(let_chains, is_sorted)]

pub mod scp;

pub use scp::{Value, Binop, Unop};
pub use scp::parse_scp;

pub mod expr;
pub mod nest;
pub mod decompile;
pub mod calls;

#[repr(transparent)]
pub struct Write(pub Box<dyn std::io::Write>);
impl Write {
	pub fn write_fmt(&mut self, args: std::fmt::Arguments<'_>) {
		self.0.write_fmt(args).unwrap()
	}
}
