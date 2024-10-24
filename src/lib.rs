#![feature(let_chains, is_sorted)]

use snafu::ResultExt as _;

pub mod expr;
pub mod scp;
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

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
	Global(expr::Global),
	Function(Function),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
	pub name: String,
	pub args: Vec<expr::Arg>,
	pub body: Vec<decompile::Stmt>,
	pub is_prelude: bool,
	pub dup: bool,
}

#[derive(Debug, snafu::Snafu)]
pub enum Error {
	#[snafu(display("Failed to parse SCP"))]
	ParseSCP { source: scp::Error },
	#[snafu(display("Failed to produce tree"))]
	Nest { source: nest::Error },
	#[snafu(display("Failed to decompile"))]
	Decompile { source: decompile::Error },
	#[snafu(display("Failed to infer calls"))]
	InferCalls { source: calls::Error },
}

pub fn decompile(data: &[u8]) -> Result<Vec<Item>, Error> {
	let scp = scp::parse_scp(data).context(ParseSCPSnafu)?;
	let mut items = Vec::new();
	for item in &scp.items {
		match item {
			scp::Item::Global(g) => {
				items.push(Item::Global(g.clone()));
			}
			scp::Item::Function(f) => {
				let body = nest::decompile(f).context(NestSnafu)?;
				let mut body = decompile::decompile(f.args.len(), &body).context(DecompileSnafu)?;
				let dup = calls::infer_calls(&scp.items, &f.called, &mut body).context(InferCallsSnafu)?;
				items.push(Item::Function(Function {
					name: f.name.clone(),
					args: f.args.clone(),
					body,
					is_prelude: f.is_prelude,
					dup,
				}));
			}
		}
	}
	Ok(items)
}
