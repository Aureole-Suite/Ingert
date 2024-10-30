#[derive(Debug, Clone)]
pub struct Settings {
	pub use_lines: bool,
	pub show_lines: bool,
}

impl Default for Settings {
	fn default() -> Self {
		Self {
			use_lines: true,
			show_lines: true,
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Space {
	Tight,
	Line(u32),
	Space,
}

#[derive(Debug, Clone)]
pub struct Token {
	pub space: Space,
	pub line: Option<u16>,
	pub text: String,
}

pub fn layout(tokens: &[Token], settings: &Settings) -> String {
	let mut out = String::new();
	for tok in tokens {
		use std::fmt::Write;
		match tok.space {
			Space::Tight => {},
			Space::Line(l) => {
				out.push('\n');
				for _ in 0..l {
					out.push('\t');
				}
			}
			Space::Space => out.push(' '),
		}
		out.push_str(&tok.text);
	}
	out
}
