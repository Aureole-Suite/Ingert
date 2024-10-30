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

pub fn naive_layout(tokens: &[Token]) -> String {
	let mut out = String::new();
	for tok in tokens {
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

pub fn layout(tokens: &[Token]) -> String {
	let mut out = String::new();
	for tok in tokens {
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
