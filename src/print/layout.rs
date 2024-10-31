use std::fmt::Write;

#[derive(Debug, Clone)]
pub struct Token {
	pub space: bool,
	pub line: bool,
	pub indent: u32,
	pub align: Option<u16>,
	pub fill: bool,
	pub text: String,
}

pub fn naive_layout(tokens: &[Token]) -> String {
	let mut out = String::new();
	naive_layout_to(&mut out, tokens);
	out
}

fn naive_layout_to(out: &mut String, tokens: &[Token]) {
	for tok in tokens {
		if tok.line {
			push_line(out, tok.indent);
		} else if tok.space {
			out.push(' ');
		}
		out.push_str(&tok.text);
	}
}

fn push_line(out: &mut String, indent: u32) {
	out.push('\n');
	for _ in 0..indent {
		out.push('\t');
	}
}

pub fn layout(tokens: &[Token]) -> String {
	#[derive(Debug, Clone, Copy)]
	struct State {
		line: u32,
		string_offset: usize,
		token: usize,
	}

	let mut current = State {
		line: 1,
		string_offset: 0,
		token: 0,
	};
	let mut last = current;
	let mut fill_index = None;

	let mut out = String::new();
	for (i, tok) in tokens.iter().enumerate() {
		let mut do_indent = false;
		if tok.line {
			out.push('\n');
			current.line += 1;
			do_indent = true;
			if tok.fill {
				fill_index = Some(out.len());
			}
		}

		if let Some(align) = tok.align {
			let diff = align as i32 - current.line as i32;
			if diff > 0 {
				let fill_pos = fill_index.unwrap_or(out.len());
				out.insert_str(fill_pos, &"\n".repeat(diff as usize));
				current.line += diff as u32;
				do_indent = tok.line || fill_index.is_none();
			} else if diff < 0 {
				write!(out, "/*{diff}*/").unwrap();
			}
			fill_index = None;
			current.string_offset = out.len();
			last = current;
		}

		if do_indent {
			for _ in 0..tok.indent {
				out.push('\t');
			}
		} else if tok.space {
			out.push(' ');
		}

		out.push_str(&tok.text);
	}
	out
}
