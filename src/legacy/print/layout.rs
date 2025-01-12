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
	let mut last_string_pos = 0;
	let mut last_index = 0;
	let mut last_line = 0;
	let mut line = 1;
	let mut fill_index = None;

	let mut out = String::new();
	for (index, tok) in tokens.iter().enumerate() {
		let mut do_indent = false;

		if tok.line {
			out.push('\n');
			line += 1;
			do_indent = true;
			if tok.fill {
				fill_index = Some(out.len());
			}
		}

		if let Some(align) = tok.align {
			let diff = align as i32 - line as i32;
			if diff < 0 {
				out.truncate(last_string_pos);
				fill_index = None;
				line = last_line;
				for tok2 in &tokens[last_index+1..index] {
					if tok2.space {
						out.push(' ');
					}
					out.push_str(&tok2.text);
				}
				if align < last_line {
					write!(out, "/*{diff}*/").unwrap();
				}
			}

			let diff = align as i32 - line as i32;
			if diff > 0 {
				let fill_pos = fill_index.unwrap_or(out.len());
				out.insert_str(fill_pos, &"\n".repeat(diff as usize));
				line += diff as u16;
				do_indent = tok.line || fill_index.is_none();
			}
		}

		if do_indent {
			for _ in 0..tok.indent {
				out.push('\t');
			}
		} else if tok.space {
			out.push(' ');
		}

		out.push_str(&tok.text);

		if tok.align.is_some() {
			fill_index = None;
			last_string_pos = out.len();
			last_index = index;
			last_line = line;
		}
	}
	out
}
