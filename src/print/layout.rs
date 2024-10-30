use std::fmt::Write;

#[derive(Debug, Clone)]
pub struct Token {
	pub space: bool,
	pub line: bool,
	pub indent: u32,
	pub align: Option<u16>,
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

pub fn layout(mut tokens: &[Token]) -> String {
	let mut out = String::new();
	let mut last = 1;
	let mut pending_line = false;
	while let Some((seg, next)) = next_segment(&mut tokens) {
		let line_diff = next.align.unwrap() as i32 - last as i32;
		let seg = if pending_line {
			push_line(&mut out, seg[0].indent);
			out.push_str(&seg[0].text);
			&seg[1..]
		} else {
			seg
		};
		let formatted_count = seg.iter().filter(|t| t.line).count() as i32;

		for tok in seg {
			if tok.line && formatted_count <= line_diff {
				push_line(&mut out, tok.indent);
			} else if tok.space {
				out.push(' ');
			}
			out.push_str(&tok.text);
		}

		if line_diff < 0 {
			write!(out, "/*{line_diff}*/").unwrap();
		}
		for _ in formatted_count+1..line_diff {
			out.push('\n');
		}
		pending_line = formatted_count < line_diff;
		last = next.align.unwrap();
	}
	naive_layout_to(&mut out, tokens);
	out
}

fn next_segment<'a>(tokens: &mut &'a [Token]) -> Option<(&'a [Token], &'a Token)> {
	let pos = tokens.iter().enumerate().position(|(i, t)| t.align.is_some() && i > 0)?;
	let (seg, rest) = tokens.split_at(pos);
	*tokens = rest;
	Some((seg, &rest[0]))
}
