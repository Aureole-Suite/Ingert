use super::error::Errors;

#[derive(Debug, Clone)]
pub struct Tokens(Vec<RawToken>);

#[derive(Clone)]
struct RawToken {
	start: u32,
	end: u32,
	line: Option<u32>,
	spacing: Spacing,
	token: TokenKind,
	matched: u32,
}

impl RawToken {
	fn span(&self) -> std::ops::Range<usize> {
		self.start as usize..self.end as usize
	}
}

impl std::fmt::Debug for RawToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let space = match self.spacing { Spacing::Alone => "+", Spacing::Joined => "" };
		f.write_fmt(format_args!("{}..{}:", self.start, self.end))?;
		if let Some(line) = self.line {
			f.write_fmt(format_args!("{}@", line))?;
		}
		f.write_fmt(format_args!("{:?}{}", self.token, space))?;
		if self.matched > 0 {
			f.write_fmt(format_args!("~{}", self.matched))?;
		}
		Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Spacing {
	Alone,
	Joined,
}

#[derive(Debug, Clone)]
pub enum TokenKind {
	Ident(Box<str>),
	String(Box<str>),
	Int(i64),
	Float(f64),
	Punct(char),
}

pub fn lex(src: &str) -> (Tokens, Errors) {
	let mut lexer = Lex::new(src);
	let mut tokens = Vec::new();
	while let Some(token) = lexer.lex() {
		tokens.push(token);
	}
	let mut errors = lexer.errors;

	let mut stack = Vec::new();
	for (i, token) in tokens.iter_mut().enumerate() {
		match token.token {
			TokenKind::Punct(o@('('|'['|'{')) => stack.push((i, o, token)),
			TokenKind::Punct(c@(')'|']'|'}')) => {
				let open_delim = match c {
					')' => '(',
					']' => '[',
					'}' => '{',
					_ => unreachable!(),
				};
				if let Some((j, o, open)) = stack.pop() {
					if o != open_delim {
						errors.fatal("mismatched delimiter", token.span())
							.note("doesn't match", open.span());
					}
					let diff = (i - j) as u32;
					open.matched = diff;
					token.matched = diff;
				} else {
					errors.fatal("unmatched delimiter", token.span());
				}
			}
			_ => {}
		}
	}
	for (_, _, open) in stack {
		errors.fatal("unclosed delimiter", open.span());
	}

	(Tokens(tokens), errors)
}

struct Lex<'a> {
	src: &'a str,
	pos: usize,
	errors: Errors,
}

impl Lex<'_> {
	fn new(src: &str) -> Lex<'_> {
		let mut lex = Lex { src, pos: 0, errors: Errors::new() };
		lex.skip_whitespace();
		lex
	}

	fn peek_char(&self) -> Option<char> {
		self.src[self.pos..].chars().next()
	}

	fn next_char(&mut self) -> Option<char> {
		let c = self.peek_char();
		self.pos += c.map_or(0, |c| c.len_utf8());
		c
	}

	fn consume_if(&mut self, f: impl FnOnce(char) -> bool) -> bool {
		let c = self.peek_char();
		if c.is_some_and(f) {
			self.next_char();
			true
		} else {
			false
		}
	}

	fn consume(&mut self, s: &str) -> bool {
		if self.src[self.pos..].starts_with(s) {
			self.pos += s.len();
			true
		} else {
			false
		}
	}

	fn skip_whitespace(&mut self) -> Spacing {
		let start = self.pos;
		loop {
			if self.consume(" ") || self.consume("\t") || self.consume("\n") || self.consume("\r") {
				continue;
			}
			if self.consume("//") {
				while self.peek_char().is_some_and(|c| c != '\n') {
					self.next_char();
				}
				continue;
			}
			break;
		}
		if self.pos == start {
			Spacing::Joined
		} else {
			Spacing::Alone
		}
	}

	fn lex(&mut self) -> Option<RawToken> {
		let start = self.pos;
		let line = self.lex_line();
		if line.is_some() && self.skip_whitespace() == Spacing::Alone {
			self.errors.warning("line number should not be followed by whitespace", start..self.pos);
		}
		let token = self.lex_token()?;
		let end = self.pos;
		let spacing = self.skip_whitespace();
		Some(RawToken { start: start as u32, end: end as u32, line, spacing, token, matched: 0 })
	}

	fn lex_line(&mut self) -> Option<u32> {
		let start = self.pos;
		while self.consume_if(|c| c.is_ascii_digit()) {}
		if self.pos != start && self.consume("@") {
			let line = self.src[start..self.pos - 1].parse().expect("valid line number");
			Some(line)
		} else {
			self.pos = start;
			None
		}
	}

	fn lex_token(&mut self) -> Option<TokenKind> {
		let start = self.pos;

		if self.consume_if(unicode_ident::is_xid_start) {
			while self.consume_if(unicode_ident::is_xid_continue) {}
			return Some(TokenKind::Ident(self.src[start..self.pos].into()))
		}

		if self.consume("0x") {
			let numstart = self.pos;
			while self.consume_if(|c| c.is_ascii_hexdigit()) {}
			let n = if self.pos == numstart {
				self.errors.error("invalid hex literal", start..self.pos);
				0
			} else {
				i64::from_str_radix(&self.src[numstart..self.pos], 16).expect("valid hex literal")
			};
			return Some(TokenKind::Int(n))
		}

		if matches!(self.peek_char(), Some('-' | '0'..='9')) {
			self.consume_if(|c| c == '-');
			let digitstart = self.pos;
			while self.consume_if(|c| c.is_ascii_digit()) {}

			if self.pos == digitstart {
				// If no digits, it's just a minus sign
				return Some(TokenKind::Punct('-'))
			}

			if self.consume(".") {
				while self.consume_if(|c| c.is_ascii_digit()) {}
				match self.src[start..self.pos].parse() {
					Ok(f) => return Some(TokenKind::Float(f)),
					Err(_) => {
						self.errors.error("invalid float literal", start..self.pos);
						return Some(TokenKind::Float(0.0))
					}
				}
			} else {
				match self.src[start..self.pos].parse() {
					Ok(n) => return Some(TokenKind::Int(n)),
					Err(_) => {
						self.errors.error("invalid int literal", start..self.pos);
						return Some(TokenKind::Int(0))
					}
				}
			}
		}

		if self.consume("\"") {
			return Some(TokenKind::String(self.lex_string(start, "\"")));
		}

		if self.consume("`") {
			return Some(TokenKind::Ident(self.lex_string(start, "`")));
		}

		if let Some(c) = self.next_char() {
			return Some(TokenKind::Punct(c))
		}

		None
	}

	fn lex_string(&mut self, start: usize, delim: &str) -> Box<str> {
		let mut s = String::new();
		loop {
			let escstart = self.pos;
			if self.consume(delim) {
				break;
			}
			match self.next_char() {
				Some('\\') => {
					if self.consume(delim) {
						s.push_str(delim);
						continue;
					}
					if let Some(c) = self.lex_escape() {
						s.push(c);
					} else {
						self.errors.error("invalid escape sequence", escstart..self.pos);
					}
				}
				Some(c) => s.push(c),
				None => {
					self.errors.error("unterminated string", start..self.pos);
					break;
				}
			}
		}
		s.into_boxed_str()
	}

	fn lex_escape(&mut self) -> Option<char> {
		match self.next_char()? {
			'\\' => Some('\\'),
			'n' => Some('\n'),
			'r' => Some('\r'),
			't' => Some('\t'),
			'x' => {
				let hexstart = self.pos;
				if self.consume_if(|c| c.is_ascii_hexdigit()) && self.consume_if(|c| c.is_ascii_hexdigit()) {
					let hex = &self.src[hexstart..self.pos];
					u8::from_str_radix(hex, 16).ok().map(char::from)
				} else {
					None
				}
			}
			_ => None,
		}
	}
}

#[cfg(test)]
mod test {
	#[test]
	fn test() {
		unsafe { compact_debug::enable(true) };
		dbg!(super::lex(r#"foo bar _おはよう　123 0x123 -123 1.23 -1.23 "foo\nbar" (`foo\`bar`)"#));
		dbg!(super::lex(r#"-.0"#));
		let delims_str = "(()[]{})";
		dbg!(super::lex(delims_str));
		for i in 0..delims_str.len() {
			let mut s = delims_str.to_owned();
			s.replace_range(i..i+1, "");
			dbg!(super::lex(&s));
		}
		dbg!(super::lex(r#"123@ 123"#));
		panic!();
	}
}
