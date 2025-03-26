#[derive(Debug, Clone)]
pub struct Tokens(Vec<RawToken>);

#[derive(Clone)]
struct RawToken {
	start: u32,
	end: u32,
	spacing: Spacing,
	token: TokenKind,
	matched: u32,
}

impl std::fmt::Debug for RawToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let space = match self.spacing { Spacing::Alone => "+", Spacing::Joined => "" };
		f.write_fmt(format_args!("{}..{}@{:?}{}", self.start, self.end, self.token, space))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Spacing {
	Alone,
	Joined,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Delimiter {
	Paren,
	Brace,
	Bracket,
}

#[derive(Debug, Clone)]
pub enum TokenKind {
	Ident(Box<str>),
	String(Box<str>),
	Int(i64),
	Float(f64),
	Punct(char),
}

#[derive(Debug, Clone, Copy)]
pub struct Error {
	kind: ErrorKind,
	start: u32,
	end: u32,
}

#[derive(Debug, Clone, Copy)]
pub enum ErrorKind {
	InvalidIntLiteral,
	InvalidFloatLiteral,
	InvalidHexLiteral,
	UnterminatedString,
	BadEscape,
}

pub fn lex(src: &str) -> (Tokens, Vec<Error>) {
	let mut lexer = Lex::new(src);
	let mut tokens = Vec::new();
	while let Some(token) = lexer.lex() {
		tokens.push(token);
	}
	(Tokens(tokens), lexer.errors)
}

struct Lex<'a> {
	src: &'a str,
	pos: usize,
	errors: Vec<Error>,
}

impl Lex<'_> {
	fn new(src: &str) -> Lex<'_> {
		let mut lex = Lex { src, pos: 0, errors: Vec::new() };
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

	fn error(&mut self, start: usize, kind: ErrorKind) {
		self.errors.push(Error { kind, start: start as u32, end: self.pos as u32 });
	}

	fn lex(&mut self) -> Option<RawToken> {
		let start = self.pos as u32;
		let token = self.lex_token()?;
		let end = self.pos as u32;
		let spacing = self.skip_whitespace();
		Some(RawToken { start, end, spacing, token, matched: 0 })
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
				self.error(start, ErrorKind::InvalidHexLiteral);
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
						self.error(start, ErrorKind::InvalidFloatLiteral);
						return Some(TokenKind::Float(0.0))
					}
				}
			} else {
				match self.src[start..self.pos].parse() {
					Ok(n) => return Some(TokenKind::Int(n)),
					Err(_) => {
						self.error(start, ErrorKind::InvalidIntLiteral);
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
						self.error(escstart, ErrorKind::BadEscape);
					}
				}
				Some(c) => s.push(c),
				None => {
					self.error(start, ErrorKind::UnterminatedString);
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
		dbg!(super::lex(r#"foo bar _おはよう　123 0x123 -123 1.23 -1.23 "foo\nbar" (`foo\`bar`)"#));
		dbg!(super::lex(r#"- "#));
		panic!();
	}
}
