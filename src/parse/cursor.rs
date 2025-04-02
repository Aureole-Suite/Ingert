use std::ops::Range;

use super::lex::{RawToken, TokenKind, Tokens};

impl Tokens {
	pub fn cursor(&self) -> Cursor {
		Cursor { tokens: &self.0, range: 1..self.0.len() - 1, pos: 1, expect: Vec::new() }
	}
}

#[derive(Clone)]
pub struct Cursor<'a> {
	tokens: &'a [RawToken],
	range: Range<usize>,
	pos: usize,
	expect: Vec<Expect>
}

#[derive(Debug, Clone)]
enum Expect {
	Str(&'static str),
	Char(char),
}

impl From<&'static str> for Expect {
	fn from(s: &'static str) -> Self {
		Expect::Str(s)
	}
}

impl From<char> for Expect {
	fn from(c: char) -> Self {
		Expect::Char(c)
	}
}

pub struct PendingError<'a>(Cursor<'a>);

type PResult<'a, T> = std::result::Result<T, PendingError<'a>>;
pub type Result<T, E=Error> = std::result::Result<T, E>;

#[derive(Debug)]
pub struct Error {
	pub span: Range<usize>,
	pub expect: Vec<Expect>,
}

impl From<PendingError<'_>> for Error {
	fn from(PendingError(cursor): PendingError) -> Self {
		Error { span: cursor.next_span(), expect: cursor.expect }
	}
}

impl std::fmt::Display for Expect {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Expect::Str(s) => write!(f, "{}", s),
			Expect::Char(c) => write!(f, "{}", c),
		}
	}
}

impl std::fmt::Display for Error {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "expected ")?;
		if self.expect.len() == 1 {
			write!(f, "{}", self.expect[0])?;
		} else {
			write!(f, "one of ")?;
			for (i, expect) in self.expect.iter().enumerate() {
				if i != 0 {
					write!(f, ", ")?;
				}
				write!(f, "{}", expect)?;
			}
		}
		write!(f, " at {:?}", self.span)
	}
}

impl<'a> Cursor<'a> {
	pub fn at_end(&self) -> bool {
		self.pos == self.range.end
	}

	pub fn prev_span(&self) -> Range<usize> {
		self.tokens[self.pos - 1].span()
	}

	pub fn next_span(&self) -> Range<usize> {
		self.tokens[self.pos].span()
	}

	pub fn space_span(&self) -> Range<usize> {
		self.prev_span().end..self.next_span().start
	}

	pub fn skip_any(&mut self) {
		assert!(!self.at_end());
		if self.tokens[self.pos].matched > 0 {
			self.pos += self.tokens[self.pos].matched as usize;
		}
		self.pos += 1;
	}

	// End is always a Punct(')}]\0'), so we don't need to check that for most cases

	pub fn ident(&mut self) -> PResult<'a, String> {
		match &self.tokens[self.pos].token {
			TokenKind::Ident(ident) => self.ok(ident.to_string()),
			_ => self.err("<ident>"),
		}
	}

	pub fn keyword(&mut self, keyword: &'static str) -> PResult<'a, ()> {
		match &self.tokens[self.pos].token {
			TokenKind::Ident(ident) if **ident == *keyword => self.ok(()),
			_ => self.err(keyword),
		}
	}

	pub fn string(&mut self) -> PResult<'a, String> {
		match &self.tokens[self.pos].token {
			TokenKind::String(string) => self.ok(string.to_string()),
			_ => self.err("<string>"),
		}
	}

	pub fn int(&mut self) -> PResult<'a, i32> {
		match &self.tokens[self.pos].token {
			TokenKind::Int(int) => self.ok(*int),
			_ => self.err("<int>"),
		}
	}

	pub fn float(&mut self) -> PResult<'a, f32> {
		match &self.tokens[self.pos].token {
			TokenKind::Float(float) => self.ok(*float),
			_ => self.err("<float>"),
		}
	}

	fn punct_inner(&self, punct: char, pos: usize) -> bool {
		assert!(!"()[]{}\0".contains(punct));
		matches!(&self.tokens[pos].token, TokenKind::Punct(p) if *p == punct)
	}

	pub fn punct(&mut self, punct: char) -> PResult<'a, ()> {
		if self.punct_inner(punct, self.pos) {
			self.ok(())
		} else {
			self.err(punct)
		}
	}

	pub fn operator(&mut self, operator: &'static str) -> PResult<'a, ()> {
		assert!(!operator.is_empty());
		let mut pos = self.pos;
		for c in operator.chars() {
			if pos != self.pos && !self.space_span().is_empty() {
				return self.err(operator);
			}
			if !self.punct_inner(c, pos) {
				return self.err(operator);
			}
			pos += 1;
		}
		self.pos = pos;
		self.expect.clear();
		Ok(())
	}

	pub fn delim(&mut self, delim: char) -> PResult<'a, Cursor<'a>> {
		assert!("([{".contains(delim));
		let token = &self.tokens[self.pos];
		match &token.token {
			TokenKind::Punct(c) if *c == delim => {
				let range = self.pos + 1.. self.pos + token.matched as usize;
				self.pos += token.matched as usize;
				self.ok(Cursor { tokens: self.tokens, pos: range.start, range, expect: Vec::new() })
			}
			_ => self.err(delim),
		}
	}

	fn ok<T>(&mut self, value: T) -> PResult<'a, T> {
		self.pos += 1;
		self.expect.clear();
		Ok(value)
	}

	fn err<T>(&mut self, expect: impl Into<Expect>) -> PResult<'a, T> {
		self.expect.push(expect.into());
		Err(PendingError(self.clone()))
	}

	pub fn fail(&self) -> PResult<'a, !> {
		Err(PendingError(self.clone()))
	}
}

impl std::fmt::Debug for Cursor<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		f.debug_tuple("Cursor").field(&Draw(self)).finish()
	}
}

struct Draw<'a>(&'a Cursor<'a>);

impl std::fmt::Debug for Draw<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		let mut debug = f.debug_list();
		let mut pos = self.0.pos;
		while pos < self.0.range.end {
			debug.entry(&self.0.tokens[pos]);
			if self.0.tokens[pos].matched > 0 {
				pos += self.0.tokens[pos].matched as usize;
			}
			pos += 1;
		}
		debug.finish()
	}
}
