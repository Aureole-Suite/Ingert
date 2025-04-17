use super::error::Errors;
use crate::lex::{Cursor, CursorError};

#[derive(Debug)]
pub struct Parser<'a, 'e> {
	pub cursor: Cursor<'a>,
	pub expect: Vec<Expect>,
	pub errors: &'e mut Errors,
}

#[derive(Debug)]
struct State {
	pos: usize,
	expect: Vec<Expect>,
}

#[derive(Debug, Clone)]
pub enum Expect {
	Str(&'static str),
	Char(char),
	Nt(&'static str),
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

impl std::fmt::Display for Expect {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Expect::Str(s) => write!(f, "'{}'", s),
			Expect::Char('\0') => write!(f, "end of input"),
			Expect::Char(c) => write!(f, "'{}'", c),
			Expect::Nt(k) => write!(f, "{}", k),
		}
	}
}

pub struct Error;

impl From<CursorError> for Error {
	fn from(_: CursorError) -> Self {
		Error
	}
}

pub struct ParseError {
	expect: Vec<Expect>,
}

impl std::fmt::Display for ParseError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "expected ")?;
		for (i, expect) in self.expect.iter().enumerate() {
			if i > 0 {
				write!(f, ", ")?;
			}
			write!(f, "{}", expect)?;
		}
		Ok(())
	}
}

pub type Result<T, E=Error> = std::result::Result<T, E>;

impl<'a, 'e> Parser<'a, 'e> {
	pub fn new(cursor: Cursor<'a>, errors: &'e mut Errors) -> Self {
		Self {
			cursor,
			expect: Vec::new(),
			errors,
		}
	}

	pub fn peek<'e2>(&'e2 mut self) -> Parser<'a, 'e2> {
		Parser {
			cursor: self.cursor.clone(),
			expect: Vec::new(),
			errors: self.errors,
		}
	}

	pub fn ident(&mut self) -> Result<&'a str> {
		self.test(Expect::Nt("identifier"), |p| Ok(p.cursor.ident()?))
	}

	pub fn keyword(&mut self, word: &'static str) -> Result<()> {
		self.test(word, |p| Ok(p.cursor.keyword(word)?))
	}

	pub fn string(&mut self) -> Result<&'a str> {
		self.test(Expect::Nt("string"), |p| Ok(p.cursor.string()?))
	}

	pub fn int(&mut self) -> Result<i32> {
		self.test(Expect::Nt("int"), |p| Ok(p.cursor.int()?))
	}

	pub fn float(&mut self) -> Result<f32> {
		self.test(Expect::Nt("float"), |p| Ok(p.cursor.float()?))
	}

	pub fn punct(&mut self, punct: char) -> Result<()> {
		self.test(punct, |p| Ok(p.cursor.punct(punct)?))
	}

	pub fn operator(&mut self, op: &'static str) -> Result<()> {
		self.test(op, |p| Ok(p.cursor.operator(op)?))
	}

	pub fn delim<'e2>(&'e2 mut self, delim: char) -> Result<Parser<'a, 'e2>> {
		self.delim_later(delim)
			.map(|cursor| Parser::new(cursor, self.errors))
	}

	pub fn delim_later(&mut self, delim: char) -> Result<Cursor<'a>> {
		self.test(delim, |p| Ok(p.cursor.delim(delim)?))
	}

	pub fn at_end(&mut self) -> bool {
		if self.cursor.at_end() {
			true
		} else {
			self.expect.push(Expect::Char(self.cursor.end_delim()));
			false
		}
	}

	fn test<T>(&mut self, what: impl Into<Expect>, f: impl FnOnce(&mut Parser<'a, '_>) -> Result<T>) -> Result<T> {
		let mut clone = self.peek();
		match f(&mut clone) {
			Ok(v) => {
				self.cursor = clone.cursor;
				self.expect.clear();
				Ok(v)
			}
			Err(e) => {
				self.expect.push(what.into());
				Err(e)
			}
		}
	}

	pub fn report(&mut self, f: impl FnOnce(&mut Cursor<'a>)) {
		let expect = std::mem::take(&mut self.expect);
		self.errors.error(ParseError { expect }.to_string(), self.cursor.next_span());
		f(&mut self.cursor);
	}

	pub fn line(&self) -> Option<u16> {
		self.cursor.line()
	}

	pub fn prev_span(&self) -> std::ops::Range<usize> {
		self.cursor.prev_span()
	}

	pub fn next_span(&self) -> std::ops::Range<usize> {
		self.cursor.next_span()
	}
}
