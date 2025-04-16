use std::cell::RefCell;
use std::rc::Rc;

use super::lex::{Cursor, CursorError};

#[derive(Debug, Clone)]
pub struct Parser<'a> {
	cursor: Cursor<'a>,
	state: Rc<RefCell<State>>,
}

#[derive(Debug)]
struct State {
	pos: usize,
	expect: Vec<Expect>,
}

#[derive(Debug, Clone)]
enum Expect {
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
	fn from(CursorError: CursorError) -> Self {
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

impl<'a> Parser<'a> {
	pub fn new(cursor: Cursor<'a>) -> Self {
		Self {
			state: Rc::new(RefCell::new(State {
				pos: cursor.pos(),
				expect: Vec::new(),
			})),
			cursor,
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

	pub fn delim(&mut self, delim: char) -> Result<Cursor<'a>> {
		self.test(delim, |p| Ok(p.cursor.delim(delim)?))
	}

	pub fn at_end(&self) -> bool {
		if self.cursor.at_end() {
			true
		} else {
			let mut state = self.state.borrow_mut();
			if state.pos == self.cursor.pos() {
				state.expect.push(Expect::Char(self.cursor.end_delim()))
			}
			false
		}
	}

	fn test<T>(&mut self, what: impl Into<Expect>, f: impl FnOnce(&mut Self) -> Result<T>) -> Result<T> {
		let mut clone = self.clone();
		match f(&mut clone) {
			Ok(v) => {
				self.cursor = clone.cursor;
				let mut state = self.state.borrow_mut();
				if state.pos < self.cursor.pos() {
					state.expect.clear();
					state.pos = self.cursor.pos();
				}
				Ok(v)
			}
			Err(e) => {
				let mut state = self.state.borrow_mut();
				if state.pos == self.cursor.pos() {
					state.expect.push(what.into());
				}
				Err(e)
			}
		}
	}

	pub fn report(&mut self, f: impl FnOnce(&mut Cursor<'a>, ParseError)) {
		let mut state = self.state.borrow_mut();
		let expect = std::mem::take(&mut state.expect);
		self.cursor.set_pos(state.pos);
		f(&mut self.cursor, ParseError { expect });
		state.pos = self.cursor.pos();
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
