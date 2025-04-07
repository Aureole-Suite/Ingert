use std::ops::Range;

use super::lex::{RawToken, TokenKind, Tokens};

impl Tokens {
	pub fn cursor(&self) -> Cursor {
		Cursor { tokens: &self.0, range: 0..self.0.len() - 1, pos: 1 }
	}
}

#[derive(Clone)]
pub struct Cursor<'a> {
	tokens: &'a [RawToken],
	range: Range<usize>,
	pos: usize,
}

pub struct Error;

pub type Result<T, E=Error> = std::result::Result<T, E>;

impl<'a> Cursor<'a> {
	pub fn pos(&self) -> usize {
		assert!(self.pos > self.range.start);
		assert!(self.pos <= self.range.end);
		self.pos
	}

	pub fn set_pos(&mut self, pos: usize) {
		println!("{:?}, {}", self.range, pos);
		assert!(pos > self.range.start);
		assert!(pos <= self.range.end);
		self.pos = pos;
	}

	pub fn start_delim(&self) -> char {
		let TokenKind::Punct(v) = self.tokens[self.range.start].token else {
			unreachable!();
		};
		v
	}

	pub fn end_delim(&self) -> char {
		let TokenKind::Punct(v) = self.tokens[self.range.end].token else {
			unreachable!();
		};
		v
	}

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

	pub fn ident(&mut self) -> Result<&'a str> {
		match &self.tokens[self.pos].token {
			TokenKind::Ident(ident) => {
				self.pos += 1;
				Ok(ident.as_ref())
			}
			_ => Err(Error),
		}
	}

	pub fn keyword(&mut self, keyword: &'static str) -> Result<()> {
		match &self.tokens[self.pos].token {
			TokenKind::Ident(ident) if **ident == *keyword => {
				self.pos += 1;
				Ok(())
			}
			_ => Err(Error),
		}
	}

	pub fn string(&mut self) -> Result<&'a str> {
		match &self.tokens[self.pos].token {
			TokenKind::String(string) => {
				self.pos += 1;
				Ok(string.as_ref())
			}
			_ => Err(Error),
		}
	}

	pub fn int(&mut self) -> Result<i32> {
		match &self.tokens[self.pos].token {
			TokenKind::Int(int) => {
				self.pos += 1;
				Ok(*int)
			}
			_ => Err(Error),
		}
	}

	pub fn float(&mut self) -> Result<f32> {
		match &self.tokens[self.pos].token {
			TokenKind::Float(float) => {
				self.pos += 1;
				Ok(*float)
			}
			_ => Err(Error),
		}
	}

	fn punct_inner(&self, punct: char, pos: usize) -> bool {
		assert!(!"()[]{}\0".contains(punct));
		matches!(&self.tokens[pos].token, TokenKind::Punct(p) if *p == punct)
	}

	pub fn punct(&mut self, punct: char) -> Result<()> {
		if self.punct_inner(punct, self.pos) {
			self.pos += 1;
			Ok(())
		} else {
			Err(Error)
		}
	}

	pub fn operator(&mut self, operator: &'static str) -> Result<()> {
		assert!(!operator.is_empty());
		let mut pos = self.pos;
		for c in operator.chars() {
			if pos != self.pos && !self.space_span().is_empty() {
				return Err(Error);
			}
			if !self.punct_inner(c, pos) {
				return Err(Error);
			}
			pos += 1;
		}
		self.pos = pos;
		Ok(())
	}

	pub fn delim(&mut self, delim: char) -> Result<Cursor<'a>> {
		assert!("([{".contains(delim));
		let token = &self.tokens[self.pos];
		match &token.token {
			TokenKind::Punct(c) if *c == delim => {
				let range = self.pos.. self.pos + token.matched as usize;
				self.pos += token.matched as usize + 1;
				Ok(Cursor { tokens: self.tokens, pos: range.start + 1, range })
			}
			_ => Err(Error),
		}
	}

	pub fn line(&self) -> Option<u16> {
		self.tokens[self.pos].line.map(|l| l as u16)
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
