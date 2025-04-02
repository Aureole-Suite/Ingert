use std::ops::Range;

use super::lex::{RawToken, TokenKind, Tokens};

impl Tokens {
	pub fn cursor(&self) -> Cursor {
		Cursor { tokens: &self.0, range: 1..self.0.len() - 1, pos: 1 }
	}
}

#[derive(Clone)]
pub struct Cursor<'a> {
	tokens: &'a [RawToken],
	range: Range<usize>,
	pos: usize,
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

	pub fn ident(&mut self) -> Option<String> {
		match &self.tokens[self.pos].token {
			TokenKind::Ident(ident) => {
				self.pos += 1;
				Some(ident.to_string())
			}
			_ => None,
		}
	}

	pub fn keyword(&mut self, keyword: &str) -> Option<()> {
		match &self.tokens[self.pos].token {
			TokenKind::Ident(ident) if **ident == *keyword => {
				self.pos += 1;
				Some(())
			}
			_ => None,
		}
	}

	pub fn string(&mut self) -> Option<String> {
		match &self.tokens[self.pos].token {
			TokenKind::String(string) => {
				self.pos += 1;
				Some(string.to_string())
			}
			_ => None,
		}
	}

	pub fn int(&mut self) -> Option<i32> {
		match &self.tokens[self.pos].token {
			TokenKind::Int(int) => {
				self.pos += 1;
				Some(*int)
			}
			_ => None,
		}
	}

	pub fn float(&mut self) -> Option<f32> {
		match &self.tokens[self.pos].token {
			TokenKind::Float(float) => {
				self.pos += 1;
				Some(*float)
			}
			_ => None,
		}
	}

	pub fn punct(&mut self, punct: char) -> Option<()> {
		assert!(!"()[]{}\0".contains(punct));
		match &self.tokens[self.pos].token {
			TokenKind::Punct(p) if *p == punct => {
				self.pos += 1;
				Some(())
			}
			_ => None,
		}
	}

	pub fn operator(&mut self, operator: &str) -> Option<()> {
		assert!(!operator.is_empty());
		let start = self.pos;
		let mut chars = operator.chars();
		let ok = self.punct(chars.next().unwrap()).is_some()
			&& chars.all(|c| self.space_span().is_empty() && self.punct(c).is_some());
		if ok {
			Some(())
		} else {
			self.pos = start;
			None
		}
	}

	pub fn delim(&mut self, delim: char) -> Option<Cursor<'a>> {
		assert!("([{".contains(delim));
		let token = &self.tokens[self.pos];
		match &token.token {
			TokenKind::Punct(c) if *c == delim => {
				self.pos += 1;
				let range = self.pos .. self.pos + token.matched as usize - 1;
				let sub = Cursor { tokens: self.tokens, range, pos: self.pos };
				self.pos += token.matched as usize;
				Some(sub)
			}
			_ => None,
		}
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
