use crate::diag::Diagnostic;

use super::{parser::Expect, Parser, Result};

pub struct Alt<'a, 'b, 'e, T> {
	parser: &'b mut Parser<'a, 'e>,
	value: Option<T>,
	committed: bool,

	max_pos: usize,
	max_expect: Vec<Expect>,
	max_errors: Vec<Diagnostic>,
}

impl<'a, 'b, 'e, T> Alt<'a, 'b, 'e, T> {
	pub fn new(parser: &'b mut Parser<'a, 'e>) -> Self {
		Self {
			value: None,
			committed: false,

			max_pos: parser.cursor.pos(),
			max_expect: Vec::new(),
			max_errors: Vec::new(),

			parser,
		}
	}

	pub fn test(mut self, f: impl FnOnce(&mut TryParser<'a, '_>) -> Result<T>) -> Self {
		if !self.committed {
			let n = self.parser.errors.errors.len();
			let mut clone = TryParser {
				parser: self.parser.peek(),
				committed: false,
				rejected: false,
			};

			if let Ok(value) = f(&mut clone) {
				self.value = Some(value);
				clone.commit();
			}

			if clone.committed {
				self.committed = true;
				self.parser.cursor = clone.parser.cursor;
			} else if clone.rejected {
				self.parser.errors.errors.truncate(n);
			} else if clone.parser.cursor.pos() >= self.max_pos {
				if clone.parser.cursor.pos() > self.max_pos {
					self.max_expect.clear();
					self.max_errors.clear();
				}
				self.max_pos = clone.parser.cursor.pos();
				self.max_expect.extend(clone.parser.expect);
				self.max_errors.extend(clone.parser.errors.errors.drain(n..));
			}
		}
		self
	}

	pub fn finish(self) -> Result<T> {
		if !self.committed {
			if self.parser.cursor.pos() != self.max_pos {
				self.parser.expect.clear();
			}
			self.parser.cursor.set_pos(self.max_pos);
			self.parser.expect.extend(self.max_expect);
			self.parser.errors.errors.extend(self.max_errors);
		}
		self.value.ok_or(super::parser::Error)
	}
}

pub struct TryParser<'a, 'e> {
	parser: Parser<'a, 'e>,
	committed: bool,
	rejected: bool,
}

impl<'a, 'e> std::ops::Deref for TryParser<'a, 'e> {
	type Target = Parser<'a, 'e>;

	fn deref(&self) -> &Self::Target {
		&self.parser
	}
}

impl std::ops::DerefMut for TryParser<'_, '_> {
	fn deref_mut(&mut self) -> &mut Self::Target {
		&mut self.parser
	}
}

impl TryParser<'_, '_> {
	pub fn commit(&mut self) {
		self.committed = true;
	}

	pub fn reject(&mut self) {
		self.rejected = true;
	}
}
