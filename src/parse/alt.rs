use super::{Error, Parser, Result};

pub struct Alt<'a, 'b, 'e, T> {
	parser: &'b mut Parser<'a, 'e>,
	value: Option<T>,
	committed: bool,
}

impl<'a, 'b, 'e, T> Alt<'a, 'b, 'e, T> {
	pub fn new(parser: &'b mut Parser<'a, 'e>) -> Self {
		Self {
			parser,
			value: None,
			committed: false,
		}
	}

	pub fn test(mut self, f: impl FnOnce(&mut TryParser<'a, '_>) -> Result<T>) -> Self {
		if !self.committed {
			let n = self.parser.errors.errors.len();
			let mut clone = TryParser {
				parser: self.parser.peek(),
				committed: false,
			};
			if let Ok(value) = f(&mut clone) {
				self.parser.cursor = clone.parser.cursor;
				self.value = Some(value);
				self.committed = true;
			} else if clone.committed {
				self.committed = true;
			} else {
				self.parser.errors.errors.truncate(n);
			}
		}
		self
	}

	pub fn finish(self) -> Result<T> {
		self.value.ok_or(Error)
	}
}

pub struct TryParser<'a, 'e> {
	parser: Parser<'a, 'e>,
	committed: bool,
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
}
