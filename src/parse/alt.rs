use super::{Error, Parser, Result};

pub struct Alt<'a, 'b, T> {
	parser: &'b mut Parser<'a>,
	value: Option<T>,
	committed: bool,
}

impl<'a, 'b, T> Alt<'a, 'b, T> {
	pub fn new(parser: &'b mut Parser<'a>) -> Self {
		Self {
			parser,
			value: None,
			committed: false,
		}
	}

	pub fn test(mut self, f: impl FnOnce(&mut TryParser<'a>) -> Result<T>) -> Self {
		if !self.committed {
			let mut clone = TryParser {
				parser: self.parser.clone(),
				committed: false,
			};
			if let Ok(value) = f(&mut clone) {
				*self.parser = clone.parser;
				self.value = Some(value);
				self.committed = true;
			}
			if clone.committed {
				self.committed = true;
			}
		}
		self
	}

	pub fn finish(self) -> Result<T> {
		self.value.ok_or(Error)
	}
}

pub struct TryParser<'a> {
	parser: Parser<'a>,
	committed: bool,
}

impl<'a> std::ops::Deref for TryParser<'a> {
	type Target = Parser<'a>;

	fn deref(&self) -> &Self::Target {
		&self.parser
	}
}

impl std::ops::DerefMut for TryParser<'_> {
	fn deref_mut(&mut self) -> &mut Self::Target {
		&mut self.parser
	}
}

impl TryParser<'_> {
	pub fn commit(&mut self) {
		self.committed = true;
	}
}
