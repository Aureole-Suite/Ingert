use super::{Error, Parser, Result};

pub struct Alt<'a, 'b, T> {
	parser: &'b mut Parser<'a>,
	value: Option<T>
}

impl<'a, 'b, T> Alt<'a, 'b, T> {
	pub fn new(parser: &'b mut Parser<'a>) -> Self {
		Self {
			parser,
			value: None
		}
	}

	pub fn test(mut self, f: impl FnOnce(&mut Parser<'a>) -> Result<T>) -> Self {
		if self.value.is_none() {
			let mut clone = self.parser.clone();
			if let Ok(value) = f(&mut clone) {
				*self.parser = clone;
				self.value = Some(value);
			}
		}
		self
	}

	pub fn finish(self) -> Result<T> {
		self.value.ok_or(Error)
	}
}
