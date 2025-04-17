use std::ops::Range;

use indexmap::IndexMap;

use crate::scp::Label;
use crate::parse::error::Errors;

struct LabelInfo {
	label: Label,
	defined: Option<Range<usize>>,
	referenced: Option<Range<usize>>,
}

pub struct Labels {
	labels: IndexMap<String, LabelInfo>,
}

impl Labels {
	pub fn new() -> Self {
		Self {
			labels: IndexMap::new(),
		}
	}

	pub fn add(&mut self, ident: &str, errors: &mut Errors, span: Range<usize>, def: bool) -> Label {
		let n = self.labels.len() as u32;
		let label = self.labels.entry(ident.to_owned())
			.or_insert_with(|| LabelInfo {
				label: Label(n),
				defined: None,
				referenced: None,
			});
		if def {
			if let Some(prev) = &label.defined {
				errors.error("label already defined", span.clone())
					.note("previously defined here", prev.clone());
			}
			label.defined = Some(span);
		} else if label.referenced.is_none() {
			label.referenced = Some(span);
		}
		label.label
	}

	pub fn finish<T: crate::labels::LabelsMut>(
		self,
		mut stmts: Vec<T>,
		errors: &mut Errors,
	) -> Vec<T> {
		let mut error = false;
		for label in self.labels.into_values() {
			match (label.defined, label.referenced) {
				(Some(_), Some(_)) => {}
				(Some(d), None) => {
					errors.warning("label not referenced", d);
				}
				(None, Some(r)) => {
					error = true;
					errors.error("label not defined", r);
				}
				(None, None) => unreachable!(),
			}
		}

		if error {
			stmts.clear();
		} else {
			crate::labels::normalize(&mut stmts, 0).expect("failed to normalize labels");
		}
		stmts
	}
}
