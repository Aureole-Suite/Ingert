use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Label(pub u32);

pub trait Labels {
	fn defined(&self) -> Option<&Label>;
	fn referenced(&self, f: impl FnMut(&Label));
}

pub trait LabelsMut {
	fn defined_mut(&mut self) -> Option<&mut Label>;
	fn referenced_mut(&mut self, f: impl FnMut(&mut Label));
}

#[derive(Debug, snafu::Snafu)]
pub enum LabelError {
	#[snafu(display("duplicate label: {label:?}"))]
	Duplicate { label: Label },
	#[snafu(display("missing label: {label:?}"))]
	Missing { label: Label },
}

pub fn normalize<T: LabelsMut>(ops: &mut Vec<T>, mut number: u32) -> Result<u32, LabelError> {
	// Maps labels to their normalized numbers
	let mut labels = HashMap::new();
	// Set of consecutive labels; subsequent labels are erased
	let mut conflated = HashSet::new();

	let mut prev = false;
	for op in ops.iter_mut() {
		if let Some(l) = op.defined_mut() {
			if labels.insert(*l, Label(number)).is_some() {
				return Err(LabelError::Duplicate { label: *l });
			}
			if prev {
				conflated.insert(*l);
			} else {
				number += 1;
			}
			prev = true;
		} else {
			prev = false;
		}
	}

	let mut referenced = HashSet::new();
	for op in ops.iter_mut() {
		let mut missing = None;
		op.referenced_mut(|l| {
			referenced.insert(*l);
			if let Some(n) = labels.get(l) {
				*l = *n;
			} else {
				missing.get_or_insert(*l);
			}
		});
		if let Some(l) = missing {
			return Err(LabelError::Missing { label: l });
		}
	}
	for c in conflated {
		referenced.remove(&c);
	}

	ops.retain_mut(|op| op.defined_mut().is_none_or(|l| referenced.contains(&std::mem::replace(l, labels[l]))));

	Ok(number)
}

pub fn max_label<T: Labels>(ops: &[T]) -> u32 {
	ops.iter().filter_map(|op| op.defined()).map(|Label(n)| n).max().map_or(0, |n| *n + 1)
}

pub fn backrefs<T: Labels>(ops: &[T]) -> HashSet<Label> {
	let mut seen = HashSet::new();
	let mut backrefs = HashSet::new();
	for op in ops {
		if let Some(l) = op.defined() {
			seen.insert(*l);
		}
		op.referenced(|l| {
			if !seen.insert(*l) {
				backrefs.insert(*l);
			}
		});
	}
	backrefs
}
