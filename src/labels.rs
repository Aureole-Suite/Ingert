use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Label(pub u32);

pub trait Labels {
	fn defined(&self) -> Option<&Label>;
	fn referenced(&self, f: impl FnMut(&Label));
}

pub trait LabelsMut: Labels {
	fn defined_mut(&mut self) -> Option<&mut Label>;
	fn referenced_mut(&mut self, f: impl FnMut(&mut Label));
}

#[derive(Debug, snafu::Snafu)]
pub enum LabelError {
	#[snafu(display("duplicate labels: {labels:?}"))]
	Duplicate { labels: Vec<Label> },
	#[snafu(display("missing labels: {labels:?}"))]
	Missing { labels: Vec<Label> },
}

pub fn normalize<T: LabelsMut>(ops: &mut Vec<T>, mut number: u32) -> Result<u32, LabelError> {
	let mut referenced = HashSet::new();
	for op in ops.iter() {
		op.referenced(|l| {
			referenced.insert(*l);
		});
	}

	let mut duplicate = HashSet::new();

	let mut labels = HashMap::new();
	let mut prev = false;
	for op in ops.iter() {
		if let Some(&label) = op.defined() {
			if !referenced.contains(&label) {
				continue;
			}
			if labels.insert(label, Label(number)).is_some() {
				duplicate.insert(label);
			}
			if prev {
				referenced.remove(&label);
			}
			prev = true;
		} else if prev {
			prev = false;
			number += 1;
		}
	}

	if !duplicate.is_empty() {
		let mut duplicates = duplicate.into_iter().collect::<Vec<_>>();
		duplicates.sort();
		return Err(LabelError::Duplicate { labels: duplicates });
	}

	let mut missing = referenced.iter().filter(|l| !labels.contains_key(l)).copied().collect::<Vec<_>>();
	missing.sort();
	if !missing.is_empty() {
		return Err(LabelError::Missing { labels: missing });
	}

	ops.retain_mut(|op| op.defined().is_none_or(|l| referenced.contains(l)));

	for op in ops.iter_mut() {
		if let Some(l) = op.defined_mut() {
			*l = labels[l];
		}
		op.referenced_mut(|l| {
			*l = labels[l];
		});
	}

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
