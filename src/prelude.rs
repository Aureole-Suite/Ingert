use std::collections::{HashMap, HashSet};

use crate::{CallKind, Expr, Function, Item, Lvalue, Stmt};

#[derive(Debug, Clone, Default)]
pub struct Prelude {
	order: Vec<String>,
	functions: HashMap<String, (Function, Tiebreak)>,
}

impl Prelude {
	pub fn new() -> Self {
		Self::default()
	}

	pub fn add(&mut self, items: &mut Vec<Item>) {
		let mut prelude_funcs = Vec::new();
		items.retain_mut(|item| {
			match item {
				Item::Function(f) => {
					if f.is_prelude {
						prelude_funcs.push(f.name.clone());
						if let Some(prev) = self.functions.get(&f.name) {
							if prev.0 != *f {
								tracing::warn!("{} differs from prelude", f.name);
								true
							} else {
								false
							}
						} else {
							let tie = tiebreak_score(f);
							self.functions.insert(f.name.clone(), (f.clone(), tie));
							false
						}
					} else {
						true
					}
				}
				_ => true,
			}
		});
		self.order = ordered_union(
			std::mem::take(&mut self.order),
			prelude_funcs,
			|a, b| self.functions[a].1 < self.functions[b].1,
		);
	}

	pub fn into_scena(mut self) -> Vec<Item> {
		let items = self.order.iter()
			.map(|name| self.functions.remove(name).unwrap().0)
			.map(Item::Function)
			.collect::<Vec<_>>();
		assert!(self.functions.is_empty());
		items
	}
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum Tiebreak {
	Syscall(u8, u8),
	Name(String),
}

fn tiebreak_score(v: &Function) -> Tiebreak {
	if let [Stmt::Expr(expr), Stmt::Return(_, None)] | [Stmt::Return(_, Some(expr))] = v.body.as_slice()
		&& let Expr::Call(_, CallKind::System(a, b), args) = expr
		&& args.iter().zip(0..).all(|(arg, i)| matches!(arg, Expr::Var(_, Lvalue::Stack(j)) if j.0 == i))
	{
		Tiebreak::Syscall(*a, *b)
	} else {
		Tiebreak::Name(v.name.clone())
	}
}

fn ordered_union<T: Clone + std::hash::Hash + Eq>(
	a: Vec<T>,
	b: Vec<T>,
	tiebreak: impl Fn(&T, &T) -> bool,
) -> Vec<T> {
	let in_a = HashSet::<_>::from_iter(a.iter().cloned());
	let in_b = HashSet::<_>::from_iter(b.iter().cloned());
	let mut aa = a.into_iter().peekable();
	let mut bb = b.into_iter().peekable();
	let mut out = Vec::new();
	while let Some(a) = aa.peek() && let Some(b) = bb.peek() {
		if a == b {
			out.push(aa.next().unwrap());
			bb.next();
		} else {
			let only_a = !in_b.contains(a);
			let only_b = !in_a.contains(b);
			if only_a && !only_b {
				out.push(aa.next().unwrap());
			} else if !only_a && only_b {
				out.push(bb.next().unwrap());
			} else if tiebreak(a, b) {
				out.push(aa.next().unwrap());
			} else {
				out.push(bb.next().unwrap());
			}
		}
	}
	out.extend(aa);
	out.extend(bb);
	let mut dedup = HashSet::new();
	out.retain(|v| dedup.insert(v.clone()));
	out
}

