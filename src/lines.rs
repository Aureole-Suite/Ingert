use std::collections::HashSet;

use crate::scp::{Op, Label};

#[derive(Debug, Clone, PartialEq)]
pub enum Why {
	Explicit,
	Prefix,
	Fill,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Line {
	Spurious(u16),
	Label(Label),
	Op {
		line: u16,
		why: Why,
		op: Op,
	}
}

pub fn lines(tokens: &[(Label, Op)]) -> Vec<Line> {
	let mut out = Vec::new();
	let all_labels = tokens.iter().filter_map(|(_, op)| match op {
		Op::Goto(l) | Op::If(l) | Op::If2(l) | Op::_25(l) => Some(*l),
		_ => None,
	}).collect::<HashSet<_>>();

	let mut line = 0;
	let mut lines = Vec::new();
	let mut iter = tokens.iter().filter(|a| !matches!(a.1, Op::Pop(_))).peekable();
	let mut explicit = false;
	while let Some((label, op)) = iter.next() {
		if all_labels.contains(label) {
			out.push(Line::Label(*label));
			// println!("  {:?}", out.last().unwrap());
		}

		if let Op::Line(line) = op
			&& let Some((_, Op::Line(_))) = iter.peek()
		{
			lines.push(*line);
			// println!("Line({}) push", line);
			continue
		}

		if let Op::Line(line_) = op {
			line = *line_;
			explicit = true;
			// println!("Line({})", line_);
			continue;
		}

		macro_rules! test { ($pat:pat, $($tt:tt)*) => { if let $pat = op && $($tt)* { true } else { false } } }
		if let Some(p) = lines.last().copied() && (
			test!(Op::Unop(_), p != line)
			|| test!(Op::Binop(_) | Op::GetTemp(0), let Some((_, Op::Line(next))) = iter.peek() && p == *next)
			|| test!(Op::CallSystem(..), let Some((_, Op::Line(next))) = iter.peek() && line < p && p < *next)
			|| test!(Op::If(_) | Op::SetGlobal(_) | Op::Debug(_), let Some((_, Op::Line(_))) = iter.peek())
			|| test!(Op::Return, let Some((_, Op::Line(_))) | None = iter.peek())
		) {
			lines.pop();
			out.push(Line::Op {
				line: p,
				why: Why::Prefix,
				op: op.clone(),
			});
		} else {
			out.push(Line::Op {
				line,
				why: if explicit { Why::Explicit } else { Why::Fill },
				op: op.clone(),
			});
		}
		// println!("  {:?}", out.last().unwrap());
		explicit = false;
	}
	// These are typically empty, but if they aren't, we should still emit them
	for line in lines {
		out.push(Line::Spurious(line));
		println!("{:?}", out.last().unwrap());
	}
	out
}
