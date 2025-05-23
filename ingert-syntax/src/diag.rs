use std::ops::Range;
use std::fmt::Debug;

pub struct Errors {
	pub errors: Vec<Diagnostic>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Severity {
	Info,
	Warning,
	Error,
	Fatal,
}

pub struct Diagnostic {
	pub severity: Severity,
	pub main: Note,
	pub notes: Vec<Note>,
}

impl Diagnostic {
	pub fn sort_key(&self) -> impl Ord {
		(self.main.span.start, self.severity)
	}
}

pub struct Note {
	pub desc: String,
	pub span: Range<usize>,
}

impl Errors {
	pub fn new() -> Self {
		Self {
			errors: Vec::new(),
		}
	}

	pub fn info(&mut self, desc: impl Into<String>, span: Range<usize>) -> &mut Diagnostic {
		self.push(Severity::Info, desc, span)
	}

	pub fn warning(&mut self, desc: impl Into<String>, span: Range<usize>) -> &mut Diagnostic {
		self.push(Severity::Warning, desc, span)
	}

	pub fn error(&mut self, desc: impl Into<String>, span: Range<usize>) -> &mut Diagnostic {
		self.push(Severity::Error, desc, span)
	}

	pub fn fatal(&mut self, desc: impl Into<String>, span: Range<usize>) -> &mut Diagnostic {
		self.push(Severity::Fatal, desc, span)
	}

	fn push(&mut self, severity: Severity, desc: impl Into<String>, span: Range<usize>) -> &mut Diagnostic {
		self.errors.push(Diagnostic {
			severity,
			main: Note { desc: desc.into(), span },
			notes: Vec::new(),
		});
		self.errors.last_mut().unwrap()
	}

	pub fn max_severity(&self) -> Severity {
		self.errors.iter().map(|e| e.severity).max().unwrap_or(Severity::Info)
	}
}

impl Diagnostic {
	pub fn note(&mut self, desc: impl Into<String>, span: Range<usize>) -> &mut Self {
		self.notes.push(Note { desc: desc.into(), span });
		self
	}
}

impl Default for Errors {
	fn default() -> Self {
		Self::new()
	}
}

impl Debug for Errors {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.debug_struct("Errors").field("errors", &self.errors).finish_non_exhaustive()
	}
}

impl Debug for Diagnostic {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let mut tup = f.debug_tuple(&format!("{:?}", self.severity));
		tup.field(&self.main);
		for note in &self.notes {
			tup.field(note);
		}
		tup.finish()
	}
}

impl Debug for Note {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.write_fmt(format_args!("{:?}:{}", self.span, self.desc))
	}
}
