use std::borrow::Cow;

use crate::scp::{CallArg, CallKind};
use crate::scena::{ArgType, Called, Function, Line, Scena, Name};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Space {
	None,
	Inline,
	Block(usize),
}

struct Ctx {
	out: String,
	space: Space,
	indent: usize,
}

impl Ctx {
	fn new() -> Self {
		Self {
			out: String::new(),
			space: Space::None,
			indent: 0,
		}
	}

	fn token(&mut self, word: impl Into<Cow<'static, str>>) {
		self.do_space(true);
		self.out.push_str(&word.into());
		self.set_space(Space::Inline);
	}

	fn word(&mut self, word: &'static str) {
		self.token(word);
	}

	fn ident(&mut self, name: String) {
		self.token(name);
	}

	fn sym(&mut self, sym: &'static str) {
		self.do_space(false);
		self.out.push_str(sym);
		self.set_space(Space::None);
	}

	fn _sym(&mut self, sym: &'static str) {
		self.do_space(true);
		self.out.push_str(sym);
		self.set_space(Space::None);
	}

	fn sym_(&mut self, sym: &'static str) {
		self.do_space(false);
		self.out.push_str(sym);
		self.set_space(Space::Inline);
	}

	fn _sym_(&mut self, sym: &'static str) {
		self.do_space(true);
		self.out.push_str(sym);
		self.set_space(Space::Inline);
	}

	fn do_space(&mut self, inline: bool) {
		match self.space {
			Space::None => {}
			Space::Inline => {
				if inline {
					self.out.push(' ');
				}
			}
			Space::Block(n) => {
				for _ in 0..=n {
					self.out.push('\n');
				}
				for _ in 0..self.indent {
					self.out.push('\t');
				}
			}
		}
		self.space = Space::None;
	}

	fn set_space(&mut self, space: Space) {
		self.space = self.space.max(space);
	}

	fn arglist<I: IntoIterator>(&mut self, args: I, mut f: impl FnMut(&mut Self, I::Item)) {
		self.sym("(");
		for (i, arg) in args.into_iter().enumerate() {
			if i != 0 {
				self.sym_(",");
			}
			f(self, arg);
		}
		self.sym_(")");
	}

	fn block<I: IntoIterator>(&mut self, block: I, mut f: impl FnMut(&mut Self, I::Item)) {
		self._sym_("{");
		self.indent += 1;
		for stmt in block {
			self.set_space(Space::Block(0));
			f(self, stmt);
			self.set_space(Space::Block(0));
		}
		self.indent -= 1;
		self._sym_("}");
	}

	fn value(&mut self, value: &crate::scena::Value) {
		match value {
			crate::scena::Value::Int(v) => self.token(format!("{v}")),
			crate::scena::Value::Float(v) => self.token(format!("{v:?}")),
			crate::scena::Value::String(v) => self.token(format!("{v:?}")),
		}
	}

	fn line(&mut self, line: Line) {
		if let Some(line) = line {
			self.token(format!("{line}"));
			self.sym("@");
		}
	}
}

pub fn print(scena: &Scena) -> String {
	let mut ctx = Ctx::new();
	for (k, v) in &scena.globals {
		todo!()
	}

	for (k, v) in &scena.functions {
		print_function(&mut ctx, k, v);
		ctx.set_space(Space::Block(1));
	}

	ctx.out
}

fn print_function(ctx: &mut Ctx, name: &str, f: &Function) {
	if f.is_prelude {
		ctx.word("prelude");
	}
	ctx.word("fn");
	ctx.ident(name.to_owned());
	ctx.arglist(f.args.iter().enumerate(), |ctx, (i, arg)| {
		ctx.line(arg.line);
		ctx.ident(format!("arg{i}"));
		ctx.sym_(":");
		match arg.ty {
			ArgType::Number => ctx.word("num"),
			ArgType::String => ctx.word("str"),
			ArgType::NumberRef => ctx.word("&num"), // not a word but whatever
		}

		if let Some(default) = &arg.default {
			ctx._sym_("=");
			ctx.value(default);
		}
	});

	match &f.called {
		Called::Raw(calls) => {
			ctx.word("calls");
			ctx.block(calls, |ctx, call| {
				match &call.kind {
					CallKind::Normal(name) => print_name(ctx, name),
					CallKind::Tailcall(name) => {
						ctx.word("become");
						print_name(ctx, name);
					}
					CallKind::Syscall(a, b) => {
						ctx.token(format!("system[{},{}]", a, b));
					}
				}
				ctx.arglist(&call.args, |ctx, arg| {
					match arg {
						CallArg::Value(value) => ctx.value(value),
						CallArg::Call => ctx.word("call"),
						CallArg::Var => ctx.word("var"),
						CallArg::Expr => ctx.word("expr"),
					}
				});
				ctx.sym_(";");
			});
		},
		Called::Merged(true) => ctx.word("dup"),
		Called::Merged(false) => {}
	}
}

fn print_name(ctx: &mut Ctx, name: &Name) {
	if let Some(local) = name.as_local() {
		ctx.ident(local.clone());
	} else {
		ctx.ident(name.0.clone());
		ctx.sym(".");
		ctx.ident(name.1.clone());
	}
}
