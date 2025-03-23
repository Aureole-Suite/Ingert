use std::borrow::Cow;

use crate::scp::{CallArg, CallKind};
use crate::scena::{ArgType, Called, Function, Line, Scena, Name};

pub struct Ctx {
	out: String,
}
impl Ctx {
	fn new() -> Self {
		Self {
			out: String::new(),
		}
	}

	fn word(&mut self, word: impl Into<Cow<'static, str>>) {
		self.out.push_str(&word.into());
	}

	fn ident(&mut self, name: &str) {
		self.out.push_str(name);
	}

	fn sym(&mut self, sym: &'static str) {
		self.out.push_str(sym);
	}

	fn _sym(&mut self, sym: &'static str) {
		self.out.push_str(sym);
	}

	fn sym_(&mut self, sym: &'static str) {
		self.out.push_str(sym);
	}

	fn _sym_(&mut self, sym: &'static str) {
		self.out.push_str(sym);
	}

	fn arglist<I: IntoIterator>(&mut self, args: I, mut f: impl FnMut(&mut Self, I::Item)) {
		self.sym("(");
		for (i, arg) in args.into_iter().enumerate() {
			if i != 0 {
				self.sym_(",");
			}
			f(self, arg);
		}
		self.sym(")");
	}

	fn block<I: IntoIterator>(&mut self, block: I, mut f: impl FnMut(&mut Self, I::Item)) {
		self.sym("{");
		for stmt in block {
			f(self, stmt);
			self.sym_(";");
		}
		self.sym("}");
	}

	fn value(&mut self, value: &crate::scena::Value) {
		match value {
			crate::scena::Value::Int(v) => self.word(format!("{v}")),
			crate::scena::Value::Float(v) => self.word(format!("{v:?}")),
			crate::scena::Value::String(v) => self.word(format!("{v:?}")),
		}
	}

	fn line(&mut self, line: Line) {
		if let Some(line) = line {
			self.word(format!("{line}"));
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
	}

	ctx.out
}

fn print_function(ctx: &mut Ctx, name: &str, f: &Function) {
	if f.is_prelude {
		ctx.word("prelude");
	}
	ctx.word("fn");
	ctx.ident(name);
	ctx.arglist(f.args.iter().enumerate(), |ctx, (i, arg)| {
		ctx.line(arg.line);
		ctx.ident(&format!("arg{i}"));
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
						ctx.word(format!("system[{},{}]", a, b));
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
			});
		},
		Called::Merged(true) => ctx.word("dup"),
		Called::Merged(false) => {}
	}
}

fn print_name(ctx: &mut Ctx, name: &Name) {
	if let Some(local) = name.as_local() {
		ctx.ident(local);
	} else {
		ctx.ident(&name.0);
		ctx.sym(".");
		ctx.ident(&name.1);
	}
}
