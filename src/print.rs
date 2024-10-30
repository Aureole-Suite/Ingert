use crate::{CallKind, Expr, Item, Lvalue, StackVar, Stmt};
use crate::expr::{op_prio, Type, Value};

mod layout;

use layout::Token;

#[derive(Debug, Clone)]
pub struct Settings {
	pub use_lines: bool,
	pub show_lines: bool,
}

impl Default for Settings {
	fn default() -> Self {
		Self {
			use_lines: true,
			show_lines: true,
		}
	}
}

struct Ctx {
	settings: Settings,
	indent: u32,
	next_space: bool,
	next_line: bool,
	next_indent: u32,
	next_align: Option<u16>,
	out: Vec<Token>,
}

impl Ctx {
	fn tight(&mut self) -> &mut Self {
		self.next_space = false;
		self
	}

	fn line(&mut self) -> &mut Self {
		self.next_line = true;
		self.next_indent = self.indent;
		self
	}

	fn align(&mut self, align: Option<u16>) -> &mut Self {
		assert_eq!(self.next_align, None);
		self.next_align = align;
		if self.settings.show_lines && let Some(align) = align {
			self.token(format_args!("{align}@")).tight();
		}
		self
	}

	fn semi(&mut self) -> &mut Self {
		self.tight().token(";")
	}

	fn token(&mut self, text: impl ToString) -> &mut Self {
		self.out.push(Token {
			space: self.next_space,
			line: self.next_line,
			indent: if self.next_line { self.next_indent } else { self.next_indent + 5 },
			align: self.next_align,
			text: text.to_string(),
		});
		self.next_space = true;
		self.next_line = false;
		self.next_align = None;
		self
	}
}

pub fn print(scena: &[Item], settings: Settings) -> String {
	let mut ctx = Ctx {
		settings,
		indent: 0,
		next_space: false,
		next_line: false,
		next_indent: 0,
		next_align: None,
		out: Vec::new(),
	};
	for i in scena {
		item(&mut ctx, i);
	}

	if ctx.settings.use_lines {
		layout::layout(&ctx.out)
	} else {
		layout::naive_layout(&ctx.out)
	}
}

fn item(ctx: &mut Ctx, item: &Item) {
	match item {
		Item::Global(g) => {
			ctx.line().align(g.line).token("global");
			ctx.token(&g.name).tight().token(":");
			ty(ctx, g.ty);
			ctx.semi();
		}
		Item::Function(f) => {
			ctx.line();
			if f.is_prelude {
				ctx.token("prelude");
			}
			ctx.token("function").token(&f.name);
			let mut n = 0;
			args(ctx, &f.args, |ctx, arg| {
				ctx.token(format_args!("arg{}", n)).tight().token(":");
				if arg.out {
					ctx.token("&").tight();
				}
				ty(ctx, arg.ty);
				if let Some(def) = &arg.default {
					ctx.token("=");
					value(ctx, def);
				}
				n += 1;
			});
			block(ctx, &f.body);
		}
	}
}

fn ty(ctx: &mut Ctx, ty: Type) {
	match ty {
		Type::Number => ctx.token("num"),
		Type::String => ctx.token("str"),
	};
}

fn args<T>(ctx: &mut Ctx, args: &[T], mut each: impl FnMut(&mut Ctx, &T)) {
	ctx.tight().token("(").tight();
	let mut iter = args.iter();
	if let Some(arg) = iter.next() {
		each(ctx, arg);
		for arg in iter {
			ctx.tight().token(",");
			each(ctx, arg);
		}
	}
	ctx.tight().token(")");
}

fn value(ctx: &mut Ctx, value: &Value) {
	match value {
		Value::Int(v) => ctx.token(format_args!("{v:?}")),
		Value::Float(v) => ctx.token(format_args!("{v:?}")),
		Value::String(v) => ctx.token(format_args!("{v:?}")),
	};
}

fn block(ctx: &mut Ctx, body: &[Stmt]) {
	ctx.token("{");
	ctx.indent += 1;
	for s in body {
		ctx.line();
		stmt(ctx, s);
	}
	ctx.indent -= 1;
	// TODO should this line be included if empty?
	ctx.line();
	ctx.token("}");
}

fn stmt(ctx: &mut Ctx, s: &Stmt) {
	match s {
		Stmt::Expr(e) => {
			expr(ctx, e);
			ctx.semi();
		},
		Stmt::PushVar(l, lv, e) => {
			ctx.align(*l).token("var").token(format_args!("var{}", lv.0));
			if let Some(e) = e {
				ctx.token("=");
				expr(ctx, e);
			}
			ctx.semi();
		}
		Stmt::Set(l, lv, e) => {
			ctx.align(*l);
			lvalue(ctx, lv);
			ctx.token("=");
			expr(ctx, e);
			ctx.semi();
		}
		Stmt::Debug(l, es) => {
			ctx.align(*l).token("debug");
			args(ctx, es, expr);
			ctx.semi();
		}
		Stmt::If(l, a, b, c) => {
			ctx.align(*l).token("if");
			expr(ctx, a);
			block(ctx, b);
			if let Some(c) = c {
				ctx.token("else");
				if let [s@Stmt::If(..)] = c.as_slice() {
					stmt(ctx, s);
				} else {
					block(ctx, c);
				}
			}
		}
		Stmt::While(l, a, b) => {
			ctx.align(*l).token("while");
			expr(ctx, a);
			block(ctx, b);
		}
		Stmt::Switch(l, a, b) => {
			ctx.align(*l).token("switch");
			expr(ctx, a);
			ctx.token("{");
			for (n, s) in b {
				if let Some(n) = n {
					ctx.line().token("case").token(n).tight().token(":");
				} else {
					ctx.line().token("default").tight().token(":");
				}
				ctx.indent += 1;
				for s in s {
					ctx.line();
					stmt(ctx, s);
				}
				ctx.indent -= 1;
			}
			ctx.line();
			ctx.token("}");
		}
		Stmt::Break => {
			ctx.token("break").semi();
		}
		Stmt::Continue => {
			ctx.token("continue").semi();
		}
		Stmt::Return(l, v) => {
			ctx.align(*l).token("return");
			if let Some(v) = v {
				expr(ctx, v);
			}
			ctx.semi();
		}
	}
}

fn lvalue(ctx: &mut Ctx, lv: &Lvalue) {
	match lv {
		Lvalue::Stack(s) => var(ctx, *s),
		Lvalue::Deref(s) => {
			ctx.token("*").tight();
			var(ctx, *s);
		}
		Lvalue::Global(n) => {
			ctx.token(n);
		}
	}
}

fn expr(ctx: &mut Ctx, e: &Expr) {
	expr0(ctx, e, 0);
}

fn expr0(ctx: &mut Ctx, e: &Expr, prio: u32) {
	match e {
		Expr::Value(l, v) => {
			ctx.align(*l);
			value(ctx, v)
		}
		Expr::Var(l, v) => {
			ctx.align(*l);
			lvalue(ctx, v)
		}
		Expr::Ref(l, v) => {
			ctx.align(*l).token("&").tight();
			var(ctx, *v);
		}
		Expr::Call(l, c, a) => {
			ctx.align(*l);
			call(ctx, c);
			args(ctx, a, |ctx, e| expr0(ctx, e, 0));
		}
		Expr::Unop(l, o, a) => {
			ctx.align(*l).token(o).tight();
			expr0(ctx, a, 10);
		}
		Expr::Binop(l, o, a, b) => {
			let p = op_prio(*o);
			if p < prio {
				ctx.token("(").tight();
			}
			expr0(ctx, a, p);
			ctx.align(*l).token(o);
			expr0(ctx, b, p + 1);
			if p < prio {
				ctx.tight().token(")");
			}
		}
	}
}

fn call(ctx: &mut Ctx, c: &CallKind) {
	match c {
		CallKind::System(a, b) => ctx.token(format_args!("system[{a},{b}]")),
		CallKind::Func(a) => ctx.token(a),
		CallKind::Tail(a) => ctx.token("tailcall").token(a),
	};
}

fn var(ctx: &mut Ctx, v: StackVar) {
	if v.0 < 0 {
		ctx.token(format_args!("arg{}", !v.0));
	} else {
		ctx.token(format_args!("var{}", v.0));
	}
}
