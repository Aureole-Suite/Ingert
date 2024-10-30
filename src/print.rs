use crate::{CallKind, Expr, Function, Item, Lvalue, StackVar, Stmt};
use crate::expr::{op_prio, Type, Value};

mod layout;

use layout::{Space, Token, layout};
pub use layout::Settings;

struct Ctx {
	settings: Settings,
	indent: u32,
	next_line: Option<u16>,
	next_space: Space,
	out: Vec<Token>,
}

impl Ctx {
	fn tight(&mut self) -> &mut Self {
		self.next_space = self.next_space.min(Space::Tight);
		self
	}

	fn line(&mut self) -> &mut Self {
		self.next_space = self.next_space.min(Space::Line(self.indent));
		self
	}

	fn align(&mut self, line: Option<u16>) -> &mut Self {
		assert_eq!(self.next_line, None);
		self.next_line = line;
		if self.settings.show_lines && let Some(line) = line {
			self.token(format_args!("{line}@")).tight();
		}
		self
	}

	fn semi(&mut self) -> &mut Self {
		self.tight().token(";")
	}

	fn token(&mut self, text: impl ToString) -> &mut Self {
		self.out.push(Token {
			space: self.next_space,
			line: self.next_line.take(),
			text: text.to_string(),
		});
		self.next_space = Space::Space;
		self
	}
}

pub fn print(scena: &[Item], settings: Settings) -> String {
	let mut ctx = Ctx {
		settings,
		indent: 0,
		next_line: None,
		next_space: Space::Tight,
		out: Vec::new(),
	};
	for i in scena {
		item(&mut ctx, i);
	}

	layout(&ctx.out, &ctx.settings)
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
	ctx.indent += 2;
	expr0(ctx, e, 0);
	ctx.indent -= 2;
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
