use crate::{CallKind, Expr, Function, Item, Lvalue, StackVar, Stmt};
use crate::expr::{op_prio, Type, Value};

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
	out: String,
	line: u16,
}

impl Ctx {
	fn write_fmt(&mut self, args: std::fmt::Arguments) {
		use std::fmt::Write;
		self.out.write_fmt(args).unwrap()
	}

	fn space(&mut self, line: Option<u16>, inline: bool) {
		let lines = if self.settings.use_lines {
			line.unwrap_or(self.line).saturating_sub(self.line).min(10)
		} else {
			!(inline as u16)
		};
		if lines == 0 {
			if !inline {
				write!(self, " ");
			}
		} else {
			for _ in 0..lines {
				writeln!(self)
			}
			for _ in 0..self.indent + (!inline as u32) {
				write!(self, "\t");
			}
		}

		if let Some(line) = line {
			self.line = line
		}
	}

	fn space_explicit(&mut self, line: Option<u16>, inline: bool) {
		self.space(line, inline);
		if let Some(line) = line && self.settings.show_lines {
			write!(self, "{line}@");
		}
	}
}

pub fn print(scena: &[Item], settings: Settings) -> String {
	let mut ctx = Ctx {
		settings,
		indent: 0,
		out: String::new(),
		line: 0,
	};
	for i in scena {
		item(&mut ctx, i);
	}
	ctx.out
}

fn item(ctx: &mut Ctx, item: &Item) {
	match item {
		Item::Global(g) => {
			ctx.space_explicit(g.line, false);
			write!(ctx, "global {}", g.name);
			ty(ctx, &g.name, g.ty, false);
			write!(ctx, ";");
		}
		Item::Function(f) => {
			let line = f.body.first_line();
			ctx.space(line, false);
			if f.is_prelude {
				write!(ctx, "prelude ");
			}
			write!(ctx, "function {}", f.name);
			let mut n = 0;
			args(ctx, &f.args, |ctx, arg| {
				let name = format!("arg{}", n);
				ty(ctx, &name, arg.ty, arg.out);
				if let Some(def) = &arg.default {
					write!(ctx, " = ");
					value(ctx, def);
				}
				n += 1;
			});
			block(ctx, &f.body);
		}
	}
}

fn ty(ctx: &mut Ctx, name: &str, ty: Type, is_ref: bool) {
	write!(ctx, "{name}: ");
	if is_ref {
		write!(ctx, "&");
	}
	match ty {
		Type::Number => write!(ctx, "num"),
		Type::String => write!(ctx, "str"),
	}
}

fn args<T>(ctx: &mut Ctx, args: &[T], mut each: impl FnMut(&mut Ctx, &T)) {
	write!(ctx, "(");
	let mut iter = args.iter();
	if let Some(arg) = iter.next() {
		each(ctx, arg);
		for arg in iter {
			write!(ctx, ", ");
			each(ctx, arg);
		}
	}
	write!(ctx, ")");
}

fn value(ctx: &mut Ctx, value: &Value) {
	match value {
		Value::Int(v) => write!(ctx, "{}", v),
		Value::Float(v) => write!(ctx, "{:?}", v),
		Value::String(v) => write!(ctx, "{:?}", v),
	}
}

fn block(ctx: &mut Ctx, body: &[Stmt]) {
	write!(ctx, " {{");
	ctx.indent += 1;
	for s in body {
		stmt(ctx, s);
	}
	ctx.indent -= 1;
	ctx.space(None, false);
	write!(ctx, "}}");
}

fn stmt(ctx: &mut Ctx, s: &Stmt) {
	match s {
		Stmt::Expr(e) => {
			expr(ctx, e);
			write!(ctx, ";");
		},
		Stmt::PushVar(lv, e) => {
			write!(ctx, "let var{}", lv.0);
			if let Some(e) = e {
				write!(ctx, " = ");
				expr(ctx, e);
			}
			write!(ctx, ";");
		}
		Stmt::Set(lv, e) => {
			lvalue(ctx, lv);
			write!(ctx, " = ");
			expr(ctx, e);
			write!(ctx, ";");
		}
		Stmt::Line(l) => {
			ctx.space_explicit(Some(*l), false);
		}
		Stmt::Debug(es) => {
			write!(ctx, "debug");
			args(ctx, es, expr);
		}
		Stmt::If(a, b, c) => {
			write!(ctx, "if ");
			expr(ctx, a);
			block(ctx, b);
			if let Some(c) = c {
				write!(ctx, " else ");
				if matches!(c.as_slice(), [Stmt::If(..)] | [Stmt::Line(..), Stmt::If(..)]) {
					for s in c {
						stmt(ctx, s);
					}
				} else {
					block(ctx, c);
				}
			}
		}
		Stmt::While(a, b) => {
			write!(ctx, "while ");
			expr(ctx, a);
			block(ctx, b);
		}
		Stmt::Switch(a, b) => {
			write!(ctx, "switch ");
			expr(ctx, a);
			write!(ctx, " {{");
			for (n, s) in b {
				ctx.space(None, false);
				if let Some(n) = n {
					write!(ctx, "case {n}:");
				} else {
					write!(ctx, "default:");
				}
				ctx.indent += 1;
				for s in s {
					stmt(ctx, s);
				}
				ctx.indent -= 1;
			}
			ctx.space(None, false);
			write!(ctx, "}}");
		}
		Stmt::Break => {
			write!(ctx, "break;");
		}
		Stmt::Continue => {
			write!(ctx, "continue;");
		}
		Stmt::Return(v) => {
			write!(ctx, "return");
			if let Some(v) = v {
				write!(ctx, " ");
				expr(ctx, v);
			}
			write!(ctx, ";");
		}
	}
}

fn lvalue(ctx: &mut Ctx, lv: &Lvalue) {
	match lv {
		Lvalue::Stack(s) => var(ctx, *s),
		Lvalue::Deref(s) => {
			write!(ctx, "*");
			var(ctx, *s);
		}
		Lvalue::Global(n) => write!(ctx, "{}", n),
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
			ctx.space_explicit(*l, true);
			value(ctx, v)
		}
		Expr::Var(l, v) => {
			ctx.space_explicit(*l, true);
			lvalue(ctx, v)
		}
		Expr::Ref(l, v) => {
			ctx.space_explicit(*l, true);
			write!(ctx, "&");
			var(ctx, *v);
		}
		Expr::Call(l, c, a) => {
			ctx.space_explicit(*l, true);
			call(ctx, c);
			args(ctx, a, |ctx, e| expr0(ctx, e, 0));
		}
		Expr::Unop(l, o, a) => {
			ctx.space_explicit(*l, true);
			write!(ctx, "{}", o);
			expr0(ctx, a, 10);
		}
		Expr::Binop(l, o, a, b) => {
			let p = op_prio(*o);
			if p < prio {
				write!(ctx, "(");
			}
			expr0(ctx, a, p);
			write!(ctx, " ");
			ctx.space_explicit(*l, true);
			write!(ctx, "{}", o);
			write!(ctx, " ");
			expr0(ctx, b, p + 1);
			if p < prio {
				write!(ctx, ")");
			}
		}
	}
}

fn call(ctx: &mut Ctx, c: &CallKind) {
	match c {
		CallKind::System(a, b) => write!(ctx, "system[{a},{b}]"),
		CallKind::Func(a) => write!(ctx, "{a}"),
		CallKind::Tail(a) => write!(ctx, "tailcall {a}"),
	}
}

fn var(ctx: &mut Ctx, v: StackVar) {
	if v.0 < 0 {
		write!(ctx, "arg{}", -v.0);
	} else {
		write!(ctx, "var{}", v.0);
	}
}

trait FirstLine {
	fn first_line(&self) -> Option<u16>;
}

impl<T: FirstLine> FirstLine for [T] {
	fn first_line(&self) -> Option<u16> {
		self.iter().find_map(T::first_line)
	}
}

impl<T: FirstLine> FirstLine for Vec<T> {
	fn first_line(&self) -> Option<u16> {
		self.as_slice().first_line()
	}
}

impl<T: FirstLine> FirstLine for Option<T> {
	fn first_line(&self) -> Option<u16> {
		self.as_ref().and_then(T::first_line)
	}
}

impl<T: FirstLine + ?Sized> FirstLine for Box<T> {
	fn first_line(&self) -> Option<u16> {
		T::first_line(self)
	}
}

impl<T: FirstLine + ?Sized> FirstLine for &mut T {
	fn first_line(&self) -> Option<u16> {
		T::first_line(self)
	}
}

impl FirstLine for Stmt {
	fn first_line(&self) -> Option<u16> {
		match self {
			Stmt::Expr(e) => e.first_line(),
			Stmt::PushVar(_, e) => e.first_line(),
			Stmt::Set(_, e) => e.first_line(),
			Stmt::Line(l) => Some(*l),
			Stmt::Debug(es) => es.first_line(),
			Stmt::If(a, b, c) => a.first_line().or_else(|| b.first_line()).or_else(|| c.first_line()),
			Stmt::While(a, b) => a.first_line().or_else(|| b.first_line()),
			Stmt::Switch(a, b) => a.first_line().or_else(|| b.iter().find_map(|(_, s)| s.first_line())),
			Stmt::Break => todo!(),
			Stmt::Continue => todo!(),
			Stmt::Return(v) => v.first_line(),
		}
	}
}

impl FirstLine for Expr {
	fn first_line(&self) -> Option<u16> {
		match self {
			Expr::Value(l, _) => *l,
			Expr::Var(l, _) => *l,
			Expr::Ref(l, _) => *l,
			Expr::Call(l, _, args) => l.or_else(|| args.first_line()),
			Expr::Unop(l, _, a) => l.or_else(|| a.first_line()),
			Expr::Binop(l, _, a, b) => a.first_line().or(*l).or_else(|| b.first_line()),
		}
	}
}
