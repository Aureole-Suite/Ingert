use std::borrow::Cow;

use crate::scp::{CallArg, CallKind, Label, Op};
use crate::scena::{ArgType, Binop, Body, Called, Expr, FlatStmt, Function, Line, Name, Place, Scena, Stmt, Unop};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
enum Space {
	#[default]
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

	fn token(&mut self, word: impl Into<Cow<'static, str>>) -> &mut Self {
		self.do_space(true);
		self.out.push_str(&word.into());
		self.set_space(Space::Inline);
		self
	}

	fn word(&mut self, word: &'static str) -> &mut Self {
		self.token(word)
	}

	fn ident(&mut self, name: String) -> &mut Self {
		self.token(name)
	}

	fn sym(&mut self, sym: &'static str) -> &mut Self {
		self.do_space(false);
		self.out.push_str(sym);
		self.set_space(Space::None);
		self
	}

	fn _sym(&mut self, sym: &'static str) -> &mut Self {
		self.do_space(true);
		self.out.push_str(sym);
		self.set_space(Space::None);
		self
	}

	fn sym_(&mut self, sym: &'static str) -> &mut Self {
		self.do_space(false);
		self.out.push_str(sym);
		self.set_space(Space::Inline);
		self
	}

	fn _sym_(&mut self, sym: &'static str) -> &mut Self {
		self.do_space(true);
		self.out.push_str(sym);
		self.set_space(Space::Inline);
		self
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

	fn value(&mut self, value: &crate::scena::Value) -> &mut Self {
		match value {
			crate::scena::Value::Int(v) => self.token(format!("{v}")),
			crate::scena::Value::Float(v) => self.token(format!("{v:?}")),
			crate::scena::Value::String(v) => self.token(format!("{v:?}")),
		}
	}

	fn name(&mut self, name: &Name) -> &mut Self {
		if let Some(local) = name.as_local() {
			self.ident(local.clone())
		} else {
			self.ident(name.0.clone()).sym(".").ident(name.1.clone())
		}
	}

	fn label(&mut self, label: &Label) -> &mut Self {
		self._sym("$").ident(format!("L{}", label.0))
	}


	fn line(&mut self, line: &Line) {
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
		ctx.line(&arg.line);
		ctx.ident(format!("arg{i}"));
		ctx.sym_(":");
		match arg.ty {
			ArgType::Number => ctx.word("num"),
			ArgType::String => ctx.word("str"),
			ArgType::NumberRef => ctx._sym("&").word("num"),
		};

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
					CallKind::Normal(name) => ctx.name(name),
					CallKind::Tailcall(name) => ctx.word("become").name(name),
					CallKind::Syscall(a, b) => ctx.token(format!("system[{},{}]", a, b)),
				};
				ctx.arglist(&call.args, |ctx, arg| {
					match arg {
						CallArg::Value(value) => ctx.value(value),
						CallArg::Call => ctx.word("call"),
						CallArg::Var => ctx.word("var"),
						CallArg::Expr => ctx.word("expr"),
					};
				});
				ctx.sym_(";");
			});
		},
		Called::Merged(true) => {
			ctx.word("dup");
		},
		Called::Merged(false) => {}
	}

	match &f.body {
		Body::Asm(ops) => {
			ctx.word("asm");
			ctx.block(ops, print_op);
		}
		Body::Flat(stmts) => {
			ctx.word("flat");
			ctx.block(stmts, print_flat_stmt);
		}
		Body::Tree(stmts) => print_block(ctx, stmts)
	}
}

fn print_op(ctx: &mut Ctx, op: &Op) {
	match op {
		Op::Label(label) => {
			ctx.indent -= 1;
			ctx.label(label).sym_(":");
			ctx.indent += 1;
			return;
		}
		Op::Push(value) => ctx.word("push").value(value),
		Op::Pop(n) => ctx.word("pop").token(n.to_string()),
		Op::PushNull => ctx.word("push_null"),
		Op::GetVar(slot) => ctx.word("get_var").token(slot.0.to_string()),
		Op::GetRef(slot) => ctx.word("get_ref").token(slot.0.to_string()),
		Op::PushRef(slot) => ctx.word("push_ref").token(slot.0.to_string()),
		Op::SetVar(slot) => ctx.word("set_var").token(slot.0.to_string()),
		Op::SetRef(slot) => ctx.word("set_ref").token(slot.0.to_string()),
		Op::GetGlobal(name) => ctx.word("get_global").ident(name.clone()),
		Op::SetGlobal(name) => ctx.word("set_global").ident(name.clone()),
		Op::GetTemp(n) => ctx.word("get_temp").token(n.to_string()),
		Op::SetTemp(n) => ctx.word("set_temp").token(n.to_string()),
		Op::Binop(binop) => ctx.word(match binop {
			Binop::Add => "add",
			Binop::Sub => "sub",
			Binop::Mul => "mul",
			Binop::Div => "div",
			Binop::Mod => "mod",
			Binop::Eq => "eq",
			Binop::Ne => "ne",
			Binop::Gt => "gt",
			Binop::Ge => "ge",
			Binop::Lt => "lt",
			Binop::Le => "le",
			Binop::BitAnd => "bit_and",
			Binop::BitOr => "bit_or",
			Binop::BoolAnd => "bool_and",
			Binop::BoolOr => "bool_or",
		}),
		Op::Unop(unop) => ctx.word(match unop {
			Unop::Neg => "neg",
			Unop::BoolNot => "bool_not",
			Unop::BitNot => "bit_not",
		}),
		Op::Jnz(label) => ctx.word("jnz").label(label),
		Op::Jz(label) => ctx.word("jz").label(label),
		Op::Goto(label) => ctx.word("goto").label(label),
		Op::CallLocal(b) => ctx.word("call_local").ident(b.clone()),
		Op::CallExtern(name, n) => ctx.word("call_extern").name(name).token(n.to_string()),
		Op::CallTail(name, b) => ctx.word("call_tail").name(name).token(b.to_string()),
		Op::CallSystem(a, b, n) => ctx.word("call_system").token(a.to_string()).token(b.to_string()).token(n.to_string()),
		Op::PrepareCallLocal(label) => ctx.word("prepare_call_local").label(label),
		Op::PrepareCallExtern(label) => ctx.word("prepare_call_extern").label(label),
		Op::Return => ctx.word("return"),
		Op::Line(l) => ctx.word("line").token(l.to_string()),
		Op::Debug(n) => ctx.word("debug").token(n.to_string()),
	};
}

fn print_flat_stmt(ctx: &mut Ctx, stmt: &FlatStmt) {
	match stmt {
		FlatStmt::Label(label) => {
			ctx.indent -= 1;
			ctx.label(label).sym_(":");
			ctx.indent += 1;
			return;
		}
		FlatStmt::Expr(expr) => print_expr(ctx, expr),
		FlatStmt::Set(l, place, expr) => {
			ctx.line(l);
			print_place(ctx, place);
			ctx._sym_("=");
			print_expr(ctx, expr);
		}
		FlatStmt::Return(l, expr, pop) => {
			ctx.line(l);
			ctx.token(format!("return[{pop}]"));
			if let Some(expr) = expr {
				print_expr(ctx, expr);
			}
		}
		FlatStmt::If(l, expr, label) => {
			ctx.line(l);
			ctx.word("if");
			print_expr(ctx, expr);
			ctx.label(label);
		}
		FlatStmt::Goto(label, pop) => {
			if *pop != 0 {
				ctx.token(format!("goto[{pop}]"));
			} else {
				ctx.word("goto");
			}
			ctx.label(label);
		}
		FlatStmt::Switch(l, expr, items, label) => {
			ctx.line(l);
			ctx.word("switch");
			print_expr(ctx, expr);
			ctx.block(items, |ctx, (value, label)| {
				ctx.token(value.to_string());
				ctx._sym_("=>");
				ctx.label(label);
				ctx.sym_(";");
			});
			ctx.label(label);
		}
		FlatStmt::PushVar(l) => {
			ctx.line(l);
			ctx.word("push_var");
		}
		FlatStmt::PopVar(n) => {
			ctx.token(format!("pop_var[{n}]"));
		}
		FlatStmt::Debug(l, exprs) => {
			ctx.line(l);
			ctx.word("debug");
			ctx.arglist(exprs, print_expr);
		}
		FlatStmt::Tailcall(l, name, exprs, pop) => {
			ctx.line(l);
			ctx.token(format!("tailcall[{pop}]"));
			ctx.name(name);
			ctx.arglist(exprs, print_expr);
		}
	}
	ctx.sym_(";");
}

fn print_block(ctx: &mut Ctx, stmts: &[Stmt]) {
	ctx.block(stmts, print_stmt);
}

fn print_stmt(ctx: &mut Ctx, stmt: &Stmt) {
	match stmt {
		Stmt::Expr(expr) => {
			print_expr(ctx, expr);
		}
		Stmt::Set(l, place, expr) => {
			ctx.line(l);
			print_place(ctx, place);
			ctx._sym_("=");
			print_expr(ctx, expr);
		}
		Stmt::Return(l, expr) => {
			ctx.line(l);
			ctx.word("return");
			if let Some(expr) = expr {
				print_expr(ctx, expr);
			}
		}
		Stmt::If(l, expr, then, els) => {
			ctx.line(l);
			ctx.word("if");
			print_expr(ctx, expr);
			print_block(ctx, then);
			if let Some(els) = els {
				ctx.word("else");
				print_block(ctx, els);
			}
		}
		Stmt::While(l, expr, body) => {
			ctx.line(l);
			ctx.word("while");
			print_expr(ctx, expr);
			print_block(ctx, body);
		}
		Stmt::Switch(l, expr, cases) => {
			ctx.line(l);
			ctx.word("switch");
			print_expr(ctx, expr);
			ctx.block(cases, |ctx, (value, body)| {
				match value {
					Some(value) => ctx.word("case").token(value.to_string()),
					None => ctx.word("default"),
				}.sym(":");
				ctx.indent += 1;
				for stmt in body {
					ctx.set_space(Space::Block(0));
					print_stmt(ctx, stmt);
				}
				ctx.indent -= 1;
			});
		}
		Stmt::Block(stmts) => {
			print_block(ctx, stmts);
		}
		Stmt::Break => {
			ctx.word("break");
		}
		Stmt::Continue => {
			ctx.word("continue");
		}
		Stmt::PushVar(l) => {
			ctx.line(l);
			ctx.word("var");
		}
		Stmt::Debug(l, exprs) => {
			ctx.line(l);
			ctx.word("debug");
			ctx.arglist(exprs, print_expr);
		}
		Stmt::Tailcall(l, name, exprs) => {
			ctx.line(l);
			ctx.word("tailcall");
			ctx.name(name);
			ctx.arglist(exprs, print_expr);
		}
	}
}

fn print_place(ctx: &mut Ctx, place: &Place) {
	fn var(v: u32) -> String {
		format!("var{v}")
	}
	match place {
		Place::Var(n) => ctx.ident(var(*n)),
		Place::Deref(n) => ctx._sym("*").ident(var(*n)),
		Place::Global(name) => ctx.ident(name.clone()),
	};
}

fn print_expr(ctx: &mut Ctx, expr: &Expr) {
	print_expr_prec(ctx, expr, 0);

	fn print_expr_prec(ctx: &mut Ctx, expr: &Expr, prec: u32) {
		match expr {
			Expr::Value(l, value) => {
				ctx.line(l);
				ctx.value(value);
			}
			Expr::Var(l, place) => {
				ctx.line(l);
				print_place(ctx, place);
			}
			Expr::Ref(l, n) => {
				ctx.line(l);
				ctx._sym("&");
				print_place(ctx, &Place::Var(*n));
			}
			Expr::Call(l, name, exprs) => {
				ctx.line(l);
				ctx.name(name);
				ctx.arglist(exprs, print_expr);
			}
			Expr::Syscall(l, a, b, exprs) => {
				ctx.line(l);
				ctx.token(format!("system[{a},{b}]"));
				ctx.arglist(exprs, print_expr);
			}
			Expr::Unop(l, unop, expr) => {
				ctx.line(l);
				if *unop == Unop::Neg && matches!(**expr, Expr::Value(..)) {
					ctx.sym("(");
					print_expr_prec(ctx, expr, 10);
					ctx.sym_(")");
				} else {
					let op = match unop {
						Unop::Neg => "-",
						Unop::BoolNot => "!",
						Unop::BitNot => "~",
					};
					ctx._sym(op);
					print_expr_prec(ctx, expr, 10);
				}
			}
			Expr::Binop(l, binop, expr, expr1) => {
				let (op, op_prio) = binop_prio(*binop);
				if op_prio < prec {
					ctx._sym("(");
				}
				print_expr_prec(ctx, expr, op_prio);
				ctx.line(l);
				ctx._sym_(op);
				print_expr_prec(ctx, expr1, op_prio + 1);
				if op_prio < prec {
					ctx.sym_(")");
				}
			}
		}
	}
}

fn binop_prio(op: Binop) -> (&'static str, u32) {
	use Binop::*;
	match op {
		Mul => ("*", 7),
		Div => ("/", 7),
		Mod => ("%", 7),
		Add => ("+", 6),
		Sub => ("-", 6),
		BitAnd => ("&", 5),
		BitOr => ("|", 4),
		Eq => ("==", 3),
		Ne => ("!=", 3),
		Gt => (">", 3),
		Ge => (">=", 3),
		Lt => ("<", 3),
		Le => ("<=", 3),
		BoolAnd => ("&&", 2),
		BoolOr => ("||", 1),
	}
}
