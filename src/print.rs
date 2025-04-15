use std::borrow::Cow;

use crate::scp::{Call, CallArg, CallKind, GlobalType, Label, Op};
use crate::scena::{ArgType, Binop, Body, Called, Expr, FlatStmt, FlatVar, Function, Global, Line, Name, Place, Scena, Stmt, Unop, Value, Var};

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
	vars: Vec<String>,
	nargs: u32,
}

impl Ctx {
	fn new() -> Self {
		Self {
			out: String::new(),
			space: Space::None,
			indent: 0,
			vars: Vec::new(),
			nargs: 0,
		}
	}

	fn token(&mut self, word: impl Into<Cow<'static, str>>) {
		self.do_space(true);
		self.out.push_str(&word.into());
		self.set_space(Space::Inline);
	}

	fn word(&mut self, word: &'static str) {
		self.token(word)
	}

	fn ident(&mut self, name: String) {
		if is_normal_ident(name.as_str()) {
			self.token(name);
		} else {
			self.do_space(true);
			self.str(name.as_str(), '`');
			self.set_space(Space::Inline);
		}
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
		if self.out.is_empty() {
			self.space = Space::None;
			return;
		}
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

	fn arglist<I: IntoIterator>(&mut self, args: I, mut f: impl FnMut(I::Item, &mut Self)) {
		self.sym("(");
		for (i, arg) in args.into_iter().enumerate() {
			if i != 0 {
				self.sym_(",");
			}
			f(arg, self);
		}
		self.sym_(")");
	}

	fn block<I: IntoIterator>(&mut self, block: I, mut f: impl FnMut(I::Item, &mut Self)) {
		self._sym_("{");
		self.indent += 1;
		for stmt in block {
			self.set_space(Space::Block(0));
			f(stmt, self);
		}
		self.set_space(Space::Block(0));
		self.indent -= 1;
		self._sym_("}");
	}

	fn str(&mut self, str: &str, delim: char) {
		self.do_space(true);
		self.out.push(delim);
		for char in str.chars() {
			match char {
				'\\' => self.out.push_str("\\\\"),
				'\n' => self.out.push_str("\\n"),
				'\r' => self.out.push_str("\\r"),
				'\t' => self.out.push_str("\\t"),
				c if c == delim => {
					self.out.push('\\');
					self.out.push(c);
				}
				c => self.out.push(c),
			}
		}
		self.out.push(delim);
		self.set_space(Space::Inline);
	}

	fn line(&mut self, line: &Line) {
		if let Some(line) = line {
			self.token(format!("{line}"));
			self.sym("@");
		}
	}
}

fn is_normal_ident(ident: &str) -> bool {
	let mut iter = ident.chars();
	iter.next().is_some_and(unicode_ident::is_xid_start) && iter.all(unicode_ident::is_xid_continue)
}

enum Item<'a> {
	Global(&'a str, &'a Global),
	Function(&'a str, &'a Function),
}

pub fn print(scena: &Scena) -> String {
	let mut ctx = Ctx::new();
	for item in items(scena) {
		match item {
			Item::Global(name, global) => {
				ctx.set_space(Space::Block(0));
				print_global(&mut ctx, name, global);
				ctx.set_space(Space::Block(0));
			}
			Item::Function(name, function) => {
				if let Some(wr) = as_wrapper(function) {
					ctx.set_space(Space::Block(0));
					print_wrapper(&mut ctx, name, function, &wr);
					ctx.set_space(Space::Block(0));
				} else {
					ctx.set_space(Space::Block(1));
					print_function(&mut ctx, name, function);
					ctx.set_space(Space::Block(1));
				}
			}
		}
	}

	if !ctx.out.is_empty() {
		ctx.out.push('\n');
	}

	ctx.out
}

fn items(scena: &Scena) -> Vec<Item<'_>> {
	let mut items = Vec::new();
	let mut globals = scena.globals.iter().peekable();
	for func in &scena.functions {
		if let Some(func_line) = crate::scena::first_line(&func.1.body) {
			while let Some(g) = globals.next_if(|l| l.1.line.is_none_or(|v| v <= func_line)) {
				items.push(Item::Global(g.0, g.1));
			}
		}
		items.push(Item::Function(func.0, func.1));
	}
	for g in globals {
		items.push(Item::Global(g.0, g.1));
	}
	items
}

fn print_global(ctx: &mut Ctx, name: &str, global: &Global) {
	ctx.line(&global.line);
	ctx.word("global");
	ctx.ident(name.to_owned());
	ctx.sym_(":");
	global.ty.print(ctx);
	ctx.sym_(";");
}

struct SyscallWrapper {
	ret: bool,
	args: (u8, u8),
}

fn as_wrapper(f: &Function) -> Option<SyscallWrapper> {
	let Body::Tree(body) = &f.body else { return None };
	let (ret, a, b, args) = match body.as_slice() {
		[Stmt::Return(None, Some(Expr::Syscall(None, a, b, args)))] => (true, *a, *b, args),
		[Stmt::Expr(Expr::Syscall(None, a, b, args)), Stmt::Return(None, None)] => (false, *a, *b, args),
		_ => return None,
	};
	dbg!(a, b, args);
	for (i, arg) in args.iter().rev().enumerate().rev() {
		if arg != &Expr::Var(None, Place::Var(Var(i as u32))) {
			return None;
		}
	}
	Some(SyscallWrapper {
		ret,
		args: (a, b),
	})
}

fn print_wrapper(ctx: &mut Ctx, name: &str, f: &Function, wr: &SyscallWrapper) {
	if f.is_prelude {
		ctx.word("prelude");
	}
	ctx.word("fn");
	ctx.ident(name.to_owned());

	ctx._sym_("=");
	if wr.ret {
		ctx.word("return");
	}
	ctx.token(format!("system[{},{}]", wr.args.0, wr.args.1));

	ctx.arglist(f.args.iter(), |arg, ctx| {
		ctx.line(&arg.line);
		arg.ty.print(ctx);

		if let Some(default) = &arg.default {
			ctx._sym_("=");
			default.print(ctx);
		}
	});

	match &f.called {
		Called::Raw(calls) => {
			ctx.word("calls");
			ctx.block(calls, Call::print);
		}
		Called::Merged(true) => {
			ctx.word("dup");
		}
		Called::Merged(false) => {}
	}

	ctx.sym_(";");
}

fn print_function(ctx: &mut Ctx, name: &str, f: &Function) {
	ctx.vars.clear();
	for i in (0..f.args.len()).rev() {
		ctx.vars.push(format!("arg{i}"));
	}
	ctx.nargs = f.args.len() as u32;

	if f.is_prelude {
		ctx.word("prelude");
	}
	ctx.word("fn");
	ctx.ident(name.to_owned());
	ctx.arglist(f.args.iter().rev().enumerate().rev(), |(i, arg), ctx| {
		ctx.line(&arg.line);
		Var(i as u32).print(ctx);
		ctx.sym_(":");
		arg.ty.print(ctx);

		if let Some(default) = &arg.default {
			ctx._sym_("=");
			default.print(ctx);
		}
	});

	match &f.called {
		Called::Raw(calls) => {
			ctx.word("calls");
			ctx.block(calls, Call::print);
		}
		Called::Merged(true) => {
			ctx.word("dup");
		}
		Called::Merged(false) => {}
	}

	match &f.body {
		Body::Asm(ops) => {
			ctx.word("asm");
			ctx.block(ops, Op::print);
		}
		Body::Flat(stmts) => {
			ctx.word("flat");
			ctx.block(stmts, FlatStmt::print);
		}
		Body::Tree(stmts) => {
			ctx.block(stmts, Stmt::print);
		}
	}
}

trait Print {
	fn print(&self, ctx: &mut Ctx);
}

impl Print for GlobalType {
	fn print(&self, ctx: &mut Ctx) {
		match self {
			GlobalType::Number => ctx.word("num"),
			GlobalType::String => ctx.word("str"),
		}
	}
}

impl Print for ArgType {
	fn print(&self, ctx: &mut Ctx) {
		match self {
			ArgType::Number => ctx.word("num"),
			ArgType::String => ctx.word("str"),
			ArgType::NumberRef => {
				ctx._sym("&");
				ctx.word("num");
			}
		}
	}
}

impl Print for Call {
	fn print(&self, ctx: &mut Ctx) {
		match &self.kind {
			CallKind::Normal(name) => {
				name.print(ctx);
			}
			CallKind::Tailcall(name) => {
				ctx.word("become");
				name.print(ctx);
			}
			CallKind::Syscall(a, b) => {
				ctx.token(format!("system[{},{}]", a, b));
			}
		};
		ctx.arglist(&self.args, CallArg::print);
		ctx.sym_(";");
	}
}

impl Print for CallArg {
	fn print(&self, ctx: &mut Ctx) {
		match self {
			CallArg::Value(value) => value.print(ctx),
			CallArg::Call => ctx.word("call"),
			CallArg::Var => ctx.word("var"),
			CallArg::Expr => ctx.word("expr"),
		}
	}
}

impl Print for Op {
	fn print(&self, ctx: &mut Ctx) {
		match self {
			Op::Label(label) => {
				ctx.indent -= 1;
				label.print(ctx);
				ctx.sym_(":");
				ctx.indent += 1;
				return;
			}
			Op::Push(value) => {
				ctx.word("push");
				value.print(ctx);
			}
			Op::Pop(n) => {
				ctx.word("pop");
				ctx.token(n.to_string());
			}
			Op::PushNull => {
				ctx.word("push_null");
			}
			Op::GetVar(slot) => {
				ctx.word("get_var");
				ctx.token(slot.0.to_string());
			}
			Op::GetRef(slot) => {
				ctx.word("get_ref");
				ctx.token(slot.0.to_string());
			}
			Op::PushRef(slot) => {
				ctx.word("push_ref");
				ctx.token(slot.0.to_string());
			}
			Op::SetVar(slot) => {
				ctx.word("set_var");
				ctx.token(slot.0.to_string());
			}
			Op::SetRef(slot) => {
				ctx.word("set_ref");
				ctx.token(slot.0.to_string());
			}
			Op::GetGlobal(name) => {
				ctx.word("get_global");
				ctx.ident(name.clone());
			}
			Op::SetGlobal(name) => {
				ctx.word("set_global");
				ctx.ident(name.clone());
			}
			Op::GetTemp(n) => {
				ctx.word("get_temp");
				ctx.token(n.to_string());
			}
			Op::SetTemp(n) => {
				ctx.word("set_temp");
				ctx.token(n.to_string());
			}
			Op::Binop(binop) => {
				ctx.word(match binop {
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
				});
			}
			Op::Unop(unop) => {
				ctx.word(match unop {
					Unop::Neg => "neg",
					Unop::BoolNot => "bool_not",
					Unop::BitNot => "bit_not",
				});
			}
			Op::Jnz(label) => {
				ctx.word("jnz");
				label.print(ctx);
			}
			Op::Jz(label) => {
				ctx.word("jz");
				label.print(ctx);
			}
			Op::Goto(label) => {
				ctx.word("goto");
				label.print(ctx);
			}
			Op::CallLocal(b) => {
				ctx.word("call_local");
				ctx.ident(b.clone());
			}
			Op::CallExtern(name, n) => {
				ctx.word("call_extern");
				name.print(ctx);
				ctx.token(n.to_string());
			}
			Op::CallTail(name, b) => {
				ctx.word("call_tail");
				name.print(ctx);
				ctx.token(b.to_string());
			}
			Op::CallSystem(a, b, n) => {
				ctx.word("call_system");
				ctx.token(a.to_string());
				ctx.token(b.to_string());
				ctx.token(n.to_string());
			}
			Op::PrepareCallLocal(label) => {
				ctx.word("prepare_call_local");
				label.print(ctx);
			}
			Op::PrepareCallExtern(label) => {
				ctx.word("prepare_call_extern");
				label.print(ctx);
			}
			Op::Return => {
				ctx.word("return");
			}
			Op::Line(l) => {
				ctx.word("line");
				ctx.token(l.to_string());
			}
			Op::Debug(n) => {
				ctx.word("debug");
				ctx.token(n.to_string());
			}
		};
		ctx.sym_(";");
	}
}

impl Print for FlatStmt {
	fn print(&self, ctx: &mut Ctx) {
		match self {
			FlatStmt::Label(label) => {
				ctx.indent -= 1;
				label.print(ctx);
				ctx.sym_(":");
				ctx.indent += 1;
				return;
			}
			FlatStmt::Expr(expr) => {
				expr.print(ctx);
			}
			FlatStmt::Set(l, place, expr) => {
				ctx.line(l);
				place.print(ctx);
				ctx._sym_("=");
				expr.print(ctx);
			}
			FlatStmt::Return(l, expr, pop) => {
				ctx.line(l);
				ctx.token(format!("return[{pop}]"));
				if let Some(expr) = expr {
					expr.print(ctx);
				}
			}
			FlatStmt::If(l, expr, label) => {
				ctx.line(l);
				ctx.word("if");
				expr.print(ctx);
				label.print(ctx);
			}
			FlatStmt::Goto(label, pop) => {
				if *pop != 0 {
					ctx.token(format!("goto[{pop}]"));
				} else {
					ctx.word("goto");
				}
				label.print(ctx);
			}
			FlatStmt::Switch(l, expr, items, label) => {
				ctx.line(l);
				ctx.word("switch");
				expr.print(ctx);
				ctx.block(items, |(value, label), ctx| {
					ctx.token(value.to_string());
					ctx._sym_("=>");
					label.print(ctx);
					ctx.sym_(";");
				});
				label.print(ctx);
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
				exprs.print(ctx);
			}
			FlatStmt::Tailcall(l, name, exprs, pop) => {
				ctx.line(l);
				ctx.token(format!("tailcall[{pop}]"));
				name.print(ctx);
				exprs.print(ctx);
			}
		}
		ctx.sym_(";");
	}
}

impl Print for Vec<Stmt> {
	fn print(&self, ctx: &mut Ctx) {
		let size = ctx.vars.len();
		ctx.block(self, Stmt::print);
		ctx.vars.truncate(size);
	}
}

impl Print for Stmt {
	fn print(&self, ctx: &mut Ctx) {
		match self {
			Stmt::Expr(expr) => {
				expr.print(ctx);
				ctx.sym_(";");
			}
			Stmt::Set(l, place, expr) => {
				ctx.line(l);
				place.print(ctx);
				ctx._sym_("=");
				expr.print(ctx);
				ctx.sym_(";");
			}
			Stmt::Return(l, expr) => {
				ctx.line(l);
				ctx.word("return");
				if let Some(expr) = expr {
					expr.print(ctx);
				}
				ctx.sym_(";");
			}
			Stmt::If(l, expr, then, els) => {
				ctx.line(l);
				ctx.word("if");
				expr.print(ctx);
				then.print(ctx);
				if let Some(els) = els {
					ctx.word("else");
					if let [Stmt::If(..)] = els.as_slice() {
						els[0].print(ctx);
					} else {
						els.print(ctx);
					}
				}
			}
			Stmt::While(l, expr, body) => {
				ctx.line(l);
				ctx.word("while");
				expr.print(ctx);
				body.print(ctx);
			}
			Stmt::Switch(l, expr, cases) => {
				ctx.line(l);
				ctx.word("switch");
				expr.print(ctx);
				ctx.block(cases, |(value, body), ctx| {
					match value {
						Some(value) => {
							ctx.word("case");
							ctx.token(value.to_string());
							ctx.sym_(":")
						}
						None => {
							ctx.word("default");
							ctx.sym_(":")
						}
					};
					ctx.indent += 1;
					for stmt in body {
						ctx.set_space(Space::Block(0));
						stmt.print(ctx);
					}
					ctx.indent -= 1;
				});
			}
			Stmt::Block(stmts) => {
				stmts.print(ctx);
			}
			Stmt::Break => {
				ctx.word("break");
				ctx.sym_(";");
			}
			Stmt::Continue => {
				ctx.word("continue");
				ctx.sym_(";");
			}
			Stmt::PushVar(l, d, e) => {
				ctx.vars.push(format!("var{}", d.0 - ctx.nargs));
				ctx.line(l);
				ctx.word("var");
				d.print(ctx);
				if let Some(e) = e {
					ctx._sym_("=");
					e.print(ctx);
				}
				ctx.sym_(";");
			}
			Stmt::Debug(l, exprs) => {
				ctx.line(l);
				ctx.word("debug");
				exprs.print(ctx);
				ctx.sym_(";");
			}
			Stmt::Tailcall(l, name, exprs) => {
				ctx.line(l);
				ctx.word("tailcall");
				name.print(ctx);
				exprs.print(ctx);
				ctx.sym_(";");
			}
		}
	}
}

impl<V: Print> Print for Place<V> {
	fn print(&self, ctx: &mut Ctx) {
		match self {
			Place::Var(n) => {
				n.print(ctx);
			}
			Place::Deref(n) => {
				ctx._sym("*");
				n.print(ctx);
			}
			Place::Global(name) => {
				ctx.ident(name.clone());
			}
		}
	}
}

impl<V: Print> Print for Expr<V> {
	fn print(&self, ctx: &mut Ctx) {
		self.print_prec(ctx, 0);
	}
}

#[allow(private_bounds)] // false positive
impl<V: Print> Expr<V> {
	fn print_prec(&self, ctx: &mut Ctx, prec: u32) {
		match self {
			Expr::Value(l, value) => {
				ctx.line(l);
				value.print(ctx);
			}
			Expr::Var(l, place) => {
				ctx.line(l);
				place.print(ctx);
			}
			Expr::Ref(l, n) => {
				ctx.line(l);
				ctx._sym("&");
				n.print(ctx);
			}
			Expr::Call(l, name, exprs) => {
				ctx.line(l);
				name.print(ctx);
				exprs.print(ctx);
			}
			Expr::Syscall(l, a, b, exprs) => {
				ctx.line(l);
				ctx.token(format!("system[{a},{b}]"));
				exprs.print(ctx);
			}
			Expr::Unop(l, unop, expr) => {
				ctx.line(l);
				if *unop == Unop::Neg && matches!(**expr, Expr::Value(..)) {
					ctx._sym("-");
					ctx.sym("(");
					expr.print_prec(ctx, 0);
					ctx.sym_(")");
				} else {
					let op = match unop {
						Unop::Neg => "-",
						Unop::BoolNot => "!",
						Unop::BitNot => "~",
					};
					ctx._sym(op);
					expr.print_prec(ctx, 10);
				}
			}
			Expr::Binop(l, binop, left, right) => {
				let (op, op_prio) = binop_prio(*binop);
				if op_prio < prec {
					ctx._sym("(");
				}
				left.print_prec(ctx, op_prio);
				ctx.line(l);
				ctx._sym_(op);
				right.print_prec(ctx, op_prio + 1);
				if op_prio < prec {
					ctx.sym_(")");
				}
			}
		}
	}
}

impl<V: Print> Print for Vec<Expr<V>> {
	fn print(&self, ctx: &mut Ctx) {
		ctx.arglist(self, Expr::print);
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

impl Print for Var {
	fn print(&self, ctx: &mut Ctx) {
		if let Some(name) = ctx.vars.get(self.0 as usize) {
			ctx.ident(name.clone());
		} else {
			ctx.ident(format!("unknown_var{}", self.0));
		}
	}
}

impl Print for FlatVar {
	fn print(&self, ctx: &mut Ctx) {
		ctx._sym("#");
		ctx.token(format!("{}", self.0));
	}
}

impl Print for Value {
	fn print(&self, ctx: &mut Ctx) {
		match self {
			Value::Int(v) => ctx.token(format!("{v}")),
			Value::Float(v) => ctx.token(format!("{v:?}")),
			Value::String(v) => ctx.str(v, '"'),
		}
	}
}

impl Print for Name {
	fn print(&self, ctx: &mut Ctx) {
		if let Some(local) = self.as_local() {
			ctx.ident(local.clone());
		} else {
			ctx.ident(self.0.clone());
			ctx.sym(".");
			ctx.ident(self.1.clone());
		}
	}
}

impl Print for Label {
	fn print(&self, ctx: &mut Ctx) {
		ctx._sym("$");
		ctx.ident(format!("L{}", self.0));
	}
}
