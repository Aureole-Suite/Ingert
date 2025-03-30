use crate::scena::{Expr, FlatStmt, Stmt, Line};

impl Expr {
	pub fn line(&self) -> Option<Line> {
		match self {
			Self::Value(l, _) => Some(*l),
			Self::Var(l, _) => Some(*l),
			Self::Ref(l, _) => Some(*l),
			Self::Call(l, _, _) => Some(*l),
			Self::Syscall(l, _, _, _) => Some(*l),
			Self::Unop(l, _, _) => Some(*l),
			Self::Binop(l, _, _, _) => Some(*l),
		}
	}

	pub fn line_mut(&mut self) -> Option<&mut Line> {
		match self {
			Self::Value(l, _) => Some(l),
			Self::Var(l, _) => Some(l),
			Self::Ref(l, _) => Some(l),
			Self::Call(l, _, _) => Some(l),
			Self::Syscall(l, _, _, _) => Some(l),
			Self::Unop(l, _, _) => Some(l),
			Self::Binop(l, _, _, _) => Some(l),
		}
	}
}

impl FlatStmt {
	pub fn line(&self) -> Option<Line> {
		match self {
			Self::Label(_) => None,
			Self::Expr(_) => None, // Not sure about this one
			Self::Set(l, _, _) => Some(*l),
			Self::Return(l, _, _) => Some(*l),
			Self::If(l, _, _) => Some(*l),
			Self::Goto(_, _) => None,
			Self::Switch(l, _, _, _) => Some(*l),
			Self::PushVar(l) => Some(*l),
			Self::PopVar(_) => None,
			Self::Debug(l, _) => Some(*l),
			Self::Tailcall(l, _, _, _) => Some(*l),
		}
	}

	pub fn line_mut(&mut self) -> Option<&mut Line> {
		match self {
			Self::Label(_) => None,
			Self::Expr(_) => None, // Not sure about this one
			Self::Set(l, _, _) => Some(l),
			Self::Return(l, _, _) => Some(l),
			Self::If(l, _, _) => Some(l),
			Self::Goto(_, _) => None,
			Self::Switch(l, _, _, _) => Some(l),
			Self::PushVar(l) => Some(l),
			Self::PopVar(_) => None,
			Self::Debug(l, _) => Some(l),
			Self::Tailcall(l, _, _, _) => Some(l),
		}
	}
}

impl Stmt {
	pub fn line(&self) -> Option<Line> {
		match self {
			Self::Expr(e) => e.line(),
			Self::Set(l, _, _) => Some(*l),
			Self::Return(l, _) => Some(*l),
			Self::If(l, _, _, _) => Some(*l),
			Self::While(l, _, _) => Some(*l),
			Self::Switch(l, _, _) => Some(*l),
			Self::Block(_) => None,
			Self::Break => None,
			Self::Continue => None,
			Self::PushVar(l, _, _) => Some(*l),
			Self::Debug(l, _) => Some(*l),
			Self::Tailcall(l, _, _) => Some(*l),
		}
	}

	pub fn line_mut(&mut self) -> Option<&mut Line> {
		match self {
			Self::Expr(e) => e.line_mut(),
			Self::Set(l, _, _) => Some(l),
			Self::Return(l, _) => Some(l),
			Self::If(l, _, _, _) => Some(l),
			Self::While(l, _, _) => Some(l),
			Self::Switch(l, _, _) => Some(l),
			Self::Block(_) => None,
			Self::Break => None,
			Self::Continue => None,
			Self::PushVar(l, _, _) => Some(l),
			Self::Debug(l, _) => Some(l),
			Self::Tailcall(l, _, _) => Some(l),
		}
	}
}

pub fn sink(stmt: &mut FlatStmt) {
	let lines = &mut Vec::new();
	match stmt {
		FlatStmt::Label(_) => {}
		FlatStmt::Expr(expr) => {
			sink_expr(expr, lines);
		}
		FlatStmt::Set(l, _, expr) => {
			sink_expr(expr, lines);
			lines.push(l);
		}
		FlatStmt::Return(l, expr, _) => {
			if let Some(expr) = expr {
				sink_expr(expr, lines);
			}
			lines.push(l);
		}
		FlatStmt::If(l, expr, _) => {
			sink_expr(expr, lines);
			lines.push(l);
		}
		FlatStmt::Goto(_, _) => {}
		FlatStmt::Switch(l, expr, _, _) => {
			sink_expr(expr, lines);
			lines.push(l);
		}
		FlatStmt::PushVar(l) => {
			lines.push(l);
		}
		FlatStmt::PopVar(_) => {}
		FlatStmt::Debug(l, exprs) => {
			sink_exprs(exprs, lines);
			lines.push(l);
		}
		FlatStmt::Tailcall(l, _, exprs, _) => {
			sink_exprs(exprs, lines);
			lines.push(l);
		}
	}
	sink_lines(lines);
}

fn sink_expr<'a>(expr: &'a mut Expr, lines: &mut Vec<&'a mut Line>) {
	match expr {
		Expr::Value(l, _) => {
			lines.push(l);
		}
		Expr::Var(l, _) => {
			lines.push(l);
		}
		Expr::Ref(l, _) => {
			lines.push(l);
		}
		Expr::Call(l, _, exprs) | Expr::Syscall(l, _, _, exprs) => {
			sink_exprs(exprs, lines);
			sink_lines(lines);
			lines.push(l);
		}
		Expr::Unop(l, _, expr) => {
			sink_expr(expr, lines);
			lines.push(l);
		}
		Expr::Binop(l, _, a, b) => {
			sink_expr(b, lines);
			sink_lines(lines);
			sink_expr(a, lines);
			lines.push(l);
		}
	}
}

fn sink_exprs<'a>(exprs: &'a mut [Expr], lines: &mut Vec<&'a mut Line>) {
	for expr in exprs.iter_mut().rev() {
		sink_lines(lines);
		sink_expr(expr, lines);
	}
}


fn sink_lines(lines: &mut Vec<&mut Line>) {
	let count = lines.iter().take_while(|l| l.is_some()).count();
	let part2 = lines.len() - count;
	if part2 != 0 {
		for i in (0..count).rev() {
			let (a, b) = lines[i..].split_at_mut(part2);
			std::mem::swap(a[0], b[0]);
		}
	}
	lines.clear();
}
