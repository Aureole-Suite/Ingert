use crate::scena::{Expr, FlatStmt, Line};

impl Expr {
	pub fn line(&self) -> Option<Line> {
		match self {
			Self::Value(l, _) => Some(*l),
			Self::Var(l, _) => Some(*l),
			Self::Ref(l, _) => Some(*l),
			Self::Call(l, _, _, _) => Some(*l),
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
			Self::Call(l, _, _, _) => Some(l),
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
			Self::While(l, _, _) => Some(*l),
			Self::Goto(_) => None,
			Self::Switch(l, _, _, _) => Some(*l),
			Self::PushVar(l) => Some(*l),
			Self::PopVar(_) => None,
			Self::Debug(l, _) => Some(*l),
			Self::Tailcall(l, _, _, _, _) => Some(*l),
		}
	}

	pub fn line_mut(&mut self) -> Option<&mut Line> {
		match self {
			Self::Label(_) => None,
			Self::Expr(_) => None, // Not sure about this one
			Self::Set(l, _, _) => Some(l),
			Self::Return(l, _, _) => Some(l),
			Self::If(l, _, _) => Some(l),
			Self::While(l, _, _) => Some(l),
			Self::Goto(_) => None,
			Self::Switch(l, _, _, _) => Some(l),
			Self::PushVar(l) => Some(l),
			Self::PopVar(_) => None,
			Self::Debug(l, _) => Some(l),
			Self::Tailcall(l, _, _, _, _) => Some(l),
		}
	}
}
