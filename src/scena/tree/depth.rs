use crate::scena::{Expr, Place};

pub trait Adjust: Sized {
	fn adjust(&self, val: u32) -> Self;

	fn add(&self, val: usize) -> Self {
		self.adjust(val as u32)
	}

	fn sub(&self, val: usize) -> Self {
		self.adjust(val as u32)
	}
}

impl Adjust for Expr {
	fn adjust(&self, val: u32) -> Self {
		match self {
			Expr::Value(l, value) => Expr::Value(*l, value.clone()),
			Expr::Var(l, place) => Expr::Var(*l, place.adjust(val)),
			Expr::Ref(l, v) => Expr::Ref(*l, val - v),
			Expr::Call(l, name, exprs) => Expr::Call(*l, name.clone(), exprs.adjust(val)),
			Expr::Syscall(l, a, b, exprs) => Expr::Syscall(*l, *a, *b, exprs.adjust(val)),
			Expr::Unop(l, unop, expr) => Expr::Unop(*l, *unop, expr.adjust(val)),
			Expr::Binop(l, binop, expr, expr1) => Expr::Binop(*l, *binop, expr.adjust(val), expr1.adjust(val)),
		}
	}
}

impl Adjust for Place {
	fn adjust(&self, val: u32) -> Self {
		match self {
			Place::Var(v) => Place::Var(val - v),
			Place::Deref(v) => Place::Deref(val - v),
			Place::Global(s) => Place::Global(s.clone()),
		}
	}
}

impl<T: Adjust> Adjust for Vec<T> {
	fn adjust(&self, val: u32) -> Self {
		self.iter().map(|x| x.adjust(val)).collect()
	}
}

impl<T: Adjust> Adjust for Option<T> {
	fn adjust(&self, val: u32) -> Self {
		self.as_ref().map(|x| x.adjust(val))
	}
}

impl<T: Adjust> Adjust for Box<T> {
	fn adjust(&self, val: u32) -> Self {
		Box::new((**self).adjust(val))
	}
}
