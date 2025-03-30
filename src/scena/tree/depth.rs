use crate::scena::{Expr, Place, Var, FlatVar};

pub trait MapVars<V>: Sized {
	type Output<U>;
	fn map_vars<U>(&self, f: &impl Fn(&V) -> U) -> Self::Output<U>;
}

pub trait MapToTree: MapVars<FlatVar> {
	fn add(&self, val: usize) -> Self::Output<Var> {
		self.map_vars(&|x| Var(val as u32 - x.0))
	}
}

pub trait MapToFlat: MapVars<Var> {
	fn sub(&self, val: usize) -> Self::Output<FlatVar> {
		self.map_vars(&|x| FlatVar(val as u32 - x.0))
	}
}

impl<T: MapVars<FlatVar>> MapToTree for T {}
impl<T: MapVars<Var>> MapToFlat for T {}

impl<V> MapVars<V> for Expr<V> {
	type Output<U> = Expr<U>;
	fn map_vars<U>(&self, f: &impl Fn(&V) -> U) -> Self::Output<U> {
		match self {
			Expr::Value(l, value) => Expr::Value(*l, value.clone()),
			Expr::Var(l, place) => Expr::Var(*l, place.map_vars(f)),
			Expr::Ref(l, v) => Expr::Ref(*l, f(v)),
			Expr::Call(l, name, exprs) => Expr::Call(*l, name.clone(), exprs.map_vars(f)),
			Expr::Syscall(l, a, b, exprs) => Expr::Syscall(*l, *a, *b, exprs.map_vars(f)),
			Expr::Unop(l, unop, expr) => Expr::Unop(*l, *unop, expr.map_vars(f)),
			Expr::Binop(l, binop, expr, expr1) => Expr::Binop(*l, *binop, expr.map_vars(f), expr1.map_vars(f)),
		}
	}
}

impl<V> MapVars<V> for Place<V> {
	type Output<U> = Place<U>;
	fn map_vars<U>(&self, f: &impl Fn(&V) -> U) -> Self::Output<U> {
		match self {
			Place::Var(v) => Place::Var(f(v)),
			Place::Deref(v) => Place::Deref(f(v)),
			Place::Global(s) => Place::Global(s.clone()),
		}
	}
}

impl<V, T: MapVars<V>> MapVars<V> for Vec<T> {
	type Output<U> = Vec<T::Output<U>>;
	fn map_vars<U>(&self, f: &impl Fn(&V) -> U) -> Self::Output<U> {
		self.iter().map(|x| x.map_vars(f)).collect()
	}
}

impl<V, T: MapVars<V>> MapVars<V> for Option<T> {
	type Output<U> = Option<T::Output<U>>;
	fn map_vars<U>(&self, f: &impl Fn(&V) -> U) -> Self::Output<U> {
		self.as_ref().map(|x| x.map_vars(f))
	}
}

impl<V, T: MapVars<V>> MapVars<V> for Box<T> {
	type Output<U> = Box<T::Output<U>>;
	fn map_vars<U>(&self, f: &impl Fn(&V) -> U) -> Self::Output<U> {
		Box::new((**self).map_vars(f))
	}
}
