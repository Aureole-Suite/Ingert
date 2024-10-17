#![feature(is_sorted, is_none_or)]
use std::cell::Cell;
use std::collections::{BTreeMap, VecDeque};

use gospel::read::{Le as _, Reader};
use snafu::ResultExt as _;

#[extend::ext]
impl Reader<'_> {
	fn vec3(&mut self) -> Result<glam::Vec3, gospel::read::Error> {
		Ok(glam::Vec3::new(self.f32()?, self.f32()?, self.f32()?))
	}
}

#[derive(Debug, snafu::Snafu)]
#[snafu(module(scp), context(suffix(false)))]
pub enum ScpError {
	#[snafu(display("invalid read (at {location})"), context(false))]
	Read {
		source: gospel::read::Error,
		#[snafu(implicit)]
		location: snafu::Location,
	},
	#[snafu(display("invalid string (at {location})"), context(false))]
	String {
		source: StringError,
		#[snafu(implicit)]
		location: snafu::Location,
	},
	#[snafu(display("invalid opcode {op:02X} at {pos}"))]
	Op { op: u8, pos: usize },
	#[snafu(display("invalid value {value:?}"))]
	BadValue {
		value: Value,
	},
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
	pub start: Label,
	pub a0: u8,
	pub a1: u8,
	pub a2: Vec<Value>,
	pub args: Vec<Value>,
	pub called: Vec<(i32, u16, Vec<TaggedValue>)>,
	pub checksum: u32,
	pub name: String,
	pub index: u32,
}

fn multi<T>(
	f: &mut Reader,
	n: usize,
	g: impl Fn(&mut Reader) -> Result<T, ScpError>,
) -> Result<Vec<T>, ScpError> {
	(0..n).map(|_| g(f)).collect()
}

fn parse_functions(f: &mut Reader<'_>, n_entries: u32) -> Result<Vec<Function>, ScpError> {
	let mut entries = Vec::with_capacity(n_entries as usize);
	for index in 0..n_entries {
		let start = Label(f.u32()?);
		let argc = f.u8()? as usize;
		let a0 = f.u8()?;
		let a1 = f.u8()?;
		let a2c = f.u8()? as usize;
		let a2p = f.u32()? as usize;
		let argp = f.u32()? as usize;
		let calledc = f.u32()? as usize;
		let calledp = f.u32()? as usize;
		let checksum = f.u32()?;
		let name = string_value(f)?;

		let a2 = multi(&mut f.at(a2p)?, a2c, value)?;
		let args = multi(&mut f.at(argp)?, argc, value)?;
		let called = multi(&mut f.at(calledp)?, calledc, |f| {
			let x = f.i32()?;
			let y = f.u16()?;
			let z = f.u16()? as usize;
			let w = f.u32()? as usize;
			let z = multi(&mut f.at(w)?, z, tagged_value)?;
			Ok((x, y, z))
		})?;
		entries.push(Function {
			start,
			a0,
			a1,
			a2,
			args,
			called,
			checksum,
			name,
			index,
		});
	}
	// they're sorted by name, but that's not useful here
	entries.sort_by_key(|e| e.start);
	Ok(entries)
}

#[derive(Clone, PartialEq)]
pub enum Value {
	Uint(u32),
	Int(i32),
	Float(f32),
	String(String),
}

impl std::fmt::Debug for Value {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Uint(v) => write!(f, "{:#X}", v),
			Self::Int(v) => write!(f, "{v}"),
			Self::Float(v) => v.fmt(f),
			Self::String(v) => v.fmt(f),
		}
	}
}

#[derive(Clone, PartialEq)]
pub struct TaggedValue(Value, u32);

impl std::fmt::Debug for TaggedValue {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		if self.1 == 0 {
			self.0.fmt(f)
		} else {
			write!(f, "{:?}:{}", self.0, self.1)
		}
	}
}

fn f30(v: u32) -> f32 {
	fn float_len(v: &f32) -> usize {
		use std::io::Write;
		struct Counter(usize);
		impl Write for Counter {
			fn write(&mut self, v: &[u8]) -> std::io::Result<usize> {
				self.0 += v.len();
				Ok(v.len())
			}
			fn flush(&mut self) -> std::io::Result<()> {
				Ok(())
			}
		}
		let mut c = Counter(0);
		write!(&mut c, "{:?}", v).unwrap();
		c.0
	}
	(0..4)
		.map(|n| f32::from_bits(v << 2 | n))
		.min_by_key(float_len)
		.unwrap()
}

#[derive(Debug, snafu::Snafu)]
#[snafu(module(string), context(suffix(false)))]
pub enum StringError {
	#[snafu(display("invalid read"), context(false))]
	Read {
		source: gospel::read::Error,
	},
	#[snafu(display("invalid string: {lossy_string:?}"))]
	Utf8 {
		source: std::str::Utf8Error,
		lossy_string: String,
	},
}

fn string(f: &mut Reader) -> Result<String, StringError> {
	let zs = f.cstr()?.to_bytes();
	let s = std::str::from_utf8(zs).with_context(|_| string::Utf8 {
		lossy_string: String::from_utf8_lossy(zs).into_owned(),
	})?;
	Ok(s.to_string())
}

fn value(f: &mut Reader) -> Result<Value, ScpError> {
	let v = f.u32()?;
	let hi = v >> 30;
	let lo = v & 0x3FFFFFFF;
	match hi {
		0 => Ok(Value::Uint(lo)),
		1 => Ok(Value::Int((lo as i32) << 2 >> 2)),
		2 => Ok(Value::Float(f30(lo))),
		3 => Ok(Value::String(string(&mut f.at(lo as usize)?)?)),
		_ => unreachable!(),
	}
}

fn string_value(f: &mut Reader) -> Result<String, ScpError> {
	match value(f)? {
		Value::String(s) => Ok(s),
		value => scp::BadValue { value }.fail(),
	}
}

fn tagged_value(f: &mut Reader) -> Result<TaggedValue, ScpError> {
	Ok(TaggedValue(value(f)?, f.u32()?))
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Label(u32);

impl std::fmt::Debug for Label {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "l{:X}", self.0)
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum Op {
	Push(Value),
	Pop(u8),
	GetVar(i32),
	_03(i32),
	_04(i32),
	SetVar(i32),
	_06(i32),
	_07(u32),
	_08(u32),
	GetGlobal(u8),
	SetGlobal(u8),
	Goto(Label),
	Syscall(u16),
	Return,
	If2(Label),
	If(Label),
	Op(u8),
	CallFunc(Value, Value, u8),
	_23(Value, Value, u8),
	Syscall2(u8, u8, u8),
	_25(Label),
	Line(u16),
	_27(u8),
}

pub struct Scp {
	pub functions: Vec<Function>,
	pub extras: Vec<TaggedValue>,
	pub ops: Vec<(Label, Op, Label)>,
}

pub fn parse_da(data: &[u8]) -> Result<Scp, ScpError> {
	tracing::info!("reading");
	let mut f = Reader::new(data);
	f.check(b"#scp")?;
	f.check_u32(24)?;
	let n_entries = f.u32()?;
	let code_start = f.u32()?;
	let n3 = f.u32()?;
	f.check_u32(0)?;

	let functions = parse_functions(&mut f, n_entries)?;
	f.seek(code_start as usize)?;
	let extras = multi(&mut f, n3 as usize, tagged_value)?;

	let last_offset = functions
		.iter()
		.map(|e| e.start.0)
		.max()
		.unwrap_or(code_start);
	let last_offset = Cell::new(last_offset);

	let label = |f: &mut Reader| -> Result<Label, ScpError> {
		let l = Label(f.u32()?);
		if l.0 > last_offset.get() {
			last_offset.set(l.0);
		}
		Ok(l)
	};

	let mut ops = Vec::new();
	loop {
		let start = f.pos();
		let op = f.u8()?;
		let op = match op {
			0 => {
				f.check_u8(4)?; // number of bytes to push?
				Op::Push(value(&mut f)?)
			}
			1 => Op::Pop(f.u8()?),
			2 => Op::GetVar(f.i32()?),
			3 => Op::_03(f.i32()?),
			4 => Op::_04(f.i32()?),
			5 => Op::SetVar(f.i32()?),
			6 => Op::_06(f.i32()?),
			7 => Op::_07(f.u32()?),
			8 => Op::_08(f.u32()?),
			9 => Op::GetGlobal(f.u8()?),
			10 => Op::SetGlobal(f.u8()?),
			11 => Op::Goto(label(&mut f)?),
			12 => Op::Syscall(f.u16()?),
			13 => Op::Return,
			14 => Op::If2(label(&mut f)?),
			15 => Op::If(label(&mut f)?),
			16..=33 => Op::Op(op),
			34 => Op::CallFunc(value(&mut f)?, value(&mut f)?, f.u8()?),
			35 => Op::_23(value(&mut f)?, value(&mut f)?, f.u8()?),
			36 => {
				let a = f.u8()?;
				let b = f.u8()?;
				let c = f.u8()?;
				if c != 0 {
					f.check_u8(1)?;
					f.check_u8(4 * c)?;
				}
				Op::Syscall2(a, b, c)
			}
			37 => Op::_25(label(&mut f)?),
			38 => Op::Line(f.u16()?),
			39 => Op::_27(f.u8()?),
			40.. => return scp::Op { op, pos: start }.fail(),
		};
		let end = op == Op::Return && f.pos() > last_offset.get() as usize;
		ops.push((Label(start as u32), op, Label(f.pos() as u32)));
		if end {
			break;
		}
	}

	Ok(Scp {
		functions,
		extras,
		ops,
	})
}

#[derive(Clone, PartialEq)]
enum Expr {
	Value(Value),
	Var(i32),
	Syscall(u16, Vec<Expr>),
	Syscall2(u8, u8, Vec<Expr>),
	CallFunc(Value, Value, Vec<Expr>),
	Unop(u8, Box<Expr>),
	Binop(u8, Box<Expr>, Box<Expr>),
	Local,
	Arg,
	_07(u32),
	Global(u8),
}

impl std::fmt::Debug for Expr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Expr::Value(v) => v.fmt(f),
			Expr::Var(n) => f.debug_tuple("Var").field(n).finish(),
			Expr::Syscall(n, v) => f.debug_tuple("Syscall").field(n).field(v).finish(),
			Expr::Syscall2(a, b, v) => f.debug_tuple("Syscall2").field(a).field(b).field(v).finish(),
			Expr::CallFunc(a, b, v) => f.debug_tuple("CallFunc").field(a).field(b).field(v).finish(),
			Expr::Unop(v, a) => f.debug_tuple("Unop").field(v).field(a).finish(),
			Expr::Binop(v, a, b) => f.debug_tuple("Binop").field(v).field(a).field(b).finish(),
			Expr::Local => f.write_str("Local"),
			Expr::Arg => f.write_str("Arg"),
			Expr::_07(v) => f.debug_tuple("_07").field(v).finish(),
			Expr::Global(v) => f.debug_tuple("Global").field(v).finish(),
		}
	}
}

struct Ctx<'a> {
	scp: &'a Scp,

	stack: VecDeque<Expr>,
	current_func: u32,
	pos: usize,
}

impl<'a> Ctx<'a> {
	fn pos(&self) -> Label {
		match self.scp.ops.get(self.pos) {
			Some(t) => t.0,
			None => self.scp.ops.last().unwrap().2,
		}
	}

	fn next(&mut self) -> Option<&'a Op> {
		let v = self.peek();
		self.pos += 1;
		v
	}

	fn peek(&self) -> Option<&'a Op> {
		self.scp.ops.get(self.pos).map(|(_, a, _)| a)
	}

	fn push(&mut self, e: Expr) {
		self.stack.push_front(e);
	}

	#[track_caller]
	fn pop(&mut self) -> Expr {
		self.stack.pop_front().unwrap()
	}

	fn pop_n(&mut self, n: usize) -> Vec<Expr> {
		self.stack.drain(..n).collect()
	}

	fn push_call(&mut self, i: Indent, call: Expr) {
		if self.peek() == Some(&Op::GetGlobal(0)) {
			self.next();
			self.stack.push_front(call);
		} else {
			println!("{i}{call:?}");
		}
	}
}

pub fn stuff(scp: &Scp) {
	let functions = scp
		.functions
		.iter()
		.map(|f| (f.start, f))
		.collect::<BTreeMap<_, _>>();

	let mut ctx = Ctx {
		scp,
		stack: VecDeque::new(),
		current_func: 0,
		pos: 0,
	};

	while ctx.peek().is_some() {
		if let Some(f) = functions.get(&ctx.pos()) {
			ctx.current_func = f.index;
			println!("\nfunction {} {:?} {:?}", f.name, (f.a0, f.a1, &f.a2, f.checksum), f.args);
			assert_eq!(ctx.stack, &[]);
			for _ in &f.args {
				ctx.stack.push_front(Expr::Arg);
			}
		}

		stmt(&mut ctx, Indent(1));

		// match op {
		// 	Op::Push(v) => {
		// 		stack.push_front(Expr::Value(v.clone()));
		// 	}
		// 	Op::Pop(n) => {
		// 		line = format!("01({n})");
		// 		for _ in 0..*n/4 {
		// 			stack.pop_front();
		// 		}
		// 	}
		// 	Op::_07(n) => {
		// 		stack.push_front(Expr::_07(*n));
		// 	}
		// 	Op::_08(n) => {
		// 		let a = stack.pop_front().unwrap();
		// 		line = format!("08({n}) = {a:?}");
		// 	}
		//
		// 	Op::GetGlobal(n) => {
		// 		stack.push_front(Expr::Global(*n));
		// 	}
		// 	Op::SetGlobal(n) => {
		// 		let a = stack.pop_front().unwrap();
		// 		line = format!("Global({n}) = {a:?}");
		// 		stack.push_front(a);
		// 	}
		//
		// 	Op::GetVar(v) => {
		// 		let d = 4 * stack.len() as i32;
		// 		stack.push_front(Expr::Var(*v + d));
		// 	}
		// 	Op::SetVar(v) => {
		// 		let a = stack.pop_front().unwrap();
		// 		let d = 4 * stack.len() as i32;
		// 		line = format!("Var({}) = {:?}", *v + d, a);
		// 	}
		//
		// 	Op::Op(n@(16..=30)) => {
		// 		let a = stack.pop_front().unwrap();
		// 		let b = stack.pop_front().unwrap();
		// 		stack.push_front(Expr::Binop(*n, a.into(), b.into()));
		// 	}
		// 	Op::Op(n@32) => {
		// 		let a = stack.pop_front().unwrap();
		// 		stack.push_front(Expr::Unop(*n, a.into()));
		// 	}
		// 	Op::Op(n) => {
		// 		line = format!("Op({n})");
		// 		stack.push_front(Expr::Op(*n))
		// 	}
		// 	Op::If2(l) => {
		// 		let a = stack.pop_front().unwrap();
		// 		line = format!("if2 {:?} {:?}", a, l);
		// 	}
		// 	Op::If(l) => {
		// 		let a = stack.pop_front().unwrap();
		// 		line = format!("if {:?} {:?}", a, l);
		// 	}
		// 	Op::Goto(l) => {
		// 		line = format!("goto {:?}", l);
		// 	}
		// 	Op::_27(n) => { // something about messages
		// 		let a = stack.pop_front().unwrap();
		// 		line = format!("27({n}) {:?}", a);
		// 	}
		// 	Op::CallFunc(a, b, n) => {
		// 		let it = stack.drain(..*n as usize).collect::<Vec<_>>();
		// 		line = format!("call {:?} {:?} {:?}", a, b, it);
		// 	}
		// 	Op::Syscall(n) => {
		// 		let pos = stack
		// 			.iter()
		// 			.position(|v| v == &Expr::Value(&Value::Uint(end.0)));
		// 		if pos.is_some_and(|pos| {
		// 			stack.get(pos + 1) == Some(&Expr::Value(&Value::Uint(current_func)))
		// 		}) {
		// 			let mut it = stack.drain(..pos.unwrap() + 2).collect::<Vec<_>>();
		// 			it.pop();
		// 			it.pop();
		// 			let call = Expr::Syscall(*n, it);
		// 			if syscall_returns(*n) {
		// 				stack.push_front(call);
		// 			} else {
		// 				line = format!("{call:?}");
		// 			}
		// 		} else {
		// 			line = format!("?syscall {} {:?}", n, stack);
		// 		}
		// 	}
		// 	Op::Syscall2(a, b, c) => {
		// 		let it = stack.drain(..*c as usize).collect::<Vec<_>>();
		// 		let call = Expr::_24(*a, *b, it);
		// 		if _24_returns((*a, *b)) {
		// 			stack.push_front(call);
		// 		} else {
		// 			line = format!("{call:?}");
		// 		}
		// 	}
		// 	Op::Line(_) => {}
		// 	_ => {
		// 		line = format!("{:?}", op);
		// 	}
		// }
		//
		// if !line.is_empty() {
		// 	println!("  {line} {stack:?}");
		// }
	}
}

#[derive(Debug, Clone, PartialEq)]
struct Indent(usize);
impl Indent {
	fn inc(&self) -> Self {
		Self(self.0 + 1)
	}
}
impl std::fmt::Display for Indent {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		for _ in 0..self.0 {
			write!(f, "  ")?;
		}
		Ok(())
	}
}

fn switch_cases(ctx: &mut Ctx<'_>) -> BTreeMap<Label, Vec<Option<i32>>> {
	let mut cases = Vec::new();
	let default = loop {
		match &ctx.next().unwrap() {
			Op::GetGlobal(0) => {
				let Op::Push(Value::Int(n)) = ctx.next().unwrap() else { unreachable!() };
				let Op::Op(21) = ctx.next().unwrap() else { unreachable!() };
				let Op::If2(target) = ctx.next().unwrap() else { unreachable!() };
				cases.push((*n, *target));
			}
			Op::Goto(default) => break *default,
			op => unreachable!("unexpected {op:?} in switch"),
		}
	};
	assert!(cases.is_sorted_by_key(|(_, a)| a));
	let mut inv_cases = BTreeMap::<Label, Vec<Option<i32>>>::new();
	for (n, target) in cases {
		inv_cases.entry(target).or_default().push(Some(n));
	}
	inv_cases.entry(default).or_default().push(None);
	inv_cases
}

fn stmt(ctx: &mut Ctx<'_>, i: Indent) {
	match ctx.next().unwrap() {
		Op::Push(v) => {
			ctx.push(Expr::Value(v.clone()));
		}
		Op::Pop(n) => {
			for _ in 0..*n / 4 {
				ctx.pop(); // TODO must be Local or Arg
			}
		}
		Op::GetVar(n) => {
			let d = 4 * ctx.stack.len() as i32;
			ctx.push(Expr::Var(*n + d));
		}
		Op::SetVar(n) => {
			let a = ctx.pop();
			let d = 4 * ctx.stack.len() as i32;
			println!("{i}Var({}) = {:?}", *n + d, a);
		}
		Op::SetGlobal(0) if ctx.peek() == Some(&Op::GetGlobal(0)) => {
			let a = ctx.pop();
			let cases = switch_cases(ctx);
			println!("{i}switch {a:?} {{");
			let mut the_end = None;
			for (target, n) in cases {
				assert_eq!(ctx.pos(), target);
				for n in n {
					match n {
						Some(n) => println!("{i}  case {n}:"),
						None => println!("{i}  default:"),
					}
				}
				loop {
					if let Some(Op::Goto(t)) = ctx.peek() {
						ctx.next();
						assert!(the_end.is_none_or(|e| e == *t));
						the_end = Some(*t);
						println!("{i}    break");
						break
					}
					stmt(ctx, i.inc().inc());
				}
			}
			assert_eq!(ctx.pos(), the_end.unwrap());
			println!("{i}}}");
		}
		Op::SetGlobal(0) => {
			let a = ctx.pop();
			println!("{i}return {a:?}");
		}
		Op::Syscall(n) => {
			let pos = ctx
				.stack
				.iter()
				.position(|v| v == &Expr::Value(Value::Uint(ctx.pos().0)))
				.unwrap();
			let it = ctx.pop_n(pos);
			assert_eq!(ctx.pop(), Expr::Value(Value::Uint(ctx.pos().0)));
			assert_eq!(ctx.pop(), Expr::Value(Value::Uint(ctx.current_func)));
			ctx.push_call(i, Expr::Syscall(*n, it));
		}
		Op::Return => {
			println!("{i}(end)");
			assert_eq!(ctx.stack, &[]);
		}
		Op::If(mut target) => {
			let a = ctx.pop();
			let has_else = false;
			println!("{i}if {a:?} {{");
			loop {
				if ctx.pos() >= target {
					assert_eq!(ctx.pos(), target);
					break
				}
				if let Some(Op::Goto(t)) = ctx.peek() {
					if !has_else {
						ctx.next();
						println!("{i}}} else {{");
						target = *t;
						continue
					} else {
						unreachable!();
					}
				}
				stmt(ctx, i.inc());
			}
			println!("{i}}}");
		}
		Op::Op(n @ (16..=30)) => {
			// 16: + (probably)
			// 21: == (certain)
			// 29: &
			// 30: |
			// 32: !
			let b = ctx.pop();
			let a = ctx.pop();
			ctx.push(Expr::Binop(*n, a.into(), b.into()));
		}
		Op::Op(n @ 32) => {
			let a = ctx.pop();
			ctx.push(Expr::Unop(*n, a.into()));
		}
		Op::CallFunc(a, b, n) => {
			let it = ctx.pop_n(*n as usize);
			ctx.push_call(i, Expr::CallFunc(a.clone(), b.clone(), it));
		}
		Op::Syscall2(a, b, c) => {
			let it = ctx.pop_n(*c as usize);
			ctx.push_call(i, Expr::Syscall2(*a, *b, it));
		}
		&Op::_25(target) => {
			println!("{i}_25 {{");
			loop {
				if ctx.pos() >= target {
					assert_eq!(ctx.pos(), target);
					break
				}
				stmt(ctx, i.inc());
			}
			println!("{i}}}");
		}
		Op::Line(_) => {}
		op => {
			println!("{i}!!{op:?} {:?}", ctx.stack);
			unimplemented!("{op:?} {:?}", ctx.stack);
		}
	}
}
