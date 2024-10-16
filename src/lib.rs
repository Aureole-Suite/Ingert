use std::{
	cell::Cell,
	collections::{HashSet, VecDeque},
	num::NonZeroU8,
};

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
	#[snafu(display("invalid string: {lossy_string:?}"))]
	Utf8 {
		source: std::str::Utf8Error,
		lossy_string: String,
	},
	#[snafu(display("invalid opcode {op:02X} at {pos}"))]
	Op { op: u8, pos: usize },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
	pub start: Label,
	pub count1: u8,
	pub count2: u8,
	pub a1: Vec<Value>,
	pub a2: Vec<Value>,
	pub a4: Vec<(i32, u16, Vec<TaggedValue>)>,
	pub a5: u32,
	pub a6: Value,
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
	for _ in 0..n_entries {
		let start = Label(f.u32()?);
		let count0 = f.u8()? as usize;
		let count1 = f.u8()?;
		let count2 = f.u8()?;
		let count3 = f.u8()? as usize;
		let a1 = f.u32()? as usize;
		let a2 = f.u32()? as usize;
		let a3 = f.u32()? as usize;
		let a4 = f.u32()? as usize;
		let a5 = f.u32()?;
		let a6 = value(f)?;

		let a1 = multi(&mut f.at(a1)?, count3, value)?;
		let a2 = multi(&mut f.at(a2)?, count0, value)?;
		let a4 = multi(&mut f.at(a4)?, a3, |f| {
			let x = f.i32()?;
			let y = f.u16()?;
			let z = f.u16()? as usize;
			let w = f.u32()? as usize;
			let z = multi(&mut f.at(w)?, z, tagged_value)?;
			Ok((x, y, z))
		})?;
		entries.push(Function {
			start,
			count1,
			count2,
			a1,
			a2,
			a4,
			a5,
			a6,
		});
	}
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

fn value(f: &mut Reader) -> Result<Value, ScpError> {
	let v = f.u32()?;
	let hi = v >> 30;
	let lo = v & 0x3FFFFFFF;
	match hi {
		0 => Ok(Value::Uint(lo)),
		1 => Ok(Value::Int((lo as i32) << 2 >> 2)),
		2 => Ok(Value::Float(f30(lo))),
		3 => {
			let zs = f.at(lo as usize)?.cstr()?.to_bytes();
			let s = std::str::from_utf8(zs).with_context(|_| scp::Utf8 {
				lossy_string: String::from_utf8_lossy(zs).into_owned(),
			})?;
			Ok(Value::String(s.to_string()))
		}
		_ => unreachable!(),
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
	GetReturn(u8),
	SetReturn(u8),
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

fn syscall_returns(n: u16) -> bool {
	false &&
	!matches!(n,
		| 6..=9 | 12 | 15 | 20 | 28..=29 | 31 | 45..=46 | 53 | 56 | 60..=63 | 68..=72 | 74..=76 | 82 | 87 | 92 | 93 | 95..=99
		| 100..=102 | 104 | 107..=108 | 111 | 113 | 115..=120 | 122..=123 | 125..=126 | 128 | 132 | 134 | 137..=140 | 148..=164 | 166 | 171..=183 | 186..=187 | 189..=199
		| 200..=208 | 215..=225 | 227..=228 | 230 | 232 | 234..=242 | 244 | 248..=257 | 260..=271 | 273 | 275..=284 | 291..=293 | 296..=298
		| 300..=302 | 304..=307 | 309..=320 | 322..=328 | 330..=338 | 369 | 383
		| 405 | 464 | 470
	)
}

fn _24_returns(v: (u8, u8)) -> bool {
	false &&
	!matches!(v,
		| (0, 1 | 2 | 5 | 6 | 8 | 9 | 13)
		| (1, 2 | 4 | 10 | 11 | 14 | 16 | 23 | 28 | 47 | 137 | 145 | 153 | 154 | 156 | 171 | 183)
		| (2, 0 | 2 | 3 | 4 | 5 | 12 | 22 | 23 | 24 | 25 | 26 | 27)
		| (3, 0 | 1 | 30 | 19 | 20 | 21 | 23 | 32 | 33 | 34 | 35 | 37)
		| (4, 0..=2)
		| (5, 0..=3 | 6..=11)
		| (6, 0 | 16 | 30 | 32..=33 | 35)
		| (9, 0)
		| (11, 10 | 13 | 20 | 27 | 28 | 39 | 44 | 49 | 50 | 51 | 80)
		| (12, 0..=4 | 6)
		| (13, 19 | 20 | 89)
		| (15, 0..=3)
		| (16, 0 | 1 | 3 | 6 | 9 | 12 | 13 | 14 | 16 | 19)
		| (17, 2)
		| (19, 5..=6 | 10..=14 | 18..=20)
		| (20, 0 | 3)
		| (21, 0)
		| (22, 0..=6 | 8..=12 | 15..=16)
		| (23, 8 | 11 | 22 | 23)
		| (24, 0 | 2 | 13 | 14)
		| (25, 0..=3 | 8..=9 | 11..=12 | 14..=15 | 17..=18 | 20..=21 | 28 | 30)
		| (26, 0..=2 | 4 | 5)
		| (27, 0..=1)
		| (28, 1 | 5)
	)
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
			0x00 => {
				f.check_u8(4)?; // number of bytes to push?
				Op::Push(value(&mut f)?)
			}
			0x01 => Op::Pop(f.u8()?),
			0x02 => Op::GetVar(f.i32()?),
			0x03 => Op::_03(f.i32()?),
			0x04 => Op::_04(f.i32()?),
			0x05 => Op::SetVar(f.i32()?),
			0x06 => Op::_06(f.i32()?),
			0x07 => Op::_07(f.u32()?),
			0x08 => Op::_08(f.u32()?),
			0x09 => Op::GetReturn(f.u8()?),
			0x0A => Op::SetReturn(f.u8()?),
			0x0B => Op::Goto(label(&mut f)?),
			0x0C => Op::Syscall(f.u16()?),
			0x0D => Op::Return,
			0x0E => Op::If2(label(&mut f)?),
			0x0F => Op::If(label(&mut f)?),
			0x10..=0x21 => Op::Op(op),
			0x22 => Op::CallFunc(value(&mut f)?, value(&mut f)?, f.u8()?),
			0x23 => Op::_23(value(&mut f)?, value(&mut f)?, f.u8()?),
			0x24 => {
				let a = f.u8()?;
				let b = f.u8()?;
				let c = f.u8()?;
				if c != 0 {
					f.check_u8(1)?;
					f.check_u8(4 * c)?;
				}
				Op::Syscall2(a, b, c)
			}
			0x25 => Op::_25(label(&mut f)?),
			0x26 => Op::Line(f.u16()?),
			0x27 => Op::_27(f.u8()?),
			0x28.. => return scp::Op { op, pos: start }.fail(),
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

pub fn stuff(scp: &Scp) {
	let labels = {
		let mut labels = HashSet::new();
		for (_, op, _) in &scp.ops {
			match op {
				Op::Goto(l) => labels.insert(*l),
				Op::If2(l) => labels.insert(*l),
				Op::If(l) => labels.insert(*l),
				Op::_25(l) => labels.insert(*l),
				_ => false,
			};
		}
		labels
	};

	let functions = scp
		.functions
		.iter()
		.enumerate()
		.map(|f| (f.1.start, f))
		.collect::<std::collections::HashMap<_, _>>();

	#[derive(Clone, PartialEq)]
	enum Expr<'a> {
		Value(&'a Value),
		Var(i32),
		Syscall(u16, Vec<Expr<'a>>),
		_24(u8, u8, Vec<Expr<'a>>),
		Op(u8),
		Unop(u8, Box<Expr<'a>>),
		Binop(u8, Box<Expr<'a>>, Box<Expr<'a>>),
		Local,
		Arg,
		_07(u32),
		Result(u8),
	}

	impl std::fmt::Debug for Expr<'_> {
		fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
			match self {
				Expr::Value(v) => v.fmt(f),
				Expr::Var(n) => f.debug_tuple("Var").field(n).finish(),
				Expr::Syscall(n, v) => f.debug_tuple("Syscall").field(n).field(v).finish(),
				Expr::_24(a, b, v) => f.debug_tuple("_24").field(a).field(b).field(v).finish(),
				Expr::Op(v) => f.debug_tuple("Op").field(v).finish(),
				Expr::Unop(v, a) => f.debug_tuple("Unop").field(v).field(a).finish(),
				Expr::Binop(v, a, b) => f.debug_tuple("Binop").field(v).field(a).field(b).finish(),
				Expr::Local => f.write_str("Local"),
				Expr::Arg => f.write_str("Arg"),
				Expr::_07(v) => f.debug_tuple("_07").field(v).finish(),
				Expr::Result(v) => f.debug_tuple("Result").field(v).finish(),
			}
		}
	}

	let mut stack = VecDeque::new();
	let mut current_func = 0;
	for (start, op, end) in &scp.ops {
		if let Some((i, f)) = functions.get(start) {
			current_func = *i as u32;
			println!("\nfunction {:?}, {:?} {:?}", f.a6, f.a1, f.a2);
			for v in &f.a4 {
				println!("  :{:?}", v);
			}
			stack.clear();
			for _ in &f.a2 {
				stack.push_front(Expr::Arg);
			}
		}
		if labels.contains(start) {
			println!("{start:?}:");
		}

		let mut line = String::new();
		line = format!("{op:?}");

		match op {
			Op::Push(v) => {
				stack.push_front(Expr::Value(v));
			}
			Op::Pop(n) => {
				line = format!("01({n})");
				for _ in 0..*n/4 {
					stack.pop_front().unwrap();
				}
			}
			Op::_07(n) => {
				stack.push_front(Expr::_07(*n));
			}
			Op::_08(n) => {
				let a = stack.pop_front().unwrap();
				line = format!("08({n}) = {a:?}");
			}
			Op::SetReturn(n) => {
				let a = stack.pop_front().unwrap();
				line = format!("SetReturn({n}) = {a:?}");
			}
			Op::GetReturn(n) => {
				stack.push_front(Expr::Result(*n));
			}
			Op::GetVar(v) => {
				let d = 4 * stack.len() as i32;
				stack.push_front(Expr::Var(*v + d));
			}
			Op::SetVar(v) => {
				let a = stack.pop_front().unwrap();
				let d = 4 * stack.len() as i32;
				line = format!("Var({}) = {:?}", *v + d, a);
			}
			Op::Op(n@(16..=30)) => {
				let a = stack.pop_front().unwrap();
				let b = stack.pop_front().unwrap();
				stack.push_front(Expr::Binop(*n, a.into(), b.into()));
			}
			Op::Op(n@32) => {
				let a = stack.pop_front().unwrap();
				stack.push_front(Expr::Unop(*n, a.into()));
			}
			Op::Op(n) => {
				line = format!("Op({n})");
				stack.push_front(Expr::Op(*n))
			}
			Op::If2(l) => {
				let a = stack.pop_front().unwrap();
				line = format!("if2 {:?} {:?}", a, l);
				stack.push_front(Expr::Local); // TODO no idea what if2 is
			}
			Op::If(l) => {
				let a = stack.pop_front().unwrap();
				line = format!("if {:?} {:?}", a, l);
			}
			Op::Goto(l) => {
				line = format!("goto {:?}", l);
			}
			Op::_27(n) => { // something about messages
				let a = stack.pop_front().unwrap();
				line = format!("27({n}) {:?}", a);
			}
			Op::CallFunc(a, b, n) => {
				let it = stack.drain(..*n as usize).collect::<Vec<_>>();
				line = format!("call {:?} {:?} {:?}", a, b, it);
			}
			Op::Syscall(n) => {
				let pos = stack
					.iter()
					.position(|v| v == &Expr::Value(&Value::Uint(end.0)));
				if pos.is_some_and(|pos| {
					stack.get(pos + 1) == Some(&Expr::Value(&Value::Uint(current_func)))
				}) {
					let mut it = stack.drain(..pos.unwrap() + 2).collect::<Vec<_>>();
					it.pop();
					it.pop();
					let call = Expr::Syscall(*n, it);
					if syscall_returns(*n) {
						stack.push_front(call);
					} else {
						line = format!("{call:?}");
					}
				} else {
					line = format!("?syscall {} {:?}", n, stack);
				}
			}
			Op::Syscall2(a, b, c) => {
				let it = stack.drain(..*c as usize).collect::<Vec<_>>();
				let call = Expr::_24(*a, *b, it);
				if _24_returns((*a, *b)) {
					stack.push_front(call);
				} else {
					line = format!("{call:?}");
				}
			}
			// Op::_09(_) => {
			// 	unreachable!(); // only used after syscall?
			// }
			Op::Line(_) => {}
			_ => {
				line = format!("{:?}", op);
			}
		}

		if !line.is_empty() {
			println!("  {line} {stack:?}");
		}
	}
}
