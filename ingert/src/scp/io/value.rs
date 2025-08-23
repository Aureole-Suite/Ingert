use gospel::read::{Le as _, Reader};
use gospel::write::{Label, Le as _, Writer};
use snafu::ResultExt as _;
pub use super::super::Value;

#[derive(Debug, snafu::Snafu)]
pub enum ValueError {
	#[snafu(display("invalid read (at {location})"), context(false))]
	Read {
		source: gospel::read::Error,
		#[snafu(implicit)]
		location: snafu::Location,
	},
	#[snafu(display("unexpected special value {v}"))]
	Special { v: u32 },
	#[snafu(display("invalid string: {lossy_string:?}"))]
	Utf8 {
		source: std::str::Utf8Error,
		lossy_string: String,
	},
	#[snafu(display("expected string, got {value:?}"))]
	NeedString { value: Value }
}

pub fn value(f: &mut Reader) -> Result<Value, ValueError> {
	let v = f.u32()?;
	let hi = v >> 30;
	let lo = v & 0x3FFFFFFF;
	match hi {
		0 => SpecialSnafu { v }.fail()?,
		1 => Ok(Value::Int((lo as i32) << 2 >> 2)),
		2 => Ok(Value::Float(f30(lo))),
		3 => Ok(Value::String(string(&mut f.at(lo as usize)?)?)),
		_ => unreachable!(),
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

fn string(f: &mut Reader) -> Result<String, ValueError> {
	let zs = f.cstr()?.to_bytes();
	let s = std::str::from_utf8(zs).with_context(|_| Utf8Snafu {
		lossy_string: String::from_utf8_lossy(zs).into_owned(),
	})?;
	Ok(s.to_string())
}

pub fn string_value(f: &mut Reader) -> Result<String, ValueError> {
	match value(f)? {
		Value::String(s) => Ok(s),
		value => NeedStringSnafu { value }.fail(),
	}
}

pub fn write_value(f: &mut Writer, g: &mut Writer, v: &Value) {
	match v {
		Value::Int(v) => write_int_value(f, *v),
		Value::Float(v) => write_float_value(f, *v),
		Value::String(v) => write_string_value(f, g, v),
	}
}

pub fn write_int_value(f: &mut Writer, v: i32) {
	f.u32(1 << 30 | (v as u32 & 0x3FFFFFFF));
}

pub fn write_float_value(f: &mut Writer, v: f32) {
	f.u32(2 << 30 | (f32::to_bits(v) >> 2));
}

pub fn write_string_value(f: &mut Writer, g: &mut Writer, v: &str) {
	write_string_label(f, write_string(g, v))
}

pub fn write_string(g: &mut Writer, v: &str) -> Label {
	let pos = g.here();
	g.slice(v.as_bytes());
	g.u8(0);
	pos
}

pub fn write_string_label(f: &mut Writer, pos: Label) {
	f.delay(move |c| Ok((3 << 30 | c.label(pos)? as u32).to_le_bytes()));
}
