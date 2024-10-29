#![feature(let_chains)]
use clap::Parser;
use std::path::{Path, PathBuf};
use std::collections::{BTreeMap, HashMap, HashSet};

#[derive(clap::Parser)]
struct Args {
	files: Vec<PathBuf>,
}

fn main() {
	tracing_subscriber::fmt::fmt()
		.with_writer(std::io::stderr)
		.init();
	let args = Args::parse();

	let mut prelude = BTreeMap::new();
	let mut prelude_order = Vec::new();
	for file in &args.files {
		let _span = tracing::info_span!("file", path = %file.display()).entered();
		if file.ends_with("mon9996_c00.da") {
			// this one has a very different format, where the first table is half the width
			tracing::info!("Skipping mon9996_c00.da");
			continue;
		}
		let Ok(data) = std::fs::read(file) else {
			tracing::error!("Failed to read file");
			continue;
		};

		let mut scena = ingert::decompile(&data).unwrap();
		let out = Path::new("out").join(file.file_name().unwrap());
		let out = std::fs::File::create(&out).unwrap();
		let out = std::io::BufWriter::new(out);
		let mut out = ingert::Write(Box::new(out));

		let mut prelude_funcs = Vec::new();
		scena.retain_mut(|item| {
			match item {
				ingert::Item::Function(f) => {
					if f.is_prelude {
						prelude_funcs.push(f.name.clone());
						if let Some(prev) = prelude.get(&f.name) {
							if prev != f {
								tracing::warn!("{} differs from prelude", f.name);
								true
							} else {
								false
							}
						} else {
							prelude.insert(f.name.clone(), f.clone());
							false
						}
					} else {
						true
					}
				}
				_ => true,
			}
		});
		prelude_order.push(prelude_funcs);

		for item in &scena {
			match item {
				ingert::Item::Global(g) => {
					if let Some(line) = g.line {
						write!(out, "line {} ", line);
					}
					writeln!(out, "global {} = {}", g.name, g.ty);
				}
				ingert::Item::Function(f) => {
					if f.is_prelude {
						write!(out, "prelude ");
					}
					write_fn(&mut out, f);
				}
			}
		}

		let printed = ingert::print::print(&scena, ingert::print::Settings {
			use_lines: true,
			show_lines: true,
		});

		writeln!(out, "{printed}");
	}

	let tiebreak = prelude.iter().map(|(k, v)| (k, tiebreak_score(v))).collect::<HashMap<_, _>>();
	let prelude_order = prelude_order.into_iter()
		.fold(Vec::new(), |a, b| ordered_union(a, b, |a, b| tiebreak[a] < tiebreak[b]));

	let out = Path::new("out").join("prelude.da");
	let out = std::fs::File::create(&out).unwrap();
	let out = std::io::BufWriter::new(out);
	let mut out = ingert::Write(Box::new(out));

	for name in prelude_order {
		write_fn(&mut out, &prelude.remove(&name).unwrap());
	}
	for f in prelude.values() {
		write_fn(&mut out, f);
	}
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum Tiebreak<'a> {
	Syscall(u8, u8),
	Name(&'a str),
}

fn tiebreak_score(v: &ingert::Function) -> Tiebreak<'_> {
	use ingert::*;
	if let [Stmt::Expr(expr), Stmt::Return(_, None)] | [Stmt::Return(_, Some(expr))] = v.body.as_slice()
		&& let Expr::Call(_, CallKind::System(a, b), args) = expr
		&& args.iter().zip(0..).all(|(arg, i)| matches!(arg, Expr::Var(_, Lvalue::Stack(j)) if j.0 == i))
	{
		Tiebreak::Syscall(*a, *b)
	} else {
		Tiebreak::Name(&v.name)
	}
}

fn ordered_union<T: Clone + std::hash::Hash + Eq>(
	a: Vec<T>,
	b: Vec<T>,
	tiebreak: impl Fn(&T, &T) -> bool,
) -> Vec<T> {
	let in_a = HashSet::<_>::from_iter(a.iter().cloned());
	let in_b = HashSet::<_>::from_iter(b.iter().cloned());
	let mut aa = a.into_iter().peekable();
	let mut bb = b.into_iter().peekable();
	let mut out = Vec::new();
	while let Some(a) = aa.peek() && let Some(b) = bb.peek() {
		if a == b {
			out.push(aa.next().unwrap());
			bb.next();
		} else {
			let only_a = !in_b.contains(a);
			let only_b = !in_a.contains(b);
			if only_a && !only_b {
				out.push(aa.next().unwrap());
			} else if !only_a && only_b {
				out.push(bb.next().unwrap());
			} else if tiebreak(a, b) {
				out.push(aa.next().unwrap());
			} else {
				out.push(bb.next().unwrap());
			}
		}
	}
	out.extend(aa);
	out.extend(bb);
	let mut dedup = HashSet::new();
	out.retain(|v| dedup.insert(v.clone()));
	out
}

fn write_fn(out: &mut ingert::Write, f: &ingert::Function) {
	write!(out, "function {}(", f.name);
	for (i, arg) in f.args.iter().enumerate() {
		if i != 0 {
			write!(out, ", ");
		}
		write!(out, "{}", arg);
	}
	writeln!(out, ")");
	if f.dup {
		writeln!(out, " (dup)");
	}
	struct Block<'a>(&'a [ingert::decompile::Stmt]);
	impl std::fmt::Display for Block<'_> {
		fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
			ingert::decompile::Stmt::display_block(self.0, f, 1)
		}
	}
	writeln!(out, "{}", Block(&f.body));
}
