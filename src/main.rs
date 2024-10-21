use clap::Parser;
use std::path::{Path, PathBuf};

#[derive(clap::Parser)]
struct Args {
	files: Vec<PathBuf>,
}

fn main() {
	tracing_subscriber::fmt::fmt()
		.with_writer(std::io::stderr)
		.init();
	let args = Args::parse();
	for file in args.files {
		process_file(&file);
	}
}

#[tracing::instrument]
fn process_file(file: &PathBuf) {
	// if !file.ends_with("t5400.da") {
	// 	return;
	// }
	if file.ends_with("mon9996_c00.da") {
		// this one has a very different format, where the first table is half the width
		tracing::info!("Skipping mon9996_c00.da");
		return;
	}
	tracing::trace!("reading");
	let Ok(data) = std::fs::read(file) else {
		tracing::error!("Failed to read file");
		return;
	};

	let _ = std::panic::catch_unwind(|| {
		let out = Path::new("out").join(file.file_name().unwrap());
		let out = std::fs::File::create(&out).unwrap();
		let out = std::io::BufWriter::new(out);
		let mut out = ingert::Write(Box::new(out));
		let scp = ingert::parse_scp(&data).unwrap();
		for (name, val) in &scp.globals {
			writeln!(out, "global {name} = {val}");
		}
		if !scp.globals.is_empty() {
			writeln!(out);
		}
		let mut functions = scp.functions.iter().collect::<Vec<_>>();
		functions.sort_by_key(|f| f.start);
		for f in &functions {
			writeln!(out, "{}", f);
			for c in &f.code {
				writeln!(out, "  {c:?}");
			}
			writeln!(out);

			let mut was_line = false;
			let stmts = ingert::nest::decompile(f).unwrap();
			for stmt in &stmts {
				if !was_line {
					write!(out, "  ");
				}
				if let ingert::nest::Stmt::Line(_) = stmt {
					was_line = true;
					write!(out, "{stmt} ");
				} else {
					was_line = false;
					writeln!(out, "{stmt}");
				}
			}
			writeln!(out);

			let f = ingert::decompile::decompile(f.args.len(), &stmts).unwrap();
			struct Block<'a>(&'a [ingert::decompile::Stmt]);
			impl std::fmt::Display for Block<'_> {
				fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
					ingert::decompile::Stmt::display_block(self.0, f, 1)
				}
			}
			writeln!(out, "{}", Block(&f));
		}
	});
}
