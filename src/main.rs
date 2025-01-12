#![feature(let_chains)]
use clap::Parser;
use std::path::{Path, PathBuf};

#[derive(clap::Parser)]
struct Args {
	files: Vec<PathBuf>,
}

fn main() {
	tracing_subscriber::fmt::fmt()
		.with_max_level(tracing::Level::TRACE)
		.with_writer(std::io::stderr)
		.init();
	unsafe { compact_debug::enable(true) };
	let args = Args::parse();

	let mut prelude = ingert::legacy::prelude::Prelude::new();
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
		// println!("{:#?}", ingert::scp::parse_scp(&data));

		let scp = ingert::legacy::scp::parse_scp(&data).unwrap();
		let scp2 = ingert::scp::read(&data).unwrap();
		let data2 = ingert::scp::write(&scp2).unwrap();
		std::fs::write("a.bin", &data).unwrap();
		std::fs::write("b.bin", &data2).unwrap();
		let mut scena = ingert::legacy::decompile(&data).unwrap();
		// prelude.add(&mut scena);

		// print(file.file_name().unwrap(), &scena);
	}

	print("prelude.da", &prelude.into_scena());
}

fn print(name: impl AsRef<Path>, scena: &[ingert::legacy::Item]) {
	let out = Path::new("out").join(name);
	let _span = tracing::info_span!("print", path = %out.display()).entered();
	let printed = ingert::legacy::print::print(scena, ingert::legacy::print::Settings {
		show_lines: true,
		..ingert::legacy::print::Settings::default()
	});
	std::fs::write(out, &printed).unwrap();

	let (a, b) = ingert::legacy::parse::parse(&printed).unwrap();
	similar_asserts::assert_eq!(scena, &a);
}
