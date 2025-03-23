#![feature(let_chains)]
use clap::Parser;
use std::path::PathBuf;
use tracing_subscriber::prelude::*;

#[derive(clap::Parser)]
struct Args {
	files: Vec<PathBuf>,
}

fn main() {
	tracing_subscriber::registry()
		.with(tracing_subscriber::fmt::layer().with_writer(std::io::stderr))
		.with(tracing_subscriber::EnvFilter::from_default_env())
		.init();
	unsafe { compact_debug::enable(true) };
	let args = Args::parse();

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

		// let scp = ingert::legacy::scp::parse_scp(&data).unwrap();
		let scp2 = ingert::scp::read(&data).unwrap();
		let mut scena2 = ingert::scena::from_scp(scp2.clone());
		let _data2 = ingert::scp::write(&scp2).unwrap();
		ingert::scena::decompile(&mut scena2);
		// println!("{:#?}", scena);
		// std::fs::write("a.bin", &data).unwrap();
		// std::fs::write("b.bin", &data2).unwrap();
		// let mut scena = ingert::legacy::decompile(&data).unwrap();
		// prelude.add(&mut scena);

		// print(file.file_name().unwrap(), &scena);
	}
}
