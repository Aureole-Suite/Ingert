use clap::Parser;
use std::path::PathBuf;

#[derive(clap::Parser)]
struct Args {
	files: Vec<PathBuf>,
}

fn main() {
	tracing::subscriber::set_global_default(tracing_subscriber::FmtSubscriber::new()).unwrap();
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

	match ingert::parse_da(&data) {
		Ok(v) => ingert::stuff(&v),
		Err(e) => tracing::error!("Error: {}", snafu::Report::from_error(e)),
	}
}
