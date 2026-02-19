use anyhow::Context as _;
use clap::Parser;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use ingert::scena::DecompileOptions;
use ingert_syntax::diag;
use walkdir::WalkDir;
use std::path::{Path, PathBuf};
use std::process::ExitCode;
use tracing_subscriber::prelude::*;

#[derive(clap::ValueEnum, Clone, Copy)]
enum DecompileMode {
	Asm,
	Flat,
	Tree,
}

#[derive(clap::Parser)]
struct Args {
	files: Vec<PathBuf>,

	#[clap(long, default_value = "tree", help = "Decompile mode")]
	mode: DecompileMode,
	#[clap(long, help = "Do not minimize called table")]
	no_called: bool,
	#[clap(long, help = "Do not perform steps that fail to roundtrip")]
	roundtrip: bool,

	#[clap(long, short, help = "Output file")]
	output: Option<PathBuf>,
}

fn main() -> ExitCode {
	tracing_subscriber::registry()
		.with(tracing_subscriber::fmt::layer().with_writer(std::io::stderr))
		.with(tracing_subscriber::EnvFilter::builder()
			.with_default_directive(tracing::Level::INFO.into())
			.from_env_lossy())
		.init();
	let args = Args::parse();

	let mut success = true;

	if args.output.is_some() && args.files.len() > 1 {
		tracing::error!("Cannot specify output file with multiple input files");
		success = false;
	} else {
		for path in &args.files {
			let _span = tracing::info_span!("process_arg", path = %path.display()).entered();
			if !path.exists() {
				tracing::error!("File does not exist: {}", path.display());
				success = false;
			} else if path.is_dir() {
				success &= handle_dir(&args, path, args.output.as_deref());
			} else {
				success &= handle_file(&args, path, args.output.as_deref());
			}
		}
	}

	if success {
		ExitCode::SUCCESS
	} else {
		windows_wait();
		ExitCode::FAILURE
	}
}

#[cfg(target_os = "windows")]
fn windows_wait() {
	use windows_sys::Win32::System::Console::{GetConsoleProcessList, GetConsoleWindow};
	if unsafe { GetConsoleWindow() }.is_null() {
		return;
	}
	let process_count: u32 = unsafe { GetConsoleProcessList([0].as_mut_ptr(), 1) };
	if process_count == 1 {
		std::process::Command::new("cmd").arg("/c").arg("pause").status().ok();
	}
}

#[cfg(not(target_os = "windows"))]
fn windows_wait() {}

fn handle_dir(args: &Args, path: &Path, out: Option<&Path>) -> bool {
	let mut ing = Vec::new();
	let mut dat = Vec::new();
	for entry in WalkDir::new(path).into_iter().filter_map(|v| v.ok()) {
		if entry.metadata().is_ok_and(|m| m.is_file()) {
			if entry.path().extension().is_some_and(|e| e == "ing") {
				ing.push(entry.path().strip_prefix(path).unwrap().to_owned());
			} else if entry.path().extension().is_some_and(|e| e == "dat") {
				dat.push(entry.path().strip_prefix(path).unwrap().to_owned());
			}
		}
	}

	if !ing.is_empty() && !dat.is_empty() {
		tracing::error!("Found both ing ({}) and dat ({}) files in the same directory", ing[0].display(), dat[0].display());
		false
	} else if !ing.is_empty() {
		let outdir = out_dir(path, out, ".ing", ".dat");
		let mut success = true;
		for file in ing {
			let infile = path.join(&file);
			let outfile = out_file(&outdir.join(&file), ".ing", ".dat");
			success &= compile(args, &infile, &outfile);
		}
		success
	} else if !dat.is_empty() {
		let outdir = out_dir(path, out, ".dat", ".ing");
		let mut success = true;
		for file in dat {
			let infile = path.join(&file);
			let outfile = out_file(&outdir.join(&file), ".dat", ".ing");
			success &= decompile(args, &infile, &outfile);
		}
		success
	} else {
		tracing::error!("No ing or dat files found in directory");
		false
	}
}

fn handle_file(args: &Args, path: &Path, out: Option<&Path>) -> bool {
	if path.extension().is_some_and(|e| e == "ing") {
		let infile = path;
		let outfile = out.map_or_else(|| out_file(path, ".ing", ".dat"), |x| x.to_owned());
		compile(args, infile, &outfile)
	} else if path.extension().is_some_and(|e| e == "dat") {
		let infile = path;
		let outfile = out.map_or_else(|| out_file(path, ".dat", ".ing"), |x| x.to_owned());
		decompile(args, infile, &outfile)
	} else {
		tracing::error!("File is not ing or dat");
		false
	}
}

fn out_file(path: &Path, old_suffix: &str, new_suffix: &str) -> PathBuf {
	let name = path.file_name().unwrap().to_str().unwrap();
	let name = name.strip_suffix(old_suffix).expect("suffix is already checked");
	path.with_file_name(format!("{name}{new_suffix}"))
}

fn out_dir(path: &Path, out: Option<&Path>, old_suffix: &str, new_suffix: &str) -> PathBuf {
	if let Some(out) = out {
		return out.to_owned();
	}
	let name = path.file_name().unwrap().to_str().unwrap();
	if let Some(name) = name.strip_suffix(old_suffix) {
		path.with_file_name(name)
	} else {
		path.with_file_name(format!("{name}{new_suffix}"))
	}
}

fn decompile(args: &Args, infile: &Path, outfile: &Path) -> bool {
	let _span = tracing::error_span!("decompile", file = %infile.display()).entered();
	match decompile_inner(args, infile, outfile) {
		Ok(v) => v,
		Err(e) => {
			if infile.file_name().is_some_and(|n| n == "mon9996_c00.dat") {
				tracing::warn!("{e:?}");
				tracing::warn!("This file is known to be broken");
			} else {
				tracing::error!("{e:?}");
				tracing::error!("This is probably a bug in Ingert, please report it.");
			}
			false
		}
	}
}

fn compile(_args: &Args, infile: &Path, outfile: &Path) -> bool {
	let _span = tracing::error_span!("compile", file = %infile.display()).entered();
	match compile_inner(infile, outfile) {
		Ok(v) => v,
		Err(e) => {
			tracing::error!("{e:?}");
			tracing::error!("This is probably a bug in Ingert, please report it.");
			false
		}
	}
}

fn decompile_inner(args: &Args, infile: &Path, outfile: &Path) -> anyhow::Result<bool> {
	let opts = DecompileOptions {
		mode: match args.mode {
			DecompileMode::Asm => ingert::scena::DecompileMode::Asm,
			DecompileMode::Flat => ingert::scena::DecompileMode::Flat,
			DecompileMode::Tree => ingert::scena::DecompileMode::Tree,
		},
		called: !args.no_called,
		roundtrip: args.roundtrip,
	};

	let data = std::fs::read(infile).with_context(|| format!("failed to read file: {}", infile.display()))?;
	if !data.starts_with(b"#scp") {
		tracing::error!("File is not a valid scp file");
		return Ok(false);
	}
	let scp = ingert::scp::read(&data).context("failed to read scp")?;
	let mut scena = ingert::scena::from_scp(scp);
	ingert::scena::decompile(&mut scena, &opts);
	let str = ingert_syntax::print::print(&scena);
	if let Some(parent) = outfile.parent() && !parent.exists() {
		std::fs::create_dir_all(parent).with_context(|| format!("failed to create directory: {}", parent.display()))?;
	}
	std::fs::write(outfile, &str).with_context(|| format!("failed to write file: {}", outfile.display()))?;
	Ok(true)
}

fn compile_inner(infile: &Path, outfile: &Path) -> anyhow::Result<bool> {
	let source = std::fs::read_to_string(infile).with_context(|| format!("failed to read file: {}", infile.display()))?;
	let mut errors = diag::Errors::new();
	let tokens = ingert_syntax::lex::lex(&source, &mut errors);
	if errors.max_severity() >= diag::Severity::Fatal {
		print_errors(infile, &errors, &source);
		return Ok(false);
	}
	let scena = ingert_syntax::parse::parse(&tokens, &mut errors);
	print_errors(infile, &errors, &source);
	if errors.max_severity() >= diag::Severity::Error {
		return Ok(false);
	}
	let scp = ingert::scena::compile(scena).context("failed to compile")?;
	let data = ingert::scp::write(&scp).context("failed to write scp")?;
	if let Some(parent) = outfile.parent() && !parent.exists() {
		std::fs::create_dir_all(parent).with_context(|| format!("failed to create directory: {}", parent.display()))?;
	}
	std::fs::write(outfile, &data)?;
	Ok(true)
}

fn print_errors(path: &Path, errors: &diag::Errors, str: &str) {
	let writer = StandardStream::stderr(ColorChoice::Always);
	let config = codespan_reporting::term::Config::default();
	let file = SimpleFile::new(path.display().to_string(), &str);

	let mut errors = errors.errors.iter().collect::<Vec<_>>();
	errors.sort_by(|a, b| a.sort_key().cmp(&b.sort_key()));

	for error in &errors {
		let diag = to_diagnostic(error);
		codespan_reporting::term::emit(&mut writer.lock(), &config, &file, &diag).unwrap();
	}
}

fn to_diagnostic(error: &diag::Diagnostic) -> Diagnostic<()> {
	let severity = match error.severity {
		diag::Severity::Fatal => codespan_reporting::diagnostic::Severity::Error,
		diag::Severity::Error => codespan_reporting::diagnostic::Severity::Error,
		diag::Severity::Warning => codespan_reporting::diagnostic::Severity::Warning,
		diag::Severity::Info => codespan_reporting::diagnostic::Severity::Note,
	};
	Diagnostic::new(severity)
		.with_message(&error.main.desc)
		.with_label(Label::primary((), error.main.span.clone()).with_message(&error.main.desc))
		.with_labels_iter(error.notes.iter().map(|note| {
			Label::secondary((), note.span.clone()).with_message(&note.desc)
		}))
}
