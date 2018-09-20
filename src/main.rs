#![feature(uniform_paths)]
use clap::{
    app_from_crate, crate_authors, crate_description, crate_name, crate_version, AppSettings, Arg,
};
use failure::{bail, Fallible};
use ink::{Story, StoryState};
use std::{
    ffi::{OsStr, OsString},
    fs::File,
    path::Path,
};

fn main() -> Fallible<()> {
    include_str!("../Cargo.toml"); // Force recompile when manifest is modified

    let args = app_from_crate!()
        .settings(&[
            AppSettings::ArgRequiredElseHelp,
            AppSettings::DontCollapseArgsInUsage,
            AppSettings::UnifiedHelpMessage,
        ]).arg(
            Arg::with_name("input")
                .takes_value(true)
                .required(true)
                .empty_values(false)
                .value_name("FILE")
                .validator_os(validate_file_exists)
                .help("The input ink file"),
        ).arg(
            Arg::with_name("output")
                .short("o")
                .long("output")
                .visible_alias("out")
                .takes_value(true)
                .empty_values(false)
                .value_name("COMPILED-FILE")
                .validator_os(validate_not_dir)
                .help("Output compiled file"),
        ).arg(
            Arg::with_name("compile")
                .short("c")
                .long("compile")
                .help("Validate and compile the input file then exit; does not play file"),
        ).arg(
            Arg::with_name("verbose")
                .short("v")
                .long("verbose")
                .help("Enable verbose diagnostic messages"),
        ).arg(
            Arg::with_name("quiet")
                .short("q")
                .long("quiet")
                .conflicts_with("verbose")
                .help("Disable all diagnostic messages except errors"),
        ).get_matches();

    // Initialize logging
    if args.is_present("verbose") {
        simple_logging::log_to_stderr(log::LevelFilter::Trace);
    } else if args.is_present("quiet") {
        simple_logging::log_to_stderr(log::LevelFilter::Error);
    } else {
        simple_logging::log_to_stderr(log::LevelFilter::Info);
    }

    // Load story
    let infile = Path::new(args.value_of_os("input").unwrap());
    let story = if !infile.ends_with(".ink.json") {
        // TODO: Does not currently support compiling from non-JSON files
        bail!("ERROR: Compiling .ink files is not implemented yet!");
    } else {
        Story::read_json(File::open(infile)?)?
    };

    // Write output
    for &outfile in args.value_of_os("output").map(Path::new).iter() {
        if outfile != infile {
            story.write_json(File::create(outfile)?)?
        }
    }

    // Play in interactive mode
    if !args.is_present("compile") {
        begin(&story);
    }

    Ok(())
}

fn validate_file_exists(s: &OsStr) -> Result<(), OsString> {
    let path = Path::new(s);
    if !path.is_file() {
        let mut err = s.to_os_string();
        err.push(" is not a valid file");
        Err(err)
    } else {
        Ok(())
    }
}

fn validate_not_dir(s: &OsStr) -> Result<(), OsString> {
    let path = Path::new(s);
    if path.is_dir() {
        let mut err = s.to_os_string();
        err.push(" is a directory");
        Err(err)
    } else {
        Ok(())
    }
}

fn begin(story: &Story) {
    let _state = StoryState::new(&story);
}
