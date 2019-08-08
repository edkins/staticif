#[macro_use]
extern crate clap;
#[macro_use]
extern crate log;

mod check;
mod error;
mod parse;

use std::fs::read_to_string;
use std::process::exit;

use crate::check::check;
use crate::error::Error;
use crate::parse::parse_file;

fn process(input_string: &str) -> Result<(),Error> {
    let m = parse_file(&input_string)?;
    warn!("{:#?}", m);
    check(&m)?;
    warn!("OK");
    Ok(())
}

fn run(input_file: &str) -> Result<(),Error> {
    let input_string = read_to_string(&input_file)?;
    process(&input_string).map_err(|e| e.to_msg(&input_string))?;
    Ok(())
}

fn main() {
    let matches = clap_app!(staticif =>
            (version: "0.1.0")
            (@arg INPUT: +required "Sets the input file to use")
            (@arg quiet: -q "Disables output messages")
            (@arg verbosity: -v +multiple "Increases message verbosity")
        ).get_matches();

    stderrlog::new()
        .module(module_path!())
        .quiet(matches.is_present("quiet"))
        .verbosity(matches.occurrences_of("verbosity") as usize)
        .timestamp(stderrlog::Timestamp::Millisecond)
        .init()
        .unwrap();

    let input_file = matches.value_of("INPUT").unwrap();
    
    match run(input_file) {
        Ok(()) => {}
        Err(Error::Msg(msg)) => {
            error!("\n{}", msg);
            exit(1)
        }
        Err(e) => {
            error!("{:?}", e);
            exit(1)
        }
    }
}
