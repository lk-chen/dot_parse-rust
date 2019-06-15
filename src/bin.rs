use std::env;
use std::fs;

mod dot_parse;

fn main() -> Result<(), &'static str> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        return Err("Expect exact one file name");
    }

    let file_name = &args[1];
    let graph_str = fs::read_to_string(file_name).expect("Failed reading file");

    dot_parse::parse(&graph_str);
    Ok(())
}