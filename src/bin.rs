use std::env;
use std::fs;

extern crate libdot_parse;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("Expect exact one file name");
        return
    }

    let graph_str = match fs::read_to_string(&args[1]) {
        Ok(content) => content,
        Err(err) => {
            println!("Error reading file: {:?}", err);
            return
        }
    };
    let tokens: Vec<&str> = graph_str.split_whitespace().collect();
    if let Some(graph) = libdot_parse::Graph::parse_from(tokens.as_slice()) {
        // println!("{:?}", graph)
    } else {
        println!("FAILED")
    }
}