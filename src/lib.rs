extern crate dot;

mod ll_parser;

pub fn parse(graph_str: &str) {
    println!("File content:");
    println!("{}", graph_str);
    ll_parser::parse_graph(graph_str);
}