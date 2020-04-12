# dot_parse-rust

A library for parsing Graphviz DOT language files.

# Usage

## As executable

```
cargo run filename.txt
```

## As library

```
extern crate libdot_parse;

if let Some(graph) = libdot_parse::Graph::parse_from(tokens.as_slice()) {
    println!("{:?}", graph)
} else {
    println!("FAILED")
}
```

# Known Issues

* **strict** keyword doesn't apply
* required to use **;** to separate statments
* executable only separates tokens with whitespace, e.g. **color=blue** doesn't work, while you have to give **color = blue**

# License

MIT
