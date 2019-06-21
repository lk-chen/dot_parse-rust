
pub struct Node {
    id: String,
    label: String,
}

pub struct Edge {
    from: String,
    to: String,
    label: String,
}

pub struct Graph {
    nodes: Vec<Node>,
    edges: Vec<Edge>,
}

impl Graph {
    fn add_node(&mut self, node: Node) {
        self.nodes.push(node)
    }
}

fn parse_node(node_str: &String) -> Node {
    Node {
        id: "abc".to_string(),
        label: "def".to_string(),
    }
}

pub fn parse_graph(graph_str: &str) -> Graph {
    let tokens: Vec<&str> = graph_str.split_whitespace().collect();

    parse_graph_tokens(&tokens)
}

fn parse_graph_tokens(graph_tokens: &Vec<&str>) -> Graph {
    println!("{:?}", graph_tokens);
    let graph = Graph {
        nodes: Vec::new(),
        edges: Vec::new(),
    };

    graph
}
