extern crate dot;

use std::collections::HashMap;

enum CompassPt {N, NE, E, SE, S, SW, W, NW, C, UND}

pub struct Stmt<'a> {
    node_stmt: Option<NodeStmt<'a>>,
    edge_stmt: Option<EdgeStmt<'a>>,
    attr_stmt: Option<AttrStmt<'a>>,
    subgraph: Option<SubGraph<'a>>,
    key_value: (dot::Id<'a>, dot::Id<'a>)
}

type StmtList<'a> = Vec<Stmt<'a>>;

pub struct Port<'a> {
    id: Option<dot::Id<'a>>,
    compass_pt: Option<CompassPt>
}

pub struct NodeId<'a> {
    id: dot::Id<'a>,
    port: Option<Port<'a>>
}

type AList<'a> = HashMap<dot::Id<'a>, dot::Id<'a>>;

type AttrList<'a> = Vec<AList<'a>>;

enum AttrStmtKey {GRAPH, NODE, EDGE}

pub struct AttrStmt<'a> {
    key: AttrStmtKey,
    attr_list : AttrList<'a>
}

pub struct NodeStmt<'a> {
    id: NodeId<'a>,
    attr_list : Option<AttrList<'a>>
}

pub struct SubGraph<'a> {
    id: Option<dot::Id<'a>>,
    stmt_list: StmtList<'a>
}

pub struct EdgeRhs<'a> {
    edgeop: std::borrow::Cow<'a, str>,  //An edgeop is -> in directed graphs and -- in undirected graphs.
    node_id : Option<NodeId<'a>>,
    subgraph: Option<SubGraph<'a>>
}

pub struct EdgeStmt<'a> {
    node_id : Option<NodeId<'a>>,
    subgraph: Option<SubGraph<'a>>,
    rhs: Vec<EdgeRhs<'a>>,
    attr_list: Option<AttrList<'a>>
}

pub struct Graph<'a> {
    strict: bool,
    kind: dot::Kind,
    id: Option<dot::Id<'a>>,
    stmt_list: StmtList<'a>
}

#[cfg(test)]
mod tests {
    #[test]
    fn s() {
        assert!(true);
    }
}