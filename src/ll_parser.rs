extern crate dot;

use std::collections::HashMap;

macro_rules! matches(
    ($e:expr, $p:pat) => (
        match $e {
            $p => true,
            _ => false
        }
    )
);

#[derive(Copy, Clone)]
enum CompassPt {N, NE, E, SE, S, SW, W, NW, C, UND}

pub struct Stmt<'a> {
    node_stmt: Option<NodeStmt<'a>>,
    edge_stmt: Option<EdgeStmt<'a>>,
    attr_stmt: Option<AttrStmt<'a>>,
    subgraph: Option<SubGraph<'a>>,
    key_value: (dot::Id<'a>, dot::Id<'a>)
}

type StmtList<'a> = Vec<Stmt<'a>>;

#[derive(Default)]
pub struct Port<'a> {
    id: Option<dot::Id<'a>>,
    compass_pt: Option<CompassPt>,
}

impl PartialEq for Port<'_> {
    fn eq(&self, other: &Self) -> bool {
        fn id_match(lhs: &Port, rhs: &Port) -> bool {
            match ((*lhs).id.as_ref(), (*rhs).id.as_ref()) {
                (None, None) => true,
                (Some(a), Some(b)) => a.as_slice() == b.as_slice(),
                _ => false,
            }
        }

        match (self.compass_pt, other.compass_pt) {
            (None, None) => id_match(self, other),
            (Some(a), Some(_b)) => !matches!(a, _b) && id_match(self, other),
            _ => false,
        }
    }
}

impl<'a> Port<'a> {
    fn parse_from(tokens: Vec<&str>) -> Result<Port<'_>, ()> {
        if tokens.len() == 2 {
            if tokens[0] != ":" {
                return Err(());
            } else {
                match Port::parse_compass_pt_from(tokens[1]) {
                    Ok(comp) => Ok(Port{compass_pt: Some(comp), ..Default::default()}),
                    Err(_) => {
                        match dot::Id::new(tokens[1]) {
                            Ok(id) => Ok(Port{id: Some(id), ..Default::default()}),
                            Err(_) => Err(()),
                        }
                    }
                }
            }
        } else if tokens.len() == 4 {
            if tokens[0] != ":" || tokens[2] != ":" {
                return Err(())
            } else {
                match Port::parse_compass_pt_from(tokens[3]) {
                    Err(_) => Err(()),
                    Ok(comp) => {
                        match dot::Id::new(tokens[1]) {
                            Ok(id) => Ok(Port{id: Some(id), compass_pt: Some(comp)}),
                            Err(_) => Err(())
                        }
                    }
                }
            }
        } else {
            Err(())
        }
    }

    fn parse_compass_pt_from(token: &str) -> Result<CompassPt, ()> {
        match token {
            "n" => Ok(CompassPt::N),
            "ne" => Ok(CompassPt::NE),
            "e" => Ok(CompassPt::E) ,
            "se" => Ok(CompassPt::SE),
            "s" => Ok(CompassPt::S),
            "sw" => Ok(CompassPt::SW),
            "w" => Ok(CompassPt::W),
            "nw" => Ok(CompassPt::NW),
            "c" => Ok(CompassPt::C),
            "_" => Ok(CompassPt::UND),
            _ => Err(()),
        }
    }
}

pub struct NodeId<'a> {
    id: dot::Id<'a>,
    port: Option<Port<'a>>
}

impl<'a> NodeId<'a> {
    fn parse_from(tokens: Vec<&str>) -> Result<NodeId<'_>, ()> {
        if tokens.len() == 0 {
            return Err(());
        }
        match dot::Id::new(tokens[0]) {
            Err(_) => Err(()),
            Ok(id) => {
                match NodeId::parse_option_port_from(tokens[1..].to_vec()) {
                    Err(_) => Err(()),
                    Ok(port) => Ok(NodeId{id: id, port: port}),
                }
            }
        }
    }

    fn parse_option_port_from(tokens: Vec<&str>) -> Result<Option<Port<'_>>, ()> {
        if tokens.len() == 0 {
            return Ok(None)
        }
        match Port::parse_from(tokens) {
            Ok(port) => Ok(Some(port)),
            Err(_) => Err(()),
        }
    }
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

// impl NodeStmt<'_> {
//     fn parse_from(tokens: Vec<&str>) -> Result<NodeStmt<'_>, &'static str> {

//     }
// }

#[cfg(test)]
mod tests {
    use super::{NodeId};

    #[test]
    fn parse_nodeid() {
        let n1 = NodeId::parse_from(["id1"].to_vec()).unwrap();
        assert_eq!(n1.id.name(), "id1");
    }
}