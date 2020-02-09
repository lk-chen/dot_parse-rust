extern crate dot;

use std::collections::HashMap;
use std::fmt;

macro_rules! matches(
    ($e:expr, $p:pat) => (
        match $e {
            $p => true,
            _ => false
        }
    )
);

fn unwrap_to_printable<'a>(id: &'a Option<dot::Id<'_>>) -> Option<&'a str> {
    match &id {
        None => None,
        Some(a) => Some(a.as_slice()),
    }
}

#[derive(Copy, Clone)]
enum CompassPt {
    N,
    NE,
    E,
    SE,
    S,
    SW,
    W,
    NW,
    C,
    UND,
}

impl fmt::Debug for CompassPt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let comp_str = match self {
            CompassPt::N => "N",
            CompassPt::NE => "NE",
            CompassPt::E => "E",
            CompassPt::SE => "SE",
            CompassPt::S => "S",
            CompassPt::SW => "SW",
            CompassPt::W => "W",
            CompassPt::NW => "NW",
            CompassPt::C => "C",
            CompassPt::UND => "UND",
        };
        write!(f, "{:?}", comp_str)
    }
}

pub struct Stmt<'a> {
    node_stmt: Option<NodeStmt<'a>>,
    edge_stmt: Option<EdgeStmt<'a>>,
    attr_stmt: Option<AttrStmt<'a>>,
    subgraph: Option<SubGraph<'a>>,
    key_value: (dot::Id<'a>, dot::Id<'a>),
}

type StmtList<'a> = Vec<Stmt<'a>>;

#[derive(Default)]
pub struct Port<'a> {
    id: Option<dot::Id<'a>>,
    compass_pt: Option<CompassPt>,
}

impl fmt::Debug for Port<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Port{{id={:?}, compass={:?}}}",
            unwrap_to_printable(&self.id),
            self.compass_pt
        )
    }
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
            (Some(a), Some(_b)) => matches!(a, _b) && id_match(self, other),
            _ => false,
        }
    }
}

impl<'a> Port<'a> {
    fn parse_from(tokens: &[&'a str]) -> Result<Port<'a>, ()> {
        fn parse_compass_pt_from(token: &str) -> Result<CompassPt, ()> {
            match token {
                "n" => Ok(CompassPt::N),
                "ne" => Ok(CompassPt::NE),
                "e" => Ok(CompassPt::E),
                "se" => Ok(CompassPt::SE),
                "s" => Ok(CompassPt::S),
                "sw" => Ok(CompassPt::SW),
                "w" => Ok(CompassPt::W),
                "nw" => Ok(CompassPt::NW),
                "c" => Ok(CompassPt::C),
                "_" => Ok(CompassPt::UND),
                _ => Err(()),
            }
        };

        if tokens.len() == 2 {
            if tokens[0] != ":" {
                return Err(());
            } else {
                match parse_compass_pt_from(tokens[1]) {
                    Ok(comp) => Ok(Port {
                        compass_pt: Some(comp),
                        ..Default::default()
                    }),
                    Err(_) => match dot::Id::new(tokens[1]) {
                        Ok(id) => Ok(Port {
                            id: Some(id),
                            ..Default::default()
                        }),
                        Err(_) => Err(()),
                    },
                }
            }
        } else if tokens.len() == 4 {
            if tokens[0] != ":" || tokens[2] != ":" {
                return Err(());
            } else {
                match parse_compass_pt_from(tokens[3]) {
                    Err(_) => Err(()),
                    Ok(comp) => match dot::Id::new(tokens[1]) {
                        Ok(id) => Ok(Port {
                            id: Some(id),
                            compass_pt: Some(comp),
                        }),
                        Err(_) => Err(()),
                    },
                }
            }
        } else {
            Err(())
        }
    }
}

pub struct NodeId<'a> {
    id: dot::Id<'a>,
    port: Option<Port<'a>>,
}

impl<'a> NodeId<'a> {
    fn parse_from(tokens: &[&'a str]) -> Result<NodeId<'a>, ()> {
        fn parse_option_port_from<'b>(tokens: &[&'b str]) -> Result<Option<Port<'b>>, ()> {
            if tokens.len() == 0 {
                return Ok(None);
            }
            match Port::parse_from(tokens) {
                Ok(port) => Ok(Some(port)),
                Err(_) => Err(()),
            }
        };

        if tokens.len() == 0 {
            return Err(());
        }
        match dot::Id::new(tokens[0]) {
            Err(_) => Err(()),
            Ok(id) => match parse_option_port_from(&tokens[1..]) {
                Err(_) => Err(()),
                Ok(port) => Ok(NodeId { id: id, port: port }),
            },
        }
    }
}

// define AListImpl so it's easy to change to another impl.
type AListImpl<'a> = HashMap<std::borrow::Cow<'a, str>, dot::Id<'a>>;

trait AList {
    fn parse_from<'a>(tokens: &[&'a str]) -> Result<AListImpl<'a>, ()>;
}

impl AList for AListImpl<'_> {
    fn parse_from<'a>(tokens: &[&'a str]) -> Result<AListImpl<'a>, ()> {
        fn parse_helper<'b>(tokens: &[&'b str], slice_idx: usize) -> Result<AListImpl<'b>, ()> {
            match AListImpl::parse_from(&tokens[slice_idx..]) {
                Err(err_msg) => Err(err_msg),
                Ok(mut sub_list) => {
                    if tokens[1] != "=" {
                        Err(())
                    } else {
                        match dot::Id::new(tokens[2]) {
                            Err(_) => Err(()),
                            Ok(id) => {
                                sub_list.insert(std::borrow::Cow::Borrowed(tokens[0]), id);
                                Ok(sub_list)
                            }
                        }
                    }
                }
            }
        };

        match tokens.len() {
            0 => Ok(HashMap::new()),
            // exclusive range is experimental
            1 => Err(()),
            2 => Err(()),
            3 => parse_helper(tokens, 3),
            _ => match tokens[3] {
                ";" => parse_helper(tokens, 4),
                "," => parse_helper(tokens, 4),
                _ => parse_helper(tokens, 3),
            },
        }
    }
}

type AttrListImpl<'a> = Vec<AListImpl<'a>>;

trait AttriList {
    fn parse_from<'a>(tokens: &[&'a str]) -> Result<AttrListImpl<'a>, ()>;
}

impl AttriList for AttrListImpl<'_> {
    fn parse_from<'a>(tokens: &[&'a str]) -> Result<AttrListImpl<'a>, ()> {
        match tokens.first() {
            None => Ok(AttrListImpl::new()),
            Some(&"[") => match tokens.iter().position(|x| x == &"]") {
                None => Err(()),
                Some(idx_right_bracket) => {
                    match Self::parse_from(&tokens[idx_right_bracket + 1..]) {
                        Err(err_msg) => Err(err_msg),
                        Ok(mut sub_attr_list) => {
                            match AListImpl::parse_from(&tokens[1..idx_right_bracket]) {
                                Err(err_msg) => Err(err_msg),
                                Ok(a_list) => {
                                    sub_attr_list.push(a_list);
                                    Ok(sub_attr_list)
                                }
                            }
                        }
                    }
                }
            },
            _ => Err(()),
        }
    }
}

enum AttrStmtKey {
    GRAPH,
    NODE,
    EDGE,
}

pub struct AttrStmt<'a> {
    key: AttrStmtKey,
    attr_list: AttrListImpl<'a>,
}

pub struct NodeStmt<'a> {
    id: NodeId<'a>,
    attr_list: Option<AttrListImpl<'a>>,
}

impl<'a> NodeStmt<'a> {
    fn parse_from(tokens: &[&'a str]) -> Result<NodeStmt<'a>, ()> {
        match tokens.iter().position(|x| x == &"[") {
            None => match NodeId::parse_from(tokens) {
                Err(err_msg) => Err(err_msg),
                Ok(node_id) => Ok(NodeStmt {
                    id: node_id,
                    attr_list: None,
                }),
            },
            Some(idx_left_bracket) => {
                match (
                    NodeId::parse_from(&tokens[..idx_left_bracket]),
                    AttrListImpl::parse_from(&tokens[idx_left_bracket..]),
                ) {
                    (Ok(node_id), Ok(attr_list)) => Ok(NodeStmt {
                        id: node_id,
                        attr_list: Some(attr_list),
                    }),
                    _ => Err(()),
                }
            }
        }
    }
}

pub struct SubGraph<'a> {
    id: Option<dot::Id<'a>>,
    stmt_list: StmtList<'a>,
}

pub struct EdgeRhs<'a> {
    edgeop: std::borrow::Cow<'a, str>, //An edgeop is -> in directed graphs and -- in undirected graphs.
    node_id: Option<NodeId<'a>>,
    subgraph: Option<SubGraph<'a>>,
}

pub struct EdgeStmt<'a> {
    node_id: Option<NodeId<'a>>,
    subgraph: Option<SubGraph<'a>>,
    rhs: Vec<EdgeRhs<'a>>,
    attr_list: Option<AttrListImpl<'a>>,
}

pub struct Graph<'a> {
    strict: bool,
    kind: dot::Kind,
    id: Option<dot::Id<'a>>,
    stmt_list: StmtList<'a>,
}

// impl NodeStmt<'_> {
//     fn parse_from(tokens: Vec<&str>) -> Result<NodeStmt<'_>, &'static str> {

//     }
// }

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn format_print() {
        assert_eq!(
            format!(
                "{:?}",
                Port {
                    id: Some(::dot::Id::new("id_1").unwrap()),
                    ..Default::default()
                }
            ),
            "Port{id=Some(\"id_1\"), compass=None}"
        );
        assert_eq!(
            format!(
                "{:?}",
                Port {
                    compass_pt: Some(CompassPt::NE),
                    ..Default::default()
                }
            ),
            "Port{id=None, compass=Some(\"NE\")}"
        );
    }

    #[test]
    fn parse_nodeid() {
        let n1 = NodeId::parse_from(&["id1"]).unwrap();
        assert_eq!(n1.id.name(), "id1");
        assert!(n1.port.is_none());

        let n2 = NodeId::parse_from(&["id1", ":", "id2"]).unwrap();
        assert_eq!(n2.id.name(), "id1");
        assert_eq!(n2.port.unwrap().id.unwrap().name(), "id2");

        let n3 = NodeId::parse_from(&["id1", ":", "n"]).unwrap();
        assert_eq!(n3.id.name(), "id1");
        assert!(matches!(n3.port.unwrap().compass_pt.unwrap(), CompassPt::N));

        let n4 = NodeId::parse_from(&["id1", ":", "id2", ":", "c"]).unwrap();
        assert_eq!(n4.id.name(), "id1");
        assert_eq!(
            n4.port.unwrap(),
            Port {
                id: Some(::dot::Id::new("id2").unwrap()),
                compass_pt: Some(CompassPt::C)
            }
        );
    }

    // Helper function to check if (key, value) exists in |alist|.
    fn alist_entry_match(alist: &AListImpl, key: &str, value: &str) -> bool {
        alist
            .get(std::borrow::Borrow::borrow(key))
            .unwrap()
            .as_slice()
            == value
    }

    #[test]
    fn parse_alist() {
        let alist_1 = AListImpl::parse_from(&[]).unwrap();
        assert_eq!(alist_1.len(), 0);

        let alist_2 = AListImpl::parse_from(&["id1", "=", "value1"]).unwrap();
        assert_eq!(alist_2.len(), 1);
        assert!(alist_entry_match(&alist_2, "id1", "value1"));

        let alist_3 =
            AListImpl::parse_from(&["id1", "=", "value1", ";", "id2", "=", "value2"]).unwrap();
        assert_eq!(alist_3.len(), 2);
        assert!(alist_entry_match(&alist_3, "id1", "value1"));
        assert!(alist_entry_match(&alist_3, "id2", "value2"));

        let alist_4 =
            AListImpl::parse_from(&["id1", "=", "value1", "id2", "=", "value2", ","]).unwrap();
        assert_eq!(alist_4.len(), 2);
        assert!(alist_entry_match(&alist_4, "id1", "value1"));
        assert!(alist_entry_match(&alist_4, "id2", "value2"));

        let alist_5 = AListImpl::parse_from(&["id1", "value1"]);
        assert!(!alist_5.is_ok(), alist_5.unwrap());

        let alist_6 = AListImpl::parse_from(&["id1", "=", "value1", ":"]);
        assert!(!alist_6.is_ok(), alist_6.unwrap());
    }

    #[test]
    fn parse_attrlist() {
        {
            let attr_list = AttrListImpl::parse_from(&[]).unwrap();
            assert!(attr_list.is_empty());
        }

        {
            let attr_list = AttrListImpl::parse_from(&["[", "id1", "=", "value1", "]"]).unwrap();
            assert_eq!(attr_list.len(), 1);
            assert!(alist_entry_match(&attr_list[0], "id1", "value1"));
        }

        {
            let attr_list =
                AttrListImpl::parse_from(&["[", "]", "[", "id1", "=", "value1", "]"]).unwrap();
            assert_eq!(attr_list.len(), 2);
        }

        {
            let attr_list =
                AttrListImpl::parse_from(&["[", "]", ",", "[", "id1", "=", "value1", "]"]);
            assert!(!attr_list.is_ok(), attr_list.unwrap());
        }

        {
            let attr_list = AttrListImpl::parse_from(&["[", "]", "[", "id1", "value1", "]"]);
            assert!(!attr_list.is_ok(), attr_list.unwrap());
        }
    }

    #[test]
    fn parse_node_stmt() {
        {
            let node_stmt = NodeStmt::parse_from(&[]);
            assert!(!node_stmt.is_ok(), node_stmt.unwrap());
        }

        {
            let tokens: Vec<&str> = "id1 : id2 : nw".split_whitespace().collect();
            let node_stmt = NodeStmt::parse_from(tokens.as_slice()).unwrap();
            assert_eq!(node_stmt.id.id.name(), "id1");
            assert_eq!(
                node_stmt.id.port.unwrap(),
                Port {
                    id: Some(::dot::Id::new("id2").unwrap()),
                    compass_pt: Some(CompassPt::NW)
                }
            )
        }

        {
            let tokens: Vec<&str> = "id1 [ id2 = value2 ]".split_whitespace().collect();
            let node_stmt = NodeStmt::parse_from(tokens.as_slice()).unwrap();
            assert_eq!(node_stmt.id.id.name(), "id1");
            assert_eq!(node_stmt.attr_list.unwrap().len(), 1)
        }

        {
            let tokens: Vec<&str> = "id1 : id2 : sw [ id3 = value3 ] [ id4 = value4 ]"
                .split_whitespace()
                .collect();
            let node_stmt = NodeStmt::parse_from(tokens.as_slice()).unwrap();
            assert_eq!(node_stmt.id.id.name(), "id1");
            assert_eq!(node_stmt.attr_list.unwrap().len(), 2)
        }
    }
}
