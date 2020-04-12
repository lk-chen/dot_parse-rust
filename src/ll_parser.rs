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

trait Parsable<'t> {
    type Output;
    fn parse_from(edgeop: &'t str, tokens: &[&'t str]) -> Result<Self::Output, (usize, &'t str)>;
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

// This is to help parse: ID '=' ID.
struct AssignmentStmt<'a> {
    lhs: IdWrapper<'a>,
    rhs: IdWrapper<'a>,
}

impl<'a> Parsable<'a> for AssignmentStmt<'a> {
    type Output = AssignmentStmt<'a>;
    // |edgeop| is ignored.
    fn parse_from(_edgeop: &'a str, tokens: &[&'a str]) -> Result<Self::Output, (usize, &'a str)> {
        match &tokens[..] {
            [lhs_str, "=", rhs_str] => match (IdWrapper::new(lhs_str), IdWrapper::new(rhs_str)) {
                (Ok(lhs), Ok(rhs)) => Ok(AssignmentStmt { lhs: lhs, rhs: rhs }),
                (Err(_), _) => Err((0, "Invalid ID")),
                (_, Err(_)) => Err((2, "Invalid ID")),
            },
            _ => Err((1, "Expect '='")),
        }
    }
}

#[derive(Default)]
pub struct Stmt<'a> {
    node_stmt: Option<NodeStmt<'a>>,
    edge_stmt: Option<EdgeStmt<'a>>,
    attr_stmt: Option<AttrStmt<'a>>,
    assign_stmt: Option<AssignmentStmt<'a>>,
    subgraph: Option<SubGraph<'a>>,
}

impl<'a> Stmt<'a> {
    fn parse_from(edgeop: &'a str, tokens: &[&'a str]) -> Result<Stmt<'a>, (usize, &'a str)> {
        fn try_different_stmt<'b, T: Parsable<'b, Output = T>>(
            tokens_internal: &[&'b str],
            edgeop: &'b str,
            idx_err: &mut usize,
            err_msg: &mut String,
        ) -> Option<T> {
            match T::parse_from(&edgeop, &tokens_internal) {
                Err((idx_err_edge, err_msg_edge)) => {
                    if idx_err_edge < *idx_err {
                        *idx_err = idx_err_edge;
                        *err_msg = err_msg_edge.to_string();
                    }
                    None
                }
                Ok(stmt) => Some(stmt),
            }
        }

        let mut result = Stmt {
            ..Default::default()
        };
        let mut idx_err: usize = tokens.len();
        let mut err_msg = String::new();

        if let Some(node_stmt) =
            try_different_stmt::<NodeStmt>(&tokens, &edgeop, &mut idx_err, &mut err_msg)
        {
            result.node_stmt = Some(node_stmt);
            Ok(result)
        } else if let Some(edge_stmt) =
            try_different_stmt::<EdgeStmt>(&tokens, &edgeop, &mut idx_err, &mut err_msg)
        {
            result.edge_stmt = Some(edge_stmt);
            Ok(result)
        } else if let Some(attr_stmt) =
            try_different_stmt::<AttrStmt>(&tokens, &edgeop, &mut idx_err, &mut err_msg)
        {
            result.attr_stmt = Some(attr_stmt);
            Ok(result)
        } else if let Some(assign_stmt) =
            try_different_stmt::<AssignmentStmt>(&tokens, &edgeop, &mut idx_err, &mut err_msg)
        {
            result.assign_stmt = Some(assign_stmt);
            Ok(result)
        } else if let Some(sub_graph) =
            try_different_stmt::<SubGraph>(&tokens, &edgeop, &mut idx_err, &mut err_msg)
        {
            result.subgraph = Some(sub_graph);
            Ok(result)
        } else {
            Err((0, ""))
        }
    }
}

struct StmtList<'a>(Vec<Stmt<'a>>);

impl StmtList<'_> {
    fn parse_from<'a>(
        edgeop: &'a str,
        tokens: &[&'a str],
    ) -> Result<StmtList<'a>, (usize, &'a str)> {
        if tokens.len() == 0 {
            return Ok(StmtList(vec![]));
        }
        if let Ok(stmt) = Stmt::parse_from(edgeop, tokens) {
            return Ok(StmtList(vec![stmt]));
        }
        for (token_idx, token) in tokens.iter().enumerate() {
            // TODO: per standard, ';' is optional.
            if token == &";" {
                match (
                    Stmt::parse_from(edgeop, &tokens[0..token_idx]),
                    StmtList::parse_from(edgeop, &tokens[token_idx + 1..]),
                ) {
                    (Ok(stmt), Ok(mut stmt_list)) => {
                        stmt_list.0.insert(0, stmt);
                        return Ok(stmt_list);
                    }
                    _ => {}
                }
            }
        }
        // TODO: give reasonable err position.
        Err((0, "failed parsing statement list."))
    }
}

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

struct IdWrapper<'a>(dot::Id<'a>);

impl<'a> fmt::Debug for IdWrapper<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.0.as_slice())
    }
}

impl<'a> PartialEq for IdWrapper<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.0.as_slice() == other.0.as_slice()
    }
}

impl<'a> PartialEq<&str> for IdWrapper<'a> {
    fn eq(&self, other: &&str) -> bool {
        self.0.as_slice() == *other
    }
}

impl<'a> IdWrapper<'a> {
    fn new(name: &'a str) -> Result<IdWrapper<'a>, ()> {
        match (name, dot::Id::new(name)) {
            ("graph", _) => Err(()),
            ("node", _) => Err(()),
            ("edge", _) => Err(()),
            ("digraph", _) => Err(()),
            ("strict", _) => Err(()),
            (_, Err(_)) => Err(()),
            (_, Ok(id)) => Ok(IdWrapper(id)),
        }
    }
}

pub struct NodeId<'a> {
    id: IdWrapper<'a>,
    port: Option<Port<'a>>,
}

impl fmt::Debug for NodeId<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "NodeId{{id={:?}, port={:?}}}", self.id, self.port)
    }
}

impl PartialEq for NodeId<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id && self.port == other.port
    }
}

impl<'a> NodeId<'a> {
    fn parse_from(tokens: &[&'a str]) -> Result<NodeId<'a>, ((usize, &'a str))> {
        fn parse_option_port_from<'b>(
            tokens: &[&'b str],
        ) -> Result<Option<Port<'b>>, (usize, &'b str)> {
            if tokens.len() == 0 {
                return Ok(None);
            }
            match Port::parse_from(tokens) {
                Ok(port) => Ok(Some(port)),
                Err(_) => Err((0, "error parsing port")),
            }
        };

        if tokens.len() == 0 {
            return Err((0, "expecting node id"));
        }
        match IdWrapper::new(tokens[0]) {
            Err(_) => Err((0, "cannot be used as id")),
            Ok(id) => match parse_option_port_from(&tokens[1..]) {
                Err((idx_err, err_msg)) => Err((idx_err + 1, err_msg)),
                Ok(port) => Ok(NodeId { id: id, port: port }),
            },
        }
    }
}

struct AList<'a>(HashMap<std::borrow::Cow<'a, str>, IdWrapper<'a>>);

impl<'a> fmt::Debug for AList<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl<'a> AList<'a> {
    fn parse_from(tokens: &[&'a str]) -> Result<AList<'a>, (usize, &'a str)> {
        fn parse_helper<'b>(
            tokens: &[&'b str],
            slice_idx: usize,
        ) -> Result<AList<'b>, (usize, &'b str)> {
            match (
                AssignmentStmt::parse_from("", &tokens[0..3]),
                AList::parse_from(&tokens[slice_idx..]),
            ) {
                (Ok(assignment), Ok(mut sub_list)) => {
                    sub_list.0.insert(assignment.lhs.0.name(), assignment.rhs);
                    Ok(sub_list)
                }
                (Err(err_info), _) => Err(err_info),
                (_, Err((idx_err, err_msg))) => Err((idx_err + 3, err_msg)),
            }
        };

        match tokens.len() {
            0 => Ok(AList::new()),
            // exclusive range is experimental
            1 => Err((0, "Expecting assignment statement")),
            2 => Err((0, "Expecting assignment statement")),
            3 => parse_helper(tokens, 3),
            _ => match tokens[3] {
                ";" => parse_helper(tokens, 4),
                "," => parse_helper(tokens, 4),
                _ => parse_helper(tokens, 3),
            },
        }
    }

    fn new() -> AList<'a> {
        AList(HashMap::new())
    }

    fn get(&self, key: &str) -> Option<&IdWrapper<'a>> {
        self.0.get(std::borrow::Borrow::borrow(key))
    }

    fn len(&self) -> usize {
        self.0.len()
    }
}

struct AttrList<'a>(Vec<AList<'a>>);

impl<'a> fmt::Debug for AttrList<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl AttrList<'_> {
    fn parse_from<'a>(tokens: &[&'a str]) -> Result<AttrList<'a>, (usize, &'a str)> {
        // Recursive closure not allowed.
        struct Internal {}
        impl Internal {
            fn parse_helper<'b>(tokens: &[&'b str]) -> Result<AttrList<'b>, (usize, &'b str)> {
                match tokens.first() {
                    None => Ok(AttrList(vec![])),
                    Some(&"[") => match tokens.iter().position(|x| x == &"]") {
                        None => Err((tokens.len(), &"expecting ']'")),
                        Some(idx_right_bracket) => {
                            match Self::parse_helper(&tokens[idx_right_bracket + 1..]) {
                                Err((idx_err, err_msg)) => {
                                    Err((idx_err + idx_right_bracket + 1, err_msg))
                                }
                                Ok(mut sub_attr_list) => {
                                    match AList::parse_from(&tokens[1..idx_right_bracket]) {
                                        Err(_) => Err((1, &"internal error")),
                                        Ok(a_list) => {
                                            sub_attr_list.0.push(a_list);
                                            Ok(sub_attr_list)
                                        }
                                    }
                                }
                            }
                        }
                    },
                    _ => Err((0, &"expecting '['")),
                }
            }
        }

        match Internal::parse_helper(tokens) {
            Err(err_info) => Err(err_info),
            Ok(result) => match result.0.len() {
                0 => Err((0, &"expecting valid AttrList here")),
                _ => Ok(result),
            },
        }
    }
}

enum AttrStmtKey {
    GRAPH,
    NODE,
    EDGE,
}

impl PartialEq for AttrStmtKey {
    fn eq(&self, other: &Self) -> bool {
        matches!(self, other)
    }
}

impl fmt::Debug for AttrStmtKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            AttrStmtKey::GRAPH => "GRAPH",
            AttrStmtKey::NODE => "NODE",
            AttrStmtKey::EDGE => "EDGE",
        };
        write!(f, "{:?}", s)
    }
}

pub struct AttrStmt<'a> {
    key: AttrStmtKey,
    attr_list: AttrList<'a>,
}

impl<'a> Parsable<'a> for AttrStmt<'a> {
    type Output = AttrStmt<'a>;
    // |edgeop| is ignored.
    fn parse_from(_edgeop: &'a str, tokens: &[&'a str]) -> Result<Self::Output, (usize, &'a str)> {
        let parse_helper =
            |attr_key: AttrStmtKey, tokens: &[&'a str]| -> Result<AttrStmt<'a>, (usize, &'a str)> {
                match AttrList::parse_from(&tokens[1..]) {
                    Err((idx_err, err_msg)) => Err((idx_err + 1, err_msg)),
                    Ok(attr_list) => Ok(AttrStmt {
                        key: attr_key,
                        attr_list: attr_list,
                    }),
                }
            };

        match tokens.first() {
            Some(&"graph") => parse_helper(AttrStmtKey::GRAPH, &tokens),
            Some(&"node") => parse_helper(AttrStmtKey::NODE, &tokens),
            Some(&"edge") => parse_helper(AttrStmtKey::EDGE, &tokens),
            _ => Err((0, &"AttrStmt must starts with graph|node|edge")),
        }
    }
}

pub struct NodeStmt<'a> {
    id: NodeId<'a>,
    attr_list: Option<AttrList<'a>>,
}

// impl<'a> fmt::Debug for NodeStmt<'a> {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         let s = match self.attr_list {
//             None => format!("NodeStmt{{id={:?}}}", self.id),
//             Some(attr_list) => format!("NodeStmt{{id={:?}, attr_list={:?}}}", self.id, attr_list),
//         };
//         write!(f, "{:?}", s)
//     }
// }

impl<'a> Parsable<'a> for NodeStmt<'a> {
    type Output = NodeStmt<'a>;
    // |edgeop| is ignored.
    fn parse_from(_edgeop: &'a str, tokens: &[&'a str]) -> Result<Self::Output, (usize, &'a str)> {
        match tokens.iter().position(|x| x == &"[") {
            None => match NodeId::parse_from(tokens) {
                Err(err_info) => Err(err_info),
                Ok(node_id) => Ok(NodeStmt {
                    id: node_id,
                    attr_list: None,
                }),
            },
            Some(idx_left_bracket) => {
                match (
                    NodeId::parse_from(&tokens[..idx_left_bracket]),
                    AttrList::parse_from(&tokens[idx_left_bracket..]),
                ) {
                    (Ok(node_id), Ok(attr_list)) => Ok(NodeStmt {
                        id: node_id,
                        attr_list: Some(attr_list),
                    }),
                    (Err(err_info), _) => Err(err_info),
                    (Ok(_), Err((idx_err, err_msg))) => Err((idx_err + idx_left_bracket, err_msg)),
                }
            }
        }
    }
}

pub struct SubGraph<'a> {
    id: Option<dot::Id<'a>>,
    stmt_list: StmtList<'a>,
}

impl<'a> Parsable<'a> for SubGraph<'a> {
    type Output = SubGraph<'a>;
    // |edgeop| is ignored.
    fn parse_from(edgeop: &'a str, tokens: &[&'a str]) -> Result<SubGraph<'a>, (usize, &'a str)> {
        let parse_stmt_list_and_return = |id: Option<dot::Id<'a>>,
                                          id_stmt_start: usize|
         -> Result<SubGraph<'a>, (usize, &'a str)> {
            match StmtList::parse_from(edgeop, &tokens[id_stmt_start..tokens.len() - 1]) {
                Err((idx_err, err_msg)) => Err((idx_err + id_stmt_start, err_msg)),
                Ok(stmt_list) => Ok(SubGraph {
                    id: id,
                    stmt_list: stmt_list,
                }),
            }
        };

        let key_subgraph: &str = "subgraph";
        let right_brace: &str = "}";
        match (tokens.iter().position(|x| x == &"{"), tokens.last()) {
            (Some(idx_left_brace), Some(&right_brace)) => match idx_left_brace {
                0 => parse_stmt_list_and_return(None, 1),
                1 => match tokens[0] {
                    key_subgraph => parse_stmt_list_and_return(None, 2),
                    _ => Err((0, "expecting subgraph")),
                },
                2 => match (tokens[0], dot::Id::new(tokens[1])) {
                    (_, Err(_)) => Err((1, "expecting valid id")),
                    (key_subgraph, Ok(id)) => parse_stmt_list_and_return(Some(id), 3),
                    (_, Ok(_)) => Err((0, "expecting subgraph")),
                },
                _ => Err((idx_left_brace, "wrong grammer")),
            },
            (None, _) => Err((0, "expecting '{'")),
            (_, None) => Err((tokens.len(), "expecting '}'")),
        }
    }
}

type NodeOrSubgraph<'b> = (Option<NodeId<'b>>, Option<SubGraph<'b>>, (usize, &'b str));

fn parse_node_or_subgraph<'a>(tokens_n_s: &[&'a str]) -> NodeOrSubgraph<'a> {
    match (
        NodeId::parse_from(tokens_n_s),
        SubGraph::parse_from("", tokens_n_s),
    ) {
        (Ok(node_id), _) => (Some(node_id), None, (0, "")),
        (_, Ok(subgraph)) => (None, Some(subgraph), (0, "")),
        (Err((idx_err_node, err_msg_node)), Err((idx_err_subg, err_msg_subg))) => {
            // If we move more forward when parsing node, guess it's a node.
            if idx_err_node >= idx_err_subg {
                (None, None, (idx_err_node, err_msg_node))
            } else {
                (None, None, (idx_err_subg, err_msg_subg))
            }
        }
    }
}

#[derive(Default)]
pub struct EdgeRhs<'a> {
    // It stores a sequence of node_id or subgraph.
    node_id_or_subgraph: Vec<(Option<NodeId<'a>>, Option<SubGraph<'a>>)>,
}

impl<'a> EdgeRhs<'a> {
    fn parse_from(edgeop: &'a str, tokens: &[&'a str]) -> Result<EdgeRhs<'a>, (usize, &'a str)> {
        // Helper function for recursive call.
        fn parse_internal<'b>(
            edgeop: &'b str,
            tokens_internal: &[&'b str],
        ) -> Result<EdgeRhs<'b>, (usize, &'b str)> {
            let merge_and_return = |node_or_subgraph: NodeOrSubgraph<'b>,
                                    idx_res_tokens_start: usize,
                                    res_tokens: &[&'b str]|
             -> Result<EdgeRhs<'b>, (usize, &'b str)> {
                match node_or_subgraph {
                    (None, None, (idx_err, err_msg)) => Err((idx_err, err_msg)),
                    (Some(node_id), None, _) => match parse_internal(edgeop, res_tokens) {
                        Err((idx_sub_err, sub_err_msg)) => {
                            Err((idx_sub_err + idx_res_tokens_start, sub_err_msg))
                        }
                        Ok(mut sub_rhs) => {
                            sub_rhs.node_id_or_subgraph.insert(0, (Some(node_id), None));
                            Ok(sub_rhs)
                        }
                    },
                    (None, Some(subgraph), _) => match parse_internal(edgeop, res_tokens) {
                        Err((idx_sub_err, sub_err_msg)) => {
                            Err((idx_sub_err + idx_res_tokens_start, sub_err_msg))
                        }
                        Ok(mut sub_rhs) => {
                            sub_rhs
                                .node_id_or_subgraph
                                .insert(0, (None, Some(subgraph)));
                            Ok(sub_rhs)
                        }
                    },
                    (Some(_), Some(_), _) => panic!("Should never happen"),
                }
            };

            match tokens_internal.len() {
                0 => Ok(EdgeRhs {
                    ..Default::default()
                }),
                _ => {
                    match (
                        tokens_internal.iter().position(|x| x == &edgeop),
                        tokens_internal[1..].iter().position(|x| x == &edgeop),
                    ) {
                        (Some(0), None) => merge_and_return(
                            parse_node_or_subgraph(&tokens_internal[1..]),
                            tokens_internal.len(),
                            &tokens_internal[0..0],
                        ),
                        (Some(0), Some(idx_next_edgeop)) => merge_and_return(
                            parse_node_or_subgraph(&tokens_internal[1..idx_next_edgeop + 1]),
                            idx_next_edgeop,
                            &tokens_internal[idx_next_edgeop + 1..],
                        ),
                        (_, _) => Err((0, "edgeRHS should begin with edgeop")),
                    }
                }
            }
        };

        match (tokens.len(), edgeop) {
            (0, _) => Err((0, "edgeRHS expects non-empty token list")),
            (_, "->") | (_, "--") => parse_internal(edgeop, tokens),
            _ => Err((0, "edgeop should be '->'|'--'")),
        }
    }
}

pub struct EdgeStmt<'a> {
    // It stores a sequence of node_id or subgraph.
    node_id_or_subgraph: Vec<(Option<NodeId<'a>>, Option<SubGraph<'a>>)>,
    attr_list: Option<AttrList<'a>>,
}

impl<'a> Parsable<'a> for EdgeStmt<'a> {
    type Output = EdgeStmt<'a>;
    fn parse_from(edgeop: &'a str, tokens: &[&'a str]) -> Result<Self::Output, (usize, &'a str)> {
        let parse_edge = |idx_edge_rhs: usize,
                          tokens_internal: &[&'a str]|
         -> Result<EdgeStmt<'a>, (usize, &'a str)> {
            match (
                parse_node_or_subgraph(&tokens_internal[0..idx_edge_rhs]),
                EdgeRhs::parse_from(edgeop, &tokens_internal[idx_edge_rhs..]),
            ) {
                ((None, None, (idx_err, err_msg)), _) => Err((idx_err, err_msg)),
                (_, Err((idx_err, err_msg))) => Err((idx_err + idx_edge_rhs, err_msg)),
                ((Some(_), Some(_), _), _) => panic!("Shouldn't happen"),
                ((opt_node, opt_subgraph, _), Ok(mut edge_rhs)) => {
                    edge_rhs
                        .node_id_or_subgraph
                        .insert(0, (opt_node, opt_subgraph));
                    Ok(EdgeStmt {
                        node_id_or_subgraph: edge_rhs.node_id_or_subgraph,
                        attr_list: None,
                    })
                }
            }
        };

        match (
            tokens.iter().position(|x| x == &edgeop),
            tokens.iter().position(|x| x == &"["),
        ) {
            (Some(idx_edge_rhs), Some(idx_attr_list)) => {
                if idx_attr_list <= idx_edge_rhs {
                    return Err((
                        idx_attr_list,
                        "Attr list should be at the end of statement.",
                    ));
                }
                match (
                    parse_edge(idx_edge_rhs, &tokens[0..idx_attr_list]),
                    AttrList::parse_from(&tokens[idx_attr_list..]),
                ) {
                    (Err(err_info), _) => Err(err_info),
                    (_, Err((idx_err, err_msg))) => Err((idx_err + idx_attr_list, err_msg)),
                    (Ok(edge), Ok(attr_list)) => Ok(EdgeStmt {
                        node_id_or_subgraph: edge.node_id_or_subgraph,
                        attr_list: Some(attr_list),
                    }),
                }
            }
            (Some(idx_edge_rhs), None) => parse_edge(idx_edge_rhs, &tokens[0..]),
            (None, Some(idx_attr_list)) => Err((idx_attr_list, "Expect edgeop")),
            (None, None) => Err((tokens.len(), "Expect edgeop")),
        }
    }
}

pub struct Graph<'a> {
    strict: bool,
    kind: dot::Kind,
    id: Option<dot::Id<'a>>,
    stmt_list: StmtList<'a>,
}

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
        {
            let mut alist = AList::new();
            alist.0.insert(
                std::borrow::Cow::Borrowed("a"),
                IdWrapper::new("a1").unwrap(),
            );
            assert_eq!(format!("{:?}", alist), "{\"a\": \"a1\"}");
        }
        {
            let tokens: Vec<&str> = "[ a = v1 ] [ b = v2 ] [ c = v3 ]"
                .split_whitespace()
                .collect();
            let attr_list = AttrList::parse_from(&tokens).unwrap();
            assert_eq!(
                format!("{:?}", attr_list),
                "[{\"c\": \"v3\"}, {\"b\": \"v2\"}, {\"a\": \"v1\"}]"
            );
        }
    }

    #[test]
    fn parse_nodeid() {
        let n1 = NodeId::parse_from(&["id1"]).unwrap();
        assert_eq!(n1.id, "id1");
        assert!(n1.port.is_none());

        let n2 = NodeId::parse_from(&["id1", ":", "id2"]).unwrap();
        assert_eq!(n2.id, "id1");
        assert_eq!(n2.port.unwrap().id.unwrap().name(), "id2");

        let n3 = NodeId::parse_from(&["id1", ":", "n"]).unwrap();
        assert_eq!(n3.id, "id1");
        assert!(matches!(n3.port.unwrap().compass_pt.unwrap(), CompassPt::N));

        let n4 = NodeId::parse_from(&["id1", ":", "id2", ":", "c"]).unwrap();
        assert_eq!(n4.id, "id1");
        assert_eq!(
            n4.port.unwrap(),
            Port {
                id: Some(::dot::Id::new("id2").unwrap()),
                compass_pt: Some(CompassPt::C)
            }
        );
    }

    // Helper function to check if (key, value) exists in |alist|.
    fn alist_entry_match(alist: &AList, key: &str, value: &str) -> bool {
        alist.get(key).unwrap() == &value
    }

    #[test]
    fn parse_alist() {
        let alist_1 = AList::parse_from(&[]).unwrap();
        assert_eq!(alist_1.len(), 0);

        let alist_2 = AList::parse_from(&["id1", "=", "value1"]).unwrap();
        assert_eq!(alist_2.len(), 1);
        assert!(alist_entry_match(&alist_2, "id1", "value1"));

        let alist_3 =
            AList::parse_from(&["id1", "=", "value1", ";", "id2", "=", "value2"]).unwrap();
        assert_eq!(alist_3.len(), 2);
        assert!(alist_entry_match(&alist_3, "id1", "value1"));
        assert!(alist_entry_match(&alist_3, "id2", "value2"));

        let alist_4 =
            AList::parse_from(&["id1", "=", "value1", "id2", "=", "value2", ","]).unwrap();
        assert_eq!(alist_4.len(), 2);
        assert!(alist_entry_match(&alist_4, "id1", "value1"));
        assert!(alist_entry_match(&alist_4, "id2", "value2"));

        let alist_5 = AList::parse_from(&["id1", "value1"]);
        assert!(!alist_5.is_ok(), alist_5.unwrap());

        let alist_6 = AList::parse_from(&["id1", "=", "value1", ":"]);
        assert!(!alist_6.is_ok(), alist_6.unwrap());
    }

    #[test]
    fn parse_attrlist() {
        {
            match AttrList::parse_from(&[]) {
                Err((idx_err, err_msg)) => {
                    assert_eq!(idx_err, 0, "{}", err_msg);
                }
                Ok(_) => {
                    assert!(false, "cannot be empty");
                }
            }
        }

        {
            match AttrList::parse_from(&["[", "id1", "=", "value1", "]"]) {
                Err((idx_err, err_msg)) => {
                    assert!(false, "error at index {}: {}", idx_err, err_msg);
                }
                Ok(attr_list) => {
                    assert_eq!(attr_list.0.len(), 1);
                    assert!(alist_entry_match(&attr_list.0[0], "id1", "value1"));
                }
            }
        }

        {
            match AttrList::parse_from(&["[", "]", "[", "id1", "=", "value1", "]"]) {
                Err((idx_err, err_msg)) => {
                    assert!(false, "error at index {}: {}", idx_err, err_msg);
                }
                Ok(attr_list) => {
                    assert_eq!(attr_list.0.len(), 2);
                }
            }
        }

        {
            let attr_list = AttrList::parse_from(&["[", "]", ",", "[", "id1", "=", "value1", "]"]);
            assert!(!attr_list.is_ok(), attr_list.unwrap());
        }

        {
            let attr_list = AttrList::parse_from(&["[", "]", "[", "id1", "value1", "]"]);
            assert!(!attr_list.is_ok(), attr_list.unwrap());
        }
    }

    #[test]
    fn parse_node_stmt() {
        {
            let node_stmt = NodeStmt::parse_from("", &[]);
            assert!(!node_stmt.is_ok(), node_stmt.unwrap());
        }

        {
            let tokens: Vec<&str> = "id1 : id2 : nw".split_whitespace().collect();
            let node_stmt = NodeStmt::parse_from("", tokens.as_slice()).unwrap();
            assert_eq!(node_stmt.id.id, "id1");
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
            let node_stmt = NodeStmt::parse_from("", tokens.as_slice()).unwrap();
            assert_eq!(node_stmt.id.id, "id1");
            assert_eq!(node_stmt.attr_list.unwrap().0.len(), 1)
        }

        {
            let tokens: Vec<&str> = "id1 : id2 : sw [ id3 = value3 ] [ id4 = value4 ]"
                .split_whitespace()
                .collect();
            let node_stmt = NodeStmt::parse_from("", tokens.as_slice()).unwrap();
            assert_eq!(node_stmt.id.id, "id1");
            assert_eq!(node_stmt.attr_list.unwrap().0.len(), 2)
        }
    }

    #[test]
    fn parse_attr_stmt() {
        {
            let tokens: Vec<&str> = "[ ]".split_whitespace().collect();
            let node_stmt = AttrStmt::parse_from("", tokens.as_slice());
            assert!(!node_stmt.is_ok(), node_stmt.unwrap());
        }

        {
            let tokens: Vec<&str> = "graph [ ]".split_whitespace().collect();
            let node_stmt = AttrStmt::parse_from("", tokens.as_slice()).unwrap();
            assert_eq!(node_stmt.key, AttrStmtKey::GRAPH);
        }

        {
            let tokens: Vec<&str> = "edge".split_whitespace().collect();
            let node_stmt = AttrStmt::parse_from("", tokens.as_slice());
            assert!(!node_stmt.is_ok(), node_stmt.unwrap());
        }

        {
            let tokens: Vec<&str> = "other_keyword []".split_whitespace().collect();
            let node_stmt = AttrStmt::parse_from("", tokens.as_slice());
            assert!(!node_stmt.is_ok(), node_stmt.unwrap());
        }
    }

    #[test]
    fn parse_subgraph() {
        {
            let tokens: Vec<&str> = "{ }".split_whitespace().collect();
            let maybe_subgraph = SubGraph::parse_from("", tokens.as_slice());
            match maybe_subgraph {
                Ok(subgraph) => {
                    assert!(subgraph.id.is_none());
                }
                Err((idx_err, err_msg)) => {
                    assert!(false, format!("{} {}", idx_err, err_msg));
                }
            };
        }

        {
            let tokens: Vec<&str> = "subgraph { }".split_whitespace().collect();
            let maybe_subgraph = SubGraph::parse_from("", tokens.as_slice());
            match maybe_subgraph {
                Ok(subgraph) => {
                    assert!(subgraph.id.is_none());
                }
                Err((idx_err, err_msg)) => {
                    assert!(false, format!("{} {}", idx_err, err_msg));
                }
            };
        }

        {
            let tokens: Vec<&str> = "subgraph id1 { }".split_whitespace().collect();
            let maybe_subgraph = SubGraph::parse_from("", tokens.as_slice());
            match maybe_subgraph {
                Ok(subgraph) => {
                    assert!(subgraph.id.is_some());
                    assert_eq!(subgraph.id.unwrap().name(), "id1");
                }
                Err((idx_err, err_msg)) => {
                    assert!(false, format!("{} {}", idx_err, err_msg));
                }
            };
        }

        {
            let tokens: Vec<&str> = "subgraph [ ]".split_whitespace().collect();
            let maybe_subgraph = SubGraph::parse_from("", tokens.as_slice());
            match maybe_subgraph {
                Ok(subgraph) => {
                    assert!(false, subgraph);
                }
                Err((idx_err, _)) => {
                    assert_eq!(idx_err, 0);
                }
            };
        }

        {
            let tokens: Vec<&str> = "wrong_keyword [ ]".split_whitespace().collect();
            let maybe_subgraph = SubGraph::parse_from("", tokens.as_slice());
            match maybe_subgraph {
                Ok(subgraph) => {
                    assert!(false, subgraph);
                }
                Err((idx_err, _)) => {
                    assert_eq!(idx_err, 0);
                }
            };
        }
    }

    #[test]
    fn parse_edge_rhs() {
        {
            let tokens: Vec<&str> = "-- id1".split_whitespace().collect();
            let maybe_edge_rhs = EdgeRhs::parse_from("--", tokens.as_slice());
            match maybe_edge_rhs {
                Ok(edge_rhs) => {
                    assert!(edge_rhs.node_id_or_subgraph.len() == 1);
                    assert!(edge_rhs.node_id_or_subgraph[0].1.is_none());
                }
                Err((idx_err, err_msg)) => {
                    assert!(false, format!("{} {}", idx_err, err_msg));
                }
            };
        }

        {
            let tokens: Vec<&str> = "-> id1 -> id2 : id3 : n".split_whitespace().collect();
            let maybe_edge_rhs = EdgeRhs::parse_from("->", tokens.as_slice());
            match maybe_edge_rhs {
                Ok(edge_rhs) => {
                    assert!(edge_rhs.node_id_or_subgraph.len() == 2);
                    assert!(edge_rhs.node_id_or_subgraph[0].1.is_none());
                }
                Err((idx_err, err_msg)) => {
                    assert!(false, format!("{} {}", idx_err, err_msg));
                }
            };
        }

        // TODO: add test for subgraph.
    }

    #[test]
    fn parse_edge_stmt() {
        {
            let tokens: Vec<&str> = "id1 -- id2 -- id3 : id4 : n".split_whitespace().collect();
            let maybe_edge = EdgeStmt::parse_from("--", tokens.as_slice());
            match maybe_edge {
                Ok(edge) => {
                    assert!(edge.node_id_or_subgraph.len() == 3);
                    assert!(edge.node_id_or_subgraph[2].1.is_none());
                    assert_eq!(
                        edge.node_id_or_subgraph[2].0,
                        Some(NodeId {
                            id: IdWrapper::new("id3").unwrap(),
                            port: Some(Port {
                                id: Some(::dot::Id::new("id4").unwrap()),
                                compass_pt: Some(CompassPt::C)
                            })
                        })
                    );
                    assert!(edge.attr_list.is_none());
                }
                Err((idx_err, err_msg)) => {
                    assert!(false, format!("{} {}", idx_err, err_msg));
                }
            };
        }

        {
            let tokens: Vec<&str> = "id1 -> id2 [ ]".split_whitespace().collect();
            let maybe_edge = EdgeStmt::parse_from("->", tokens.as_slice());
            match maybe_edge {
                Ok(edge) => {
                    assert!(edge.node_id_or_subgraph.len() == 2);
                    assert!(edge.attr_list.is_some());
                }
                Err((idx_err, err_msg)) => {
                    assert!(false, format!("{} {}", idx_err, err_msg));
                }
            };
        }

        // TODO: test subgraph
    }

    #[test]
    fn parse_stmt() {
        {
            let tokens: Vec<&str> = "id1 : id2".split_whitespace().collect();
            let maybe_stmt = Stmt::parse_from("--", tokens.as_slice());
            match maybe_stmt {
                Ok(stmt) => {
                    assert!(stmt.node_stmt.is_some());
                    assert_eq!(stmt.node_stmt.unwrap().id.id, "id1")
                }
                Err((idx_err, err_msg)) => {
                    assert!(false, format!("{} {}", idx_err, err_msg));
                }
            };
        }

        {
            let tokens: Vec<&str> = "id1 -- id2 : id3".split_whitespace().collect();
            let maybe_stmt = Stmt::parse_from("--", tokens.as_slice());
            match maybe_stmt {
                Ok(stmt) => {
                    assert!(stmt.edge_stmt.is_some());
                    assert_eq!(stmt.edge_stmt.unwrap().node_id_or_subgraph.len(), 2);
                }
                Err((idx_err, err_msg)) => {
                    assert!(false, format!("{} {}", idx_err, err_msg));
                }
            };
        }

        {
            let tokens: Vec<&str> = "graph [ ]".split_whitespace().collect();
            let maybe_stmt = Stmt::parse_from("--", tokens.as_slice());
            match maybe_stmt {
                Ok(stmt) => {
                    assert!(stmt.attr_stmt.is_some(), stmt);
                    assert_eq!(stmt.attr_stmt.unwrap().key, AttrStmtKey::GRAPH);
                }
                Err((idx_err, err_msg)) => {
                    assert!(false, format!("{} {}", idx_err, err_msg));
                }
            };
        }

        {
            let tokens: Vec<&str> = "id1 = value1".split_whitespace().collect();
            let maybe_stmt = Stmt::parse_from("--", tokens.as_slice());
            match maybe_stmt {
                Ok(stmt) => {
                    assert!(stmt.assign_stmt.is_some(), stmt);
                    let assign_stmt = stmt.assign_stmt.unwrap();
                    assert_eq!(assign_stmt.lhs.0.as_slice(), "id1");
                    assert_eq!(assign_stmt.rhs.0.as_slice(), "value1");
                }
                Err((idx_err, err_msg)) => {
                    assert!(false, format!("{} {}", idx_err, err_msg));
                }
            };
        }

        {
            let tokens: Vec<&str> = "subgraph { }".split_whitespace().collect();
            let maybe_stmt = Stmt::parse_from("--", tokens.as_slice());
            match maybe_stmt {
                Ok(stmt) => {
                    assert!(stmt.subgraph.is_some(), stmt);
                }
                Err((idx_err, err_msg)) => {
                    assert!(false, format!("{} {}", idx_err, err_msg));
                }
            };
        }
    }

    #[test]
    fn parse_stmt_list() {
        {
            let tokens: Vec<&str> =
                "id1 : id2 ; id3 : id4 ; node1 [ ] ; node2 -- node3 ; subgraph { }"
                    .split_whitespace()
                    .collect();
            let maybe_stmtlist = StmtList::parse_from("--", tokens.as_slice());
            match maybe_stmtlist {
                Ok(stmtlist) => {
                    assert_eq!(stmtlist.0.len(), 5);
                }
                Err((idx_err, err_msg)) => {
                    assert!(false, format!("{} {}", idx_err, err_msg));
                }
            };
        }
    }
}
