use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use crate::lib::{find_path, Node, SearchEnd};
use crate::lib::campus_data::read_osm_data;

#[derive(Debug)]
struct TestGraph {
    neighbors: HashMap<String, Vec<String>>,
}

#[derive(Clone, Debug)]
struct TestNode {
    name: String,
    graph: Rc<TestGraph>,
}
impl PartialEq for TestNode {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}
impl Eq for TestNode {}

impl Hash for TestNode {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}
impl Node<u8> for TestNode {
    fn get_neighbors(&self) -> Vec<(Self, u8)>
    where
        Self: Sized,
    {
        let empty = vec![];
        let neighbors = self.graph.neighbors.get(&self.name).unwrap_or(&empty);
        neighbors
            .iter()
            .map(|name| {
                (
                    TestNode {
                        name: name.clone(),
                        graph: Rc::clone(&self.graph),
                    },
                    2,
                )
            })
            .collect()
    }

    fn heuristic_cost(&self, other: &Self) -> crate::lib::Result<u8> {
        Ok(1)
    }
}
#[test]
fn test_node_path_find() {
    let graph = Rc::new(TestGraph {
        neighbors: {
            let mut map = HashMap::new();
            map.insert(
                String::from("a"),
                vec!["b", "c"].into_iter().map(String::from).collect(),
            );
            map.insert(
                String::from("b"),
                vec!["c", "d"].into_iter().map(String::from).collect(),
            );
            map.insert(
                String::from("c"),
                vec!["d", "e"].into_iter().map(String::from).collect(),
            );
            map
        },
    });
    let start_node = TestNode {
        name: "a".to_string(),
        graph: Rc::clone(&graph),
    };
    let end_node = TestNode {
        name: "e".to_string(),
        graph: Rc::clone(&graph),
    };
    if let Ok(path) = find_path(&start_node, SearchEnd::node(&end_node)) {
        println!("path found is: {:?}", path);
    } else {
        println!("no path found");
    }

    let is_end = |n: &TestNode| n.name == "e".to_string();
    let heuristic = |n: &TestNode| 1;
    if let Ok(path) = find_path(&start_node, SearchEnd::Custom(is_end, heuristic)) {
        println!("path found is: {:?}", path);
    } else {
        println!("no path found");
    }
}

#[test]
fn test_read_osm_data() {
    let data = read_osm_data("data/rit.osm", Default::default());
    println!("Done!");
}
