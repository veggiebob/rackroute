use num::Zero;
use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashMap, HashSet};
use std::fmt::Debug;
use std::hash::{Hash, Hasher};

pub mod campus_data;
pub mod campus_directions;
pub mod debug_draw;
pub mod map_optimization;
#[cfg(test)]
pub mod test;

trait Node<D> {
    /// Get all the neighbors of a node and
    fn get_neighbors(&self) -> Vec<(Self, D)>
    where
        Self: Sized;

    /// Give an underestimated cost to get from self to other
    fn heuristic_cost(&self, other: &Self) -> Result<D>;
}

/// Used for custom ordering of min-heap for A* algorithm
#[derive(Debug)]
struct SearchNode<N, C> {
    node: N,
    current_cost: C,
}

impl<N: Eq, C: PartialEq> Eq for SearchNode<N, C> {}
impl<N: Eq, C: PartialEq> PartialEq<Self> for SearchNode<N, C> {
    fn eq(&self, other: &Self) -> bool {
        self.node == other.node && self.current_cost == other.current_cost
    }
}
impl<N: Eq, C: PartialEq + PartialOrd> PartialOrd<Self> for SearchNode<N, C> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.current_cost
            .partial_cmp(&other.current_cost)
            .map(|o| o.reverse()) // this is a min heap
    }
}
impl<N: Eq, C: PartialEq + PartialOrd> Ord for SearchNode<N, C> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

#[derive(Debug)]
pub enum Error {
    NoPathFound,
    CampusError(campus_data::CampusError),
    DebugDrawingError(String),
}

type Result<T> = std::result::Result<T, Error>;

pub enum SearchEnd<'a, N, P, H> {
    Custom(P, H),
    Node(&'a N),
}

impl<'a, N, C> SearchEnd<'a, N, fn(&N) -> bool, fn(&N) -> C> {
    pub fn node(node: &'a N) -> Self {
        SearchEnd::Node(node)
    }
}

/// Use rich nodes to run A* on a potentially unbound graph.
pub fn find_path<C, N, P, H>(start: &N, end: SearchEnd<N, P, H>) -> Result<(Vec<N>, C)>
where
    N: Clone + PartialEq + Eq + Hash + Node<C> + Debug,
    C: PartialOrd + Zero + Clone,
    P: Fn(&N) -> bool,
    H: Fn(&N) -> C,
{
    let heuristic = |n: N| match &end {
        SearchEnd::Custom(_p, heuristic) => Ok(heuristic(&n)),
        SearchEnd::Node(end_node) => end_node.heuristic_cost(&n),
    };
    let is_end = |n: N| match &end {
        SearchEnd::Custom(predicate, _h) => predicate(&n),
        SearchEnd::Node(end_node) => &n == *end_node,
    };
    let mut open: BinaryHeap<SearchNode<N, C>> = {
        let mut open = BinaryHeap::new();
        open.push(SearchNode {
            node: start.clone(),
            current_cost: C::zero(),
        });
        open
    };
    let mut closed: HashSet<N> = HashSet::new();
    let mut gscore: HashMap<N, C> = {
        let mut gscore: HashMap<N, C> = HashMap::new();
        gscore.insert(start.clone(), C::zero());
        gscore
    };
    let mut parent: HashMap<N, Option<N>> = {
        let mut parent: HashMap<N, Option<N>> = HashMap::new();
        parent.insert(start.clone(), None);
        parent
    };

    let reconstruct_path = |end: &N, parent: HashMap<_, _>| {
        let mut current = end;
        let mut path = vec![end.clone()];
        while let Some(Some(p)) = parent.get(current) {
            current = p;
            path.push(current.clone());
        }
        path.reverse();
        path
    };
    let mut searched_nodes = 0;
    while let Some(current) = open.pop() {
        searched_nodes += 1;
        if let Some(actual_current_cost) = gscore.get(&current.node) {
            let actual_fscore = actual_current_cost.clone() + heuristic(current.node.clone())?;
            if actual_fscore < current.current_cost {
                // this is a duplicate
                continue;
            }
        }
        if is_end(current.node.clone()) {
            println!("Searched {} nodes and found a path", searched_nodes);
            return Ok((
                reconstruct_path(&current.node, parent.clone()),
                gscore[&current.node].clone(),
            ));
        }
        for (n, edge_cost) in current.node.get_neighbors() {
            if closed.contains(&n) {
                // it has already been finalized/visited
                continue;
            } else {
                let g = gscore[&current.node].clone() + edge_cost;
                if let Some(cost) = gscore.get(&n) {
                    // compare g scores because the heuristic stays the same
                    if &g < cost {
                        // relax this edge
                        gscore.insert(n.clone(), cost.clone());
                        parent.insert(n.clone(), Some(current.node.clone()));
                        let fscore = g + heuristic(n.clone())?;
                        // push a duplicate
                        open.push(SearchNode {
                            node: n,
                            current_cost: fscore,
                        });
                    }
                } else {
                    // we haven't seen this new node yet
                    gscore.insert(n.clone(), g.clone());
                    parent.insert(n.clone(), Some(current.node.clone()));
                    open.push(SearchNode {
                        current_cost: g + heuristic(n.clone())?,
                        node: n,
                    })
                }
            }
        }
        closed.insert(current.node.clone());
    }
    println!("Searched {} nodes, no path found", searched_nodes);
    Err(Error::NoPathFound)
}
