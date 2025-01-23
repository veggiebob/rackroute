use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
use crate::lib::campus_data::{Campus, CampusNodeID, Meters};

//
type GroupID = u32;
type GroupSize = u32;
pub fn connected_components(campus: &Campus) -> HashMap<CampusNodeID, GroupID> {
    // keep track of the groups
    let mut group_id: GroupID = 1;
    let mut group: HashMap<CampusNodeID, GroupID> = HashMap::new();
    let mut group_size: HashMap<GroupID, GroupSize> = HashMap::new();
    let mut group_parent: HashMap<GroupID, Option<GroupID>> = HashMap::new();

    let err_msg = "map_optimization::connected_components: error in algorithm";

    // for every node
    for node in campus.nodes() {
        // run dfs on each
        let gid = match group.get(&node.id) {
            Some(id) => continue,
            None => {
                let current_group = group_id;
                group.insert(node.id, current_group);
                group_size.insert(current_group, 1);
                group_parent.insert(current_group, None);
                group_id += 1;
                current_group
            }
        };
        let mut visited = HashSet::new();
        let mut stack = vec![node.id];
        while let Some(n) = stack.pop() {
            if visited.contains(&n) {
                continue;
            }
            visited.insert(n);

            if let Some(n_gid) = group.get(&n) {
                let mut o_parent = gid;
                while let Some(Some(p)) = group_parent.get(&o_parent) {
                    if o_parent == *p {
                        break;
                    }
                    o_parent = *p;
                }
                let mut c_parent = *n_gid;
                while let Some(Some(p)) = group_parent.get(&c_parent) {
                    if c_parent == *p {
                        break;
                    }
                    c_parent = *p;
                }
                if o_parent != c_parent {
                    // make the parent of the smaller one the parent of the bigger one
                    let o_size = group_size.get(&o_parent).expect(err_msg);
                    let c_size = group_size.get(&c_parent).expect(err_msg);
                    let (s_parent, b_parent) = if o_size < c_size {
                        (o_parent, c_parent)
                    } else {
                        (c_parent, o_parent)
                    };
                    group_parent.insert(s_parent, Some(b_parent));
                    group_size.insert(b_parent, o_size + c_size);
                }
                // otherwise, no need to change anything
            } else {
                group.insert(n, gid);
            }
            for (_edge, neighbor) in campus.get_adjacents(&n).expect(err_msg) {
                stack.push(neighbor.id);
            }
        }
    }
    let group_parent = group_parent.clone();
    let parent = |id: GroupID| {
        let mut id = id;
        while let Some(Some(p)) = group_parent.get(&id) {
            id = *p;
        }
        id
    };
    group.into_iter().map(|(k, v)| (k, parent(v))).collect()
}

fn get_group_dist(
    campus: &Campus,
    component_map: &HashMap<CampusNodeID, GroupID>,
) -> HashMap<(GroupID, GroupID), Meters> {
    let group_ids = component_map.values().collect::<HashSet<_>>();
    let mut group_dist: HashMap<(GroupID, GroupID), Meters> = HashMap::new();
    let err_msg = "map_optimization::get_fully_connected_edges: error in algorithm";
    let campus_nodes = campus.nodes();
    let group_nodes = {
        let mut map = HashMap::new();
        for g in group_ids.iter() {
            map.insert(
                *g,
                campus_nodes
                    .iter()
                    .filter(|n| component_map[&n.id] == **g)
                    .collect::<Vec<_>>(),
            );
        }
        map
    };
    for &&a_group in group_ids.iter() {
        for &&b_group in group_ids.iter() {
            if a_group == b_group {
                continue;
            }
            // brute force find minimum distance
            let mut min_dist = Meters::INFINITY;
            for &&a_node in group_nodes[&a_group].iter() {
                for &&b_node in group_nodes[&b_group].iter() {
                    let dist = campus.calculate_world_dist_nodes(a_node, b_node);
                    if dist < min_dist {
                        min_dist = dist;
                    }
                }
            }
            group_dist.insert((a_group, b_group), min_dist);
        }
    }
    group_dist
}

pub fn get_new_edges(
    campus: &Campus,
    relaxation: Option<f64>,
) -> Vec<(CampusNodeID, CampusNodeID)> {
    let relaxation = relaxation.unwrap_or(1.10);
    let component_map = connected_components(campus);
    let err_msg = "map_optimization::get_new_edges: error in algorithm";
    let mut group_dist = get_group_dist(campus, &component_map)
        .into_iter()
        .collect::<Vec<_>>();
    group_dist.sort_by(|(_edge1, dist1), (_edge2, dist2)| {
        dist1.partial_cmp(dist2).unwrap_or(Ordering::Equal)
    });
    let group_dist = group_dist;
    let components = component_map.values().collect::<HashSet<_>>();
    let campus_nodes = campus.nodes();
    let group_nodes = {
        let mut map = HashMap::new();
        for g in components.iter() {
            map.insert(
                *g,
                campus_nodes
                    .iter()
                    .filter(|n| component_map[&n.id] == **g)
                    .collect::<Vec<_>>(),
            );
        }
        map
    };
    let mut roots = {
        let mut roots = HashMap::new();
        for g in components.iter() {
            roots.insert(*g, *g);
        }
        roots
    };
    let mut root_size = {
        let mut root_size = HashMap::new();
        for g in components.iter() {
            root_size.insert(*g, 1);
        }
        root_size
    };
    let mut fully_connected = false;
    let mut new_edges = vec![];
    let mut pre_fully_connected_dists = vec![];
    let mut min_connected_avg_dist = 0f64;
    for ((a, b), dist) in group_dist.iter() {
        if fully_connected && *dist > min_connected_avg_dist * relaxation {
            break;
        }
        let mut a_root = *roots.get(a).expect(err_msg);
        while *roots.get(&a_root).expect(err_msg) != a_root {
            a_root = *roots.get(&a_root).expect(err_msg);
        }
        let mut b_root = *roots.get(b).expect(err_msg);
        while *roots.get(&b_root).expect(err_msg) != b_root {
            b_root = *roots.get(&b_root).expect(err_msg);
        }
        if a_root != b_root {
            let a_size = *root_size.get(&a_root).expect(err_msg);
            let b_size = *root_size.get(&b_root).expect(err_msg);
            if a_size < b_size {
                roots.insert(a_root, b_root);
                root_size.insert(b_root, a_size + b_size);
            } else {
                roots.insert(b_root, a_root);
                root_size.insert(a_root, a_size + b_size);
            }
            if a_size + b_size == components.len() {
                fully_connected = true;
                // don't break, because we might want to do more
                // than just minimum spanning tree
            }
            // do connection
            // find the closest nodes in both groups
            let mut min_dist = Meters::INFINITY;
            let mut min_edge = None;
            for a_node in group_nodes[a].iter() {
                for b_node in group_nodes[b].iter() {
                    let dist = campus.calculate_world_dist_nodes(&a_node, &b_node);
                    if dist < min_dist {
                        min_dist = dist;
                        min_edge = Some((a_node.id, b_node.id));
                    }
                }
            }
            if let Some(edge) = min_edge {
                new_edges.push(edge);
                if !fully_connected {
                    pre_fully_connected_dists.push(min_dist);
                    min_connected_avg_dist = pre_fully_connected_dists.iter().sum::<Meters>()
                        / pre_fully_connected_dists.len() as Meters;
                }
            }
        }
    }
    new_edges
}

#[cfg(test)]
mod test {
    use std::collections::HashSet;
    use enumflags2::BitFlags;
    use crate::lib::campus_data::{read_osm_data, TransMode};
    use crate::lib::map_optimization::{connected_components, get_new_edges};

    #[test]
    fn test_cc() {
        let campus = read_osm_data("./data/rit.osm", Default::default()).unwrap();
        let groups = connected_components(&campus);
        // println!("{:?}", groups);
        let group_set = groups.values().collect::<HashSet<_>>();
        println!("original # of components: {}", group_set.len());

        let new_edges = get_new_edges(&campus, None);
        println!("{} new edges", new_edges.len());
        let mut m_campus = campus;
        for (a, b) in new_edges {
            m_campus.add_edge(a, b, TransMode::Bushwhack.into(), BitFlags::default());
        }
        let campus = m_campus;
        let groups = connected_components(&campus);
        let group_set = groups.values().collect::<HashSet<_>>();
        println!("new # of components: {}", group_set.len());
        assert!(group_set.len() <= 1);
    }
}
