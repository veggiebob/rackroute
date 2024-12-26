use std::collections::{HashMap, HashSet};
use crate::campus_data::{Campus, CampusNodeID};

// 
type GroupID = u32;
type GroupSize = u32;
pub fn connected_components(campus: &Campus) -> HashMap<CampusNodeID, GroupID> {
    // keep track of the groups
    let mut group_id: GroupID = 1;
    let mut group: HashMap<CampusNodeID, GroupID> = HashMap::new();
    let mut group_size: HashMap<GroupID, GroupSize> = HashMap::new();
    let mut group_parent: HashMap<GroupID, Option<GroupID>> = HashMap::new();

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
            if visited.contains(&n) { continue }
            visited.insert(n);
            if let Some(n_gid) = group.get(&n) {
                if n_gid != &gid {
                    // make the parent of the smaller one the parent of the bigger one
                    let mut o_parent = gid;
                    while let Some(Some(p)) = group_parent.get(&o_parent) {
                        o_parent = *p;
                    }
                    let mut c_parent = *n_gid;
                    while let Some(Some(p)) = group_parent.get(&c_parent) {
                        c_parent = *p;
                    }
                    let o_size = group_size.get(&o_parent).expect("crazy");
                    let c_size = group_size.get(&c_parent).expect("crazy");
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
            for (_edge, neighbor) in campus.get_adjacents(&n).expect("crazy") {
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

#[cfg(test)]
mod test {
    use crate::*;
    use crate::campus_data::read_osm_data;
    use crate::map_optimization::connected_components;

    #[test]
    fn test_cc() {
        let campus = read_osm_data("./data/rit.osm", Default::default())
            .unwrap();
        let groups = connected_components(&campus);
        println!("{:?}", groups);
        let group_set = groups.values().collect::<HashSet<_>>();
        println!("{}", group_set.len());
    }
}