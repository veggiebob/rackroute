use std::rc::Rc;
use std::collections::HashMap;
use crate::campus_data::{Campus, CampusNodeID, CampusNode};

// 
type GroupID = u32;
type GroupSize = u32;
pub fn connected_components<'a>(campus: &'a Campus) -> Vec<Vec<&'a CampusNode>> {
    // keep track of the groups
    let mut group_id: GroupID = 1;
    let mut group: HashMap<CampusNodeID, GroupID> = HashMap::new();
    let mut group_size: HashMap<GroupID, GroupSize> = HashMap::new();
    let mut group_parent: HashMap<GroupID, Option<GroupID>> = HashMap::new();

    let init_group = |id: CampusNodeID, parent: Option<GroupID>| {
        group.insert(id, group_id);
        group_size.insert(group_id, 1);
        group_parent.insert(group_id, parent);
        group_id += 1;
    };
    // run DFS on each node, merging smaller groups where necessary
    todo!()
}