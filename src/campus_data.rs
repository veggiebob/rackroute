use std::borrow::Borrow;
use std::collections::HashMap;
use std::fs::File;
use std::hash::{Hash, Hasher};
use std::ops::{Add, Div, Index, Mul, Sub};
use std::rc::Rc;
use crate::{Error, Node, SearchEnd};
use osm_xml as osm;
use osm_xml::{Coordinate, Tag, UnresolvedReference};
use enumflags2::{bitflags, BitFlags};
use geo::{Contains, LineString};
use crate::map_optimization::get_new_edges;

pub type CampusNodeID = osm::Id;

#[derive(Debug, Default, Clone)]
pub struct Location(pub Coordinate, pub Coordinate);

#[derive(Debug, Default, Clone)]
pub struct LocDelta(pub Coordinate, pub Coordinate); // these are treated as deltas

#[bitflags]
#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum TransMode {
    Walk, Bike, Car, Bushwhack
}

#[bitflags]
#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Amenities {
    BikeRack, Parking
}

#[derive(Debug)]
pub struct Campus {
    nodes: HashMap<CampusNodeID, CampusNode>,
    edges: HashMap<CampusNodeID, Vec<CampusEdge>>,
    campus_config: CampusConfig,
    pub misc_features: Vec<SimpleEdge>,
}

#[derive(Debug, Clone)]
pub struct CampusNode {
    pub id: CampusNodeID,
    pub location: Location,
    pub amenities: BitFlags<Amenities>
}

pub type SimpleEdge = (Location, Location);
#[derive(Debug, Default)]
pub struct CampusEdge {
    pub start: CampusNodeID,
    pub end: CampusNodeID,
    pub modes: BitFlags<TransMode>,
    pub amenities: BitFlags<Amenities>,
    pub metadata: HashMap<String, String>
}

#[derive(Debug)]
pub struct CampusConfig {
    pub carry_bike_in_car: bool,
    speeds: HashMap<TransMode, MetersPerSecond>
}

pub type Meters = f64; // meters
pub type Seconds = f64; // seconds
pub type MetersPerSecond = f64; // m/s
pub type CampusDist = (TransMode, Meters);
pub type TravelCost = Seconds;

#[derive(Debug, Clone)]
pub struct TravellerState {
    campus: Rc<Campus>,
    pub me_id: CampusNodeID,
    pub bike_id: CampusNodeID,
    pub car_id: CampusNodeID,
}

// functions

#[derive(Debug)]
pub enum CampusError {
    IO(std::io::Error),
    LoadMapError(osm_xml::error::Error),
    CampusReferenceError(String, Option<CampusNodeID>),
    EmptyCampus, // there are no nodes??
    Other(String),
}
type Result<T> = std::result::Result<T, Error>;



/// Reads OSM XML file generated using OpenStreetMap.
/// See more at https://www.openstreetmap.org/about
/// If you're unsure about the config, just use the default
pub fn read_osm_data(filepath: &str, config: CampusConfig) -> Result<Campus> {
    // open file
    let file = File::open(filepath).map_err(CampusError::IO)?;
    let doc = osm::OSM::parse(file).map_err(CampusError::LoadMapError)?;
    let mut nodes = HashMap::new();
    let mut edges: HashMap<CampusNodeID, Vec<_>> = HashMap::new();
    let mut edge_count = 0;
    let mut add_edge = |edge: CampusEdge| {
        if let Some(edges) = edges.get_mut(&edge.start) {
            edges.push(edge);
        } else {
            if let None = edges.get(&edge.end) {
                // insert into edges to mark it as reachable
                edges.insert(edge.end, vec![]);
            }
            edges.insert(edge.start, vec![edge]);
        }
        edge_count += 1;
    };
    let mut misc_features = vec![];
    let check = |tags: &Vec<Tag>, key, val|
        tags.iter().any(|t| (t.key.as_str(), t.val.as_str()) == (key, val));
    let contains = |tags: &Vec<Tag>, key|
        tags.iter().any(|t| t.key.as_str() == key);
    let get_val = |tags: &Vec<Tag>, key| tags.iter().find(|t| t.key.as_str() == key).map(|t| t.val.clone());
    for (id, node) in doc.nodes.iter() {
        nodes.insert(id.clone(), CampusNode {
            location: Location(node.lat, node.lon),
            id: id.clone(),
            amenities: if check(&node.tags, "amenity", "bicycle_parking") {
                Amenities::BikeRack.into()
            } else {
                BitFlags::<Amenities>::default()
            }
        });
        // amenity: bicycle_parking
        // bicycle_parking: wall_loops
    }
    let mut parking_polygons = vec![];
    for (_id, way) in doc.ways.iter() {
        /*
        other interesting information:
            <tag k="bicycle" v="yes"/>
            <tag k="foot" v="no"/>
            <tag k="highway" v="unclassified"/>
            <tag k="horse" v="no"/>
            <tag k="lanes" v="2"/>
            <tag k="lanes:backward" v="1"/>
            <tag k="lanes:forward" v="1"/>
            <tag k="maxspeed" v="25 mph"/>
            <tag k="motor_vehicle" v="yes"/>
            <tag k="name" v="Andrews Memorial Drive"/>
            <tag k="surface" v="asphalt"/>
            ...
            <tag k="tiger:cfcc" v="A41"/>
            <tag k="tiger:county" v="Monroe, NY"/>
            <tag k="tiger:name_base" v="River Meadow"/>
            <tag k="tiger:name_type" v="Dr"/>
            <tag k="tiger:reviewed" v="no"/>
            <tag k="tiger:zip_left" v="14623"/>
            <tag k="tiger:zip_right" v="14623"/>
            ...
            <tag k="amenity" v="parking"/>
            <tag k="capacity:disabled" v="3"/>
            <tag k="parking" v="surface"/>
            <tag k="surface" v="asphalt"/>
         */
        let one_way = check(&way.tags, "oneway", "yes");
        let footpath = check(&way.tags, "foot", "yes");
        let road = check(&way.tags, "motor_vehicle", "yes")
            || contains(&way.tags, "highway");
        let bike_path = check(&way.tags, "bicycle", "yes");
        let parking = check(&way.tags, "parking", "surface") || check(&way.tags, "amenity", "parking");


        let mut modes = BitFlags::default();
        if footpath { modes |= TransMode::Walk; }
        if bike_path { modes |= TransMode::Bike; }
        if road { modes |= TransMode::Car }
        
        let is_misc_feature = !footpath && !bike_path && !road
            && !parking
            && !contains(&way.tags, "highway")
            && !contains(&way.tags, "footway")
            && !check(&way.tags, "route", "road");

        let mut prev_node = None;
        if parking {
            let coords = way.nodes.iter()
                .filter_map(|n| {
                    if let UnresolvedReference::Node(n) = n {
                        nodes.get(n).map(|n| n.location.clone())
                    } else {
                        None
                    }
                })
                .map(|location| geo::Coord { x: location.0, y: location.1 })
                .collect::<Vec<_>>();
            let polygon = geo::Polygon::new(LineString::new(coords), vec![]);
            parking_polygons.push(polygon);
            continue;
        }

        let name = get_val(&way.tags, "name");

        let metadata = {
            let mut map = HashMap::new();
            if let Some(name) = name {
                map.insert("name".to_string(), name);
            }
            map
        };
        for i in way.nodes.iter() {
            match i {
                UnresolvedReference::Node(n) => {
                    if let Some(pnode) = prev_node {
                        if is_misc_feature {
                            if let (Some(s), Some(e)) = (nodes.get(&pnode), nodes.get(n)) {
                                misc_features.push(
                                    (s.location.clone(), e.location.clone())
                                );
                            }
                        } else {
                            add_edge(CampusEdge {
                                start: pnode,
                                end: n.clone(),
                                modes: modes.clone(),
                                metadata: metadata.clone(),
                                ..Default::default()
                            });
                            if !one_way {
                                add_edge(CampusEdge {
                                    start: n.clone(),
                                    end: pnode,
                                    modes: modes.clone(),
                                    metadata: metadata.clone(),
                                    ..Default::default()
                                });
                            }
                            if parking {
                                if let Some(p) = nodes.get_mut(&pnode) {
                                    p.amenities |= Amenities::Parking;
                                }
                                if let Some(p) = nodes.get_mut(n) {
                                    p.amenities |= Amenities::Parking;
                                }
                            }
                        }
                    }
                    prev_node = Some(n.clone());
                }
                UnresolvedReference::Way(_) => {}
                UnresolvedReference::Relation(_) => {}
            }
        }
    }
    nodes.retain(|n, v|
        // only care about reachable nodes & those with a bike rack
        edges.contains_key(n) || v.has_bike_rack()
    );
    let node_error = |id| CampusError::CampusReferenceError("Node not found".to_string(), Some(id));
    for poly in parking_polygons {
        for (_id, node) in nodes.iter_mut() {
            if poly.contains(&geo::Point::new(node.location.0, node.location.1)) {
                node.amenities |= Amenities::Parking;
            }
        }
        for edge in edges.values().flatten() {
            let start = nodes[&edge.start].location.clone();
            let end = nodes[&edge.end].location.clone();
            let mid = start.clone() + (end.clone() - &start) * 0.5;
            if poly.contains(&geo::Point::new(mid.0, mid.1))
                || poly.contains(&geo::Point::new(start.0, start.1))
                || poly.contains(&geo::Point::new(end.0, end.1)) {
                nodes.get_mut(&edge.start).map(|n| n.amenities |= Amenities::Parking);
                nodes.get_mut(&edge.end).map(|n| n.amenities |= Amenities::Parking);
            }
        }
    }
    println!("Loaded {} nodes and {} edges", nodes.len(), edge_count);
    let mut campus = Campus {
        nodes,
        edges,
        campus_config: config,
        misc_features
    };
    let mut bike_rack_edges_added = 0;
    let mut edges_to_add = vec![];
    for node in campus.nodes() {
        if node.has_bike_rack() {
            let needs_connection = match campus.get_adjacents(&node.id) {
                Ok(es) => es.is_empty(),
                _ => true
            };
            if needs_connection {
                // add a 2-way walking & biking connection to the nearest node
                let ok = campus.find_closest_node_with(&node.location, |n| &n.id != &node.id)
                    .map(|(n, _)| {
                        edges_to_add.push((node.id.clone(), n.id.clone(), TransMode::Walk | TransMode::Bike));
                        edges_to_add.push((n.id.clone(), node.id.clone(), TransMode::Walk | TransMode::Bike));
                    })
                    // say something if it couldn't be connected
                    .inspect_err(|e| println!("Could not connect bike rack {}: {:?}", &node.id, e))
                    .ok();
                if ok.is_some() {
                    bike_rack_edges_added += 1;
                }
            }
        }
    }
    for (start, end, modes) in edges_to_add {
        campus.add_edge(start, end, modes, BitFlags::default());
    }
    println!("Added {} edges to connect bike racks", bike_rack_edges_added);
    Ok(campus)
}

// ----------------- impls --------------------------------
impl From<std::io::Error> for CampusError {
    fn from(e: std::io::Error) -> Self {
        CampusError::IO(e)
    }
}
impl From<osm::error::Error> for CampusError {
    fn from(e: osm::error::Error) -> Self {
        CampusError::LoadMapError(e)
    }
}
impl CampusConfig {
    pub fn new(carry_bike_in_car: bool, walk_speed: MetersPerSecond, bike_speed: MetersPerSecond) -> CampusConfig {
        CampusConfig {
            carry_bike_in_car,
            speeds: {
                let mut speeds = HashMap::new();
                speeds.insert(TransMode::Walk, walk_speed);
                speeds.insert(TransMode::Bike, bike_speed);
                speeds
            },
        }
    }
    pub fn get_time(&self, mode: &TransMode, dist: Meters) -> Seconds {
        // dist in meters
        // speed in meters per second
        let speed = self.speeds.get(&mode).expect(format!("Speed missing for {:?}", &mode).as_str());
        dist / speed
    }
    pub fn get_drive_time(&self, dist: Meters, speed_limit_mph: u32) -> Seconds {
        // convert from mph to meters per second
        // 1 mi     1 hour  1 min   1600 m
        // 1 hour   60 min  60 sec  1 mi
        let mps = speed_limit_mph as f64 * 1600. / 60. / 60.;
        dist / mps
    }
}

impl Default for CampusConfig {
    fn default() -> Self {
        CampusConfig {
            carry_bike_in_car: false,
            speeds: {
                let mut speeds = HashMap::new();
                speeds.insert(TransMode::Walk, 1.);
                speeds.insert(TransMode::Bike, 9.);
                speeds.insert(TransMode::Bushwhack, 0.7);
                speeds
            }
        }
    }
}

impl PartialEq for TravellerState {
    fn eq(&self, other: &Self) -> bool {
        (self.car_id == other.car_id)
            && (self.bike_id == other.bike_id)
            && (self.me_id == other.me_id)
    }
}

impl Eq for TravellerState {}

impl Hash for TravellerState {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.car_id.hash(state);
        self.bike_id.hash(state);
        self.me_id.hash(state);
    }
}

impl Campus {
    pub fn get_node(&self, id: &CampusNodeID) -> Result<&CampusNode> {
        self.nodes.get(id).ok_or(CampusError::CampusReferenceError("Node not found".to_string(), Some(id.clone())).into())
    }

    pub fn get_adjacents(&self, id: &CampusNodeID) -> Result<Vec<(&CampusEdge, &CampusNode)>> {
        Ok(self.edges.get(id)
            // .ok_or(CampusError::CampusReferenceError(format!("No edges connecting to {}", id), Some(*id)))?
            .map(|vs| vs.iter()
                .map(|edge|
                    (edge, self.get_node(&edge.end).expect("Edge end not in campus")))
                .collect())
               .unwrap_or(vec![]))
    }

    pub fn calculate_world_dist(&self, start: &CampusNodeID, end: &CampusNodeID) -> Result<Meters> {
        Ok((self.get_node(end)?.location.clone() - &self.get_node(start)?.location).len())
    }
    pub fn calculate_world_dist_nodes(&self, start: &CampusNode, end: &CampusNode) -> Meters {
        (start.location.clone() - &end.location).len()
    }

    pub fn calculate_len(&self, edge: &CampusEdge) -> Result<Meters> {
        Ok(self.calculate_world_dist(&edge.start, &edge.end)?)
    }

    pub fn edges(&self) -> Vec<&CampusEdge> {
        self.edges.values().flatten().collect()
    }

    pub fn nodes(&self) -> Vec<&CampusNode> {
        self.nodes.values().collect()
    }

    pub fn crop_latitudes(&mut self, minimum: Coordinate, maximum: Coordinate) {
        self.edges.retain(|_, edges| {
            edges.iter().all(|edge| {
                let Location(lat, _) = self.nodes.get(&edge.start).expect("Node not found").location;
                let Location(lat2, _) = self.nodes.get(&edge.end).expect("Node not found").location;
                lat >= minimum && lat <= maximum && lat2 >= minimum && lat2 <= maximum
            })
        });
        self.nodes.retain(|_, node| {
            let Location(lat, _) = node.location;
            lat >= minimum && lat <= maximum
        });
    }

    pub fn crop_longitudes(&mut self, minimum: Coordinate, maximum: Coordinate) {
        self.edges.retain(|_, edges| {
            edges.iter().all(|edge| {
                let Location(_, long) = self.nodes.get(&edge.start).expect("Node not found").location;
                let Location(_, long2) = self.nodes.get(&edge.end).expect("Node not found").location;
                long >= minimum && long <= maximum && long2 >= minimum && long2 <= maximum
            })
        });
        self.nodes.retain(|_, node| {
            let Location(_, long) = node.location;
            long >= minimum && long <= maximum
        });
    }

    pub fn find_closest_node(&self, location: &Location) -> Result<(&CampusNode, Meters)> {
        let mut first = true;
        let mut dist = 0.;
        let mut node = None;
        for n in self.nodes() {
            let d = (location.clone() - &n.location).len();
            if first || d < dist {
                dist = d;
                node = Some(n)
            }
            first = false;
        }
        node.map(|n| (n, dist)).ok_or(CampusError::EmptyCampus.into())
    }

    pub fn find_closest_node_with<F>(&self, location: &Location, criterion: F) -> Result<(&CampusNode, Meters)>
        where F: Fn(&CampusNode) -> bool {
        let mut first = true;
        let mut dist = 0.;
        let mut node = None;
        for n in self.nodes() {
            if !criterion(n) {
                continue;
            }
            let d = (location.clone() - &n.location).len();
            if first || d < dist {
                dist = d;
                node = Some(n)
            }
            first = false;
        }
        node.map(|n| (n, dist)).ok_or(CampusError::EmptyCampus.into())
    }

    pub fn bounding_box(&self) -> BoundingBox {
        let mut first = true;
        let mut min_x = 0.;
        let mut max_x = 0.;
        let mut min_y = 0.;
        let mut max_y = 0.;
        for node in self.nodes() {
            let Location(x, y) = node.location;
            if first {
                min_x = x;
                max_x = x;
                min_y = y;
                max_y = y;
            } else {
                if x < min_x {
                    min_x = x;
                }
                if x > max_x {
                    max_x = x;
                }
                if y < min_y {
                    min_y = y;
                }
                if y > max_y {
                    max_y = y;
                }
            }
            first = false;
        }
        ((min_x, max_x), (min_y, max_y))
    }

    pub fn add_edge(&mut self, start: CampusNodeID, end: CampusNodeID, modes: BitFlags<TransMode>, amenities: BitFlags<Amenities>) {
        self.edges.entry(start).or_insert(vec![]).push(CampusEdge {
            start,
            end,
            modes,
            amenities,
            ..Default::default()
        });
        self.edges.entry(end).or_insert(vec![]);
    }

    pub fn find_edge(&self, start: &CampusNodeID, end: &CampusNodeID) -> Option<&CampusEdge> {
        self.edges.get(start)?.iter().find(|e| &e.end == end)
    }

    pub fn make_strongly_connected(&mut self, relaxation: Option<f64>) {
        let new_edges = get_new_edges(self, relaxation);
        for (a, b) in new_edges {
            self.add_edge(a, b, TransMode::Bushwhack.into(), BitFlags::default());
            self.add_edge(b, a, TransMode::Bushwhack.into(), BitFlags::default());
        }
    }
}

impl Index<CampusNodeID> for Campus {
    type Output = CampusNode;

    fn index(&self, index: CampusNodeID) -> &Self::Output {
        self.get_node(&index).expect(format!("Node {} not found", index).as_str())
    }
}

impl TravellerState {
    pub fn new(campus: Rc<Campus>, me_id: CampusNodeID, bike_id: CampusNodeID, car_id: CampusNodeID) -> TravellerState {
        TravellerState {
            campus,
            me_id,
            bike_id,
            car_id,
        }
    }
}
impl TravellerState {

    pub fn create_goal(&self, end_id: CampusNodeID)
        -> SearchEnd<
            TravellerState,
            impl Fn(&TravellerState) -> bool + '_,
            impl Fn(&TravellerState) -> Seconds + '_
        > {
        let (end_p, end_h) = (end_id.clone(), end_id);
        SearchEnd::Custom(
            move |n: &TravellerState| {
                let end_id = end_p;
                &n.me_id == &end_id
            },
            move |n: &TravellerState| {
                let end_id = end_h;
                self.campus.campus_config.get_time(
                    &TransMode::Walk,
                    self.campus.calculate_world_dist(&n.me_id, &end_id).expect(format!(
                        "Could not calculate distance between {} and {}",
                        &n.me_id, &end_id
                    ).as_str())
                )
            }
        )
    }
}
impl Node<TravelCost> for TravellerState {
    fn get_neighbors(&self) -> Vec<(TravellerState, TravelCost)> {
        let in_car = &self.car_id == &self.me_id;
        let on_bike = &self.bike_id == &self.me_id;
        let in_parking_lot = self.campus.get_node(&self.me_id).expect("Node not in campus").has_parking();
        let at_bike_rack = self.campus.get_node(&self.me_id).expect("Node not in campus").has_bike_rack();
        let ways = self.campus.get_adjacents(&self.me_id).expect("Node not in campus");
        let mut neighbors = vec![];
        let new_campus = || Rc::clone(&self.campus);
        let walk_to = |id, dist, bring_bike| (TravellerState {
            campus: new_campus(),
            me_id: id,
            // you can walk with your bike on walking paths
            bike_id: if bring_bike { id } else { self.bike_id },
            car_id: self.car_id,
        }, self.campus.campus_config.get_time(&TransMode::Walk, dist));
        let bike_to = |id, dist| (TravellerState {
            campus: new_campus(),
            me_id: id,
            bike_id: id,
            car_id: self.car_id,
        }, self.campus.campus_config.get_time(&TransMode::Bike, dist));
        let drive_to = |id, dist, bring_bike|
            (TravellerState {
            campus: new_campus(),
            me_id: id,
            bike_id: if bring_bike { id } else { self.bike_id },
            car_id: id,
        }, self.campus.campus_config.get_drive_time(dist, 40)); // todo: use actual speed limit
        let bushwhack_to = |id, dist| (TravellerState {
            campus: new_campus(),
            me_id: id,
            bike_id: self.bike_id,
            car_id: self.car_id,
        }, self.campus.campus_config.get_time(&TransMode::Bushwhack, dist));
        // if we are in some sort of vehicle, we should be able to ride it
        for (edge, dest) in ways.iter() {
            let dist: Meters = self.campus.calculate_len(edge).expect("Edge not in campus!");
            if in_car {
                if on_bike {
                    if in_parking_lot && edge.modes.contains(TransMode::Bike) {
                        neighbors.push(bike_to(dest.id, dist));
                    }
                    if self.campus.campus_config.carry_bike_in_car && edge.modes.contains(TransMode::Car) {
                        neighbors.push(drive_to(dest.id, dist, true));
                    }
                    if in_parking_lot {
                        neighbors.push(walk_to(dest.id, dist, !at_bike_rack));
                    }
                } else {
                    if edge.modes.contains(TransMode::Car) {
                        neighbors.push(drive_to(dest.id, dist, false));
                    }
                    if in_parking_lot {
                        neighbors.push(walk_to(dest.id, dist, false)); // no bike
                    }
                }
            } else {
                if on_bike {
                    if edge.modes.contains(TransMode::Bike) {
                        neighbors.push(bike_to(dest.id, dist));
                    }
                    if at_bike_rack {
                        neighbors.push(walk_to(dest.id, dist, true));
                        neighbors.push(walk_to(dest.id, dist, false));
                    }
                } else {
                    neighbors.push(walk_to(dest.id, dist, false));
                    neighbors.push(bushwhack_to(dest.id, dist));
                }
            }
        }
        neighbors
    }

    fn heuristic_cost(&self, other: &Self) -> Result<TravelCost> {
        Ok(self.campus.campus_config.get_time(
            &TransMode::Walk,
            self.campus.calculate_world_dist(&self.me_id, &other.me_id)?
        ))
    }
}

impl CampusNode {
    pub fn has_parking(&self) -> bool {
        self.amenities.contains(Amenities::Parking)
    }
    pub fn has_bike_rack(&self) -> bool {
        self.amenities.contains(Amenities::BikeRack)
    }
}

impl Hash for CampusNode {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl PartialEq for CampusNode {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for CampusNode {}

impl CampusEdge {
    pub fn get_midpoint<C: Borrow<Campus>>(&self, campus: C) -> Location {
        let campus = campus.borrow();
        let start = campus[self.start].location.clone();
        let end = campus[self.end].location.clone();
        start.clone() + (end - &start) * 0.5f64
    }
    pub fn get_name(&self) -> Option<&String> {
        self.metadata.get("name")
    }
}

impl Add<LocDelta> for Location {
    type Output = Location;

    fn add(self, rhs: LocDelta) -> Self::Output {
        let Location(a, b) = self;
        let LocDelta(x, y) = rhs;
        Location(a + x, b + y)
    }
}

impl Sub<&Location> for Location {
    type Output = LocDelta;

    fn sub(self, rhs: &Location) -> Self::Output {
        let Location(a, b) = &self;
        let Location(x, y) = rhs;
        LocDelta(x - a, y - b)
    }
}
impl Location {
    pub fn lat(&self) -> f64 {
        self.0
    }
    pub fn lng(&self) -> f64 {
        self.1
    }
}

impl LocDelta {
    pub fn len(&self) -> Meters {
        let (x, y) = self.get_meters();
        (x * x + y * y).sqrt()
    }
    pub fn get_meters(&self) -> (Meters, Meters) {
        let lat_to_m = 111. * 1000.;
        let long_to_m = 111. * 1000.;
        let LocDelta(lat, long) = self;
        (lat * lat_to_m, long * long_to_m)
    }
}

impl Div<f64> for LocDelta {
    type Output = LocDelta;

    fn div(self, rhs: f64) -> Self::Output {
        let LocDelta(lat, long) = self;
        LocDelta(lat / rhs, long / rhs)
    }
}

impl Mul<f64> for LocDelta {
    type Output = LocDelta;

    fn mul(self, rhs: f64) -> Self::Output {
        let LocDelta(lat, long) = self;
        LocDelta(lat * rhs, long * rhs)
    }
}

impl From<CampusError> for Error {
    fn from(value: CampusError) -> Self {
        Error::CampusError(value)
    }
}

pub type BoundingBox = ((Coordinate, Coordinate), (Coordinate, Coordinate));