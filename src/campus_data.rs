use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::hash::{Hash, Hasher};
use std::ops::Sub;
use std::rc::Rc;
use crate::{campus_data, Error, Node};
use osm_xml as osm;
use osm_xml::{Coordinate, Tag, UnresolvedReference};
use enumflags2::{bitflags, BitFlags};

pub type CampusNodeID = osm::Id;

#[derive(Debug, Default, Clone)]
pub struct Location(pub Coordinate, pub Coordinate);

#[derive(Debug, Default, Clone)]
pub struct LocDelta(pub Coordinate, pub Coordinate); // these are treated as deltas

#[bitflags]
#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum TransMode {
    Walk, Bike, Car
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
    campus_config: CampusConfig
}

#[derive(Debug)]
pub struct CampusNode {
    pub id: CampusNodeID,
    pub location: Location,
    pub amenities: BitFlags<Amenities>
}

#[derive(Debug)]
pub struct CampusEdge {
    pub start: CampusNodeID,
    pub end: CampusNodeID,
    pub modes: BitFlags<TransMode>
}

#[derive(Debug)]
pub struct CampusConfig {
    pub carry_bike_in_car: bool
}

pub type WorldDist = f64;
pub type WorldTime = f64;
pub type CampusDist = (TransMode, WorldDist);
pub type TravelCost = WorldTime;

#[derive(Debug, Clone)]
struct TravellerState {
    campus: Rc<Campus>,
    me_id: CampusNodeID,
    bike_id: CampusNodeID,
    car_id: CampusNodeID,
}

// functions

#[derive(Debug)]
pub enum CampusError {
    IO(std::io::Error),
    LoadMapError(osm_xml::error::Error),
    CampusReferenceError(String, Option<CampusNodeID>),
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
            edges.insert(edge.start, vec![edge]);
        }
        edge_count += 1;
    };
    let check = |tags: &Vec<Tag>, key, val|
        tags.iter().any(|t| (t.key.as_str(), t.val.as_str()) == (key, val));
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
        let mut modes = BitFlags::<TransMode>::default();
        let one_way = check(&way.tags, "oneway", "yes");
        let parking = check(&way.tags, "parking", "surface") || check(&way.tags, "amenity", "parking");
        for t in way.tags.iter() {
            match (t.key.as_str(), t.val.as_str()) {
                ("foot", "yes") => modes |= TransMode::Walk,
                ("bicycle", "yes") => modes |= TransMode::Bike,
                ("motor_vehicle", "yes") => modes |= TransMode::Car,
                _ => {}
            }
        }
        let mut prev_node = None;
        for i in way.nodes.iter() {
            match i {
                UnresolvedReference::Node(n) => {
                    if let Some(pnode) = prev_node {
                        add_edge(CampusEdge {
                            start: pnode,
                            end: n.clone(),
                            modes: modes.clone()
                        });
                        if !one_way {
                            add_edge(CampusEdge {
                                start: n.clone(),
                                end: pnode,
                                modes: modes.clone()
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
                    prev_node = Some(n.clone());
                }
                UnresolvedReference::Way(_) => {}
                UnresolvedReference::Relation(_) => {}
            }
        }
    }
    println!("Loaded {} nodes and {} edges", nodes.len(), edge_count);
    Ok(Campus {
        nodes,
        edges,
        campus_config: config,
    })
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
    pub fn get_time(&self, mode: TransMode, dist: WorldDist) -> WorldTime {
        unimplemented!()
    }
}

impl Default for CampusConfig {
    fn default() -> Self {
        CampusConfig {
            carry_bike_in_car: false
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

    pub fn get_adjacents(&self, id: &CampusNodeID) -> Vec<(&CampusEdge, &CampusNode)> {
        todo!()
    }

    pub fn calculate_world_dist(&self, start: &CampusNodeID, end: &CampusNodeID) -> Result<WorldDist> {
        Ok((self.get_node(end)?.location.clone() - &self.get_node(start)?.location).len())
    }

    pub fn calculate_len(&self, edge: &CampusEdge) -> Result<WorldDist> {
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
}

impl Node<TravelCost> for TravellerState {
    fn get_neighbors(&self) -> Vec<(TravellerState, TravelCost)> {
        let in_car = &self.car_id == &self.me_id;
        let on_bike = &self.bike_id == &self.me_id;
        let in_parking_lot = self.campus.get_node(&self.me_id).expect("Node not in campus").has_parking();
        let at_bike_rack = self.campus.get_node(&self.me_id).expect("Node not in campus").has_bike_rack();
        let ways = self.campus.get_adjacents(&self.me_id);
        let mut neighbors = vec![];
        let new_campus = || Rc::clone(&self.campus);
        let walk_to = |id, dist, bring_bike| (TravellerState {
            campus: new_campus(),
            me_id: id,
            // you can walk with your bike on walking paths
            bike_id: if bring_bike { id } else { self.bike_id },
            car_id: self.car_id,
        }, self.campus.campus_config.get_time(TransMode::Walk, dist));
        let bike_to = |id, dist| (TravellerState {
            campus: new_campus(),
            me_id: id,
            bike_id: id,
            car_id: self.car_id,
        }, self.campus.campus_config.get_time(TransMode::Bike, dist));
        let drive_to = |id, dist, bring_bike|
            (TravellerState {
            campus: new_campus(),
            me_id: id,
            bike_id: if bring_bike { id } else { self.bike_id },
            car_id: id,
        }, self.campus.campus_config.get_time(TransMode::Car, dist));
        // if we are in some sort of vehicle, we should be able to ride it
        for (edge, dest) in ways.iter() {
            let dist: WorldDist = self.campus.calculate_len(edge).expect("Edge not in campus!");
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
                }
            }
        }
        neighbors
    }

    fn heuristic_cost(&self, other: &Self) -> Result<TravelCost> {
        Ok(self.campus.campus_config.get_time(
            TransMode::Walk,
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

impl Sub<&Location> for Location {
    type Output = LocDelta;

    fn sub(self, rhs: &Location) -> Self::Output {
        let Location(a, b) = &self;
        let Location(x, y) = rhs;
        LocDelta(x - a, y - b)
    }
}

impl LocDelta {
    pub fn len(&self) -> WorldDist {
        let LocDelta(x, y) = self;
        ((*x) * (*x) + (*y) * (*y)).sqrt()
    }
}

impl From<CampusError> for Error {
    fn from(value: CampusError) -> Self {
        Error::CampusError(value)
    }
}