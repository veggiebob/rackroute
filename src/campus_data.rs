use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::hash::{Hash, Hasher};
use std::ops::Sub;
use std::rc::Rc;
use crate::Node;
use osm_xml as osm;
use osm_xml::{Coordinate, Tag, UnresolvedReference};
use enumflags2::{bitflags, BitFlags};

pub type CampusNodeID = osm::Id;
pub struct Location(Coordinate, Coordinate);
pub struct LocDelta(Coordinate, Coordinate); // these are treated as deltas

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
    campus_config: CampusConfig
}

pub struct CampusNode {
    id: CampusNodeID,
    location: Location,
    amenities: BitFlags<Amenities>
}

pub struct CampusEdge {
    start: CampusNodeID,
    end: CampusNodeID,
    modes: BitFlags<TransMode>
}

#[derive(Debug)]
pub struct CampusConfig {
    carry_bike_in_car: bool
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

type LoadResult<T> = Result<T, Error>;
#[derive(Debug)]
pub enum Error {
    IO(std::io::Error),
    LoadMapError(osm_xml::error::Error),
    Other(String),
}

/// Reads OSM XML file generated using OpenStreetMap.
/// See more at https://www.openstreetmap.org/about
pub fn read_osm_data(filepath: &str) -> Result<Campus, Error> {
    // open file
    let file = File::open(filepath)?;
    let doc = osm::OSM::parse(file)?;
    let mut nodes = HashMap::new();
    let mut edges = vec![];
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
                        edges.push(CampusEdge {
                            start: pnode,
                            end: n.clone(),
                            modes: modes.clone()
                        });
                        if !one_way {
                            edges.push(CampusEdge {
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
    Err(Error::Other("unfinished".to_string()))
}

// ----------------- impls --------------------------------
impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Error::IO(e)
    }
}
impl From<osm::error::Error> for Error {
    fn from(e: osm::error::Error) -> Self {
        Error::LoadMapError(e)
    }
}
impl CampusConfig {
    pub fn get_time(&self, mode: TransMode, dist: WorldDist) -> WorldTime {
        unimplemented!()
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
    pub fn get_node(&self, id: &CampusNodeID) -> &CampusNode {
        todo!()
    }

    pub fn get_adjacents(&self, id: &CampusNodeID) -> Vec<(CampusEdge, CampusNode)> {
        todo!()
    }

    pub fn calculate_world_dist(&self, start: &CampusNodeID, end: &CampusNodeID) -> WorldDist {
        (&self.get_node(end).location - &self.get_node(start).location).len()
    }

    pub fn calculate_len(&self, edge: &CampusEdge) -> WorldDist {
        self.calculate_world_dist(&edge.start, &edge.end)
    }
}

impl Node<TravelCost> for TravellerState {
    fn get_neighbors(&self) -> Vec<(TravellerState, TravelCost)> {
        let in_car = &self.car_id == &self.me_id;
        let on_bike = &self.bike_id == &self.me_id;
        let in_parking_lot = self.campus.get_node(&self.me_id).has_parking();
        let at_bike_rack = self.campus.get_node(&self.me_id).has_bike_rack();
        let ways = self.campus.get_adjacents(&self.me_id);
        let mut neighbors = HashSet::new();
        let new_campus = || Rc::clone(&self.campus);
        let walk_to = |id, dist| (TravellerState {
            campus: new_campus(),
            me_id: id,
            bike_id: self.bike_id,
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
            let dist: WorldDist = self.campus.calculate_len(edge);
            if in_car {
                if on_bike {
                    if in_parking_lot && edge.modes.contains(TransMode::Bike) {
                        neighbors.insert(bike_to(dest.id, dist));
                    }
                    if self.campus.campus_config.carry_bike_in_car && edge.modes.contains(TransMode::Car) {
                        neighbors.insert(drive_to(dest.id, dist, true));
                    }
                    if in_parking_lot && at_bike_rack {
                        neighbors.insert(walk_to(dest.id, dist));
                    }
                } else {
                    if edge.modes.contains(TransMode::Car) {
                        neighbors.insert(drive_to(dest.id, dist, false));
                    }
                    if in_parking_lot {
                        neighbors.insert(walk_to(dest.id, dist));
                    }
                }
            } else {
                if on_bike {
                    if edge.modes.contains(TransMode::Bike) {
                        neighbors.insert(bike_to(dest.id, dist));
                    }
                    if at_bike_rack {
                        neighbors.insert(walk_to(dest.id, dist));
                    }
                } else {
                    neighbors.insert(walk_to(dest.id, dist));
                }
            }
        }
        neighbors.into_iter().collect()
    }

    fn heuristic_cost(&self, other: &Self) -> TravelCost {
        self.campus.campus_config.get_time(
            TransMode::Walk,
            self.campus.calculate_world_dist(&self.me_id, &other.me_id)
        )
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