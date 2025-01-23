use std::borrow::Borrow;
use std::fmt::{Display, Formatter};
use std::fs::DirEntry;
use crate::lib::campus_data::{Campus, CampusEdge, TransMode, TravellerState};

enum EdgeModeGuess {
    /// Walking (no bike)
    Walking,
    /// Driving (with bike?)
    Driving(bool),
    /// WalkBiking
    WalkBiking, // this is for when you don't know if you're walking with or riding your bike
}

enum EdgeMode {
    Walking(bool),
    Driving(bool),
    Biking,
}

fn infer_mode(start_state: &TravellerState, end_state: &TravellerState) -> EdgeModeGuess {
    let me_start = &start_state.me_id;
    let me_end = &end_state.me_id;
    let moved_bike = me_start == &start_state.bike_id && me_end == &end_state.bike_id;
    let moved_car = me_start == &start_state.car_id && me_end == &end_state.car_id;
    if moved_car {
        if moved_bike {
            EdgeModeGuess::Driving(true)
        } else {
            EdgeModeGuess::Driving(false)
        }
    } else {
        if moved_bike {
            EdgeModeGuess::WalkBiking
        } else {
            EdgeModeGuess::Walking
        }
    }
}

fn get_actual_mode<C: Borrow<Campus>>(
    campus: C,
    start_state: &TravellerState,
    end_state: &TravellerState,
) -> EdgeMode {
    let campus = campus.borrow();
    match infer_mode(start_state, end_state) {
        EdgeModeGuess::Walking => EdgeMode::Walking(false),
        EdgeModeGuess::Driving(bike) => EdgeMode::Driving(bike),
        EdgeModeGuess::WalkBiking => {
            let edge = campus
                .find_edge(&start_state.me_id, &end_state.me_id)
                .unwrap();
            if edge.modes.contains(TransMode::Bike) {
                EdgeMode::Biking
            } else {
                EdgeMode::Walking(true)
            }
        }
    }
}

fn get_path_traversal_modes<'a>(
    campus: &'a Campus,
    points: &Vec<TravellerState>,
) -> Vec<(&'a CampusEdge, EdgeMode)> {
    points
        .iter()
        .zip(points.iter().skip(1))
        .map(|(start, end)| {
            (
                campus.find_edge(&start.me_id, &end.me_id).unwrap(),
                get_actual_mode(campus, start, end),
            )
        })
        .collect()
}

pub fn get_path_traversal_description<C: Borrow<Campus>>(
    campus: C,
    points: &Vec<TravellerState>,
) -> String {
    let modes = get_path_traversal_modes(campus.borrow(), points);
    modes
        .iter()
        .map(|(edge, mode)| {
            if let Some(name) = edge.get_name() {
                format!("{}: {}", name, mode)
            } else {
                format!("{} -> {}: {}", edge.start, edge.end, mode)
            }
        })
        .collect::<Vec<_>>()
        .join("\n")
}

impl Display for EdgeMode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            EdgeMode::Walking(bike) => {
                if *bike {
                    write!(f, "🚶🚲")
                } else {
                    write!(f, "🚶")
                }
            }
            EdgeMode::Driving(bike) => {
                if *bike {
                    write!(f, "🚗🚲")
                } else {
                    write!(f, "🚗")
                }
            }
            EdgeMode::Biking => {
                write!(f, "🚲")
            }
        }
    }
}
