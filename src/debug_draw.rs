use std::rc::Rc;
use osm_xml::Coordinate;
use plotters::coord::Shift;
use plotters::prelude::*;
use plotters::style::full_palette::BROWN;
use crate::campus_data::{BoundingBox, Campus, Location};
use crate::Error;

fn aspect_ratio(bb: BoundingBox) -> f32 {
    let ((min_x, max_x), (min_y, max_y)) = bb;
    let width = max_x - min_x;
    let height = max_y - min_y;
    height as f32 / width as f32
}

fn map_to_screen(bb: BoundingBox, screen_dims: (u32, u32)) -> Box<dyn Fn(&Location) -> (i32, i32)>
{
    let (width, height) = screen_dims.clone();
    let ((min_lat, max_lat), (min_long, max_long)) = bb;
    let map_long = move |x| ((x - min_long) / (max_long - min_long) * width as f64) as i32;
    let map_lat = move |y| height as i32 - ((y - min_lat) / (max_lat - min_lat) * height as f64) as i32;
    let map_loc = move |loc: &Location| (map_long(loc.lng()), map_lat(loc.lat()));
    Box::new(map_loc)
}

pub fn create_campus_drawing(campus: Rc<Campus>) -> Result<(DrawingArea<BitMapBackend<'static>, Shift>, BoundingBox), Error> {
    let bb = campus.bounding_box();
    let aspect: f32 = aspect_ratio(bb);
    let width: i32 = 1920;
    let height: i32 = (width as f32 * aspect) as i32;
    let map_loc = map_to_screen(bb, (width as u32, height as u32));
    println!("Bounding box is {:?}", bb);

    let root = BitMapBackend::new("debug/1.png", (width as u32, height as u32)).into_drawing_area();
    root.fill(&WHITE)?;
    let draw_line = |(x1, y1): (&i32, &i32), (x2, y2): (&i32, &i32), style|
        root.draw(&PathElement::new(vec![(*x1, *y1), (*x2, *y2)], style));
    let mut edge_count = 0;
    for edge in campus.edges() {
        let (x1, y1) = map_loc(&campus.get_node(&edge.start)?.location);
        let (x2, y2) = map_loc(&campus.get_node(&edge.end)?.location);
        draw_line((&x1, &y1), (&x2, &y2), &BLACK)?;
        edge_count += 1;
    }
    for point in campus.nodes() {
        let (x, y) = map_loc(&point.location);
        let (radius, style) = if point.has_bike_rack() {
            (3, &GREEN)
        } else if point.has_parking() {
            (3, &BLUE)
        } else {
            (1, &BROWN)
        };
        root.draw(&Circle::new((x, y), radius, style))?;
    }
    Ok((root, bb))
}

impl<DB: std::error::Error + Send + Sync> From<DrawingAreaErrorKind<DB>> for Error {
    fn from(e: DrawingAreaErrorKind<DB>) -> Self {
        Error::DebugDrawingError(e.to_string())
    }
}

// tests
#[cfg(test)]
mod tests {
    use std::rc::Rc;
    use crate::campus_data::{read_osm_data, TravellerState};
    use crate::{find_path};
    use super::*;

    #[test]
    fn test_main() {
        // lat: [43.05371, 43.09635]
        // long: [-77.70424, -77.64793]
        println!("Loading campus...");
        let mut campus = read_osm_data("data/rit-bigger.osm", Default::default()).unwrap();
        campus.crop_latitudes(43.05371, 43.09635);
        campus.crop_longitudes(-77.70424, -77.64793);
        let campus = Rc::new(campus);
        println!("Drawing campus...");
        let (plot, bb) = create_campus_drawing(Rc::clone(&campus)).unwrap();
        let coord_map = map_to_screen(bb, plot.dim_in_pixel());

        println!("Finding start and end nodes...");
        // sample start: 43.08436913213423, -77.67268359047404
        // sample end: 43.08235610659231, -77.68296257654112
        // sample parking: 43.08174191943827, -77.6802958319057
        let start_loc = Location(43.06162801695506, -77.69172507479415); // the lodge
        let end_loc = Location(43.08235610659231, -77.68296257654112); // somewhere in the frats
        let (start_node, dist_start) = campus.find_closest_node(&start_loc).unwrap();
        let (end_node, dist_end) = campus.find_closest_node(&end_loc).unwrap();
        let (bike_park_node, dist_bike) = campus.find_closest_node_with(&start_loc, |n| n.has_bike_rack()).unwrap();
        let (parking_node, dist_park) = campus.find_closest_node_with(&start_loc, |n| n.has_parking()).unwrap();
        plot.draw(&Circle::new(coord_map(&start_node.location), 2, &RED)).unwrap();
        plot.draw(&Circle::new(coord_map(&end_node.location), 2, &RED)).unwrap();
        plot.draw(&Circle::new(coord_map(&bike_park_node.location), 2, &RED)).unwrap();
        plot.draw(&Circle::new(coord_map(&parking_node.location), 2, &RED)).unwrap();
        let start_id = start_node.id;
        let end_id = end_node.id;
        let bike_id = bike_park_node.id;
        let park_id = parking_node.id;
        println!("Found nodes within {} and {} and {}", dist_start, dist_end, dist_park);

        println!("Finding path...");
        let start_state = TravellerState::new(Rc::clone(&campus), start_id, bike_id, park_id);
        let end_state = TravellerState::new(Rc::clone(&campus), end_id, bike_id, park_id);
        plot.present().unwrap();
        let path = find_path(&start_state, &end_state).unwrap();
        println!("Path found with {} nodes", path.len());

        println!("Drawing path...");
        let points = path.iter().map(|node| coord_map(&campus.get_node(&node.me_id).unwrap().location)).collect::<Vec<_>>();
        plot.draw(&PathElement::new(points, &RED)).unwrap();
        plot.present().unwrap();
    }
}