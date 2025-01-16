use crate::campus_data::{read_osm_data, BoundingBox, Campus, Location, TransMode};
use crate::map_optimization::connected_components;
use crate::Error;
use full_palette::GREY;
use osm_xml::Coordinate;
use plotters::coord::Shift;
use plotters::prelude::*;
use plotters::style::full_palette::{BROWN, PURPLE};
use rand::seq::SliceRandom;
use std::borrow::Borrow;
use std::collections::HashSet;
use std::rc::Rc;

struct PlotOptions {
    pub width: u32,
    pub draw_parking: bool,
    pub draw_bike_racks: bool,
    pub draw_nodes: bool,
}

impl Default for PlotOptions {
    fn default() -> Self {
        PlotOptions {
            width: 1920,
            draw_parking: true,
            draw_bike_racks: true,
            draw_nodes: true,
        }
    }
}

fn aspect_ratio(bb: BoundingBox) -> f32 {
    let ((min_x, max_x), (min_y, max_y)) = bb;
    let width = max_x - min_x;
    let height = max_y - min_y;
    height as f32 / width as f32
}

fn map_to_screen(bb: BoundingBox, screen_dims: (u32, u32)) -> Box<dyn Fn(&Location) -> (i32, i32)> {
    let (width, height) = screen_dims.clone();
    let ((min_lat, max_lat), (min_long, max_long)) = bb;
    let map_long = move |x| ((x - min_long) / (max_long - min_long) * width as f64) as i32;
    let map_lat =
        move |y| height as i32 - ((y - min_lat) / (max_lat - min_lat) * height as f64) as i32;
    let map_loc = move |loc: &Location| (map_long(loc.lng()), map_lat(loc.lat()));
    Box::new(map_loc)
}

pub fn create_campus_drawing<C>(
    campus: C,
    filepath: &'static str,
    plot_options: &PlotOptions,
) -> Result<(DrawingArea<SVGBackend<'static>, Shift>, BoundingBox), Error>
where
    C: Borrow<Campus>,
{
    let campus = campus.borrow();
    let bb = campus.bounding_box();
    let size_ratio = plot_options.width as Coordinate / (bb.1 .1 - bb.1 .0);
    let width: i32 = (size_ratio * (bb.1 .1 - bb.1 .0)) as i32;
    let height: i32 = (size_ratio * (bb.0 .1 - bb.0 .0)) as i32;
    let map_loc = map_to_screen(bb, (width as u32, height as u32));
    println!("Bounding box is {:?}", bb);

    let root = SVGBackend::new(filepath, (width as u32, height as u32)).into_drawing_area();
    root.fill(&WHITE)?;
    let draw_line = |(x1, y1): (&i32, &i32), (x2, y2): (&i32, &i32), style| {
        root.draw(&PathElement::new(vec![(*x1, *y1), (*x2, *y2)], style))
    };
    let mut edge_count = 0;
    for (l1, l2) in campus.misc_features.iter() {
        let (x1, y1) = map_loc(l1);
        let (x2, y2) = map_loc(&l2);
        draw_line((&x1, &y1), (&x2, &y2), &GREY)?;
        edge_count += 1;
    }
    for edge in campus.edges() {
        let (x1, y1) = map_loc(&campus.get_node(&edge.start)?.location);
        let (x2, y2) = map_loc(&campus.get_node(&edge.end)?.location);
        let color = if edge.modes.contains(TransMode::Car) {
            &BLUE
        } else if edge.modes.contains(TransMode::Bike) {
            &GREEN
        } else {
            &BLACK
        };
        draw_line((&x1, &y1), (&x2, &y2), color)?;
        edge_count += 1;
    }
    for point in campus.nodes() {
        let (x, y) = map_loc(&point.location);
        let (radius, style, do_draw) = if point.has_bike_rack() {
            (3, &GREEN, plot_options.draw_bike_racks)
        } else if point.has_parking() {
            (3, &YELLOW, plot_options.draw_parking)
        } else {
            (1, &BROWN, plot_options.draw_nodes)
        };
        if do_draw {
            root.draw(&Circle::new((x, y), radius, style))?;
        }
    }
    Ok((root, bb))
}

fn generate_colorset(n: u32) -> Vec<HSLColor> {
    let mut colors = Vec::new();
    for i in 0..n {
        let hue = i as f64 / n as f64;
        let color = HSLColor(hue, 1.0, 0.5);
        colors.push(color);
    }
    // randomize order
    colors.shuffle(&mut rand::thread_rng());
    colors
}

pub fn draw_campus_components<C>(
    campus: C,
    filepath: &'static str,
) -> Result<(DrawingArea<SVGBackend<'static>, Shift>, BoundingBox), Error>
where
    C: Borrow<Campus>,
{
    let campus = campus.borrow();
    let components = connected_components(campus);
    let plot_options = PlotOptions {
        draw_nodes: false,
        draw_bike_racks: false,
        draw_parking: false,
        width: 1920,
    };
    let (plot, bb) = create_campus_drawing(campus, filepath, &plot_options)?;
    let coord_map = map_to_screen(bb, plot.dim_in_pixel());

    let group_num = components.values().collect::<HashSet<_>>().len();
    println!("Found {} groups", group_num);
    let colors = generate_colorset(group_num as u32);
    for (node, group) in components {
        let color = colors[group as usize - 1];
        let (x, y) = coord_map(&campus.get_node(&node)?.location);
        plot.draw(&Circle::new((x, y), 2, &color))?;
    }
    plot.present()?;
    Ok((plot, bb))
}

impl<DB: std::error::Error + Send + Sync> From<DrawingAreaErrorKind<DB>> for Error {
    fn from(e: DrawingAreaErrorKind<DB>) -> Self {
        Error::DebugDrawingError(e.to_string())
    }
}

// tests
#[cfg(test)]
mod tests {
    use super::*;
    use crate::campus_data::{read_osm_data, TravellerState};
    use crate::campus_directions::get_path_traversal_description;
    use crate::{find_path, SearchEnd};
    use std::rc::Rc;

    #[test]
    fn test_components() {
        let campus = read_osm_data("./data/rit-bigger.osm", Default::default()).unwrap();
        draw_campus_components(&campus, "debug/components-before.png").unwrap();
        let mut m_campus = campus;
        m_campus.make_strongly_connected(None);
        let campus = m_campus;
        draw_campus_components(&campus, "debug/components-after.png").unwrap();
    }

    #[test]
    fn test_main() {
        let timer = std::time::Instant::now();

        // lat: [43.05371, 43.09635]
        // long: [-77.70424, -77.64793]
        print!("Loading campus... ");
        let mut campus = read_osm_data("data/rit-bigger.osm", Default::default()).unwrap();
        campus.crop_latitudes(43.05371, 43.09635);
        campus.crop_longitudes(-77.70424, -77.64793);
        campus.make_strongly_connected(None);
        let campus = Rc::new(campus);
        println!("{:?}", timer.elapsed());
        let timer = std::time::Instant::now();

        print!("Drawing campus...");
        let (plot, bb) = create_campus_drawing(
            Rc::clone(&campus),
            "debug/example.svg",
            &PlotOptions {
                width: 5000,
                ..Default::default()
            },
        )
        .unwrap();
        let coord_map = map_to_screen(bb, plot.dim_in_pixel());
        println!("{:?}", timer.elapsed());
        let timer = std::time::Instant::now();

        println!("Finding start and end nodes...");
        // sample start: 43.08436913213423, -77.67268359047404
        // sample end: 43.08235610659231, -77.68296257654112
        // sample parking: 43.08174191943827, -77.6802958319057
        let start_loc = Location(43.06209938987821, -77.69455531278473); // the lodge
        let end_loc = Location(43.08235610659231, -77.68296257654112); // somewhere in the frats
        let (start_node, dist_start) = campus.find_closest_node(&start_loc).unwrap();
        let (end_node, dist_end) = campus.find_closest_node(&end_loc).unwrap();
        let (bike_park_node, dist_bike) = campus
            .find_closest_node_with(&start_loc, |n| n.has_bike_rack())
            .unwrap();
        let (parking_node, dist_park) = campus
            .find_closest_node_with(&start_loc, |n| n.has_parking())
            .unwrap();
        let (end_park_node, dist_end_park) = campus
            .find_closest_node_with(&end_loc, |n| n.has_parking())
            .unwrap();
        plot.draw(&Circle::new(coord_map(&start_node.location), 2, &RED))
            .unwrap();
        plot.draw(&Circle::new(coord_map(&end_node.location), 2, &RED))
            .unwrap();
        plot.draw(&Circle::new(coord_map(&bike_park_node.location), 2, &RED))
            .unwrap();
        plot.draw(&Circle::new(coord_map(&parking_node.location), 2, &RED))
            .unwrap();
        plot.draw(&Circle::new(coord_map(&end_park_node.location), 2, &RED))
            .unwrap();
        let start_id = start_node.id;
        let end_id = end_node.id;
        let bike_id = bike_park_node.id;
        let park_id = parking_node.id;
        let end_park_id = end_park_node.id;
        println!(
            "Found nodes within {} and {} and {}",
            dist_start, dist_end, dist_park
        );
        println!(
            "Car at {}, Bike at {}, start at {}, end at {}",
            park_id, bike_id, start_id, end_id
        );
        println!("{:?}", timer.elapsed());
        let timer = std::time::Instant::now();

        println!("Finding path...");
        let start_state = TravellerState::new(Rc::clone(&campus), start_id, bike_id, park_id);
        let end = start_state.create_goal(end_id);
        let (path, cost) = find_path(&start_state, end).unwrap();
        println!(
            "Path found with {} nodes, and estimated {} seconds in travel time",
            path.len(),
            cost
        );
        println!("{:?}", timer.elapsed());
        let timer = std::time::Instant::now();

        println!("Drawing path and saving image...");
        let points = path
            .iter()
            .map(|node| coord_map(&campus.get_node(&node.me_id).unwrap().location))
            .collect::<Vec<_>>();
        plot.draw(&PathElement::new(points, &RED)).unwrap();
        plot.present().unwrap();
        println!("{:?}", timer.elapsed());
        let timer = std::time::Instant::now();

        println!("{}", get_path_traversal_description(campus, &path));
    }
}
