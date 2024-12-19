use osm_xml::Coordinate;
use plotters::coord::Shift;
use plotters::prelude::*;
use crate::campus_data::{Campus, Location};
use crate::Error;

fn get_bounding_box<N, X, Y>(xs: X, ys: Y) -> ((N, N), (N, N))
where   N: PartialOrd + Copy + Default,
        X: Iterator<Item=N>,
        Y: Iterator<Item=N> {
    let mut first = true;
    let mut min_x = Default::default();
    let mut max_x = Default::default();
    let mut min_y = Default::default();
    let mut max_y = Default::default();
    for (x,y) in xs.zip(ys) {
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
pub fn create_campus_drawing(campus: &Campus) -> Result<DrawingArea<BitMapBackend, Shift>, Error> {
    let mut plot_edges = vec![];
    for edge in campus.edges() {
        let Location(lat, long) = campus.get_node(&edge.start)?.location;
        let Location(lat2, long2) = campus.get_node(&edge.end)?.location;
        plot_edges.push(((long, lat), (long2, lat2)));
    }
    let campus_bb =
        get_bounding_box(plot_edges.iter().map(|((x1, _), (x2, _))| vec![*x1, *x2]).flatten(),
                                     plot_edges.iter().map(|((_, y1), (_, y2))| vec![*y1, *y2]).flatten());
    let aspect = (campus_bb.1.1 - campus_bb.1.0) / (campus_bb.0.1 - campus_bb.0.0);
    let width: i32 = 1920;
    let height: i32 = (width as Coordinate * aspect) as i32;
    println!("Bounding box is {:?}", campus_bb);
    let map_x = |x| ((x - campus_bb.0.0) / (campus_bb.0.1 - campus_bb.0.0) * width as f64) as i32;
    let map_y = |y| height - ((y - campus_bb.1.0) / (campus_bb.1.1 - campus_bb.1.0) * height as f64) as i32;

    let root = BitMapBackend::new("debug/1.png", (width as u32, height as u32)).into_drawing_area();
    root.fill(&WHITE)?;
    let draw_line = |(x1, y1): (&i32, &i32), (x2, y2): (&i32, &i32), style|
        root.draw(&PathElement::new(vec![(*x1, *y1), (*x2, *y2)], style));
    // draw_line((&0, &0), (&width, &height), &RED)?;
    let mut edge_count = 0;
    for ((x, y), (x2, y2)) in plot_edges {
        let x1 = map_x(x);
        let y1 = map_y(y);
        let x2 = map_x(x2);
        let y2 = map_y(y2);
        draw_line((&x1, &y1), (&x2, &y2), &BLACK)?;
        // println!("Drew edge from ({}, {}) to ({}, {})", x1, y1, x2, y2);
        edge_count += 1;
        // break;
    }
    for point in campus.nodes() {
        let Location(lat, long) = point.location;
        let x = map_x(long);
        let y = map_y(lat);
        let style = if point.has_bike_rack() {
            &GREEN
        } else if point.has_parking() {
            &BLUE
        } else {
            continue;
        };
        root.draw(&Circle::new((x, y), 3, style))?;
    }
    println!("Drew {} edges", edge_count);
    Ok(root)
}

impl<DB: std::error::Error + Send + Sync> From<DrawingAreaErrorKind<DB>> for Error {
    fn from(e: DrawingAreaErrorKind<DB>) -> Self {
        Error::DebugDrawingError(e.to_string())
    }
}

// tests
#[cfg(test)]
mod tests {
    use crate::campus_data::read_osm_data;
    use super::*;

    #[test]
    fn test_main() {
        // lat: [43.05371, 43.09635]
        // long: [-77.70424, -77.64793]
        let mut campus = read_osm_data("data/rit-bigger.osm", Default::default()).unwrap();
        campus.crop_latitudes(43.05371, 43.09635);
        campus.crop_longitudes(-77.70424, -77.64793);
        let plot = create_campus_drawing(&campus).unwrap();
        plot.present().unwrap();
    }
}