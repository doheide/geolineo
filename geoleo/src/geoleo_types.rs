use serde::{Serialize, Deserialize};
//use serde_json::Value as JsonValue;
use std::f64::consts::PI;
use libm::{atan, exp, log, tan, sqrt};
use geojson::{FeatureCollection, JsonObject};

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct LL {
    pub lat: f64,
    pub lng: f64
}
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct Point {
    pub x: f64,
    pub y: f64
}
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct Location {
    pub guid: String,
    pub ll: LL,
    pub hidden: bool,
    pub properties: JsonObject,
}
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct Route {
    pub guid: String,
    pub loc_startend_mrid: [String; 2],
    pub route_points: Vec<LL>,
}
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct UniqueLine {
    pub guid: String,
    pub loc_startend_mrid: [String; 2],
    pub parameter: JsonObject
}
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct Line {
    pub guid: String,
    pub unique_line_mrid: String,
    pub uline_direction_inverse: bool,
    pub loc_startend_mrid: [String; 2],
    pub parameter: JsonObject
}
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct RouteLineMapping {
    pub line_mrid: String,
    pub route_mrid: String,
    pub shift_orth: f32,
    pub order: u16,
    pub invert_direction: bool
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct GeoJsonLayoutParameter {
    pub line_dist: f64,
    pub kink_dist: f64
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct ConnectedLine {
    pub start_id: String,
    pub end_id: String
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct GridData {
    pub locations: Vec<Location>,
    pub routes: Vec<Route>,
    pub unique_lines: Option<Vec<UniqueLine>>,
    pub lines: Option<Vec<Line>>,
    pub mapping: Vec<RouteLineMapping>,
    pub parameter: GeoJsonLayoutParameter,
    pub connected_lines: Vec<ConnectedLine>
}

impl GridData {
    pub fn get_connected_line_with_start_id(self, start_id: String) -> Option<ConnectedLine> {
        self.connected_lines.iter().find_map(|s|
            if s.start_id==start_id { Some(s.clone()) } else { None } )
    }
}

// **********************************************************************
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct LineSegmentInfo {
    pub line_org: [Point;2],
    pub line: [Point;2],
    pub len: f64,
    pub m: Point,
}
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct LineSegment {
    pub ll_line: [LL;2],
    pub shift: f32,
    pub ext_info: Option<LineSegmentInfo>,
}
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct LayoutLine {
    pub start_processed: bool,
    pub end_processed: bool,

    pub guid: String,
    pub line_segments: Vec<LineSegment>,
    pub line_segments_new: Option<Vec<LineSegment>>,
    pub properties: JsonObject
}
pub fn find_layout_line_by_guid(guid: String, lls: Vec<LayoutLine>) -> Option<LayoutLine> {
    lls.into_iter().find_map(|ll| {
        if ll.guid == guid {
            Some(ll.clone())
        } else { None }
    })
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct GeoJsonContent {
    pub locations: FeatureCollection,
    pub hidden_locations: FeatureCollection,
    pub lines: FeatureCollection,
    pub routes: Option<FeatureCollection>,
    pub connection_points: FeatureCollection,
}

// **********************************************************************
impl From<LL> for Point {
    fn from(ll: LL) -> Self {
        Self{ x: ll.lng, y: 180.0 / PI * (2.0 * atan(exp(ll.lat * PI / 180.0)) - PI / 2.0) }
    }
}
impl From<Point> for LL {
    fn from(p: Point) -> Self {
        Self{ lat: 180.0 / PI * log(tan(PI / 4.0 + p.y * (PI / 180.0) / 2.0)), lng: p.x}
    }
}

#[allow(dead_code)]
impl LineSegmentInfo {
    pub fn new_from_ll(ll_line: [LL;2], shift: f32, shift_amount: f64) -> Self {
        Self::new([ll_line[0].clone().into(), ll_line[1].clone().into()], shift, shift_amount)
    }
    pub fn new(line: [Point;2], shift: f32, shift_amount: f64) -> Self {
        let (dx, dy) = (line[1].x - line[0].x, line[1].y - line[0].y);
        let len = sqrt(dx*dx + dy*dy);
        let m = Point{x:dx/len, y:dy/len};
        let line_s:[Point;2] = line.iter().map(|v| Point{
            x: v.x - m.y * shift as f64 * shift_amount,
            y: v.y + m.x * shift as f64 * shift_amount
        }).collect::<Vec<Point>>().try_into().unwrap();

        LineSegmentInfo{line_org: line, line: line_s, len, m }
    }
    pub fn add_to_line_point(self, add: Point, idx: usize) -> Self {
        assert!((idx==0 || idx==1));

        let mut l = self.line.clone();
        l[idx.clone()].x += add.x; l[idx].y += add.y;
        LineSegmentInfo{ line: l, ..self }
    }
    pub fn scale_line_segment_inplace_from_org(&mut self, factor: f64) {
        let mut l = self.line.clone();
        if factor >= 0. {
            l[1].x = l[0].x + self.m.x * self.len * factor;
            l[1].y = l[0].y + self.m.y * self.len * factor;
        } else {
            l[0].x = l[1].x + self.m.x * self.len * factor;
            l[0].y = l[1].y + self.m.y * self.len * factor;
        }
        self.line = l;
    }
    pub fn calc_intersection(self, ll2: LineSegmentInfo) -> Option<(f64, f64)> {
        // calculate intersaection based on determinant
        // see https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection
        let norm = self.m.x * ll2.m.y - self.m.y * ll2.m.x;
        if norm.abs() < 1e-12 { return None; }
        let alpha = (-(self.line[0].x - ll2.line[0].x) * ll2.m.y +
            (self.line[0].y - ll2.line[0].y) * ll2.m.x) / norm / self.len;
        let beta = (-(self.line[0].x - ll2.line[0].x) * self.m.y +
            (self.line[0].y - ll2.line[0].y) * self.m.x) / norm / ll2.len;
        Some((alpha, beta))
    }
    pub fn lines_direction_comparison(self, ll2: LineSegmentInfo) -> f64 {
        self.m.x * ll2.m.x + self.m.y * ll2.m.y
    }
    pub fn calculate_circle_intersection(self, cc: Point, r: f64) -> Option<(f64, f64)> {
        let qtc = Point{x: self.line[0].x - cc.x, y: self.line[0].y-cc.y};
        let a = self.m.x*self.m.x + self.m.y*self.m.y;
        let b = 2.0*self.m.x*qtc.x + 2.0*self.m.y*qtc.y;
        let c = qtc.x*qtc.x + qtc.y*qtc.y - r*r;
        let under_sqrt = b*b-4.0*a*c;
        let under_sqrt = if 0. > under_sqrt && under_sqrt > -1e8 { 0. } else { under_sqrt };
        if under_sqrt < 0.0 { return None }
        let sqrt_val = under_sqrt.sqrt();

        if a == 0. { return None }
        let inters = (((-b - sqrt_val) / (2.*a)), ((-b + sqrt_val) / (2.*a)));
        if a < 0. {
            return Some((inters.1/self.len, inters.0/self.len))
        }
        Some((inters.0/self.len, inters.1/self.len))
    }
}

#[allow(dead_code)]
impl LineSegment {
    pub fn prepare_calculations(self, shift_amount: f64) -> Self {
        let ext_info = Some(LineSegmentInfo::new_from_ll(self.ll_line.clone(), self.shift, shift_amount));
        Self { ext_info, ..self }
    }
    pub fn calc_intersection(self, ls: LineSegment) -> Option<(f64, f64)> {
        let Some(self_lsi) = self.ext_info else { return None };
        let Some(lsi) = ls.ext_info else { return None };
        self_lsi.calc_intersection(lsi)
    }
    pub fn lines_direction_comparison(self, ls: LineSegment) -> Option<f64> {
        let Some(self_lsi) = self.ext_info else { return None };
        let Some(lsi) = ls.ext_info else { return None };
        Some(self_lsi.lines_direction_comparison(lsi))
    }
    pub fn calculate_circle_intersection(self, cc: Point, r: f64) -> Option<(f64, f64)> {
        let Some(self_lsi) = self.ext_info else { return None };
        self_lsi.calculate_circle_intersection(cc, r)
    }
}

#[allow(dead_code)]
impl LayoutLine {
    pub fn prepare_calculations(self, shift_amount: f64) -> Self {
        let lss = self.line_segments.into_iter()
            .map(|ls| ls.prepare_calculations(shift_amount)).collect();
        Self{ line_segments: lss, ..self }
    }
}


pub fn windows_mut_each<T>(v: &mut [T], n: usize, mut f: impl FnMut(&mut [T])) {
    let mut start = 0;
    let mut end = n;
    while end <= v.len()  {
        f(&mut v[start..end]);
        start += 1;
        end += 1;
    }
}


#[cfg(test)]
mod tests {
    // use itertools::Itertools;
    use super::*;

    fn round_to_n_digets(v: f64, digets: i32) -> f64 {
        let f = (10. as f64).powi(digets);
        (v*f).round()/f
    }

    #[test]
    fn conversion_test() {
        let ll = LL { lat: 1., lng: 1.0 };
        let p: Point = ll.clone().into();
        let ll2: LL = p.clone().into();
        // round after the 8-th digits assuming that below are only numerical errors
        let ll2 = LL { lat: (ll2.lat * 10000000.).round() / 10000000., lng: ll2.lng };

        println!("ll: {:?} -> p: {:?} -> ll: {:?}", &ll, &p, &ll2);
        assert_eq!(ll, ll2);
    }

    #[test]
    fn intersection_test1() {
        let l1 = LineSegmentInfo::new(
            [Point { x: 0.0, y: 0.0 }, Point { x: 1.0, y: 0.0 }], 0.0, 0.0);
        let l2 = LineSegmentInfo::new(
            [Point { x: 0.8, y: -1.0 }, Point { x: 0.8, y: 1.0 }], 0.0, 0.0);
        let (alpha, beta) = l1.calc_intersection(l2).unwrap();

        assert_eq!(alpha, 0.8, "Alpha");
        assert_eq!(beta, 0.5, "Beta");
    }
    #[test]
    fn intersection_test2() {
        let l1 = LineSegmentInfo::new(
            [Point { x: -1.0, y: -1.0 }, Point { x: 1.0, y: 1.0 }], 0.0, 0.0);
        let l2 = LineSegmentInfo::new(
            [Point { x: -1.0, y: 1.0 }, Point { x: 1.0, y: -1.0 }], 0.0, 0.0);

        let (alpha, beta) = l1.calc_intersection(l2).unwrap();

        assert_eq!(alpha, 0.5);
        assert_eq!(beta, 0.5);
    }
    #[test]
    fn intersection_test3() {
        let l1 = LineSegmentInfo::new(
            [Point { x: -1.0, y: 0.0 }, Point { x: 1.0, y: 0.0 }], 0.0, 0.0);
        let l2 = LineSegmentInfo::new(
            [Point { x: -1.0, y: 1.0 }, Point { x: 1.0, y: 1.0 }], 0.0, 0.0);
        let alpha = l1.calc_intersection(l2);

        assert_eq!(alpha, None);
    }

    #[test]
    fn adjust_lines_to_intersection() {
        let p1s = Point { x: 0.0, y: 0.0 };
        let p2e = Point { x: 1.0, y: 2.0 };
        let mut l1 = LineSegmentInfo::new(
            [p1s.clone(), Point { x: 2.0, y: 0.0 }], 0.0, 0.0);
        let mut l2 = LineSegmentInfo::new(
            [Point { x: 1., y: -1.0 }, p2e.clone()], 0.0, 0.0);

        println!("l1: {:?}", l1);
        let (alpha, beta) = l1.clone().calc_intersection(l2.clone()).unwrap();
        println!("alpha: {:?}, beta: {:?}", alpha, beta);

        l1.scale_line_segment_inplace_from_org(alpha);
        println!("l1: {:?}", l1.line);
        l2. scale_line_segment_inplace_from_org(-(1.0-beta));
        println!("l2: {:?}", l2.line);

        assert_eq!(l1.line[0], p1s, "L1 start");
        assert_eq!(l1.line[1], l2.line[0], "L1 / l2 intersection");
        assert_eq!(l2.line[1], p2e, "L2 end");
    }

    #[test]
    fn lines_direction_comparison_test_parallel() {
        let l1 = LineSegmentInfo::new(
            [Point{x: 0., y: 0.0}, Point { x: 2.0, y: 0.0 }], 0.0, 0.0);
        let l2 = LineSegmentInfo::new(
            [Point { x: 1., y: 1.0 }, Point{x:1.5, y: 1.}], 0.0, 0.0);

        let v = l1.lines_direction_comparison(l2);
        assert_eq!(v, 1.)
    }
    #[test]
    fn lines_direction_comparison_test_antiparallel() {
        let l1 = LineSegmentInfo::new(
            [Point{x: 0., y: 0.0}, Point { x: 2.0, y: 0.0 }], 0.0, 0.0);
        let l2 = LineSegmentInfo::new(
            [Point { x: 1., y: 1.0 }, Point{x: 0., y: 1.}], 0.0, 0.0);

        let v = l1.lines_direction_comparison(l2);
        assert_eq!(v, -1.)
    }
    #[test]
    fn lines_direction_comparison_test_ortho() {
        let l1 = LineSegmentInfo::new(
            [Point{x: -1.0, y: -1.0}, Point{x: 1.0, y: 1.0 }], 0.0, 0.0);
        let l2 = LineSegmentInfo::new(
            [Point{x: -1.0, y: 1.0 }, Point{x: 1.0, y: -1.0}], 0.0, 0.0);

        let v = l1.lines_direction_comparison(l2);
        assert_eq!(v, 0.0)
    }

    #[test]
    fn lines_circle_intersections_test_simple1() {
        let lsi = LineSegmentInfo::new(
            [Point{x: 0.0, y: 0.0}, Point{x: 1.0, y: 0.0 }], 0.0, 0.0);
        let cc = Point{x: 0.0, y: 0.0};

        let Some(ci) = lsi.calculate_circle_intersection(cc, 1.)
            else { panic!("Unexpected none for circle intersections") };
        assert_eq!(ci, (-1.0, 1.0));
    }
    #[test]
    fn lines_circle_intersections_test_simple2() {
        let lsi = LineSegmentInfo::new(
            [Point{x: 0.0, y: 0.0}, Point{x: 0.0, y: 1.0 }], 0.0, 0.0);
        let cc = Point{x: 0.0, y: 0.0};

        let Some(ci) = lsi.calculate_circle_intersection(cc, 0.5)
            else { panic!("Unexpected none for circle intersections") };
        assert_eq!(ci, (-0.5, 0.5));
    }
    #[test]
    fn lines_circle_intersections_test_simple3() {
        let lsi = LineSegmentInfo::new(
            [Point{x: 0.0, y: 0.0}, Point{x: 0.0, y: 1.0 }], 0.0, 0.0);
        let cc = Point{x: 0.0, y: 1.0};

        let Some(ci) = lsi.calculate_circle_intersection(cc, 0.5)
            else { panic!("Unexpected none for circle intersections") };
        assert_eq!(ci, (0.5, 1.5));
    }
    #[test]
    fn lines_circle_intersections_test_dia() {
        let lsi = LineSegmentInfo::new(
            [Point{x: 0.0, y: 0.0}, Point{x: 0.1, y: 0.1 }], 0.0, 0.0);
        let cc = Point{x: 0.0, y: 0.0};

        let Some(ci) = lsi.calculate_circle_intersection(cc, 1.0)
            else { panic!("Unexpected none for circle intersections") };
        let ci = (round_to_n_digets(ci.0,6), round_to_n_digets(ci.1,6));
        let expected_val = round_to_n_digets((200. as f64).sqrt()/2., 6);
        assert_eq!(ci, (-expected_val, expected_val));
    }
    #[test]
    fn lines_circle_intersections_test_factor() {
        let lsi = LineSegmentInfo::new(
            [Point{x: 0.0, y: 0.0}, Point{x: 0.5, y: 0.0 }], 0.0, 0.0);
        let cc = Point{x: 4.0, y: 0.0};

        let Some(ci) = lsi.calculate_circle_intersection(cc, 2.0)
            else { panic!("Unexpected none for circle intersections") };
        let ci = ((ci.0*10000.).round()/10000., (ci.1*10000.).round()/10000.);
        assert_eq!(ci, (4.0, 12.0));
    }
}



