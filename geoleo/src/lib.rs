mod geoleo_types;
mod prepared_data;

use std::string::String;
use std::collections::HashMap;
use std::vec::Vec;
use itertools::Itertools;
pub use crate::geoleo_types::{GridData, Line, Route, UniqueLine, RouteLineMapping, Location, LL,
                              LineSegment, LayoutLine, GeoJsonContent, FilterOutReport, windows_mut_each};

use geojson::{FeatureCollection, Feature, Geometry, Value, JsonObject, Position};
use crate::geoleo_types::{find_layout_line_by_guid, LineSegmentInfo, Point};

pub use crate::prepared_data::{*};

pub use tracing::{debug};


// ************************************************************************************
#[allow(dead_code)]
fn compare_startends<T: std::cmp::Eq>(v1: &[T;2], v2: &[T;2]) -> bool {
    (v1[0]==v2[0] && v1[1]==v2[1]) || (v1[1]==v2[0] && v1[0]==v2[1])
}
#[allow(dead_code)]
fn generate_missing_routes(gd: &GridData, h_locations: &HashMap<String, &Location>)
    -> Result<(Vec<Route>, Vec<RouteLineMapping>), String>
{
    let lines_with_route_ids: Vec<_> = gd.mappings.iter().map(|rlm| rlm.line_mrid.clone()).sorted().collect();
    let line_has_route = |line_guid: &String| -> bool {
        lines_with_route_ids.binary_search(line_guid).is_ok()
        //gd.mappings.iter().find(|&rlm|
        //    { rlm.line_mrid.eq(line_guid) }
        //).is_some()
    };
    let route_startend_ids: Vec<_> = gd.routes.iter().map(|r|
        format!("{}__{}", r.loc_startend_mrid[0].clone(), r.loc_startend_mrid[1].clone())).collect();
    let route_exists = |startend: &[String; 2]| -> bool {
        route_startend_ids.binary_search(&format!("{}__{}", startend[0].clone(), startend[1].clone())).is_ok()
        // gd.routes.clone().into_iter().find(|k| compare_startends(&k.loc_startend_mrid, startend)).is_some()
    };
    let reorder_startend = |x: &[String; 2]| -> [String; 2] {
        match &x[0].ge(&x[1]) {
            true => [x[0].clone(), x[1].clone()],
            false => [x[1].clone(), x[0].clone()]
        }
    };

    //
    let lines_without_route_from_lines: Vec<(_, _)> = match gd.lines.as_ref() {
        None => Vec::<(String, [String; 2])>::new(),
        Some(v) => {
            v.iter().filter(|&l|
                { !line_has_route(&l.guid) }
            ).map(|v| (v.guid.clone(), v.loc_startend_mrid.clone())).collect()
        }
    };
    let lines_without_route_from_ulines: Vec<(_, _)> =
        gd.unique_lines.iter()
            .filter(|&l|
                { !line_has_route(&l.guid) })
            .map(|v| (v.guid.clone(), v.loc_startend_mrid.clone())).collect();
    let lines_without_route: Vec<(_, _)> = lines_without_route_from_lines.into_iter()
        .chain(lines_without_route_from_ulines).collect();
    debug!("Found {} lines without route.", lines_without_route.len());

    let new_r_from_lines: Vec<[String;2]> = lines_without_route.iter()
        .map(|(_,v)| v).filter(|v| !route_exists(v))
        .map(|x| reorder_startend(x)).unique().collect();


    fn get_startend_positions(h_locations: &HashMap<String, &Location>, startend: &[String;2]) -> std::result::Result<Vec<LL>, String> {
        let loc_start = h_locations.get(&startend[0]).ok_or(
            format!("Cannot find location '{}' when getting pos", &startend[0]))?;
        let loc_end = h_locations.get(&startend[1]).ok_or(
            format!("Cannot find location '{}' when getting pos", &startend[1]))?;
        Ok(vec![loc_start.ll.clone(), loc_end.ll.clone()])
    }

    // create new routes itself
    let nrs = new_r_from_lines.iter().map(|se| {
        let guid = se.join("_");
        let nr = match get_startend_positions(&h_locations, se) {
            Err(s) => Err(s),
            Ok(llv) => Ok(Route{guid, loc_startend_mrid: se.clone(), route_points: llv})
        };
        nr
    }).collect::<Result<Vec<_>, String>>()?;
    let ors: Vec<_> = gd.routes.iter().map(|r| r.clone()).collect();
    let routes: Vec<_> = nrs.into_iter().chain(ors).collect();
    debug!("Generated routes. New number of routes: {}", routes.len());

    let new_mappings: Vec<_> = lines_without_route.iter().map(|(line_guid, startend)| {
        let ordered_mrids = reorder_startend(startend).join("_");
        RouteLineMapping {
            line_mrid: line_guid.clone(), route_mrid: ordered_mrids.clone(), shift_orth: 0.0, order: 0,
            invert_direction: ordered_mrids.eq(&startend.join("_").clone())
        } }).collect();
    let mappings: Vec<RouteLineMapping> = gd.mappings.clone().into_iter()
        .chain(new_mappings.into_iter()).collect();

    Ok((routes, mappings))
}

fn route_line_mapping_fix_ordering(mapping_in: &Vec<RouteLineMapping>) -> Vec<RouteLineMapping> {
    let mut counter: HashMap<String, f32> = HashMap::new();
    mapping_in.iter().for_each(|rlm| {
        counter.entry(rlm.route_mrid.clone()).and_modify(|v| *v -= 0.5).or_insert(0.0);
    });

    let mut mapping = mapping_in.clone();
    mapping.sort_by(|a, b| {
        let (av, bv) = (&a.shift_orth, &b.shift_orth);
        av.partial_cmp(bv).unwrap()
    });

    for rlm in mapping.iter_mut() {
        let c = counter.get_mut(&rlm.route_mrid).unwrap();
        rlm.shift_orth = c.clone();
        *c += 1.;
    }
    mapping
}

#[allow(dead_code)]
fn generate_line_segments_for_line(h_routes: &HashMap<String, &Route>, mapping: &Vec<RouteLineMapping>,
                                   cur_line_guid: String, parameter: JsonObject) -> Result<LayoutLine, String>
{
    let cur_mappings: Vec<RouteLineMapping> = mapping.clone().into_iter()
        .filter(|x| x.line_mrid.eq(&cur_line_guid))
        .sorted_by(|a, b| Ord::cmp(&a.order, &b.order))
        .collect();

    let rps = cur_mappings.into_iter().map(|rlm| {
        let Some(r) = h_routes.get(&rlm.route_mrid)
            else { return Err(format!("Route '{}' mapped to line '{}' not found", rlm.route_mrid, rlm.line_mrid)); };
        let rp_info: (f32, Vec<LL>) = if rlm.invert_direction {
            (-1.0 * rlm.shift_orth.clone(), r.route_points.clone().into_iter().rev().collect())
        } else { (rlm.shift_orth.clone(), r.route_points.clone()) };
        Ok(rp_info)
    }).collect::<Result<Vec<(f32, Vec<LL>)>, String>>()?;
    let ls: Vec<LineSegment> = rps.into_iter().flat_map(|(shift, rp)|
        rp.windows(2).map(|p| LineSegment{
            ll_line: [p[0].clone(), p[1].clone()], shift, ext_info: None }).collect::<Vec<LineSegment>>()
    ).collect();
    Ok(LayoutLine{ start_processed: false, end_processed: false, guid: cur_line_guid, line_segments: ls.clone(), line_segments_new: None, properties: parameter, })
}

#[allow(dead_code)]
fn generate_layoutlines_ulines(h_routes: &HashMap<String, &Route>, h_ulines: &HashMap<String, &UniqueLine>,
                                 mapping: &Vec<RouteLineMapping>) -> Result<Vec<LayoutLine>, String>
{
    let u = h_ulines.iter().map(|(_, l)| {
        generate_line_segments_for_line(h_routes, mapping, l.guid.clone(), l.parameter.clone())
    }).collect::<Result<Vec<LayoutLine>, String>>()?;
    Ok(u)
}

#[allow(dead_code)]
fn generate_layoutlines_lines(h_routes: &HashMap<String, &Route>, h_lines: &HashMap<String, &Line>,
                              h_ulines: &HashMap<String, &UniqueLine>, mapping: &Vec<RouteLineMapping>)
    -> Result<Vec<LayoutLine>, String>
{
    let u = h_lines.into_iter().map(|(_, l)| {
        let Some(ul) = h_ulines.get(&l.unique_line_mrid)
            else { return Err(format!("Unique line '{}' referenced by line '{}' not found", &l.unique_line_mrid, &l.guid)) };
        generate_line_segments_for_line(h_routes, mapping, ul.guid.clone(), l.parameter.clone())
    }).collect::<Result<Vec<LayoutLine>, String>>()?;
    Ok(u)
}

/// Function to generate hashmaps of the vectors of the data
///
/// # Arguments
///
/// * `gd`:
///
/// returns: (HashMap<String, Location, RandomState>, HashMap<String, Route, RandomState>, Option<HashMap<String, UniqueLine, RandomState>>, Option<HashMap<String, Line, RandomState>>)
///
#[allow(dead_code)]
fn create_hashmaps(gd: &GridData) -> (HashMap<String, &Location>, /*HashMap<String, &Route>,*/
                                      HashMap<String, &UniqueLine>, Option<HashMap<String, &Line>>)
{
    let h_locations: HashMap<_, _> = gd.locations.iter().map(|v| (v.guid.clone(), v)).collect();
    // let h_routes: HashMap<_, _> = gd.routes.iter().map(|v| (v.guid.clone(), v)).collect();
    let h_ulines: HashMap<_, _> = gd.unique_lines.iter().map(|v| (v.guid.clone(), v)).collect();
    let h_lines = match gd.lines.as_ref() {
        None => None,
        Some(v) => {
            let hm: HashMap<_, _> = v.iter()
                .map(|v| (v.guid.clone(), v)).collect();
            Some(hm)
        } };

    (h_locations, /*h_routes,*/ h_ulines, h_lines)
}

fn kink_for_first(ls: LineSegment, kink_dist: f64) -> LineSegment {
    let clse = ls.ext_info.unwrap();
    let add = Point{ x: kink_dist*clse.m.x, y: kink_dist*clse.m.y };
    let clse_new = clse.add_to_line_point(add, 0);
    LineSegment{ ext_info: Some(clse_new), ..ls }
}
fn kink_for_last(ls: LineSegment, kink_dist: f64) -> LineSegment{
    let clse = ls.ext_info.unwrap();
    let add = Point{ x: -kink_dist*clse.m.x, y: -kink_dist*clse.m.y };
    let clse_new = clse.add_to_line_point(add, 1);
    LineSegment{ ext_info: Some(clse_new), ..ls }
}
fn make_start_and_end_kink(ll: LayoutLine, kink_dist: f64) -> LayoutLine {
    let clen = ll.line_segments.len();
    let lss: Vec<_> = ll.line_segments.clone().into_iter().enumerate().map(|(i, ls)| {
        let clse = ls.clone().ext_info.unwrap();
        let kd = kink_dist.min(clse.len/2.);
        let ls = if (i==0) && (!ll.start_processed.clone())
            { kink_for_first(ls, kd) } else { ls };
        let ls = if (i==clen-1) && (!ll.end_processed.clone())
            { kink_for_last(ls, kd) } else { ls };
        ls
    }).collect();
    let lss: Vec<_> = if (!ll.start_processed.clone()) && (ll.line_segments.clone().len()>0) {
        let pll = lss[0].clone();
        let pll_ext = pll.ext_info.clone().unwrap();
        let mut ls_ext = LineSegmentInfo::new(
            [pll_ext.line_org[0].clone(), pll_ext.line[0].clone()], 0.0, 0.0);
        ls_ext.scale_line_segment_inplace_from_org(-0.5);
        let ls_start = LineSegment{ext_info: Some(ls_ext), ..pll.clone()};
        [ls_start].into_iter().chain(lss).collect()
    } else { lss };

    let ll_num_segments = lss.clone().len();
    let lss: Vec<_> = if (!ll.end_processed.clone()) && (ll_num_segments.clone()>0) {
        let ll_num_segments = ll_num_segments - 1;
        let pll = lss[ll_num_segments].clone();
        let pll_ext = pll.ext_info.clone().unwrap();
        let mut ls_ext = LineSegmentInfo::new(
            [pll_ext.line[1].clone(), pll_ext.line_org[1].clone()], 0.0, 0.0);
        ls_ext.scale_line_segment_inplace_from_org(0.5);
        let ls_end = LineSegment{ext_info: Some(ls_ext), ..pll.clone()};
        lss.into_iter().chain([ls_end]).collect()
    } else { lss };
    LayoutLine{ line_segments: lss, ..ll}
}

#[allow(dead_code)]
pub fn generate_layout_lines(gd: &GridData) -> Result<Vec<LayoutLine>, String> {
    // ***
    let (h_locations, /*h_routes_in,*/
        h_ulines, h_lines) = create_hashmaps(&gd);

    // generate missing routes
    let (routes, mappings) = generate_missing_routes(&gd, &h_locations)?;
    let h_routes: HashMap<_, _> = routes.iter().map(|v| (v.guid.clone(), v)).collect();

    // fix ordering so that there is 1 difference in the shift_orth between the lines
    let mappings = route_line_mapping_fix_ordering(&mappings);

    //
    let lls = {
        if let Some(h_lines_val) = h_lines.as_ref()
        { generate_layoutlines_lines(&h_routes,&h_lines_val, &h_ulines, &mappings) }
        else
        { generate_layoutlines_ulines(&h_routes, &h_ulines, &mappings) }
    }?;

    Ok(lls)
}

fn process_line_segment_intersection(w: &mut [LineSegment], line_dist: f64) -> Option<bool> {
    assert_eq!(w.len(), 2);

    let intersection_factors = w[0].clone().calc_intersection(w[1].clone());
    let (w0_org_shifted, w1_org_shifted) = (w[0].clone(), w[1].clone());
    let us_circle_intersections = if let Some((alpha, beta)) = intersection_factors {
        debug!("alpha: {}, beta: {}", &alpha, &beta);
        if let Some(ref mut w0) = w[0].ext_info {
            w0.scale_line_segment_inplace_from_org(alpha);
        }
        if let Some(ref mut w1) = w[1].ext_info {
            w1.scale_line_segment_inplace_from_org(-(1.-beta));
        }

        // check if intersection is too far away from original point
        let dist = {
            let org = w[0].ext_info.clone().unwrap().line_org[1].clone();
            let new = w[0].ext_info.clone().unwrap().line[1].clone();
            ((org.x-new.x).powi(2) + (org.y-new.y).powi(2)).sqrt()
        };
        let (w0_shift, w1_shift) = (w[0].shift.clone() as f64, w[1].shift.clone() as f64);
        let (w0_sign, w1_sign) = (if w0_shift > 0.0 { 1.0 } else { -1.0 }, if w1_shift > 0.0 { 1.0 } else { -1.0 });
        let (dist0_max, dist1_max) = ((w0_shift.clone() + w0_sign).abs() * line_dist, (w1_shift.clone() + w1_sign).abs() * line_dist);
        debug!("ls-intersection: dist to org / max: {} / ({}, {})", &dist, dist0_max, dist1_max);
        let rv = if (dist > dist0_max) || (dist > dist1_max) {
            //return Some(true);
            let (w0, w1) = (w[0].clone(), w[1].clone());
            let (Some(w0_ext), Some(w1_ext)) = (w0.ext_info.clone(), w1.ext_info.clone())
                else { panic!("LineSegment is expected to have a valid") };
            //let is_parallel = if w0.clone().lines_direction_comparison(w1.clone()).unwrap() > 0. { true } else { false };

            // w0
            let cc = w0_ext.line_org[1].clone();
            debug!("cc0: {:?}, w0: {:?}, w0_shift: {}, w0_sign: {}", cc, w0, w0_shift, w0_sign);
            let Some(ci) = w0_org_shifted.clone().calculate_circle_intersection(
                cc, (w0_shift+w0_sign).abs() * line_dist) else { return None };
            debug!("circle_intersection factors 0: {:?}", &ci);
            if let Some(ref mut w0m) = w[0].ext_info {
                if alpha > 1.
                { w0m.scale_line_segment_inplace_from_org(ci.1); }
            }
            // w1
            let cc = w1_ext.line_org[0].clone();
            debug!("cc1: {:?}, w1: {:?}, w1_shift: {}, w1_sign: {}", cc, w1, w1_shift, w1_sign);
            let Some(ci) = w1_org_shifted.clone().calculate_circle_intersection(
                cc, (w1_shift+w1_sign).abs() * line_dist) else { return None };
            debug!("circle_intersection factors 1: {:?}", &ci);
            if let Some(ref mut w1m) = w[1].ext_info {
                if beta < 0.
                { w1m.scale_line_segment_inplace_from_org(-(1.0 - ci.0)); }
            }

            Some(true)
        } else { Some(false) };
        rv
    }
    else { None };

    us_circle_intersections
}

#[allow(dead_code)]
pub fn layout_line_process_line_segments(ll: LayoutLine, line_dist: f64) -> LayoutLine {
    let mut llm = ll.clone();

    debug!("Processing layout-line: {:?}", ll.properties);

    windows_mut_each(&mut llm.line_segments, 2, |w| {
        process_line_segment_intersection(w, line_dist);
    });
    llm
}

#[allow(dead_code)]
pub fn process_layout_line(ll: LayoutLine, line_dist: f64) -> LayoutLine {
    // Add calculation ext info
    let ll = ll.prepare_calculations(line_dist);

    // Calculate intersections etc
    let ll= layout_line_process_line_segments(ll, line_dist);

    ll
}

pub fn process_line_finishing_merge_lines(ll_start: LayoutLine, ll_end: LayoutLine, line_dist: f64)
    -> Option<(LayoutLine, LayoutLine, Point)>
{
    let len_start = ll_start.line_segments.clone().len();
    if len_start == 0 { return None }
    let len_start = len_start-1;
    let ll_start_seg = ll_start.line_segments.clone().get(len_start).unwrap().clone();
    let ll_end_seg = ll_end.line_segments.clone().get(0).unwrap().clone();

    // let (ll_start_segd, ll_end_segd) = (ll_start_seg.clone(), ll_end_seg.clone());
    let mut ll_array = [ll_start_seg, ll_end_seg];
    let Some(use_midpoint) = process_line_segment_intersection(&mut ll_array, line_dist)
        else { panic!("process_line_finishing_merge_lines(): process_line_segment_intersection() returned None"); };

    let ll_start_segments: Vec<_> = ll_start.line_segments.clone().into_iter()
        .enumerate().filter_map(|(i, ls)| if i!=len_start { Some(ls) } else { None }).collect();
    let ll_end_segments: Vec<_> = ll_start.line_segments.clone().into_iter()
        .enumerate().filter_map(|(i, ls)| if i!=0 { Some(ls) } else { None }).collect();

    let p_start_org = ll_array[0].clone().ext_info.unwrap().line_org[1].clone();
    let p_end_org = ll_array[1].clone().ext_info.unwrap().line_org[0].clone();
    let p_start = ll_array[0].clone().ext_info.unwrap().line[1].clone();
    let p_end = ll_array[1].clone().ext_info.unwrap().line[0].clone();
    let midpoint = Point{
        x: (p_start.x.clone() + p_end.x.clone()) / 2.0,
        y: (p_start.y.clone() + p_end.y.clone()) / 2.0
    };

    let (ll_start_segments, ll_end_segments) = if !use_midpoint {
        let ll_start_segments = ll_start_segments.into_iter().chain([ll_array[0].clone()]).collect();
        let ll_end_segments = ([ll_array[1].clone()]).into_iter().chain(ll_end_segments).collect();
        (ll_start_segments, ll_end_segments)
    } else {
        let lsi_start = LineSegmentInfo{
            line_org: [p_start_org.clone(), p_start_org.clone()], line: [p_start.clone(), midpoint.clone()],
            len: 0.0, m: Point { x: 0.0, y: 0.0 },
        };
        let ll_start_segments = ll_start_segments.into_iter()
            .chain([ll_array[0].clone(), LineSegment{ext_info: Some(lsi_start), ..ll_array[0].clone()}]).collect();

        let lsi_end = LineSegmentInfo{
            line_org: [p_end_org.clone(), p_end_org.clone()], line: [midpoint.clone(), p_end.clone()],
            len: 0.0, m: Point { x: 0.0, y: 0.0 },
        };
        let ll_end_segments = ([LineSegment{ext_info: Some(lsi_end), ..ll_array[0].clone()}, ll_array[1].clone()])
            .into_iter().chain(ll_end_segments).collect();
        (ll_start_segments, ll_end_segments)
    };

    Some((
        LayoutLine{
            // line_segments: ll_start.line_segments.clone().into_iter().enumerate()
            //     .map(|(i, ls)| if i==len_start { ll_array[0].clone() } else { ls }).collect(),
            line_segments: ll_start_segments, ..ll_start.clone() },
        LayoutLine{
            // line_segments: ll_end.line_segments.clone().into_iter().enumerate()
            //     .map(|(i, ls)| if i==0 { ll_array[1].clone() } else { ls }).collect(),
            line_segments: ll_end_segments, ..ll_end.clone() },
        midpoint
    ))
}

fn get_element_from_optional_vec<T: Clone>(hm: Option<HashMap<String, T>>, mrid: String) -> Option<T> {
     if let Some(chm) = hm {
        if let Some(obj) = chm.get(mrid.as_str()).clone()
        { return Some(obj.clone()); }
    }
    None
}
pub fn process_line_merge_and_branch(lls: Vec<LayoutLine>, gd: GridData) -> (Vec<LayoutLine>, Vec<Location>) {
    let (h_locations, h_ulines,
        h_lines) = create_hashmaps(&gd);

    let line_dist: f64 = gd.parameter.line_dist.clone();
    let mut connection_dots: Vec<Location> = Vec::new();

    let mut llsm = lls.clone();
    lls.clone().into_iter().for_each(|ll| {
        if let Some(connected_lines) = gd.clone().get_connected_line_with_start_id(ll.guid.clone()) {
            let Some(ll_start) = find_layout_line_by_guid(connected_lines.start_id.clone(), lls.clone())
                else { panic!("Cannot find layout line with guid '{}'", connected_lines.start_id) };
            let Some(ll_end) = find_layout_line_by_guid(connected_lines.end_id.clone(), lls.clone())
                else { panic!("Cannot find layout line with guid '{}'", connected_lines.end_id) };

            let Some((ll_start, ll_end, midpoint)) = process_line_finishing_merge_lines(
                ll_start, ll_end, line_dist) else { panic!("Error when merging lines.") };

            for ls in llsm.iter_mut() {
                if ls.guid == ll_start.guid {
                    ls.line_segments = ll_start.line_segments.clone();
                    ls.end_processed = true;
                } else if ls.guid == ll_end.guid {
                    ls.line_segments = ll_end.line_segments.clone();
                    ls.start_processed = true;
                };
            }

            let loc_data = {
                let line = get_element_from_optional_vec(h_lines.clone(), ll_start.guid.clone());
                let uline = h_ulines.get(&ll_start.guid).cloned();
                let loc_mrid = if let Some(l) = line
                { l.loc_startend_mrid[1].clone() } else if let Some(ul) = uline
                { ul.loc_startend_mrid[1].clone() } else { panic!("process_line_merge_and_branch(): error getting loc_mrid") };
                *h_locations.get(&loc_mrid).unwrap()
            };
            let cl = Location{ll: midpoint.into(), guid: format!("{}_{}", loc_data.guid.clone(), "lala"),
                ..loc_data.clone()
            };
            connection_dots.push(cl);
        }
    });
    (llsm, connection_dots)
}

pub fn process_line_kinks_and_finish(lls: Vec<LayoutLine>, kink_dist: f64) -> Vec<LayoutLine>
{
    // make kinks
    let lls: Vec<_> = lls.into_iter().map(|ll|
        make_start_and_end_kink(ll, kink_dist)).collect();

    // make final lat/lng
    lls.into_iter().map(|lli| {
        let ll = {
            let line_segments_new: Option<Vec<_>> = Some(lli.line_segments.clone().into_iter()
                .map(|ls| {
                    let lsei = ls.ext_info.unwrap();
                    let ll_line = [LL::from(lsei.line[0].clone()), LL::from(lsei.line[1].clone())];
                    LineSegment { ll_line, ext_info: None, ..ls }
                }).collect());
            LayoutLine { line_segments_new, ..lli }
        };
        ll
    }).collect()
}

// ***
pub fn consistency_checks_and_filter(gd: GridData) -> Result<(GridData, FilterOutReport), String> {
    // filter location data
    let filtered_locs: Vec<_> = gd.locations.clone().into_iter().filter(|l|
        l.ll.lat.is_some() && l.ll.lng.is_some()).collect();
    let loc_ids: Vec<_> = filtered_locs.iter().map(|l| l.guid.clone()).sorted().collect();
    let filtered_out_locs: Vec<_> = gd.locations.iter().filter(|&l|
        loc_ids.binary_search(&l.guid).is_err())
        .map(|l| l.guid.clone()).collect();
    let gd = GridData{locations: filtered_locs, ..gd};

    let locations_ids: Vec<String> = gd.locations.iter()
        .map(|l| l.guid.clone()).sorted().collect();

    // let (h_locations, h_routes_in,
    //     h_ulines, h_lines) = create_hashmaps(&gd);

    // *****************************************
    // Filter lines and unique lines
    fn loc_startend_mrid_present(loc_startend_mrid: &[String; 2], locations_ids: &Vec<String>) -> bool {
        locations_ids.binary_search(&loc_startend_mrid[0]).is_ok() &&
            locations_ids.binary_search(&loc_startend_mrid[0]).is_ok()
    }
    // filter lines for which the locations cannot be found
    let (gd, line_ids, filtered_out_lines) = if let Some(lines) = gd.lines.as_ref() {
        let filtered_line_objs: Vec<_> = lines.iter().filter(
            |l| loc_startend_mrid_present(&l.loc_startend_mrid, &locations_ids)).cloned().collect();
        let line_ids: Vec<_> = filtered_line_objs.iter().map(|l| l.guid.clone()).sorted().collect();
        let filtered_out_lines: Vec<_> = lines.iter().filter(| l| line_ids.binary_search(&l.guid).is_err())
            .map(|l| l.guid.clone()).collect();
        (GridData{ lines: Some(filtered_line_objs), ..gd}, line_ids, filtered_out_lines)
    } else { (gd, vec![], vec![]) };
    // filter unique lines for which the locations cannot be found
    let (gd, uline_ids, filtered_out_ulines) = {
        let filtered_uline_objs: Vec<_> = gd.unique_lines.iter().filter(
            | ul| loc_startend_mrid_present(&ul.loc_startend_mrid, &locations_ids)).cloned().collect();
        let uline_ids: Vec<_> = filtered_uline_objs.iter().map(|l| l.guid.clone()).sorted().collect();
        let filtered_out_ulines: Vec<_> = gd.unique_lines.iter().filter(|ul| uline_ids.binary_search(&ul.guid).is_err())
            .map(|ul| ul.guid.clone()).collect();
        (GridData{ unique_lines: filtered_uline_objs, ..gd}, uline_ids, filtered_out_ulines)
    };

    // *****************************************
    // Filter routes
    let (gd, r_ids, filtered_out_routes) = {
        let filtered_routes = gd.routes.iter().filter(
            |r| loc_startend_mrid_present(&r.loc_startend_mrid, &locations_ids)).cloned().collect();
        let r_ids = gd.routes.iter().map(
            |r| r.guid.clone()).sorted().collect::<Vec<String>>();
        let filtered_out_routes: Vec<_> = gd.routes.iter().filter(|r| r_ids.binary_search(&r.guid).is_err())
            .map(|r| r.guid.clone()).collect();
        (GridData{routes: filtered_routes, ..gd}, r_ids, filtered_out_routes)
    };

    // *****************************************
    // Filter mapping
    let (gd, filtered_out_mappings) = {
        let check_route_line = |rlm: &RouteLineMapping| -> bool {
            r_ids.binary_search(&rlm.route_mrid).is_ok() &&
                (line_ids.binary_search(&rlm.line_mrid.clone()).is_ok() || uline_ids.binary_search(&rlm.line_mrid).is_ok())
        };
        let m_str = |route_mrid: &String, line_mrid: &String| -> String { format!("r{}_l{}", route_mrid.clone(), line_mrid.clone()) };

        let filtered_mappings: Vec<_> = gd.mappings.iter().filter(|rlm| { check_route_line(rlm) }).cloned().collect();
        let m_ids: Vec<_> = filtered_mappings.iter().map(|m| m_str(&m.route_mrid, &m.line_mrid))
            .sorted().collect();
        let filtered_out_mappings: Vec<_> = gd.mappings.iter().map(|m| m_str(&m.route_mrid, &m.line_mrid))
            .filter(|s| m_ids.binary_search(&s).is_err()).collect();
        (GridData{ mappings: filtered_mappings, ..gd}, filtered_out_mappings)
    };
    // ********************************************
    // Check lines and route data
    let start_end_index_adjust = |idx: usize, is_inverse: &bool| -> usize {
        if *is_inverse { return (idx + 1) & 1; }
        idx
    };
    if gd.lines.is_some() {
        // let h_lines = h_lines.as_ref().unwrap();
        let h_ulines: HashMap<_, _>  = gd.unique_lines.iter()
            .map(|v| (v.guid.clone(), v)).collect();
        let wrong_lines: Vec<_> = gd.lines.as_ref().unwrap().iter()
            .map(|cline| {
                if let Some(c_uline) = h_ulines.get(&cline.unique_line_mrid) {
                    if cline.loc_startend_mrid[0] !=
                        c_uline.loc_startend_mrid[start_end_index_adjust(0, &cline.uline_direction_inverse)] ||
                        cline.loc_startend_mrid[1] !=
                            c_uline.loc_startend_mrid[start_end_index_adjust(1, &cline.uline_direction_inverse)]
                    { return format!("Start and end locations of unique line '{}' referenced by line '{}' diverge", c_uline.guid, cline.guid) }
                }
                else { return format!("Cannot find unique line referenced in line {}", cline.unique_line_mrid.clone()) }
                String::from("")
            }).filter(|v| v != "").collect();
        if wrong_lines.len() > 0
        { return Err(format!("Inconsistant lines properties found:\n- {}\n", wrong_lines.join("\n- ")))}
    }
    {
        let wrong_routes: Vec<_> = gd.routes.iter().map(|r|
            if r.route_points.len() < 2 { format!("Invalid routepoints for route '{}' (less than 2)", r.guid) }
            else { format!("") })
            .filter(|v| v != "").collect();
        if wrong_routes.len() > 0
        { return Err(format!("Inconsistant route properties found:\n- {}\n", wrong_routes.join("\n- "))) }
    }

    debug!("*Consistency checks passed.\nData Details:\n  - locations: {} (filtered_out: 0)\n  - unique_lines: {} (filtered_out: {})\
            \n  - lines: {} (filtered_out: {})\n  - routes: {} (filtered_out: {})\n  - mapping: {} (filtered_out: {})",
        gd.locations.len(), gd.unique_lines.len(), filtered_out_ulines.len(),
        gd.lines.as_ref().unwrap_or(&vec![]).len(), filtered_out_lines.len(),
        gd.routes.len(), filtered_out_routes.len(), gd.mappings.len(), filtered_out_mappings.len()
    );
    Ok((gd, FilterOutReport { filtered_out_locations: filtered_out_locs, filtered_out_routes,
        filtered_out_lines, filtered_out_ulines, filtered_out_mappings }))
}

///
///
/// # Arguments
///
/// * `gd`:
///
/// returns: Result<GeoJsonContent, String>
///
/// # Examples
///
/// ```
///
/// ```
// ***
pub fn generate_geojson(gd: GridData) -> Result<GeoJsonContent, String> {
    let (gd, filer_report) = consistency_checks_and_filter(gd)?;

    // ************************************************
    let locations_fct = |hidden| -> Vec<Feature> {
        gd.locations.clone().into_iter()
            .filter(|l| l.hidden==hidden)
            .map(|l| {
                let p = vec![l.ll.lng.unwrap(), l.ll.lat.unwrap()];
                let mut prop = l.properties.clone();
                prop.insert("guid".to_string(),l.guid.clone().into());
                Feature { bbox: None,
                    geometry: Some(Geometry::from(Value::Point(p))),
                    properties: Some(prop), id: None, foreign_members: None }
            }).collect()
    };
    let locations: Vec<_> = locations_fct(false);
    let locations_hidden: Vec<_> = locations_fct(true);

    // ************************************************
    let lls = generate_layout_lines(&gd)?;

    let lls: Vec<_> = lls.into_iter()
        .map(|ll| process_layout_line(ll, gd.parameter.line_dist.clone())).collect();
    let (lls, connection_dots) = process_line_merge_and_branch(lls, gd.clone());
    let lls = process_line_kinks_and_finish(lls, gd.parameter.kink_dist.clone());

    // make final lat/lng
    // let ll = {
    //     let line_segments_new: Option<Vec<_>> = Some(lls.line_segments.clone().into_iter()
    //         .map(|ls| {
    //             let lsei = ls.ext_info.unwrap();
    //             let ll_line= [LL::from(lsei.line[0].clone()), LL::from(lsei.line[1].clone())];
    //             LineSegment{ll_line, ext_info: None, ..ls}
    //         }).collect());
    //     LayoutLine{line_segments_new, ..ll}
    // };

    let lines: Vec<_> = lls.into_iter().map(|v| {
        let ls: Vec<Position> = v.line_segments_new.unwrap().into_iter().flat_map(|p| {
            vec![p.ll_line[0].clone(), p.ll_line[1].clone()]
        }).dedup().map(|p| vec![p.lng.unwrap(), p.lat.unwrap()]).collect();
        let mut prop = v.properties.clone();
        prop.insert("guid".to_string(), v.guid.clone().into());
        Feature { bbox: None, geometry: Some(Geometry::from(Value::LineString(ls))),
            properties: Some(prop), id: None, foreign_members: None }
    }).collect();

    let connection_dots_feature: Vec<_> = connection_dots.clone().into_iter().map(|l| {
            let p = vec![l.ll.lng.unwrap(), l.ll.lat.unwrap()];
            let mut prop = l.properties.clone();
            prop.insert("guid".to_string(),l.guid.clone().into());
            Feature { bbox: None,
                geometry: Some(Geometry::from(Value::Point(p))),
                properties: Some(prop), id: None, foreign_members: None }
        }).collect();

    // ************************************************
    Ok(GeoJsonContent {
        locations: FeatureCollection::from_iter(locations),
        hidden_locations: FeatureCollection::from_iter(locations_hidden),
        lines: FeatureCollection::from_iter(lines),
        // lines: FeatureCollection{ bbox: None, features: vec![], foreign_members: None, },
        routes: None,
        connection_points: FeatureCollection::from_iter(connection_dots_feature),
        filter_report: filer_report,
    })
}


// ********************************************************************************+
// ********************************************************************************+
#[cfg(test)]
mod tests {
    use geojson::JsonValue;
    use crate::geoleo_types::GeoJsonLayoutParameter;
    use super::*;
    use prepared_data;

    fn make_name_props(name: String, region: Option<String>) -> JsonObject {
        let mut properties = JsonObject::new();
        properties.insert("name".to_string(), JsonValue::from(name));
        if region.is_some() {
            properties.insert("region".to_string(), JsonValue::from(region.unwrap()));
        }
        properties
    }

    fn setup_base_data_simple() -> GridData {
        let data = GridData {
            locations: vec![
                Location {guid: format!("guid_loc_A"), ll: LL { lat: Some(0.0), lng: Some(0.0) }, hidden: false,
                    properties: make_name_props("A".into(), Some("regA".into())) },
                Location {guid: format!("guid_loc_B"), ll: LL { lat: Option::from(0.5), lng: Option::from(1.0) }, hidden: false,
                    properties: make_name_props("B".into(), Some("regA".into())) },
            ],
            routes: vec![],
            unique_lines: vec![
                UniqueLine {guid: format!("guid_ul_1"), loc_startend_mrid: [format!("guid_loc_A"), format!("guid_loc_B")],
                    parameter: make_name_props("UL1".into(), None) }],
            lines: None,
            mappings: vec![],
            parameter: GeoJsonLayoutParameter { line_dist: 1., kink_dist: 10. },
            connected_lines: vec![],
        };
        data
    }
    fn setup_base_data_ul() -> GridData {
        let data = GridData {
            locations: vec![
                Location {
                    guid: format!("guid_loc_A"),
                    ll: LL { lat: Option::from(0.0), lng: Option::from(0.0) },
                    hidden: false,
                    properties: make_name_props("A".into(), Some("reg_one".into()))
                }, //json!({"name": "A", "region": "regA"})},
                Location {
                    guid: format!("guid_loc_filtered"),
                    ll: LL { lat: None, lng: None },
                    hidden: false,
                    properties: make_name_props("F".into(), Some("reg_one".into()))
                }, //json!({"name": "A", "region": "regA"})},
                Location {
                    guid: format!("guid_loc_B"),
                    ll: LL { lat: Option::from(0.5), lng: Option::from(1.0) },
                    hidden: false,
                    properties: make_name_props("B".into(), Some("reg_one".into()))
                }, //json!({"name": "B", "region": "regA"})},
                Location {
                    guid: format!("guid_loc_C"),
                    ll: LL { lat: Option::from(1.0), lng: Option::from(1.5) },
                    hidden: false,
                    properties: make_name_props("C".into(), Some("reg_one".into()))
                }, //json!({"name": "C", "region": "regA"})},
                Location {
                    guid: format!("guid_loc_D"),
                    ll: LL { lat: Option::from(2.0), lng: Option::from(2.0) },
                    hidden: false,
                    properties: make_name_props("D".into(), Some("reg_two".into()))
                }, //json!({"name": "D", "region": "regB"})}
            ],
            routes: vec![
                Route {
                    guid: format!("guid_r_1"),
                    loc_startend_mrid: [format!("guid_loc_B"), format!("guid_loc_A")],
                    route_points: vec![LL { lat: Option::from(0.5), lng: Option::from(1.0) },
                                       LL { lat: Option::from(1.0), lng: Option::from(0.5) },
                                       LL { lat: Option::from(0.0), lng: Option::from(0.0) }]
                },
                Route {
                    guid: format!("guid_r_2"),
                    loc_startend_mrid: [format!("guid_loc_B"), format!("guid_loc_C")],
                    route_points: vec![LL { lat: Option::from(0.5), lng: Option::from(1.0) },
                                       LL { lat: Option::from(1.0), lng: Option::from(1.5) }]
                },
            ],
            unique_lines: vec![
                UniqueLine {
                    guid: format!("guid_ul_1"),
                    loc_startend_mrid: [format!("guid_loc_A"), format!("guid_loc_C")],
                    parameter: make_name_props("UL1".into(), None)
                }, // json!({"name": "UL1"})},
                UniqueLine {
                    guid: format!("guid_ul_2"),
                    loc_startend_mrid: [format!("guid_loc_D"), format!("guid_loc_C")],
                    parameter: make_name_props("UL2".into(), None)
                }, // json!({"name": "UL2"})},
                UniqueLine {
                    guid: format!("guid_ul_3"),
                    loc_startend_mrid: [format!("guid_loc_C"), format!("guid_loc_D")],
                    parameter: make_name_props("UL3".into(), None)
                }, // json!({"name": "UL3"})},
            ],
            lines: None,
            mappings: vec![
                RouteLineMapping {
                    line_mrid: format!("guid_ul_1"),
                    route_mrid: format!("guid_r_2"),
                    shift_orth: 0.0,
                    order: 1,
                    invert_direction: false
                },
                RouteLineMapping {
                    line_mrid: format!("guid_ul_1"),
                    route_mrid: format!("guid_r_1"),
                    shift_orth: 1.0,
                    order: 0,
                    invert_direction: true
                },
            ],
            parameter: GeoJsonLayoutParameter { line_dist: 0.05, kink_dist: 0.05 },
            connected_lines: vec![],
        };
        data
    }

    #[test]
    fn generate_missing_routes_and_fix_test() {
        let gd = setup_base_data_ul();

        let (h_locations, _h_ulines, _h_lines) = create_hashmaps(&gd);
        let _h_routes: HashMap<_, _> = gd.routes.iter().map(|v| (v.guid.clone(), v)).collect();

        let rr = generate_missing_routes(&gd, &h_locations);
        assert!(!rr.is_err(), "Error generate_missing_routes: {:?}", rr);

        let (routes, mappings) = rr.unwrap();
        let h_routes: HashMap<_, _> = routes.iter().map(|v| (v.guid.clone(), v)).collect();

        let mapping = route_line_mapping_fix_ordering(&mappings);

        assert_eq!(mapping.len(), 4);
        assert_eq!(mapping[1].line_mrid, format!("guid_ul_2"));
        assert_eq!(mapping[1].route_mrid, format!("guid_loc_D_guid_loc_C"));
        assert_eq!(mapping[1].shift_orth, -0.5);
        assert_eq!(mapping[1].order, 0);
        assert_eq!(mapping[1].invert_direction, true);
        assert_eq!(mapping[2].line_mrid, format!("guid_ul_3"));
        assert_eq!(mapping[2].route_mrid, format!("guid_loc_D_guid_loc_C"));
        assert_eq!(mapping[2].shift_orth, 0.5);
        assert_eq!(mapping[2].order, 0);
        assert_eq!(mapping[2].invert_direction, false);

        let t = h_routes.get(&mapping[2].route_mrid);
        assert_eq!(t.is_some(), true);
        let tt = t.unwrap();
        assert_eq!(tt.route_points.len(), 2);
        assert_eq!(tt.route_points[0].lat.unwrap(), 2.);
        assert_eq!(tt.route_points[0].lng.unwrap(), 2.);
        assert_eq!(tt.route_points[1].lat.unwrap(), 1.);
        assert_eq!(tt.route_points[1].lng.unwrap(), 1.5);
    }

    #[test]
    fn generate_layout_lines_test() {
        let gd = setup_base_data_ul();

        // let (h_locations, h_routes_in,
        //     h_ulines, h_lines) = create_hashmaps(&gd);

        let lls = generate_layout_lines(&gd);
        assert!(lls.is_ok(), "Got error: {:?}", lls);
        let lls = lls.unwrap();

        assert_eq!(lls.len(), 3);
    }

    #[test]
    fn geojson_test() {
        let gd = setup_base_data_simple();
        let gj = generate_geojson(gd).unwrap();
        let gj_str = serde_json::to_string(&gj).unwrap();
        println!("geojson: {}", gj_str);

        assert_eq!(1, 1);
    }

    #[test]
    fn testcase_load_test() {
        let pd_gd = prepared_data::load_testcase_by_id("tc_0000".to_string(),
                                                 "./tests/test_cases".to_string()).unwrap();
        let gd = setup_base_data_ul();
        assert_eq!(pd_gd, gd);
    }
}




/*
    let l_err: Vec<_> = if let Some(clines) = gd.lines.clone() {
        clines.clone().into_iter().flat_map(|l| {
            let mut r = Vec::new();
            if h_locations.get(l.loc_startend_mrid[0].as_str()).is_none()
            { r.push(format!("Start location '{}' of line '{}' not found", &l.loc_startend_mrid[0], &l.guid)); }
            if h_locations.get(l.loc_startend_mrid[1].as_str()).is_none()
            { r.push(format!("End location '{}' of line '{}' not found", &l.loc_startend_mrid[0], &l.guid)); }
            if let Some(ch_ulines) = h_ulines.clone() {
                if ch_ulines.get(l.unique_line_mrid.as_str()).is_none() {
                    r.push(format!("Unique line guid '{}' referenced by line '{}' does not exist",
                                   &l.unique_line_mrid, &l.guid));
            } }
            r
        }).collect()
    } else { Vec::new() };

    let ul_err: Vec<_> = if let Some(culines) = gd.unique_lines.clone() {
        culines.iter().flat_map(|ul| {
            let mut r = Vec::new();
            if h_locations.get(ul.loc_startend_mrid[0].as_str()).is_none()
            { r.push(format!("Start location '{}' of unique line '{}' not found", &ul.loc_startend_mrid[0], &ul.guid)); }
            if h_locations.get(ul.loc_startend_mrid[1].as_str()).is_none()
            { r.push(format!("End location '{}' of unique line '{}' not found", &ul.loc_startend_mrid[1], &ul.guid)); }
            r
        }).collect()
    } else { Vec::new() };

    let r_err: Vec<_> = gd.routes.clone().into_iter().flat_map(|cr| {
        let mut r = Vec::new();
        if h_locations.get(cr.loc_startend_mrid[0].as_str()).is_none()
        { r.push(format!("Start location '{}' of route '{}' not found", &cr.loc_startend_mrid[0], &cr.guid)); }
        if h_locations.get(cr.loc_startend_mrid[1].as_str()).is_none()
        { r.push(format!("End location '{}' of route '{}' not found", &cr.loc_startend_mrid[1], &cr.guid)); }
        r
    }).collect();
    let rlm_err: Vec<_> = gd.mapping.clone().into_iter().flat_map(|crlm| {
        let mut r = Vec::new();
        if h_routes_in.get(crlm.route_mrid.as_str()).is_none()
        { r.push(format!("Route '{}' referenced in route line mapping not found", &crlm.route_mrid)); }
        let h_lines_found = if let Some(ch_lines) = h_lines.clone() {
            if ch_lines.get(crlm.line_mrid.as_str()).is_some() { true } else { false }
        } else { false };
        let h_ulines_found = if let Some(ch_ulines) = h_ulines.clone() {
            if ch_ulines.get(crlm.line_mrid.as_str()).is_some() { true } else { false }
        } else { false };
        if !h_lines_found && !h_ulines_found
        { r.push(format!("Line / uLine '{}' referenced in route line mapping not found", &crlm.line_mrid)); }
        r
    }).collect();

    let conl_err: Vec<_> = gd.connected_lines.clone().into_iter().flat_map(|cl| {
        let mut r = Vec::new();
        let h_lines_found = if let Some(ch_lines) = h_lines.clone() {
            if ch_lines.get(cl.start_id.as_str()).is_some() { true } else { false }
        } else { false };
        let h_ulines_found = if let Some(ch_ulines) = h_ulines.clone() {
            if ch_ulines.get(cl.start_id.as_str()).is_some() { true } else { false }
        } else { false };
        if !h_lines_found && !h_ulines_found
        { r.push(format!("Start line '{}' referenced by 'connected lines' not found", &cl.start_id)); }

        let h_lines_found = if let Some(ch_lines) = h_lines.clone() {
            if ch_lines.get(cl.end_id.as_str()).is_some() { true } else { false }
        } else { false };
        let h_ulines_found = if let Some(ch_ulines) = h_ulines.clone() {
            if ch_ulines.get(cl.end_id.as_str()).is_some() { true } else { false }
        } else { false };
        if !h_lines_found && !h_ulines_found
        { r.push(format!("End line '{}' referenced by 'connected lines' not found", &cl.end_id)); }
        r
    }).collect();

    let all_err: Vec<_> = l_err.into_iter().chain(ul_err).chain(r_err).chain(rlm_err)
        .chain(conl_err).collect();
*/

//    let r = if all_err.len() > 0 { Err(all_err.into_iter().join("\n")) }
/*    let r = if false { Err(format!("lala")) }
    else { Ok((gd, FilterReport{
        filtered_locations: vec![],
        filtered_routes, filtered_lines, filtered_ulines,
    })) };
    r*/


