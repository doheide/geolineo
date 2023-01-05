#[macro_use] extern crate rocket;

mod conf;
mod rocket_helper;

use rocket::State;
use conf::CoSe;
use rocket_dyn_templates::{Template, context};
use geoleo::{generate_geojson, GeoJsonContent, GridData, list_of_testcases, load_testcase_by_id};
use dotenv;
use rocket::http::{Status};
use rocket_helper::JSONResponder;
use rocket::serde::json::Json;
use itertools::Itertools;
use tracing_subscriber;
use std::fs;

// ************************************************************************
// ************************************************************************
#[get("/")]
fn index(cose: &State<CoSe>) -> Template {
    let list_tc = list_of_testcases(cose.conf.test_cases_subdirectory.clone());
    let list_tc_link: Vec<_> = list_tc.into_iter()
        .map(|v| (format!("/test_cases/{}/map", v.id), v))
        .sorted_by(|(_, a), (_, b)|
            a.id.partial_cmp(&b.id).unwrap()).collect();
    Template::render("index", context! {list_test_cases: list_tc_link})
}

// ************************************************************************
// ************************************************************************
#[get("/test_cases/<tc_id>/map")]
async fn tc_map(tc_id: String) -> Template {
    Template::render("map", context! {tc_name: tc_id.clone(),
        tc_geojson_url: format!("/api/v1/test_cases/{}/geo.json", tc_id)})
}

#[get("/test_cases/<tc_id>/geo.json")]
async fn tc_geojson(tc_id: String, cose: &State<CoSe>) -> JSONResponder<GeoJsonContent> {
    debug!("tc_geojson: {}", tc_id);
    let gd = match load_testcase_by_id(tc_id, cose.conf.test_cases_subdirectory.clone()) {
        Ok(d) => d, Err(e) => {
            error!("Error loading: {:?}", e);
            return JSONResponder::new_error(format!("Error when loading data: {}", e), Status::NotFound)
        }
    };

    let r = match generate_geojson(gd) {
        Ok(d) => d, Err(e) => {
            error!("Error generating: {:?}", e);
            return JSONResponder::new_error(format!("Error when loading data: {}", e), Status::InternalServerError)
        }
    };
    JSONResponder::new_data_ok(r)
}

#[post("/render", format="json", data="<grid_data>")]
async fn render_geojson(grid_data: Json<GridData>) -> JSONResponder<GeoJsonContent> {
    let r = match generate_geojson(grid_data.into_inner()) {
        Ok(d) => d, Err(e) => {
            error!("Render api -> Error: {:?}", e);
            return JSONResponder::new_error(format!("Error: {}", e), Status::BadRequest)
        }
    };
    JSONResponder::new_data_ok(r)
}
// ************************************************************************
// ************************************************************************
#[rocket::main]
async fn main() -> Result<(), rocket::Error> {
    dotenv::from_filename("local.env").ok();
    let cose = CoSe::from_env();
    tracing_subscriber::fmt::init();
    info!("cose: {:?}", &cose);

    debug!("templates: ");
    for p in fs::read_dir(cose.conf.rocket_template_dir.as_str()).unwrap()
    { debug!(" - {}", p.unwrap().path().display()); }
    debug!("tests_cases: ");
    for p in fs::read_dir(cose.conf.test_cases_subdirectory.as_str()).unwrap()
    { debug!(" - {}", p.unwrap().path().display()); }

    let _rocket = rocket::build()
        .manage(cose)
        .attach(Template::fairing())
        .mount("/", routes![index, tc_map])
        .mount("/api/v1", routes![tc_geojson, render_geojson])
        .launch()
        .await?;

    Ok(())
}