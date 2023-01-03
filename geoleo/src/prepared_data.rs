use std::collections::HashMap;
use crate::geoleo_types::GridData;
use std::fs;
use std::fs::File;
use std::io::BufReader;
use std::path::{PathBuf};
// use std::option::Option;
use itertools::Itertools;
use serde_json;
use serde::{Serialize, Deserialize};
use walkdir::WalkDir;

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct TestCaseInfo {
    pub name: String,
}

#[allow(dead_code)]
pub fn load_testcase_by_id(id: String, base_path: String) -> Result<GridData, String> {
    let tc_map: HashMap<_, _> = list_of_testcases(base_path).into_iter()
        .map(|v| (v.clone().id, v)).collect();

    let cur_path = match tc_map.get(&id).clone() {
        None => return Err(format!("Id not found. Possible: {}", tc_map.keys().join(", "))),
        Some(v) => PathBuf::from(v.clone().path)
    };
    load_testcase_by_path(cur_path)
}

pub fn load_testcase_by_path(cur_path: PathBuf) -> Result<GridData, String> {
    let file_name = cur_path.join("in.json");
    let data = match fs::read_to_string(file_name.clone()) {
        Ok(d) => d,
        Err(e) => return Err(format!("Error when reading {}: {:?}", file_name.to_str().unwrap(), e))
    };
    let gd: GridData = match serde_json::from_str(data.as_str()) {
        Ok(d) => d,
        Err(e) => return Err(format!("Error parsing json: {:?}", e))
    };

    Ok(gd)
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct TestCaseDetails {
    pub path: String,
    pub id: String,
    pub tci: TestCaseInfo,
}
#[allow(dead_code)]
pub fn list_of_testcases(base_dir: String) -> Vec<TestCaseDetails> {
    let base_dir_path = PathBuf::from(base_dir.clone());
    let paths_comps_num = base_dir_path.into_iter().count();
    WalkDir::new(
        base_dir.clone().as_str()).into_iter()
        .filter_map(|x| x.ok()).filter(|v| v.path().is_dir())
        .filter_map(|d| {
            println!("path: {:?}", d);
            let file = match File::open(d.path().join("info.json")) {
                Ok(f) => f, Err(_) => return None
            };
            let reader = BufReader::new(file);
            let tci= match serde_json::from_reader(reader) {
                Ok(d) => d, Err(_) => return None
            };
            let path = d.clone().into_path().into_os_string().into_string().unwrap();
            let id: PathBuf = d.into_path().into_iter().skip(paths_comps_num.clone()).collect();
            let id: String = id.to_str().unwrap().into();
            let id = id.replace("/", "___");
            Some(TestCaseDetails{ path, id, tci })
        })
        .collect()
}