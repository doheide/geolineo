use rocket::response::{self, Response, Responder};
use rocket::request::Request;
//use rocket::serde::{Serialize};
use serde_json;
use std::io::Cursor;
use rocket::http::{ContentType, Status};
use serde::{Serialize, Deserialize};


#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct ErrorResponder {
    pub message: String,
}
pub enum JSONResponder<T> {
    ErrResponse((Status, ErrorResponder)),
    Data((Status, T))
}
#[allow(dead_code)]
impl<T> JSONResponder<T> {
    pub fn new_error(msg: String, s: Status) -> Self{
        Self::ErrResponse((s, ErrorResponder{ message: msg }))
    }
    pub fn new_data(data: T, s: Status) -> Self {
        Self::Data((s, data))
    }
    pub fn new_data_ok(data: T) -> Self {
        Self::Data((Status::Ok, data))
    }
}

#[rocket::async_trait]
impl<'r, T: serde::Serialize> Responder<'r, 'static> for JSONResponder<T> {
    fn respond_to(self, _: &'r Request<'_>) -> response::Result<'static> {
        let (s, content) = match self {
            Self::ErrResponse((s, errr)) => {
                let err_str = match serde_json::to_string(&errr) {
                    Ok(t) => t, Err(e) =>  format!("{{\"message\": \"Serde error {:?}\"}}", e)
                };
                (s, err_str)
            },
            Self::Data((s, d)) => {
                let data_str = match serde_json::to_string(&d) {
                    Ok(t) => t, Err(e) =>  format!("{{\"message\": \"Serde error {:?}\"}}", e)
                };
                (s, data_str)
            }
        };
        Response::build()
            .header(ContentType::JSON).status(s)
            .sized_body(content.len(), Cursor::new(content))
            .ok()
    }
}
