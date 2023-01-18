use conf_from_env::{SetStructFromEnv, ConfigAndSecrets};

#[derive(SetStructFromEnv, Clone, Debug)]
pub struct Config {
    pub rust_log: String,
    pub test_cases_subdirectory: String,
    pub rocket_template_dir: String
}
#[derive(SetStructFromEnv, Clone)]
pub struct Secrets {

}

pub type CoSe = ConfigAndSecrets<Config, Secrets>;
