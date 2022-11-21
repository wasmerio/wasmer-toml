use crate::{Abi, Bindings};
use std::collections::HashMap;
use std::path::PathBuf;

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct Wapm {
    pub namespace: String,
    pub package: Option<String>,
    pub wasmer_extra_flags: Option<String>,
    pub abi: Abi,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub fs: Option<HashMap<String, PathBuf>>,
    pub bindings: Option<Bindings>,
}
