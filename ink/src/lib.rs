#![feature(uniform_paths)]
use failure::Fallible;
use serde_derive::{Deserialize, Serialize};
use std::io::{Read, Write};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Story {
    #[serde(rename = "inkVersion")]
    format_version: u32,
}

impl Story {
    const CURRENT_FORMAT_VERSION: u32 = 19;
    const MIN_FORMAT_VERSION: u32 = 18;

    pub fn new() -> Self {
        Self {
            format_version: Self::CURRENT_FORMAT_VERSION,
        }
    }

    pub fn from_reader<R: Read>(reader: R) -> Fallible<Self> {
        Ok(serde_json::from_reader(reader)?)
    }

    pub fn from_str<S: AsRef<str>>(s: S) -> Fallible<Self> {
        Ok(serde_json::from_str(s.as_ref())?)
    }

    pub fn to_writer<W: Write>(&self, writer: W) -> Fallible<()> {
        Ok(serde_json::to_writer_pretty(writer, self)?)
    }

    pub fn to_string(&self) -> Fallible<String> {
        Ok(serde_json::to_string_pretty(self)?)
    }
}
