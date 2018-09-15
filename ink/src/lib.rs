#![feature(uniform_paths)]

mod data;

pub use data::{Error, Path, PathComponent, Story};

#[derive(Debug, Clone)]
pub struct StoryState<'s> {
    story: &'s Story,
}

impl<'s> StoryState<'s> {
    pub fn new(story: &Story) -> StoryState {
        StoryState { story }
    }
}
