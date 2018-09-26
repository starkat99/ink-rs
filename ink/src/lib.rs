#![feature(uniform_paths)]

use encoding_rs_io::DecodeReaderBytes;
use failure::{Fail, Fallible};
use std::{
    collections::HashMap,
    fmt::Debug,
    io::{Read, Write},
    rc::Rc,
};

mod data;

use data::{ContainedNode, Path, PathComponent};
pub use data::{Error, Story};

pub(crate) type InternStr = string_interner::Sym;
pub(crate) type StringArena = string_interner::StringInterner<InternStr>;

#[derive(Debug, Clone)]
pub struct StoryState<'story> {
    story: &'story Story,
    output_stream: Vec<data::Object>,
    current_choices: Vec<Choice<'story>>,
    callstack: CallStack<'story>,
    variables: VariablesState,
    evaluation_stack: Vec<data::Object>,
    diverted_pointer: Pointer<'story>,
    visit_counts: HashMap<InternStr, u32>,
    turn_indices: HashMap<InternStr, u32>,
    current_turn_index: u32,
    story_seed: u32,
    previous_random: u32,
}

#[derive(Debug, Clone)]
pub(crate) struct Pointer<'story> {
    container: Option<&'story data::Container>,
    index: Option<u32>,
}

#[derive(Debug, Clone)]
pub(crate) struct CallStack<'story> {
    threads: Vec<Rc<Thread<'story>>>,
    thread_counter: u32,
}

#[derive(Debug, Clone)]
pub(crate) struct CallStackElement<'story> {
    pointer: Pointer<'story>,
    in_expression_evaluation: bool,
    temp_vars: HashMap<InternStr, data::Object>,
    stack_type: data::PushPopType,
}

#[derive(Debug, Clone)]
pub(crate) struct Thread<'story> {
    callstack: Vec<CallStackElement<'story>>,
    index: u32,
    previous_pointer: Pointer<'story>,
}

#[derive(Debug, Clone)]
pub(crate) struct Choice<'story> {
    text: InternStr,
    target_path: Path,
    source_path: Path,
    index: u32,
    original_thread_index: u32,
    thread_at_generation: Rc<Thread<'story>>,
}

#[derive(Debug, Clone)]
pub(crate) struct VariablesState {
    global_vars: HashMap<InternStr, data::Object>,
}

#[derive(Debug, Clone)]
pub(crate) enum SearchResult<'story, T: Debug + 'story> {
    Exact(&'story T),
    Partial(&'story T, usize),
}

impl<'story> StoryState<'story> {
    pub fn new(story: &Story) -> StoryState {
        StoryState {
            story,
            output_stream: Vec::default(),
            current_choices: Vec::default(),
            callstack: CallStack::new(story.root()),
            variables: VariablesState::new(),
            evaluation_stack: Vec::default(),
            diverted_pointer: Pointer::default(),
            visit_counts: HashMap::default(),
            turn_indices: HashMap::default(),
            current_turn_index: 0,
            story_seed: 0,
            previous_random: 0,
        }
    }

    pub fn read_json<R: Read>(story: &'story Story, reader: R) -> Fallible<Self> {
        let decoder = DecodeReaderBytes::new(reader);
        let value = serde_json::from_reader(decoder)?;
        data::json::value_to_story_state(&value, story)
    }

    pub fn from_json_str(story: &'story Story, s: &str) -> Fallible<Self> {
        let value = serde_json::from_str(s)?;
        data::json::value_to_story_state(&value, story)
    }

    pub fn write_json<W: Write>(&self, writer: W) -> Fallible<()> {
        serde_json::to_writer(writer, &data::json::story_state_to_value(self))?;
        Ok(())
    }

    pub fn to_json_string(&self) -> String {
        data::json::story_state_to_value(self).to_string()
    }

    pub fn to_json_string_pretty(&self) -> String {
        serde_json::to_string_pretty(&data::json::story_state_to_value(self))
            .expect("unexpected failure writing pretty json string")
    }
}

impl<'story> Pointer<'story> {
    pub(crate) fn new(container: &'story data::Container, index: Option<u32>) -> Self {
        Pointer {
            container: Some(container),
            index,
        }
    }

    pub(crate) fn start_of(container: &'story data::Container) -> Self {
        Pointer {
            container: Some(container),
            index: Some(0),
        }
    }

    pub(crate) fn is_null(&self) -> bool {
        self.container.is_none()
    }

    pub(crate) fn path(&self) -> Path {
        let mut path = self.container.map(|c| c.path().clone()).unwrap_or_default();
        if let Some(index) = self.index {
            path.push(PathComponent::from_u32(index));
        }
        path
    }
}

impl Default for Pointer<'_> {
    fn default() -> Self {
        Pointer {
            container: None,
            index: None,
        }
    }
}

impl<'story> CallStack<'story> {
    pub(crate) fn new(container: &'story data::Container) -> Self {
        let threads = vec![Rc::new(Thread::new(container))];
        CallStack {
            threads,
            thread_counter: 0,
        }
    }

    pub(crate) fn get_thread_with_index(&self, index: u32) -> Option<Rc<Thread<'story>>> {
        self.threads
            .iter()
            .find(|thread| thread.index == index)
            .map(|t| t.clone())
    }
}

impl<'story> CallStackElement<'story> {
    pub(crate) fn new(
        stack_type: data::PushPopType,
        pointer: Pointer<'story>,
        in_expression_evaluation: bool,
    ) -> Self {
        CallStackElement {
            stack_type,
            pointer,
            in_expression_evaluation,
            temp_vars: HashMap::new(),
        }
    }
}

impl<'story> Thread<'story> {
    pub(crate) fn new(container: &'story data::Container) -> Self {
        let callstack = vec![CallStackElement::new(
            data::PushPopType::Tunnel,
            Pointer::start_of(container),
            false,
        )];
        Thread {
            callstack,
            ..Thread::default()
        }
    }
}

impl<'story> Default for Thread<'story> {
    fn default() -> Self {
        Thread {
            callstack: Vec::default(),
            index: 0,
            previous_pointer: Pointer::default(),
        }
    }
}

impl VariablesState {
    pub(crate) fn new() -> Self {
        VariablesState {
            global_vars: HashMap::default(),
        }
    }
}
