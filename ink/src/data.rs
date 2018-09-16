use super::{InternStr, Pointer, SearchResult};
use bitflags::bitflags;
use encoding_rs_io::DecodeReaderBytes;
use failure::{Fail, Fallible};
use std::{
    collections::HashMap,
    io::{Read, Write},
};

pub(crate) mod json;
mod path;

pub use path::{Path, PathComponent};

#[derive(Debug, Fail)]
pub enum Error {
    #[fail(display = "invalid format: {}", _0)]
    InvalidJsonFormat(String),
    #[fail(display = "unsupported ink format version: {}", _0)]
    UnsupportedVersion(u64),
    #[fail(display = "story path not found: '{}'", _0)]
    PathNotFound(Path),
}

#[derive(Debug, Clone)]
pub struct Story {
    root: Container,
    list_definitions: ListDefinitionsMap,
}

#[derive(Debug, Clone)]
pub(crate) enum Object {
    Value(Value),
    Container(Container),
    Control(ControlCommand),
    NativeCall(NativeFunction),
    Divert(Divert),
    Choice(ChoicePoint),
    Variable(VariableReference),
    Assignment(VariableAssignment),
    Tag(InternStr),
    Glue,
    Void,
}

#[derive(Debug, Clone)]
pub(crate) struct Container {
    name: Option<InternStr>,
    count_flags: CountFlags,
    content: Vec<Object>,
    named_only_content: HashMap<InternStr, Object>,
}

#[derive(Debug, Clone)]
pub(crate) enum Value {
    Int(i32),
    Float(f32),
    String(InternStr),
    DivertTarget(Path),
    VariablePointer(InternStr, VariableScope),
    List(List),
}

#[derive(Debug, Clone)]
pub(crate) struct List {
    content: HashMap<ListItem, i32>,
    origin_names: Option<Vec<InternStr>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct ListItem(InternStr);

#[derive(Debug, Clone)]
pub(crate) struct ListDefinition {
    name: InternStr,
    items: HashMap<ListItem, i32>,
}

#[derive(Debug, Clone)]
pub(crate) struct ListDefinitionsMap {
    lists: HashMap<InternStr, ListDefinition>,
}

#[derive(Debug, Clone)]
pub(crate) enum ControlCommand {
    EvalStart,
    EvalOutput,
    EvalEnd,
    Duplicate,
    PopEvaluatedValue,
    PopFunction,
    PopTunnel,
    BeginString,
    EndString,
    NoOp,
    ChoiceCount,
    Turns,
    TurnsSince,
    ReadCount,
    Random,
    SeedRandom,
    VisitIndex,
    SequenceShuffleIndex,
    StartThread,
    Done,
    End,
    ListFromInt,
    ListRange,
    ListRandom,
}

#[derive(Debug, Clone)]
pub(crate) enum NativeFunction {
    Add,
    Subtract,
    Divide,
    Multiply,
    Modulo,
    Negate,
    Equal,
    Greater,
    Less,
    GreaterOrEqual,
    LessOrEqual,
    NotEqual,
    Not,
    And,
    Or,
    Min,
    Max,
    Power,
    Floor,
    Ceiling,
    Int,
    Float,
    Has,
    HasNot,
    Intersect,
    ListMin,
    ListMax,
    All,
    Count,
    ValueOfList,
    Invert,
}

#[derive(Debug, Clone)]
pub(crate) enum VariableScope {
    Unknown,
    Global,
    Callstack(u32),
}

#[derive(Debug, Clone)]
pub(crate) enum PushPopType {
    Tunnel,
    Function,
    FunctionEvaluation,
}

#[derive(Debug, Clone)]
pub(crate) struct Divert {
    target: DivertTarget,
    pushes_to_stack: bool,
    stack_push_type: PushPopType,
    external: bool,
    external_args: u32,
    conditional: bool,
}

#[derive(Debug, Clone)]
pub(crate) enum DivertTarget {
    Path(Path),
    Variable(InternStr),
}

#[derive(Debug, Clone)]
pub(crate) struct ChoicePoint {
    path_on_choice: Path,
    flags: ChoiceFlags,
}

#[derive(Debug, Clone)]
pub(crate) enum VariableReference {
    Name(InternStr),
    Count(Path),
}

#[derive(Debug, Clone)]
pub(crate) struct VariableAssignment {
    name: InternStr,
    new_declaration: bool,
    global: bool,
}

bitflags! {
    #[derive(Default)]
    pub(crate) struct CountFlags: u32 {
        const Visits = 0x1;
        const Turns = 0x2;
        const CountStartOnly = 0x4;
    }
}

bitflags! {
    #[derive(Default)]
    pub(crate) struct ChoiceFlags: u32 {
        const Condition = 0x01;
        const StartContent = 0x02;
        const ChoiceOnlyContent = 0x04;
        const InvisibleDefault = 0x08;
        const OnceOnly = 0x10;
    }
}

impl Story {
    pub fn read_json<R: Read>(reader: R) -> Fallible<Self> {
        let decoder = DecodeReaderBytes::new(reader);
        let value = serde_json::from_reader(decoder)?;
        json::value_to_story(value)
    }

    pub fn from_json_str(s: &str) -> Fallible<Self> {
        let value = serde_json::from_str(s.as_ref())?;
        json::value_to_story(value)
    }

    pub fn write_json<W: Write>(&self, writer: W) -> Fallible<()> {
        Ok(serde_json::to_writer(writer, &json::story_to_value(self))?)
    }

    pub fn to_json_string(&self) -> String {
        json::story_to_value(self).to_string()
    }

    pub fn to_json_string_pretty(&self) -> String {
        serde_json::to_string_pretty(&json::story_to_value(self))
            .expect("unexpected failure writing pretty json string")
    }

    pub(crate) fn get_pointer_at_path<'story>(&'story self, path: &Path) -> Pointer<'story> {
        if path.is_empty() {
            return Pointer::default();
        }

        unimplemented!()
    }

    pub(crate) fn get_container_at_path<'story>(
        &'story self,
        _path: &Path,
    ) -> SearchResult<'story, Container> {
        unimplemented!()
    }
}

impl Container {
    pub fn path(&self) -> Path {
        unimplemented!()
    }
}

impl Default for Container {
    fn default() -> Self {
        Container {
            name: None,
            count_flags: CountFlags::default(),
            content: Vec::default(),
            named_only_content: HashMap::default(),
        }
    }
}

impl Default for ListDefinitionsMap {
    fn default() -> Self {
        ListDefinitionsMap {
            lists: HashMap::default(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::fs::File;

    #[test]
    fn read_json() -> Fallible<()> {
        let _story = Story::read_json(File::open("../examples/stories/TheIntercept.ink.json")?)?;
        Ok(())
    }

    #[test]
    fn write_json() -> Fallible<()> {
        let mut decoder =
            DecodeReaderBytes::new(File::open("../examples/stories/TheIntercept.ink.json")?);
        let mut s = String::new();
        decoder.read_to_string(&mut s)?;

        let story = Story::from_json_str(&s)?;

        let output = story.to_json_string();
        // Can't really test for equality since hashing probably reordered object keys, for now just make sure it parses again
        Story::from_json_str(&output)?;
        Ok(())
    }
}
