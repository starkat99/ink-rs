#![feature(uniform_paths)]
use bitflags::bitflags;
use encoding_rs_io::DecodeReaderBytes;
use failure::{Fail, Fallible};
use internship::IStr;
use std::{
    collections::HashMap,
    io::{Read, Write},
};

mod json;
mod path;

pub use path::{Path, PathComponent};

#[derive(Debug, Fail)]
pub enum Error {
    #[fail(display = "invalid format: {}", _0)]
    InvalidJsonFormat(&'static str),
    #[fail(display = "unsupported ink format version: {}", _0)]
    UnsupportedVersion(u64),
}

#[derive(Debug, Clone)]
pub struct Story {
    root: Container,
}

#[derive(Debug, Clone)]
pub enum RuntimeObject {
    Value(Value),
    Container(Container),
    Control(ControlCommand),
    NativeCall(NativeFunction),
    Divert(Divert),
    Choice(ChoicePoint),
    Variable(VariableReference),
    Assignment(VariableAssignment),
    Tag(IStr),
    Glue,
    Void,
}

#[derive(Debug, Clone)]
pub struct Container {
    name: Option<IStr>,
    count_flags: CountFlags,
    content: Vec<RuntimeObject>,
    named_only_content: HashMap<IStr, RuntimeObject>,
}

#[derive(Debug, Clone)]
pub enum Value {
    Int(i32),
    Float(f32),
    String(IStr),
    DivertTarget(Path),
    VariablePointer(IStr, VariableScope),
    List(List),
}

#[derive(Debug, Clone)]
pub struct List {
    content: HashMap<ListItem, i32>,
    origin_names: Option<Vec<IStr>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ListItem(IStr);

#[derive(Debug, Clone)]
pub enum ControlCommand {
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
pub enum NativeFunction {
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
pub enum VariableScope {
    Unknown,
    Global,
    Callstack(u32),
}

#[derive(Debug, Clone)]
pub enum PushPopType {
    Tunnel,
    Function,
    FunctionEvaluationFromGame,
}

#[derive(Debug, Clone)]
pub struct Divert {
    target_path: Option<Path>,
    var_divert_name: Option<IStr>,
    pushes_to_stack: bool,
    stack_push_type: PushPopType,
    external: bool,
    external_args: u32,
    conditional: bool,
}

#[derive(Debug, Clone)]
pub struct ChoicePoint {
    path_on_choice: Path,
    flags: ChoiceFlags,
}

#[derive(Debug, Clone)]
pub struct VariableReference {
    name: Option<IStr>,
    path_for_count: Option<Path>,
}

#[derive(Debug, Clone)]
pub struct VariableAssignment {
    name: IStr,
    new_declaration: bool,
    global: bool,
}

bitflags! {
    #[derive(Default)]
    struct CountFlags: u32 {
        const Visits = 0x1;
        const Turns = 0x2;
        const CountStartOnly = 0x4;
    }
}

bitflags! {
    #[derive(Default)]
    struct ChoiceFlags: u32 {
        const Condition = 0x01;
        const StartContent = 0x02;
        const ChoiceOnlyContent = 0x04;
        const InvisibleDefault = 0x08;
        const OnceOnly = 0x10;
    }
}

impl Story {
    pub fn new() -> Self {
        Self {
            root: Container::default(),
        }
    }

    pub fn read_json<R: Read>(reader: R) -> Fallible<Self> {
        let decoder = DecodeReaderBytes::new(reader);
        let value = serde_json::from_reader(decoder)?;
        json::value_to_story(value)
    }

    pub fn from_str<S: AsRef<str>>(s: S) -> Fallible<Self> {
        let value = serde_json::from_str(s.as_ref())?;
        json::value_to_story(value)
    }

    pub fn write_json<W: Write>(&self, writer: W) -> Fallible<()> {
        Ok(serde_json::to_writer(writer, &json::story_to_value(self)?)?)
    }

    pub fn to_string(&self) -> Fallible<String> {
        Ok(serde_json::to_string(&json::story_to_value(self)?)?)
    }
}

impl RuntimeObject {
    pub fn name(&self) -> Option<&str> {
        match self {
            RuntimeObject::Container(c) => c.name.as_ref().map(|s| s.as_ref()),
            _ => None,
        }
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

#[cfg(test)]
mod test {
    use super::*;
    use std::fs::File;

    #[test]
    fn read_json() -> Fallible<()> {
        let _story = Story::read_json(File::open("../examples/stories/TheIntercept.ink.json")?)?;
        Ok(())
    }
}
