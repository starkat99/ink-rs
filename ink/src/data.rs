use super::{InternStr, Pointer, SearchResult, StringArena};
use bitflags::bitflags;
use encoding_rs_io::DecodeReaderBytes;
use failure::{Fail, Fallible};
use std::{
    cell::{Ref, RefCell},
    collections::HashMap,
    io::{Read, Write},
};

pub(crate) mod json;
mod path;

pub(crate) use path::{Path, PathComponent};

#[derive(Debug, Fail)]
pub enum Error {
    #[fail(display = "invalid format: {}", _0)]
    InvalidJsonFormat(String),
    #[fail(display = "unsupported ink format version: {}", _0)]
    UnsupportedVersion(u64),
    #[fail(display = "story path not found: '{}'", _0)]
    PathNotFound(String),
}

#[derive(Debug, Clone)]
pub struct Story {
    root: Object, // Always a Container
    list_definitions: ListDefinitionsMap,
    // Would rather not have this interior mutability, but story state doesn't maintain a separate arena
    string_arena: RefCell<StringArena>,
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
    content: Vec<ContainerNode>,
    named_only_content: HashMap<InternStr, ContainerNode>,
}

#[derive(Debug, Clone)]
pub(crate) struct ContainerNode {
    path: Path,
    object: Object,
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
    fn init(&mut self) {
        if let Object::Container(root) = &mut self.root {
            // Root path is name or 0
            let path: Path = match root.name {
                Some(name) => PathComponent::Name(name).into(),
                _ => Path::default(),
            };
            root.init(path)
        } else {
            panic!("story root not container");
        }
    }

    pub fn read_json<R: Read>(reader: R) -> Fallible<Self> {
        let decoder = DecodeReaderBytes::new(reader);
        let value = serde_json::from_reader(decoder)?;
        let mut story = json::value_to_story(value)?;
        story.init();
        Ok(story)
    }

    pub fn from_json_str(s: &str) -> Fallible<Self> {
        let value = serde_json::from_str(s.as_ref())?;
        let mut story = json::value_to_story(value)?;
        story.init();
        Ok(story)
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

    pub(crate) fn root(&self) -> &Container {
        // Wish there was a better way, but because we have to use Object rather than Container for root...
        if let Object::Container(root) = &self.root {
            root
        } else {
            panic!("story root not container");
        }
    }

    pub(crate) fn resolve_absolute_path<'story>(
        &'story self,
        path: &Path,
    ) -> SearchResult<'story, Object> {
        let mut current = &self.root;
        for (partial_index, comp) in path.iter().enumerate() {
            if let Object::Container(container) = current {
                match comp {
                    PathComponent::Name(name) => {
                        if let Some(node) = container.named_only_content.get(name) {
                            current = &node.object;
                        } else {
                            // No child, so partial match with parent
                            return SearchResult::Partial(current, partial_index);
                        }
                    }
                    PathComponent::Index(index) => {
                        if let Some(node) = container.content.get(*index as usize) {
                            current = &node.object;
                        } else {
                            // No child, so partial match with parent
                            return SearchResult::Partial(current, partial_index);
                        }
                    }
                    PathComponent::Parent => {} // Ignore any parents, they'll all be at beginning of path
                }
            } else {
                // Path expected more hierarchy, but we've hit a non-container
                return SearchResult::Partial(current, partial_index);
            }
        }
        SearchResult::Exact(current)
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

    pub(crate) fn resolve_str(&self, s: InternStr) -> Ref<str> {
        Ref::map(self.string_arena.borrow(), |b| {
            b.resolve(s).expect(
                "interned string did not resolve, somewhere a string was not interned properly",
            )
        })
    }

    pub(crate) fn intern_str<T: Into<String> + AsRef<str>>(&self, s: T) -> InternStr {
        self.string_arena.borrow_mut().get_or_intern(s)
    }

    pub(crate) fn path_to_string(&self, path: &Path) -> String {
        path.to_string(&self.string_arena.borrow())
            .expect("interned path did not resolve, somewhere a string was not interned properly")
    }
}

impl Container {
    fn init(&mut self, path: Path) {
        for (i, child) in self.content.iter_mut().enumerate() {
            child.init(path.with_tail_component((i as u32).into()));
        }
        for (&name, child) in &mut self.named_only_content {
            child.init(path.with_tail_component(name.into()));
        }
    }

    pub(crate) fn path(&self) -> Path {
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

impl ContainerNode {
    fn init(&mut self, path: Path) {
        if let Object::Container(container) = &mut self.object {
            container.init(path.clone());
        }
        self.path = path;
    }

    fn new(object: Object) -> Self {
        ContainerNode {
            path: Path::default(),
            object,
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
