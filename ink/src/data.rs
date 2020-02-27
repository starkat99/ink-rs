use super::{InternStr, Pointer, SearchResult, StringArena};
use bitflags::bitflags;
use encoding_rs_io::DecodeReaderBytes;
use log::{error, warn};
use std::{
    cell::{Ref, RefCell},
    collections::HashMap,
    io::{Read, Write},
};
use thiserror::Error;

pub(crate) mod json;
mod path;

pub(crate) use path::{Path, PathComponent};

#[derive(Debug, Error)]
pub enum Error {
    #[error("invalid format: {0}")]
    InvalidJsonFormat(String),
    #[error("unsupported ink format version: {0}")]
    UnsupportedVersion(u64),
    #[error("story path not found: '{0}'")]
    PathNotFound(String),
    #[error(transparent)]
    JsonError(#[from] serde_json::Error),
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone)]
pub struct Story {
    objects: HashMap<Path, Object>,
    list_definitions: ListDefinitionsMap,
    // Would rather not have this interior mutability, but story state doesn't maintain a separate arena
    string_arena: RefCell<StringArena>,
}

#[derive(Debug, Clone)]
pub(crate) enum Object {
    Container(Container),
    Value(Value),
    Control(ControlCommand, Node),
    NativeCall(NativeFunction, Node),
    Divert(Divert),
    Choice(ChoicePoint),
    Variable(VariableReference),
    Assignment(VariableAssignment),
    Tag(InternStr, Node),
    Glue(Node),
    Void(Node),
}

#[derive(Debug, Clone)]
pub(crate) struct Container {
    node: Node,
    name: Option<InternStr>,
    count_flags: CountFlags,
    num_index_children: u32,
    named_children: Vec<PathComponent>,
}

#[derive(Debug, Clone)]
pub(crate) struct Node {
    path: Path,
}

pub(crate) trait ContainedNode {
    fn node(&self) -> &Node;

    #[inline]
    fn object<'story>(&self, story: &'story Story) -> &'story Object {
        self.node().object(story)
    }

    #[inline]
    fn parent<'story>(&self, story: &'story Story) -> Option<&'story Container> {
        self.node().parent(story)
    }

    #[inline]
    fn path(&self) -> &Path {
        self.node().path()
    }

    fn resolve_path<'story>(
        &'story self,
        story: &'story Story,
        path: &Path,
    ) -> SearchResult<'story, Object> {
        if path.is_relative() {
            match self.object(story) {
                Object::Container(container) => container.find_content_at_path(story, path),
                obj => {
                    if let (Some(PathComponent::Parent), tail) = path.split_head() {
                        self.parent(story)
                            .expect("non-container object must have parent")
                            .find_content_at_path(story, &tail)
                    } else {
                        // The first component of path MUST be a parent component if we're not a container
                        error!("relative path {} cannot be resolved against a non-container object without a leading ^", story.path_to_string(path));
                        SearchResult::Partial(obj, 0)
                    }
                }
            }
        } else {
            story.resolve_absolute_path(path)
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum Value {
    Int(i32, Node),
    Float(f32, Node),
    String(InternStr, Node),
    DivertTarget(Path, Node),
    VariablePointer(InternStr, VariableScope, Node),
    List(List),
}

#[derive(Debug, Clone)]
pub(crate) struct List {
    node: Node,
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

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
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

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
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
    node: Node,
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
    node: Node,
    path_on_choice: Path,
    flags: ChoiceFlags,
}

#[derive(Debug, Clone)]
pub(crate) enum VariableReference {
    Name(InternStr, Node),
    Count(Path, Node),
}

#[derive(Debug, Clone)]
pub(crate) struct VariableAssignment {
    node: Node,
    name: InternStr,
    new_declaration: bool,
    global: bool,
}

bitflags! {
    #[derive(Default)]
    pub(crate) struct CountFlags: u32 {
        const VISITS = 0x1;
        const TURNS = 0x2;
        const COUNT_START_ONLY = 0x4;
    }
}

bitflags! {
    #[derive(Default)]
    pub(crate) struct ChoiceFlags: u32 {
        const CONDITION = 0x01;
        const START_CONTENT = 0x02;
        const CHOICE_ONLY_CONTENT = 0x04;
        const INVISIBLE_DEFAULT = 0x08;
        const ONCE_ONLY = 0x10;
    }
}

impl Story {
    pub fn read_json<R: Read>(reader: R) -> Result<Self> {
        let decoder = DecodeReaderBytes::new(reader);
        let value = serde_json::from_reader(decoder)?;
        Ok(json::value_to_story(&value)?)
    }

    pub fn from_json_str(s: &str) -> Result<Self> {
        let value = serde_json::from_str(s)?;
        Ok(json::value_to_story(&value)?)
    }

    pub fn write_json<W: Write>(&self, writer: W) -> Result<()> {
        serde_json::to_writer(writer, &json::story_to_value(self))?;
        Ok(())
    }

    pub fn to_json_string(&self) -> String {
        json::story_to_value(self).to_string()
    }

    pub fn to_json_string_pretty(&self) -> String {
        serde_json::to_string_pretty(&json::story_to_value(self))
            .expect("unexpected failure writing pretty json string")
    }

    pub(crate) fn root(&self) -> &Container {
        if let Some(Object::Container(container)) = self.objects.get(&Path::default()) {
            return container;
        }
        panic!("story has no root container");
    }

    pub(crate) fn get_object<'story>(&'story self, path: &Path) -> Option<&'story Object> {
        self.objects.get(path)
    }

    pub(crate) fn resolve_absolute_path<'story>(
        &'story self,
        path: &Path,
    ) -> SearchResult<'story, Object> {
        self.resolve_relative_path(self.root(), path)
    }

    pub(crate) fn resolve_relative_path<'story>(
        &'story self,
        container: &Container,
        path: &Path,
    ) -> SearchResult<'story, Object> {
        if let Some(obj) = self.objects.get(path) {
            SearchResult::Exact(obj)
        } else {
            // Now do a search for a partial
            let mut current = container.object(self);
            for (partial_index, comp) in path.iter().enumerate() {
                if let Object::Container(container) = current {
                    match comp {
                        PathComponent::Index(i) => {
                            if i < &container.num_index_children {
                                current = &self.objects[&current.path().with_tail_component(*comp)];
                            } else {
                                // No child, so partial match with parent
                                return SearchResult::Partial(current, partial_index);
                            }
                        }
                        PathComponent::Name(_) => {
                            if container.named_children.contains(comp) {
                                current = &self.objects[&current.path().with_tail_component(*comp)];
                            } else {
                                // No child, so partial match with parent
                                return SearchResult::Partial(current, partial_index);
                            }
                        }
                        PathComponent::Parent => {
                            if let Some(parent) = container.parent(self) {
                                current = parent.object(self);
                            } else {
                                // No parent, so partial matach with self
                                return SearchResult::Partial(current, partial_index);
                            }
                        }
                    }
                } else {
                    // Path expected more hierarchy, but we've hit a non-container
                    return SearchResult::Partial(current, partial_index);
                }
            }
            SearchResult::Exact(current)
        }
    }

    pub(crate) fn get_pointer_at_path<'story>(&'story self, path: &Path) -> Pointer<'story> {
        if path.is_empty() {
            Pointer::default()
        } else if let Some(index) = path.last().unwrap().as_index() {
            let (parent_path, _) = path.split_tail();
            let container = match self.get_container_at_path(&parent_path) {
                SearchResult::Exact(container) => container,
                SearchResult::Partial(container, _) => {
                    warn!(
                        "exact story path '{}' not found, approximated to '{}' to recover",
                        self.path_to_string(path),
                        self.path_to_string(&container.path())
                    );
                    container
                }
            };
            if container as *const Container == self.root() && !parent_path.is_empty() {
                error!(
                    "failed to find content at path {}",
                    self.path_to_string(path)
                );
            }
            Pointer::new(container, Some(index))
        } else {
            let container = match self.get_container_at_path(&path) {
                SearchResult::Exact(container) => container,
                SearchResult::Partial(container, _) => {
                    warn!(
                        "exact story path '{}' not found, approximated to '{}' to recover",
                        self.path_to_string(path),
                        self.path_to_string(&container.path())
                    );
                    container
                }
            };
            if container as *const Container == self.root() && !path.is_empty() {
                error!(
                    "failed to find content at path {}",
                    self.path_to_string(path)
                );
            }
            Pointer::new(container, None)
        }
    }

    pub(crate) fn get_container_at_path<'story>(
        &'story self,
        path: &Path,
    ) -> SearchResult<'story, Container> {
        match self.resolve_absolute_path(path) {
            SearchResult::Exact(Object::Container(container)) => SearchResult::Exact(container),
            SearchResult::Partial(Object::Container(container), len) => {
                SearchResult::Partial(container, len)
            }
            _ => {
                // It wasn't a container, but it's parent will be, so try once more with parent
                let (parent_path, _) = path.split_tail();
                match self.resolve_absolute_path(&parent_path) {
                    SearchResult::Exact(Object::Container(container)) => {
                        SearchResult::Exact(container)
                    }
                    SearchResult::Partial(Object::Container(container), len) => {
                        SearchResult::Partial(container, len)
                    }
                    _ => panic!(
                        "couldn't find container parent {} of leaf {}",
                        self.path_to_string(&parent_path),
                        self.path_to_string(path)
                    ),
                }
            }
        }
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

impl ContainedNode for Object {
    fn node(&self) -> &Node {
        match self {
            Object::Container(v) => v.node(),
            Object::Value(v) => v.node(),
            Object::Divert(v) => v.node(),
            Object::Choice(v) => v.node(),
            Object::Variable(v) => v.node(),
            Object::Assignment(v) => v.node(),
            Object::Control(_, node)
            | Object::NativeCall(_, node)
            | Object::Tag(_, node)
            | Object::Glue(node)
            | Object::Void(node) => node,
        }
    }
}

impl Container {
    pub(crate) fn find_content_at_path<'story>(
        &'story self,
        story: &'story Story,
        path: &Path,
    ) -> SearchResult<'story, Object> {
        story.resolve_relative_path(self, path)
    }

    pub(crate) fn iter_index_children<'story>(
        &'story self,
        story: &'story Story,
    ) -> impl Iterator<Item = &'story Object> {
        let path = self.path();
        (0..self.num_index_children).into_iter().map(move |i| {
            story
                .get_object(&path.with_tail_component(PathComponent::Index(i)))
                .expect("story database is missing child object")
        })
    }

    pub(crate) fn iter_named_children<'story>(
        &'story self,
        story: &'story Story,
    ) -> impl Iterator<Item = &'story Object> {
        let path = self.path();
        self.named_children.iter().map(move |comp| {
            story
                .get_object(&path.with_tail_component(*comp))
                .expect("story database is missing child object")
        })
    }

    pub(crate) fn iter_children<'story>(
        &'story self,
        story: &'story Story,
    ) -> impl Iterator<Item = &'story Object> {
        self.iter_index_children(story)
            .chain(self.iter_named_children(story))
    }

    pub(crate) fn enumerate_children<'story>(
        &'story self,
        story: &'story Story,
    ) -> impl Iterator<Item = (PathComponent, &'story Object)> {
        let path = self.path();
        (0..self.num_index_children)
            .into_iter()
            .map(move |i| {
                (
                    PathComponent::Index(i),
                    story
                        .get_object(&path.with_tail_component(PathComponent::Index(i)))
                        .expect("story database is missing child object"),
                )
            })
            .chain(self.named_children.iter().map(move |comp| {
                (
                    *comp,
                    story
                        .get_object(&path.with_tail_component(*comp))
                        .expect("story database is missing child object"),
                )
            }))
    }
}

impl Default for Container {
    fn default() -> Self {
        Container {
            node: Node::default(),
            name: None,
            count_flags: CountFlags::default(),
            num_index_children: 0,
            named_children: Vec::default(),
        }
    }
}

impl ContainedNode for Container {
    fn node(&self) -> &Node {
        &self.node
    }
}

impl Node {
    fn new(path: Path) -> Self {
        debug_assert!(!path.is_relative());
        Node { path }
    }

    fn object<'story>(&self, story: &'story Story) -> &'story Object {
        story
            .get_object(&self.path)
            .expect("node does not have matching object path in story")
    }

    fn parent<'story>(&self, story: &'story Story) -> Option<&'story Container> {
        let (parent_path, _) = self.path.split_tail();
        if let Some(Object::Container(obj)) = story.get_object(&parent_path) {
            Some(obj)
        } else {
            None
        }
    }

    fn path(&self) -> &Path {
        &self.path
    }
}

impl Default for Node {
    fn default() -> Self {
        Node {
            path: Path::default(),
        }
    }
}

impl ContainedNode for Value {
    fn node(&self) -> &Node {
        match self {
            Value::List(v) => v.node(),
            Value::Int(_, node)
            | Value::Float(_, node)
            | Value::String(_, node)
            | Value::DivertTarget(_, node)
            | Value::VariablePointer(_, _, node) => node,
        }
    }
}

impl ContainedNode for List {
    fn node(&self) -> &Node {
        &self.node
    }
}

impl Default for ListDefinitionsMap {
    fn default() -> Self {
        ListDefinitionsMap {
            lists: HashMap::default(),
        }
    }
}

impl ContainedNode for Divert {
    fn node(&self) -> &Node {
        &self.node
    }
}

impl ContainedNode for ChoicePoint {
    fn node(&self) -> &Node {
        &self.node
    }
}

impl ContainedNode for VariableReference {
    fn node(&self) -> &Node {
        match self {
            VariableReference::Name(_, node) | VariableReference::Count(_, node) => node,
        }
    }
}

impl ContainedNode for VariableAssignment {
    fn node(&self) -> &Node {
        &self.node
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use anyhow::Result;
    use std::fs::File;

    #[test]
    fn read_json() -> Result<()> {
        let _story = Story::read_json(File::open("../examples/stories/TheIntercept.ink.json")?)?;
        Ok(())
    }

    #[test]
    fn write_json() -> Result<()> {
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
