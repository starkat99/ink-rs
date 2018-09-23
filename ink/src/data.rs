use super::{InternStr, Pointer, SearchResult, StringArena};
use bitflags::bitflags;
use encoding_rs_io::DecodeReaderBytes;
use failure::{Fail, Fallible};
use log::{error, warn};
use once_cell::unsync::OnceCell;
use std::{
    cell::{Ref, RefCell},
    collections::HashMap,
    io::{Read, Write},
    ptr,
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
    root: Box<Object>, // Will always be container
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
    children: Vec<Box<Object>>,
    named_children: HashMap<InternStr, Box<Object>>,
}

#[derive(Debug, Clone)]
pub(crate) struct Node {
    parent: *const Container,
    path: OnceCell<Path>,
}

pub(crate) trait ContainedNode {
    fn node(&self) -> &Node;
    fn node_mut(&mut self) -> &mut Node;

    fn parent(&self) -> Option<&Container> {
        self.node().parent()
    }

    fn path(&self) -> &Path {
        self.node().path()
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
        Ok(json::value_to_story(&value)?)
    }

    pub fn from_json_str(s: &str) -> Fallible<Self> {
        let value = serde_json::from_str(s)?;
        Ok(json::value_to_story(&value)?)
    }

    pub fn write_json<W: Write>(&self, writer: W) -> Fallible<()> {
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
        // Wish this wasn't necessary, but we need the Object wrapper around root
        if let Object::Container(root) = &*self.root {
            root
        } else {
            unreachable!()
        }
    }

    pub(crate) fn resolve_absolute_path<'story>(
        &'story self,
        path: &Path,
    ) -> SearchResult<'story, Object> {
        let mut current = &*self.root;
        for (partial_index, comp) in path.iter().enumerate() {
            if let Object::Container(container) = current {
                match comp {
                    PathComponent::Name(name) => {
                        if let Some(node) = container.named_children.get(name) {
                            current = &node;
                        } else {
                            // No child, so partial match with parent
                            return SearchResult::Partial(current, partial_index);
                        }
                    }
                    PathComponent::Index(index) => {
                        if let Some(node) = container.children.get(*index as usize) {
                            current = &node;
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

    fn node_mut(&mut self) -> &mut Node {
        match self {
            Object::Container(v) => v.node_mut(),
            Object::Value(v) => v.node_mut(),
            Object::Divert(v) => v.node_mut(),
            Object::Choice(v) => v.node_mut(),
            Object::Variable(v) => v.node_mut(),
            Object::Assignment(v) => v.node_mut(),
            Object::Control(_, node)
            | Object::NativeCall(_, node)
            | Object::Tag(_, node)
            | Object::Glue(node)
            | Object::Void(node) => node,
        }
    }
}

impl Container {
    fn set_child_parents(&mut self, parent: *const Container) {
        for child in &mut self.children {
            (&mut child.node_mut()).parent = parent;
        }
        for (_, child) in &mut self.named_children {
            (&mut child.node_mut()).parent = parent;
        }
    }

    fn get_component_of_node(&self, node: &Node) -> Option<PathComponent> {
        let node_ptr = node as *const Node;
        self.children
            .iter()
            .position(|child| child.node() as *const Node == node_ptr)
            .map(|i| PathComponent::Index(i as u32))
            .or_else(|| {
                self.named_children
                    .iter()
                    .find(|(_, child)| child.node() as *const Node == node_ptr)
                    .map(|(&name, _)| PathComponent::Name(name))
            })
    }
}

impl Default for Container {
    fn default() -> Self {
        Container {
            node: Node::new(),
            name: None,
            count_flags: CountFlags::default(),
            children: Vec::default(),
            named_children: HashMap::default(),
        }
    }
}

impl ContainedNode for Container {
    fn node(&self) -> &Node {
        &self.node
    }

    fn node_mut(&mut self) -> &mut Node {
        &mut self.node
    }
}

impl Node {
    fn new() -> Node {
        Node {
            parent: ptr::null(),
            path: OnceCell::new(),
        }
    }

    fn parent(&self) -> Option<&Container> {
        if self.parent != ptr::null() {
            Some(unsafe { &*self.parent })
        } else {
            None
        }
    }

    fn path(&self) -> &Path {
        self.path.get_or_init(|| {
            let mut child = self;
            let mut parent = self.parent();
            let mut path = Path::default();

            // Construct path from hierarchy
            while let Some(container) = parent {
                path.push_first(
                    container
                        .get_component_of_node(child)
                        .expect("child node was not found in parent collections"),
                );

                child = container.node();
                parent = child.parent();
            }
            path
        })
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

    fn node_mut(&mut self) -> &mut Node {
        match self {
            Value::List(v) => v.node_mut(),
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

    fn node_mut(&mut self) -> &mut Node {
        &mut self.node
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

    fn node_mut(&mut self) -> &mut Node {
        &mut self.node
    }
}

impl ContainedNode for ChoicePoint {
    fn node(&self) -> &Node {
        &self.node
    }

    fn node_mut(&mut self) -> &mut Node {
        &mut self.node
    }
}

impl ContainedNode for VariableReference {
    fn node(&self) -> &Node {
        match self {
            VariableReference::Name(_, node) | VariableReference::Count(_, node) => node,
        }
    }

    fn node_mut(&mut self) -> &mut Node {
        match self {
            VariableReference::Name(_, node) | VariableReference::Count(_, node) => node,
        }
    }
}

impl ContainedNode for VariableAssignment {
    fn node(&self) -> &Node {
        &self.node
    }

    fn node_mut(&mut self) -> &mut Node {
        &mut self.node
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
