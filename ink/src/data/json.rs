use super::super::{
    CallStack, CallStackElement, Choice, InternStr, Pointer, SearchResult, StoryState, StringArena,
    Thread, VariablesState,
};
use super::{
    ChoiceFlags, ChoicePoint, Container, ContainerNode, ControlCommand, CountFlags, Divert,
    DivertTarget,
    Error::*,
    List, ListDefinition, ListDefinitionsMap, ListItem, NativeFunction,
    Object::{self, *},
    Path, PushPopType, Story, Value, VariableAssignment, VariableReference, VariableScope,
};
use failure::{bail, ensure, Fallible};
use log::{trace, warn};
use serde_json::json;
use std::{borrow::Cow, cell::RefCell, collections::HashMap};

const CURRENT_FORMAT_VERSION: u64 = 19;
const MIN_FORMAT_VERSION: u64 = 18;

const CURRENT_SAVE_STATE_VERSION: u64 = 8;
const MIN_SAVE_STATE_VERSION: u64 = 8;

pub(crate) fn value_to_story(value: serde_json::Value) -> Fallible<Story> {
    let value = &value;
    // Check format version
    let version = get_u64(value, "inkVersion")
        .ok_or_else(|| InvalidJsonFormat("missing ink format version".into()))?;
    ensure!(
        version <= CURRENT_FORMAT_VERSION && version >= MIN_FORMAT_VERSION,
        UnsupportedVersion(version)
    );
    if version < CURRENT_FORMAT_VERSION {
        warn!(
            "ink format version {} is out of date; current version: {}",
            version, CURRENT_FORMAT_VERSION
        );
    }

    // Create string arena
    let mut string_arena = StringArena::new();

    // Deserialize root container and list definitions
    let root = Object::Container(value_to_container(
        require(value, "root")?,
        None,
        &mut string_arena,
    )?);
    let list_definitions = value
        .get("listDefs")
        .map(|v| value_to_list_definitions(v, &mut string_arena))
        .unwrap_or_else(|| Ok(ListDefinitionsMap::default()))?;

    trace!("deserialized ink JSON file");
    Ok(Story {
        root,
        list_definitions,
        string_arena: RefCell::new(string_arena),
    })
}

pub(crate) fn story_to_value(story: &Story) -> serde_json::Value {
    json!({
        "inkVersion": CURRENT_FORMAT_VERSION,
        "root": container_to_value(story.root(), story, true),
        "listDefs": list_definitions_to_value(&story.list_definitions, story),
     })
}

pub(crate) fn value_to_story_state<'story>(
    value: serde_json::Value,
    story: &'story Story,
) -> Fallible<StoryState<'story>> {
    let value = &value;
    let version = get_u64(value, "inkSaveVersion")
        .ok_or_else(|| InvalidJsonFormat("missing ink save format version".into()))?;
    ensure!(
        version <= CURRENT_SAVE_STATE_VERSION && version >= MIN_SAVE_STATE_VERSION,
        UnsupportedVersion(version)
    );
    if version < CURRENT_SAVE_STATE_VERSION {
        warn!(
            "ink save format version {} is out of date; current version: {}",
            version, CURRENT_SAVE_STATE_VERSION
        );
    }

    let current_turn_index = require_u32(value, "turnIdx")?;
    let story_seed = require_u32(value, "storySeed")?;
    let previous_random = require_u32(value, "previousRandom")?;

    let callstack = value_to_callstack(require(value, "callstackThreads")?, story)?;

    let variables_obj = require_object(value, "variablesState")?;
    let mut variables = HashMap::with_capacity(variables_obj.len());
    for (name, value) in variables_obj {
        variables.insert(
            story.intern_str(&name[..]),
            value_to_ink_object(value, None, &mut story.string_arena.borrow_mut())?,
        );
    }

    let stack_array = require_array(value, "evalStack")?;
    let mut evaluation_stack = Vec::with_capacity(stack_array.len());
    for value in stack_array {
        evaluation_stack.push(value_to_ink_object(
            value,
            None,
            &mut story.string_arena.borrow_mut(),
        )?);
    }

    let stream_array = require_array(value, "outputStream")?;
    let mut output_stream = Vec::with_capacity(stream_array.len());
    for value in stream_array {
        output_stream.push(value_to_ink_object(
            value,
            None,
            &mut story.string_arena.borrow_mut(),
        )?);
    }

    let diverted_pointer = get_path(
        value,
        "currentDivertTarget",
        &mut story.string_arena.borrow_mut(),
    ).map(|path| story.get_pointer_at_path(&path))
    .unwrap_or_default();

    let visits_obj = require_object(value, "visitCounts")?;
    let mut visit_counts = HashMap::with_capacity(visits_obj.len());
    for (name, value) in visits_obj {
        visit_counts.insert(story.intern_str(&name[..]), into_u32(value)?);
    }

    let turns_obj = require_object(value, "turnIndices")?;
    let mut turn_indices = HashMap::with_capacity(turns_obj.len());
    for (name, value) in turns_obj {
        turn_indices.insert(story.intern_str(&name[..]), into_u32(value)?);
    }

    let choice_threads = get_object(value, "choiceThreads");

    let choice_array = require_array(value, "currentChoices")?;
    let mut current_choices = Vec::with_capacity(choice_array.len());
    for value in choice_array {
        current_choices.push(value_to_choice(value, story, &callstack, choice_threads)?);
    }

    Ok(StoryState {
        story,
        current_turn_index,
        story_seed,
        previous_random,
        callstack,
        variables: VariablesState {
            global_vars: variables,
        },
        evaluation_stack,
        output_stream,
        diverted_pointer,
        visit_counts,
        turn_indices,
        current_choices,
    })
}

pub(crate) fn story_state_to_value(state: &StoryState) -> serde_json::Value {
    let mut value = json!({
        "inkFormatVersion": CURRENT_FORMAT_VERSION,
        "inkSaveVersion": CURRENT_SAVE_STATE_VERSION,
        "previousRandom": state.previous_random,
        "storySeed": state.story_seed,
        "turnIdx": state.current_turn_index,
        "evalStack": state.evaluation_stack.iter().map(|v| ink_object_to_value(v, state.story)).collect::<Vec<_>>(),
        "outputStream": state.output_stream.iter().map(|v| ink_object_to_value(v, state.story)).collect::<Vec<_>>(),
        "visitCounts": state.visit_counts.iter().map(|(name, value)| (state.story.resolve_str(*name).to_string(), value)).collect::<HashMap<_, _>>(),
        "turnIndices": state.turn_indices.iter().map(|(name, value)| (state.story.resolve_str(*name).to_string(), value)).collect::<HashMap<_, _>>(),
        "variablesState": state.variables.global_vars.iter().map(|(name, value)| (state.story.resolve_str(*name).to_string(), ink_object_to_value(value, state.story))).collect::<HashMap<_, _>>(),
        "currentChoices": state.current_choices.iter().map(|v| choice_to_value(v, state.story)).collect::<Vec<_>>(),
        "callstackThreads": callstack_to_value(&state.callstack, state.story),
    });

    let obj = value.as_object_mut().unwrap();

    let choice_threads = state
        .current_choices
        .iter()
        .filter_map(|c| {
            state
                .callstack
                .get_thread_with_index(c.thread_at_generation.index)
                .map(|_| {
                    (
                        c.thread_at_generation.index.to_string(),
                        thread_to_value(&c.thread_at_generation, state.story),
                    )
                })
        }).collect::<serde_json::Map<_, _>>();
    if !choice_threads.is_empty() {
        obj.insert("choiceThreads".into(), choice_threads.into());
    }

    if !state.diverted_pointer.is_null() {
        obj.insert(
            "currentDivertTarget".into(),
            state
                .story
                .path_to_string(&state.diverted_pointer.path())
                .into(),
        );
    }

    value
}

fn value_to_container(
    value: &serde_json::Value,
    name: Option<InternStr>,
    string_arena: &mut StringArena,
) -> Fallible<Container> {
    let array = into_array(value)?;
    if array.is_empty() {
        Ok(Container::default())
    } else {
        // Deserialize all unnamed contents, skipping the special last array element
        let mut content = Vec::with_capacity(array.len() - 1);
        for value in &array[..array.len() - 1] {
            content.push(ContainerNode::new(value_to_ink_object(
                value,
                None,
                string_arena,
            )?));
        }

        // The last container element has the named contents, as well as special container fields
        let maybe_last = &array[array.len() - 1].as_object();
        let mut named_only_content =
            HashMap::with_capacity(maybe_last.map_or(0, |last| last.len()));
        if let Some(last) = maybe_last {
            for (key, value) in last.iter() {
                if key == "#n" || key == "#f" {
                    continue;
                }
                let key = string_arena.get_or_intern(&key[..]);
                named_only_content.insert(
                    key,
                    ContainerNode::new(value_to_ink_object(value, Some(key), string_arena)?),
                );
            }
        }

        // Special container fields
        // Check for name field if none given from parent
        let name = name.or_else(|| {
            maybe_last
                .and_then(|last| last.get("#n"))
                .and_then(|v| v.as_str())
                .map(|v| string_arena.get_or_intern(v))
        });
        let count_flags = maybe_last
            .and_then(|last| last.get("#f"))
            .and_then(|v| v.as_u64())
            .map(|n| n as u32)
            .and_then(CountFlags::from_bits)
            .unwrap_or_else(CountFlags::default);
        Ok(Container {
            name,
            count_flags,
            content,
            named_only_content,
        })
    }
}

fn container_to_value(
    container: &Container,
    story: &Story,
    serialize_name: bool,
) -> serde_json::Value {
    // Serialize all unnamed content first as array
    let mut array: Vec<_> = container
        .content
        .iter()
        .map(|v| ink_object_to_value(&v.object, story))
        .collect();

    // Only serialize final named only contents if needed, otherwise a null
    if !container.named_only_content.is_empty()
        || (serialize_name && container.name.is_some())
        || !container.count_flags.is_empty()
    {
        let mut last: serde_json::Map<_, _> = container
            .named_only_content
            .iter()
            .map(|(key, content)| {
                (
                    story.resolve_str(*key).to_string(),
                    ink_object_to_value(&content.object, story),
                )
            }).collect();

        if serialize_name {
            if let Some(name) = &container.name {
                last.insert(
                    "#n".to_string(),
                    story.resolve_str(*name).to_string().into(),
                );
            }
        }
        if !container.count_flags.is_empty() {
            last.insert("#f".to_string(), container.count_flags.bits().into());
        }

        array.push(last.into())
    } else {
        array.push(json!(null));
    }

    array.into()
}

fn json_object_to_ink_object(
    value: &serde_json::Value,
    string_arena: &mut StringArena,
) -> Fallible<Object> {
    // Not really an easy way to do this
    Ok(
        // DivertTarget
        if let Some(path) = get_path(value, "^->", string_arena) {
            Value(Value::DivertTarget(path))
        }
        // VariablePointer
        else if let Some(var) = get_str(value, "^var") {
            let scope = match get_i64(value, "ci") {
                Some(0) => VariableScope::Global,
                Some(context) => VariableScope::Callstack((context - 1) as u32),
                _ => VariableScope::Unknown,
            };
            Value(Value::VariablePointer(
                string_arena.get_or_intern(var),
                scope,
            ))
        }
        // Divert
        else if let Some(target_str) = get_str(value, "->")
            .or_else(|| get_str(value, "f()"))
            .or_else(|| get_str(value, "x()"))
            .or_else(|| get_str(value, "->t->"))
        {
            let tunnel = value.get("->t->").is_some();
            let pushes_to_stack = value.get("f()").is_some() || tunnel;
            let stack_push_type = if tunnel {
                PushPopType::Tunnel
            } else {
                PushPopType::Function
            };
            let external = value.get("x()").is_some();
            let conditional = value.get("c").is_some();
            let external_args = Some(external)
                .filter(|&v| v)
                .and_then(|_| get_u32(value, "exArgs"))
                .unwrap_or(0);
            let target = if value.get("var").is_some() {
                DivertTarget::Variable(string_arena.get_or_intern(target_str))
            } else {
                DivertTarget::Path(Path::from_str(target_str, string_arena))
            };
            Divert(Divert {
                target,
                pushes_to_stack,
                stack_push_type,
                external,
                external_args,
                conditional,
            })
        }
        // Choice
        else if let Some(path_on_choice) = get_path(value, "*", string_arena) {
            let flags = get_u32(value, "flg")
                .and_then(ChoiceFlags::from_bits)
                .unwrap_or(ChoiceFlags::default());
            Choice(ChoicePoint {
                path_on_choice,
                flags,
            })
        }
        // Variable References
        else if let Some(name) = get_str(value, "VAR?") {
            Variable(VariableReference::Name(string_arena.get_or_intern(name)))
        } else if let Some(path) = get_path(value, "CNT?", string_arena) {
            Variable(VariableReference::Count(path))
        }
        // Variable assignment
        else if let Some(name) = get_str(value, "VAR=").or_else(|| get_str(value, "temp=")) {
            let global = value.get("VAR=").is_some();
            let new_declaration = value.get("re").is_none();
            Assignment(VariableAssignment {
                name: string_arena.get_or_intern(name),
                new_declaration,
                global,
            })
        }
        // Tag
        else if let Some(text) = get_str(value, "#") {
            Tag(string_arena.get_or_intern(text))
        }
        // List
        else if let Some(map) = get_object(value, "list") {
            let origin_names = get_array(value, "origins").map(|values| {
                values
                    .iter()
                    .filter_map(|v| v.as_str())
                    .map(|v| string_arena.get_or_intern(v))
                    .collect()
            });
            let content = map
                .iter()
                .map(|(key, val)| {
                    (
                        ListItem(string_arena.get_or_intern(&key[..])),
                        val.as_i64().unwrap_or(0) as i32,
                    )
                }).collect();
            Value(Value::List(List {
                content,
                origin_names,
            }))
        } else {
            bail!(InvalidJsonFormat(format!(
                "unrecognized object value '{}'",
                value
            )))
        },
    )
}

fn value_to_ink_object(
    value: &serde_json::Value,
    name: Option<InternStr>,
    string_arena: &mut StringArena,
) -> Fallible<Object> {
    use serde_json::Value::*;
    Ok(match value {
        Number(n) => Value(
            n.as_i64()
                .map(|i| Value::Int(i as i32))
                .or(n.as_f64().map(|f| Value::Float(f as f32)))
                .ok_or_else(|| InvalidJsonFormat(format!("invalid number value '{}'", value)))?,
        ),
        Array(_) => Container(value_to_container(value, name, string_arena)?),
        Object(_) => json_object_to_ink_object(value, string_arena)?,

        // Regular string values
        String(s) if s.starts_with("^") => {
            Value(Value::String(string_arena.get_or_intern(&s[1..])))
        }
        String(s) if s == "\n" => Value(Value::String(string_arena.get_or_intern("\n"))),

        // TODO Could match the strings faster with hash tables
        // Control commands
        String(s) if s == "ev" => Control(ControlCommand::EvalStart),
        String(s) if s == "out" => Control(ControlCommand::EvalOutput),
        String(s) if s == "/ev" => Control(ControlCommand::EvalEnd),
        String(s) if s == "du" => Control(ControlCommand::Duplicate),
        String(s) if s == "pop" => Control(ControlCommand::PopEvaluatedValue),
        String(s) if s == "~ret" => Control(ControlCommand::PopFunction),
        String(s) if s == "->->" => Control(ControlCommand::PopTunnel),
        String(s) if s == "str" => Control(ControlCommand::BeginString),
        String(s) if s == "/str" => Control(ControlCommand::EndString),
        String(s) if s == "nop" => Control(ControlCommand::NoOp),
        String(s) if s == "choiceCnt" => Control(ControlCommand::ChoiceCount),
        String(s) if s == "turn" => Control(ControlCommand::Turns),
        String(s) if s == "turns" => Control(ControlCommand::TurnsSince),
        String(s) if s == "readc" => Control(ControlCommand::ReadCount),
        String(s) if s == "rnd" => Control(ControlCommand::Random),
        String(s) if s == "srnd" => Control(ControlCommand::SeedRandom),
        String(s) if s == "visit" => Control(ControlCommand::VisitIndex),
        String(s) if s == "seq" => Control(ControlCommand::SequenceShuffleIndex),
        String(s) if s == "thread" => Control(ControlCommand::StartThread),
        String(s) if s == "done" => Control(ControlCommand::Done),
        String(s) if s == "end" => Control(ControlCommand::End),
        String(s) if s == "listInt" => Control(ControlCommand::ListFromInt),
        String(s) if s == "range" => Control(ControlCommand::ListRange),
        String(s) if s == "lrnd" => Control(ControlCommand::ListRandom),

        // Native function calls
        String(s) if s == "+" => NativeCall(NativeFunction::Add),
        String(s) if s == "-" => NativeCall(NativeFunction::Subtract),
        String(s) if s == "/" => NativeCall(NativeFunction::Divide),
        String(s) if s == "*" => NativeCall(NativeFunction::Multiply),
        String(s) if s == "%" => NativeCall(NativeFunction::Modulo),
        String(s) if s == "_" => NativeCall(NativeFunction::Negate),
        String(s) if s == "==" => NativeCall(NativeFunction::Equal),
        String(s) if s == ">" => NativeCall(NativeFunction::Greater),
        String(s) if s == "<" => NativeCall(NativeFunction::Less),
        String(s) if s == ">=" => NativeCall(NativeFunction::GreaterOrEqual),
        String(s) if s == "<=" => NativeCall(NativeFunction::LessOrEqual),
        String(s) if s == "!=" => NativeCall(NativeFunction::NotEqual),
        String(s) if s == "!" => NativeCall(NativeFunction::Not),
        String(s) if s == "&&" => NativeCall(NativeFunction::And),
        String(s) if s == "||" => NativeCall(NativeFunction::Or),
        String(s) if s == "MIN" => NativeCall(NativeFunction::Min),
        String(s) if s == "MAX" => NativeCall(NativeFunction::Max),
        String(s) if s == "POW" => NativeCall(NativeFunction::Power),
        String(s) if s == "FLOOR" => NativeCall(NativeFunction::Floor),
        String(s) if s == "CEILING" => NativeCall(NativeFunction::Ceiling),
        String(s) if s == "INT" => NativeCall(NativeFunction::Int),
        String(s) if s == "FLOAT" => NativeCall(NativeFunction::Float),
        String(s) if s == "?" => NativeCall(NativeFunction::Has),
        String(s) if s == "!?" => NativeCall(NativeFunction::HasNot),
        String(s) if s == "L^" => NativeCall(NativeFunction::Intersect),
        String(s) if s == "LIST_MIN" => NativeCall(NativeFunction::ListMin),
        String(s) if s == "LIST_MAX" => NativeCall(NativeFunction::ListMax),
        String(s) if s == "LIST_ALL" => NativeCall(NativeFunction::All),
        String(s) if s == "LIST_COUNT" => NativeCall(NativeFunction::Count),
        String(s) if s == "LIST_VALUE" => NativeCall(NativeFunction::ValueOfList),
        String(s) if s == "LIST_INVERT" => NativeCall(NativeFunction::Invert),

        // Misc
        String(s) if s == "<>" => Glue,
        String(s) if s == "void" => Void,
        String(s) => bail!(InvalidJsonFormat(format!(
            "unrecognized string value '{}'",
            s
        ))),
        Bool(_) => bail!(InvalidJsonFormat("unexpected boolean value".into())),
        Null => bail!(InvalidJsonFormat("unexpected null value".into())),
    })
}

fn ink_object_to_value(obj: &Object, story: &Story) -> serde_json::Value {
    match obj {
        Glue => json!("<>"),
        Void => json!("void"),
        Tag(text) => json!({ "#": story.resolve_str(*text).to_string() }),
        Variable(VariableReference::Name(name)) => {
            json!({ "VAR?": story.resolve_str(*name).to_string() })
        }
        Variable(VariableReference::Count(path)) => json!({"CNT?": story.path_to_string(path)}),
        Choice(choice) => {
            json!({"*": story.path_to_string(&choice.path_on_choice), "flg": choice.flags.bits()})
        }
        Container(container) => container_to_value(container, story, false),
        Assignment(var_assign) => {
            let mut value = json!({
                if var_assign.global {
                    "VAR="
                } else {
                    "temp="
                }
                : story.resolve_str(var_assign.name).to_string()
            });

            if var_assign.new_declaration {
                let obj = value.as_object_mut().unwrap();
                obj.insert("re".to_string(), json!(true));
            }

            value
        }
        Divert(divert) => {
            let mut value = json!({
                match divert {
                    Divert { external: true, ..} => "x()",
                    // FunctionEvaluation is only for callstack
                    Divert { pushes_to_stack: true, stack_push_type: PushPopType::Function, .. } => "f()",
                    Divert { pushes_to_stack: true, stack_push_type: PushPopType::Tunnel, .. } => "->t->",
                    _ => "->",
                }
                :

                match &divert.target {
                    DivertTarget::Path(path) => story.path_to_string(path),
                    DivertTarget::Variable(name) => story.resolve_str(*name).to_string(),
                }
            });

            let obj = value.as_object_mut().unwrap();
            if let DivertTarget::Variable(_) = &divert.target {
                obj.insert("var".to_string(), json!(true));
            }
            if divert.conditional {
                obj.insert("c".to_string(), json!(true));
            }
            if divert.external_args > 0 {
                obj.insert("exArgs".to_string(), json!(divert.external_args));
            }

            value
        }

        // Values
        Value(Value::Int(n)) => json!(n),
        Value(Value::Float(n)) => json!(n),
        Value(Value::String(s)) if &story.resolve_str(*s)[..] == "\n" => json!("\n"),
        Value(Value::String(s)) => json!(format!("^{}", story.resolve_str(*s))),
        Value(Value::DivertTarget(path)) => json!({"^->": story.path_to_string(path)}),
        Value(Value::VariablePointer(name, scope)) => json!({
            "^var": story.resolve_str(*name).to_string(),
            "ci": match scope {
                VariableScope::Unknown => -1,
                VariableScope::Global => 0,
                VariableScope::Callstack(n) => (*n as i64) + 1,
        }}),
        Value(Value::List(list)) => {
            let map: serde_json::Map<_, _> = list
                .content
                .iter()
                .map(|(key, val)| (story.resolve_str(key.0).to_string(), val.clone().into()))
                .collect();
            let mut value = json!({ "list": map });

            if let Some(names) = &list.origin_names {
                let obj = value.as_object_mut().unwrap();
                let names: Vec<serde_json::Value> = names
                    .iter()
                    .map(|s| story.resolve_str(*s).to_string().into())
                    .collect();
                obj.insert("origins".to_string(), names.into());
            }
            value
        }

        // Control commands
        Control(ControlCommand::EvalStart) => json!("ev"),
        Control(ControlCommand::EvalOutput) => json!("out"),
        Control(ControlCommand::EvalEnd) => json!("/ev"),
        Control(ControlCommand::Duplicate) => json!("du"),
        Control(ControlCommand::PopEvaluatedValue) => json!("pop"),
        Control(ControlCommand::PopFunction) => json!("~ret"),
        Control(ControlCommand::PopTunnel) => json!("->->"),
        Control(ControlCommand::BeginString) => json!("str"),
        Control(ControlCommand::EndString) => json!("/str"),
        Control(ControlCommand::NoOp) => json!("nop"),
        Control(ControlCommand::ChoiceCount) => json!("choiceCnt"),
        Control(ControlCommand::Turns) => json!("turn"),
        Control(ControlCommand::TurnsSince) => json!("turns"),
        Control(ControlCommand::ReadCount) => json!("readc"),
        Control(ControlCommand::Random) => json!("rnd"),
        Control(ControlCommand::SeedRandom) => json!("srnd"),
        Control(ControlCommand::VisitIndex) => json!("visit"),
        Control(ControlCommand::SequenceShuffleIndex) => json!("seq"),
        Control(ControlCommand::StartThread) => json!("thread"),
        Control(ControlCommand::Done) => json!("done"),
        Control(ControlCommand::End) => json!("end"),
        Control(ControlCommand::ListFromInt) => json!("listInt"),
        Control(ControlCommand::ListRange) => json!("range"),
        Control(ControlCommand::ListRandom) => json!("lrnd"),

        // Native function calls
        NativeCall(NativeFunction::Add) => json!("+"),
        NativeCall(NativeFunction::Subtract) => json!("-"),
        NativeCall(NativeFunction::Divide) => json!("/"),
        NativeCall(NativeFunction::Multiply) => json!("*"),
        NativeCall(NativeFunction::Modulo) => json!("%"),
        NativeCall(NativeFunction::Negate) => json!("_"),
        NativeCall(NativeFunction::Equal) => json!("=="),
        NativeCall(NativeFunction::Greater) => json!(">"),
        NativeCall(NativeFunction::Less) => json!("<"),
        NativeCall(NativeFunction::GreaterOrEqual) => json!(">="),
        NativeCall(NativeFunction::LessOrEqual) => json!("<="),
        NativeCall(NativeFunction::NotEqual) => json!("!="),
        NativeCall(NativeFunction::Not) => json!("!"),
        NativeCall(NativeFunction::And) => json!("&&"),
        NativeCall(NativeFunction::Or) => json!("||"),
        NativeCall(NativeFunction::Min) => json!("MIN"),
        NativeCall(NativeFunction::Max) => json!("MAX"),
        NativeCall(NativeFunction::Power) => json!("POW"),
        NativeCall(NativeFunction::Floor) => json!("FLOOR"),
        NativeCall(NativeFunction::Ceiling) => json!("CEILING"),
        NativeCall(NativeFunction::Int) => json!("INT"),
        NativeCall(NativeFunction::Float) => json!("FLOAT"),
        NativeCall(NativeFunction::Has) => json!("?"),
        NativeCall(NativeFunction::HasNot) => json!("!?"),
        NativeCall(NativeFunction::Intersect) => json!("L^"),
        NativeCall(NativeFunction::ListMin) => json!("LIST_MIN"),
        NativeCall(NativeFunction::ListMax) => json!("LIST_MAX"),
        NativeCall(NativeFunction::All) => json!("LIST_ALL"),
        NativeCall(NativeFunction::Count) => json!("LIST_COUNT"),
        NativeCall(NativeFunction::ValueOfList) => json!("LIST_VALUE"),
        NativeCall(NativeFunction::Invert) => json!("LIST_INVERT"),
    }
}

fn value_to_list_definitions(
    value: &serde_json::Value,
    string_arena: &mut StringArena,
) -> Fallible<ListDefinitionsMap> {
    if value.is_null() {
        Ok(ListDefinitionsMap::default())
    } else {
        let obj = into_object(value)?;

        // Deserialize named lists
        let mut defs = HashMap::with_capacity(obj.len());
        for (name, value) in obj {
            let map = into_object(value)?;

            // Deserialize list item value pairs
            let mut items = HashMap::with_capacity(map.len());
            for (key, val) in map {
                items.insert(
                    ListItem(string_arena.get_or_intern(&key[..])),
                    into_i32(val)?,
                );
            }

            let name = string_arena.get_or_intern(&name[..]);
            defs.insert(name, ListDefinition { name, items });
        }
        Ok(ListDefinitionsMap { lists: defs })
    }
}

fn list_definitions_to_value(list_defs: &ListDefinitionsMap, story: &Story) -> serde_json::Value {
    list_defs
        .lists
        .iter()
        .map(|(name, def)| {
            // Serialize list item value pairs
            let items: serde_json::Map<_, _> = def
                .items
                .iter()
                .map(|(key, val)| (story.resolve_str(key.0).to_string(), val.clone().into()))
                .collect();
            (story.resolve_str(*name).to_string(), items.into())
        }).collect::<serde_json::Map<_, _>>()
        .into()
}

fn value_to_callstack<'story>(
    value: &serde_json::Value,
    story: &'story Story,
) -> Fallible<CallStack<'story>> {
    let thread_counter = require_u32(value, "threadCounter")?;

    let thread_array = require_array(value, "threads")?;
    let mut threads = Vec::with_capacity(thread_array.len());
    for value in thread_array {
        threads.push(value_to_thread(value, story)?);
    }

    Ok(CallStack {
        thread_counter,
        threads,
    })
}

fn callstack_to_value(callstack: &CallStack, story: &Story) -> serde_json::Value {
    json!({
        "threadCounter": callstack.thread_counter,
        "threads": callstack.threads.iter().map(|v| thread_to_value(v, story)).collect::<Vec<_>>(),
    })
}

fn value_to_thread<'story>(
    value: &serde_json::Value,
    story: &'story Story,
) -> Fallible<Thread<'story>> {
    let index = require_u32(value, "threadIndex")?;

    let previous_pointer = get_path(
        value,
        "previousContentObject",
        &mut story.string_arena.borrow_mut(),
    ).map(|path| story.get_pointer_at_path(&path))
    .unwrap_or_default();

    let callstack_array = require_array(value, "callstack")?;
    let mut callstack = Vec::with_capacity(callstack_array.len());
    for value in callstack_array {
        callstack.push(value_to_callstack_element(value, story)?);
    }

    Ok(Thread {
        index,
        previous_pointer,
        callstack,
    })
}

fn thread_to_value(thread: &Thread, story: &Story) -> serde_json::Value {
    let mut value = json!({
        "threadIndex": thread.index,
        "callstack": thread.callstack.iter().map(|v| callstack_element_to_value(v, story)).collect::<Vec<_>>(),
    });

    if !thread.previous_pointer.is_null() {
        value.as_object_mut().unwrap().insert(
            "previousContentObject".into(),
            story.path_to_string(&thread.previous_pointer.path()).into(),
        );
    }

    value
}

fn value_to_callstack_element<'story>(
    value: &serde_json::Value,
    story: &'story Story,
) -> Fallible<CallStackElement<'story>> {
    let stack_type = get_u64(value, "type")
        .and_then(|v| match v {
            0 => Some(PushPopType::Tunnel),
            1 => Some(PushPopType::Function),
            2 => Some(PushPopType::FunctionEvaluation),
            _ => None,
        }).ok_or_else(|| InvalidJsonFormat("missing callstack element type value".into()))?;

    let pointer = if let Some(path) = get_path(value, "cPath", &mut story.string_arena.borrow_mut())
    {
        let pointer_index = require_u32(value, "idx")?;

        match story.get_container_at_path(&path) {
            SearchResult::Exact(c) => Pointer::new(c, Some(pointer_index)),
            SearchResult::Partial(c, _) => {
                warn!(
                    "exact story path '{}' not found, approximated to '{}' to recover",
                    story.path_to_string(&path),
                    story.path_to_string(&c.path())
                );
                Pointer::new(c, Some(pointer_index))
            }
        }
    } else {
        Pointer::default()
    };

    let in_expression_evaluation = require_bool(value, "exp")?;

    let temp_vars_obj = require_object(value, "temp")?;
    let mut temp_vars = HashMap::with_capacity(temp_vars_obj.len());
    for (name, value) in temp_vars_obj {
        temp_vars.insert(
            story.intern_str(&name[..]),
            value_to_ink_object(value, None, &mut story.string_arena.borrow_mut())?,
        );
    }

    Ok(CallStackElement {
        stack_type,
        pointer,
        in_expression_evaluation,
        temp_vars,
    })
}

fn callstack_element_to_value(element: &CallStackElement, story: &Story) -> serde_json::Value {
    let mut value = json!({
        "exp": element.in_expression_evaluation,
        "type": match element.stack_type {
            PushPopType::Tunnel => 0,
            PushPopType::Function => 1,
            PushPopType::FunctionEvaluation => 2,
        },
        "temp": element.temp_vars.iter().map(|(name, value)| (story.resolve_str(*name).to_string(), ink_object_to_value(value, story))).collect::<HashMap<_, _>>(),
    });

    if !element.pointer.is_null() {
        let obj = value.as_object_mut().unwrap();
        obj.insert(
            "cPath".into(),
            story
                .path_to_string(&element.pointer.container.unwrap().path())
                .into(),
        );
        obj.insert("idx".into(), element.pointer.index.unwrap().into());
    }

    value
}

fn value_to_choice<'story>(
    value: &serde_json::Value,
    story: &'story Story,
    callstack: &CallStack<'story>,
    choice_threads: Option<&serde_json::Map<String, serde_json::Value>>,
) -> Fallible<Choice<'story>> {
    let text = require_str(value, "text")?;
    let index = require_u32(value, "index")?;
    let source_path = require_path(
        value,
        "originalChoicePath",
        &mut story.string_arena.borrow_mut(),
    )?;
    let target_path = require_path(value, "targetPath", &mut story.string_arena.borrow_mut())?;
    let original_thread_index = require_u32(value, "originalThreadIndex")?;

    let thread_at_generation =
        if let Some(thread) = callstack.get_thread_with_index(original_thread_index) {
            Cow::Borrowed(thread)
        } else {
            Cow::Owned(value_to_thread(
                choice_threads
                    .and_then(|threads| threads.get(&original_thread_index.to_string()))
                    .ok_or_else(|| InvalidJsonFormat("expected choice thread object".into()))?,
                story,
            )?)
        };

    Ok(Choice {
        text: story.intern_str(text),
        index,
        source_path,
        target_path,
        original_thread_index,
        thread_at_generation,
    })
}

fn choice_to_value(choice: &Choice, story: &Story) -> serde_json::Value {
    json!({
        "text": story.resolve_str(choice.text).to_string(),
        "index": choice.index,
        "originalChoicePath": story.path_to_string(&choice.source_path),
        "originalThreadIndex": choice.original_thread_index,
        "targetPath": story.path_to_string(&choice.target_path),
    })
}

// Helpers
fn require<'a>(value: &'a serde_json::Value, key: &str) -> Fallible<&'a serde_json::Value> {
    value
        .get(key)
        .ok_or_else(|| InvalidJsonFormat(format!("missing '{}'", key)).into())
}

fn get_i64(value: &serde_json::Value, key: &str) -> Option<i64> {
    value.get(key).and_then(|v| v.as_i64())
}

fn into_i64(value: &serde_json::Value) -> Fallible<i64> {
    value
        .as_i64()
        .ok_or_else(|| InvalidJsonFormat(format!("expected integer value, got '{}'", value)).into())
}

fn into_i32(value: &serde_json::Value) -> Fallible<i32> {
    Ok(into_i64(value)? as i32)
}

fn get_u64(value: &serde_json::Value, key: &str) -> Option<u64> {
    value.get(key).and_then(|v| v.as_u64())
}

fn require_u64(value: &serde_json::Value, key: &str) -> Fallible<u64> {
    get_u64(value, key).ok_or_else(|| {
        InvalidJsonFormat(format!("expected unsigned integer value for '{}'", key)).into()
    })
}

fn into_u64(value: &serde_json::Value) -> Fallible<u64> {
    value.as_u64().ok_or_else(|| {
        InvalidJsonFormat(format!("expected unsigned integer value, got '{}'", value)).into()
    })
}

fn get_u32(value: &serde_json::Value, key: &str) -> Option<u32> {
    get_u64(value, key).map(|v| v as u32)
}

fn require_u32(value: &serde_json::Value, key: &str) -> Fallible<u32> {
    Ok(require_u64(value, key)? as u32)
}

fn into_u32(value: &serde_json::Value) -> Fallible<u32> {
    Ok(into_u64(value)? as u32)
}

fn get_str<'a>(value: &'a serde_json::Value, key: &str) -> Option<&'a str> {
    value.get(key).and_then(|v| v.as_str())
}

fn require_str<'a>(value: &'a serde_json::Value, key: &str) -> Fallible<&'a str> {
    get_str(value, key)
        .ok_or_else(|| InvalidJsonFormat(format!("expected string value for '{}'", key)).into())
}

fn get_path(value: &serde_json::Value, key: &str, string_arena: &mut StringArena) -> Option<Path> {
    get_str(value, key).map(|s| Path::from_str(s, string_arena))
}

fn require_path(
    value: &serde_json::Value,
    key: &str,
    string_arena: &mut StringArena,
) -> Fallible<Path> {
    get_path(value, key, string_arena).ok_or_else(|| {
        InvalidJsonFormat(format!("expected path string value for '{}'", key)).into()
    })
}

fn get_object<'a>(
    value: &'a serde_json::Value,
    key: &str,
) -> Option<&'a serde_json::Map<String, serde_json::Value>> {
    value.get(key).and_then(|v| v.as_object())
}

fn require_object<'a>(
    value: &'a serde_json::Value,
    key: &str,
) -> Fallible<&'a serde_json::Map<String, serde_json::Value>> {
    get_object(value, key)
        .ok_or_else(|| InvalidJsonFormat(format!("expected object value for '{}'", key)).into())
}

fn into_object(value: &serde_json::Value) -> Fallible<&serde_json::Map<String, serde_json::Value>> {
    value
        .as_object()
        .ok_or_else(|| InvalidJsonFormat(format!("expected object value, got '{}'", value)).into())
}

fn get_array<'a>(value: &'a serde_json::Value, key: &str) -> Option<&'a Vec<serde_json::Value>> {
    value.get(key).and_then(|v| v.as_array())
}

fn require_array<'a>(
    value: &'a serde_json::Value,
    key: &str,
) -> Fallible<&'a Vec<serde_json::Value>> {
    get_array(value, key)
        .ok_or_else(|| InvalidJsonFormat(format!("expected array value for '{}'", key)).into())
}

fn into_array(value: &serde_json::Value) -> Fallible<&Vec<serde_json::Value>> {
    value
        .as_array()
        .ok_or_else(|| InvalidJsonFormat(format!("expected array value, got '{}'", value)).into())
}

fn get_bool(value: &serde_json::Value, key: &str) -> Option<bool> {
    value.get(key).and_then(|v| v.as_bool())
}

fn require_bool(value: &serde_json::Value, key: &str) -> Fallible<bool> {
    get_bool(value, key)
        .ok_or_else(|| InvalidJsonFormat(format!("expected boolean value for '{}'", key)).into())
}
