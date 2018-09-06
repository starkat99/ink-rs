use super::{
    ChoiceFlags, ChoicePoint, Container, ControlCommand, CountFlags, Divert, DivertTarget,
    Error::*,
    List, ListDefinition, ListDefinitionsMap, ListItem, NativeFunction,
    Object::{self, *},
    Path, PushPopType, Story, Value, VariableAssignment, VariableReference, VariableScope,
};
use failure::{bail, ensure, Fallible};
use internship::IStr;
use log::{trace, warn};
use serde_json::json;
use serde_json::Map;
use std::collections::HashMap;

const CURRENT_FORMAT_VERSION: u64 = 19;
const MIN_FORMAT_VERSION: u64 = 18;

pub(crate) fn value_to_story(value: serde_json::Value) -> Fallible<Story> {
    // Check format version
    let version = value
        .get("inkVersion")
        .and_then(|v| v.as_u64())
        .ok_or(InvalidJsonFormat("missing ink format version"))?;
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

    // Deserialize root container and list definitions
    let root = value_to_container(
        value
            .get("root")
            .ok_or(InvalidJsonFormat("missing root node"))?,
        None,
    )?;
    let list_definitions = value
        .get("listDefs")
        .map(value_to_list_definitions)
        .unwrap_or_else(|| Ok(ListDefinitionsMap::default()))?;

    trace!("deserialized ink JSON file");
    Ok(Story {
        root,
        list_definitions,
    })
}

pub(crate) fn story_to_value(story: &Story) -> serde_json::Value {
    json!({
        "inkVersion": CURRENT_FORMAT_VERSION,
        "root": container_to_value(&story.root, true),
        "listDefs": list_definitions_to_value(&story.list_definitions),
     })
}

fn value_to_container(value: &serde_json::Value, name: Option<IStr>) -> Fallible<Container> {
    let array = value
        .as_array()
        .ok_or(InvalidJsonFormat("expected container array"))?;
    if array.is_empty() {
        Ok(Container::default())
    } else {
        // Deserialize all unnamed contents, skipping the special last array element
        let mut content = Vec::with_capacity(array.len() - 1);
        for value in &array[..array.len() - 1] {
            content.push(value_to_ink_object(value, None)?);
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
                let key: IStr = key[..].into();
                named_only_content.insert(key.clone(), value_to_ink_object(value, Some(key))?);
            }
        }

        // Special container fields
        // Check for name field if none given from parent
        let name = name.or_else(|| {
            maybe_last
                .and_then(|last| last.get("#n"))
                .and_then(|v| v.as_str())
                .map(|v| v.into())
        });
        let count_flags = maybe_last
            .and_then(|last| last.get("#f"))
            .and_then(|v| v.as_i64())
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

fn container_to_value(container: &Container, serialize_name: bool) -> serde_json::Value {
    // Serialize all unnamed content first as array
    let mut array: Vec<_> = container.content.iter().map(ink_object_to_value).collect();

    // Only serialize final named only contents if needed, otherwise a null
    if !container.named_only_content.is_empty()
        || (serialize_name && container.name.is_some())
        || !container.count_flags.is_empty()
    {
        let mut last: Map<_, serde_json::Value> = container
            .named_only_content
            .iter()
            .map(|(key, content)| (key.to_string(), ink_object_to_value(content)))
            .collect();

        if serialize_name {
            if let Some(name) = &container.name {
                last.insert("#n".to_string(), name.to_string().into());
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

fn json_object_to_ink_object(obj: &serde_json::Map<String, serde_json::Value>) -> Fallible<Object> {
    // Not really an easy way to do this
    Ok(
        // DivertTarget
        if let Some(path) = obj.get("^->").and_then(|v| v.as_str()).map(Path::from_str) {
            Value(Value::DivertTarget(path))
        }
        // VariablePointer
        else if let Some(var) = obj.get("^var").and_then(|v| v.as_str()) {
            let scope = match obj.get("ci").and_then(|v| v.as_i64()) {
                Some(0) => VariableScope::Global,
                Some(context) => VariableScope::Callstack((context - 1) as u32),
                _ => VariableScope::Unknown,
            };
            Value(Value::VariablePointer(var.into(), scope))
        }
        // Divert
        else if let Some(target_str) = obj
            .get("->")
            .or_else(|| obj.get("f()"))
            .or_else(|| obj.get("x()"))
            .or_else(|| obj.get("->t->"))
            .and_then(|v| v.as_str())
        {
            let tunnel = obj.contains_key("->t->");
            let pushes_to_stack = obj.contains_key("f()") || tunnel;
            let stack_push_type = if tunnel {
                PushPopType::Tunnel
            } else {
                PushPopType::Function
            };
            let external = obj.contains_key("x()");
            let conditional = obj.contains_key("c");
            let external_args = Some(external)
                .filter(|&v| v)
                .and_then(|_| obj.get("exArgs"))
                .and_then(|v| v.as_u64())
                .unwrap_or(0) as u32;
            let target = if obj.contains_key("var") {
                DivertTarget::Variable(target_str.into())
            } else {
                DivertTarget::Path(Path::from_str(target_str))
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
        else if let Some(path_on_choice) =
            obj.get("*").and_then(|v| v.as_str()).map(Path::from_str)
        {
            let flags = obj
                .get("flg")
                .and_then(|v| v.as_u64())
                .map(|i| i as u32)
                .and_then(ChoiceFlags::from_bits)
                .unwrap_or(ChoiceFlags::default());
            Choice(ChoicePoint {
                path_on_choice,
                flags,
            })
        }
        // Variable References
        else if let Some(name) = obj.get("VAR?").and_then(|v| v.as_str()) {
            Variable(VariableReference::Name(name.into()))
        } else if let Some(path) = obj.get("CNT?").and_then(|v| v.as_str()).map(Path::from_str) {
            Variable(VariableReference::Count(path))
        }
        // Variable assignment
        else if let Some(name) = obj
            .get("VAR=")
            .or_else(|| obj.get("temp="))
            .and_then(|v| v.as_str())
        {
            let global = obj.contains_key("VAR=");
            let new_declaration = !obj.contains_key("re");
            Assignment(VariableAssignment {
                name: name.into(),
                new_declaration,
                global,
            })
        }
        // Tag
        else if let Some(text) = obj.get("#").and_then(|v| v.as_str()) {
            Tag(text.into())
        }
        // List
        else if let Some(map) = obj.get("list").and_then(|v| v.as_object()) {
            let origin_names = obj.get("origins").and_then(|v| v.as_array()).map(|values| {
                values
                    .iter()
                    .filter_map(|v| v.as_str())
                    .map(|v| v.into())
                    .collect()
            });
            let content = map
                .iter()
                .map(|(key, val)| (ListItem(key[..].into()), val.as_i64().unwrap_or(0) as i32))
                .collect();
            Value(Value::List(List {
                content,
                origin_names,
            }))
        } else {
            bail!(InvalidJsonFormat("unrecognized object value"))
        },
    )
}

fn value_to_ink_object(value: &serde_json::Value, name: Option<IStr>) -> Fallible<Object> {
    use serde_json::Value::*;
    Ok(match value {
        Number(n) => Value(
            n.as_i64()
                .map(|i| Value::Int(i as i32))
                .or(n.as_f64().map(|f| Value::Float(f as f32)))
                .ok_or(InvalidJsonFormat("invalid number value"))?,
        ),
        Array(_) => Container(value_to_container(value, name)?),
        Object(obj) => json_object_to_ink_object(obj)?,

        // Regular string values
        String(s) if s.starts_with("^") => Value(Value::String(s[1..].into())),
        String(s) if s == "\n" => Value(Value::String("\n".into())),

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
        String(_) => bail!(InvalidJsonFormat("unrecognized string value")),
        Bool(_) => bail!(InvalidJsonFormat("unexpected boolean value")),
        Null => bail!(InvalidJsonFormat("unexpected null value")),
    })
}

fn ink_object_to_value(obj: &Object) -> serde_json::Value {
    match obj {
        Glue => json!("<>"),
        Void => json!("void"),
        Tag(text) => json!({ "#": text }),
        Variable(VariableReference::Name(name)) => json!({ "VAR?": name }),
        Variable(VariableReference::Count(path)) => json!({"CNT?": path.to_string()}),
        Choice(choice) => {
            json!({"*": choice.path_on_choice.to_string(), "flg": choice.flags.bits()})
        }
        Container(container) => container_to_value(container, false),
        Assignment(var_assign) => {
            let mut value = json!({
                if var_assign.global {
                    "VAR="
                } else {
                    "temp="
                }
                : var_assign.name
            });

            let obj = value.as_object_mut().unwrap();
            if var_assign.new_declaration {
                obj.insert("re".to_string(), json!(true));
            }

            value
        }
        Divert(divert) => {
            let mut value = json!({
                match divert {
                    Divert { external: true, ..} => "x()",
                    Divert { pushes_to_stack: true, stack_push_type: PushPopType::Function, .. } => "f()",
                    Divert { pushes_to_stack: true, stack_push_type: PushPopType::Tunnel, .. } => "->t->",
                    _ => "->",
                }
                :

                match &divert.target {
                    DivertTarget::Path(path) => path.to_string(),
                    DivertTarget::Variable(name) => name.to_string(),
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
        Value(Value::String(s)) if s == "\n" => json!(s),
        Value(Value::String(s)) => json!(format!("^{}", s)),
        Value(Value::DivertTarget(path)) => json!({"^->": path.to_string()}),
        Value(Value::VariablePointer(name, scope)) => json!({"^var": name, "ci": match scope {
            VariableScope::Unknown => -1,
            VariableScope::Global => 0,
            VariableScope::Callstack(n) => (*n as i64) + 1,
        }}),
        Value(Value::List(list)) => {
            let map: Map<_, _> = list
                .content
                .iter()
                .map(|(key, val)| (key.0.to_string(), val.clone().into()))
                .collect();
            let mut value = json!({ "list": map });

            let obj = value.as_object_mut().unwrap();
            if let Some(names) = &list.origin_names {
                let names: Vec<serde_json::Value> =
                    names.iter().map(|s| s.to_string().into()).collect();
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

fn value_to_list_definitions(value: &serde_json::Value) -> Fallible<ListDefinitionsMap> {
    if value.is_null() {
        Ok(ListDefinitionsMap::default())
    } else {
        let obj = value
            .as_object()
            .ok_or(InvalidJsonFormat("expected list definitions object"))?;

        // Deserialize named lists
        let mut defs = HashMap::with_capacity(obj.len());
        for (name, value) in obj {
            let map = value
                .as_object()
                .ok_or(InvalidJsonFormat("expected list definition object"))?;

            // Deserialize list item value pairs
            let mut items = HashMap::with_capacity(map.len());
            for (key, val) in map {
                items.insert(
                    ListItem(key[..].into()),
                    val.as_i64()
                        .ok_or(InvalidJsonFormat("expected list item integer value"))?
                        as i32,
                );
            }

            let name: IStr = name[..].into();
            defs.insert(name.clone(), ListDefinition { name, items });
        }
        Ok(ListDefinitionsMap { lists: defs })
    }
}

fn list_definitions_to_value(list_defs: &ListDefinitionsMap) -> serde_json::Value {
    list_defs
        .lists
        .iter()
        .map(|(name, def)| {
            // Serialize list item value pairs
            let items: Map<_, serde_json::Value> = def
                .items
                .iter()
                .map(|(key, val)| (key.0.to_string(), val.clone().into()))
                .collect();
            (name.to_string(), items.into())
        }).collect::<serde_json::Map<_, serde_json::Value>>()
        .into()
}
