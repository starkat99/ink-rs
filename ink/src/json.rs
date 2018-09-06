use super::{
    ChoiceFlags, ChoicePoint, Container, ControlCommand, CountFlags, Divert, DivertTarget,
    Error::*,
    List, ListItem, NativeFunction,
    Object::{self, *},
    Path, PushPopType, Story, Value, VariableAssignment, VariableReference, VariableScope,
};
use failure::{bail, ensure, Fallible};
use internship::IStr;
use log::{trace, warn};
use serde_json::json;
use std::collections::HashMap;

const CURRENT_FORMAT_VERSION: u64 = 19;
const MIN_FORMAT_VERSION: u64 = 18;

pub(crate) fn value_to_story(value: serde_json::Value) -> Fallible<Story> {
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

    let root = value_to_container(
        value
            .get("root")
            .ok_or(InvalidJsonFormat("missing root node"))?,
        None,
    )?;
    trace!("deserialized ink JSON file");
    Ok(Story { root })
}

pub(crate) fn story_to_value(_story: &Story) -> Fallible<serde_json::Value> {
    let value = json!({ "inkVersion": CURRENT_FORMAT_VERSION });
    Ok(value)
}

pub(crate) fn value_to_container(
    value: &serde_json::Value,
    name: Option<&str>,
) -> Fallible<Container> {
    let array = value
        .as_array()
        .ok_or(InvalidJsonFormat("expected container array"))?;
    if array.is_empty() {
        Ok(Container::default())
    } else {
        // Deserialize all unnamed contents, skipping the special last array element
        let mut content = Vec::<Object>::with_capacity(array.len() - 1);
        for value in &array[..array.len() - 1] {
            content.push(value_to_ink_object(value, None)?);
        }

        // The last container element has the named contents, as well as special container fields
        let maybe_last = &array[array.len() - 1].as_object();
        let mut named_only_content =
            HashMap::<IStr, Object>::with_capacity(maybe_last.map_or(0, |last| last.len()));
        if let Some(last) = maybe_last {
            for (key, value) in last.iter() {
                if key == "#n" || key == "#f" {
                    continue;
                }
                named_only_content.insert(key[..].into(), value_to_ink_object(value, Some(key))?);
            }
        }

        // Special container fields
        // Check for name field if none given from parent
        let name = name
            .or_else(|| {
                maybe_last
                    .and_then(|last| last.get("#n"))
                    .and_then(|v| v.as_str())
            }).map(|s| s.into());
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
            let mut content: HashMap<ListItem, i32> = HashMap::with_capacity(map.len());
            for (key, val) in map {
                content.insert(ListItem(key[..].into()), val.as_i64().unwrap_or(0) as i32);
            }
            Value(Value::List(List {
                content,
                origin_names,
            }))
        } else {
            bail!(InvalidJsonFormat("unrecognized object value"))
        },
    )
}

fn value_to_ink_object(value: &serde_json::Value, name: Option<&str>) -> Fallible<Object> {
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
