use super::super::{InternStr, StringArena};
use std::{borrow::Cow, slice::SliceIndex};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum PathComponent {
    Index(u32),
    Name(InternStr),
    Parent,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct Path {
    components: Vec<PathComponent>,
    relative: bool,
}

impl Path {
    fn push_element(components: &mut Vec<PathComponent>, comp: PathComponent) {
        if comp.is_parent() {
            // Normalize any parents
            match components.last() {
                Some(PathComponent::Index(_)) | Some(PathComponent::Name(_)) => {
                    components.pop();
                }
                _ => components.push(comp),
            }
        } else {
            components.push(comp)
        }
    }

    pub fn relative_self() -> Self {
        Path {
            components: Vec::default(),
            relative: true,
        }
    }

    pub fn from_str(path: &str, string_arena: &mut StringArena) -> Self {
        let relative = path.starts_with(".");
        let mut components: Vec<PathComponent> = Vec::new();
        for comp in path
            .split(".")
            .filter_map(|s| PathComponent::from_str(s, string_arena))
        {
            Self::push_element(&mut components, comp);
        }
        Path {
            components,
            relative,
        }
    }

    pub fn from_components(components: &[PathComponent], relative: bool) -> Self {
        let mut comps = Vec::with_capacity(components.len());
        for comp in components {
            Self::push_element(&mut comps, *comp);
        }
        Path {
            components: comps,
            relative,
        }
    }

    pub fn from_component(component: PathComponent, relative: bool) -> Self {
        let mut comps = Vec::with_capacity(1);
        Self::push_element(&mut comps, component);
        Path {
            components: comps,
            relative,
        }
    }

    pub fn is_relative(&self) -> bool {
        self.relative
    }

    pub fn contains_named_component(&self) -> bool {
        self.components.iter().any(|c| c.is_named())
    }

    pub fn contains_name(&self, name: InternStr) -> bool {
        self.components.iter().any(|c| match c {
            PathComponent::Name(s) => *s == name,
            _ => false,
        })
    }

    pub fn contains_name_str(&self, name: &str, string_arena: &StringArena) -> bool {
        self.components.iter().any(|c| match c {
            PathComponent::Name(s) => string_arena.resolve(*s) == Some(name),
            _ => false,
        })
    }

    pub fn len(&self) -> usize {
        self.components.len()
    }

    pub fn is_empty(&self) -> bool {
        self.components.is_empty()
    }

    pub fn first(&self) -> Option<&PathComponent> {
        self.components.first()
    }

    pub fn first_mut(&mut self) -> Option<&mut PathComponent> {
        self.components.first_mut()
    }

    pub fn last(&self) -> Option<&PathComponent> {
        self.components.last()
    }

    pub fn last_mut(&mut self) -> Option<&mut PathComponent> {
        self.components.last_mut()
    }

    pub fn split_head(&self) -> (Option<&PathComponent>, Path) {
        if let Some((first, tail)) = self.components.split_first() {
            (
                Some(first),
                Path {
                    components: tail.to_owned(),
                    relative: true,
                },
            )
        } else {
            (None, Path::relative_self())
        }
    }

    pub fn split_tail(&self) -> (Path, Option<&PathComponent>) {
        if let Some((last, head)) = self.components.split_last() {
            (
                Path {
                    components: head.to_owned(),
                    relative: true,
                },
                Some(last),
            )
        } else {
            (Path::relative_self(), None)
        }
    }

    pub fn as_slice(&self) -> &[PathComponent] {
        self.components.as_slice()
    }

    pub fn as_mut_slice(&mut self) -> &mut [PathComponent] {
        self.components.as_mut_slice()
    }

    pub fn get<I>(&self, index: I) -> Option<&<I as SliceIndex<[PathComponent]>>::Output>
    where
        I: SliceIndex<[PathComponent]>,
    {
        self.components.get(index)
    }

    pub fn get_mut<I>(
        &mut self,
        index: I,
    ) -> Option<&mut <I as SliceIndex<[PathComponent]>>::Output>
    where
        I: SliceIndex<[PathComponent]>,
    {
        self.components.get_mut(index)
    }

    pub fn iter(&self) -> impl Iterator<Item = &PathComponent> {
        self.components.iter()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut PathComponent> {
        self.components.iter_mut()
    }

    pub fn push(&mut self, component: PathComponent) {
        Self::push_element(&mut self.components, component);
    }

    pub fn with_tail(&self, tail: &[PathComponent]) -> Self {
        let mut path = self.clone();
        path.components.reserve(tail.len());
        for comp in tail {
            Self::push_element(&mut path.components, *comp);
        }
        path
    }

    pub fn with_head(&self, head: &[PathComponent]) -> Self {
        let mut comps = Vec::with_capacity(head.len() + self.len());
        for comp in head {
            Self::push_element(&mut comps, *comp);
        }
        for comp in &self.components {
            Self::push_element(&mut comps, *comp);
        }
        Path {
            components: comps,
            relative: self.relative,
        }
    }

    pub fn with_tail_component(&self, tail: PathComponent) -> Self {
        let mut path = self.clone();
        Self::push_element(&mut path.components, tail);
        path
    }

    pub fn with_head_component(&self, head: PathComponent) -> Self {
        let mut comps = Vec::with_capacity(self.len() + 1);
        Self::push_element(&mut comps, head);
        for comp in &self.components {
            Self::push_element(&mut comps, *comp);
        }
        Path {
            components: comps,
            relative: self.relative,
        }
    }

    pub fn join(&mut self, path: Path) {
        if path.relative {
            self.components.reserve(path.len());
            self.extend(path.components);
        } else {
            self.components = path.components;
            self.relative = false;
        }
    }

    pub fn with_joined(&self, path: Path) -> Self {
        if path.relative {
            let mut comps = Vec::with_capacity(self.len() + path.len());
            for comp in &self.components {
                Self::push_element(&mut comps, *comp);
            }
            for comp in &path.components {
                Self::push_element(&mut comps, *comp);
            }
            Path {
                components: comps,
                relative: self.relative,
            }
        } else {
            path
        }
    }

    pub fn to_string(&self, string_arena: &StringArena) -> Option<String> {
        let mut path = String::new();
        if self.relative {
            path.push('.');
        }
        for (i, comp) in self.components.iter().enumerate() {
            if i > 0 {
                path.push('.');
            }
            if let Some(s) = comp.to_string(string_arena) {
                path.push_str(&s);
            } else {
                return None;
            }
        }

        Some(path)
    }
}

impl From<Path> for Vec<PathComponent> {
    fn from(path: Path) -> Self {
        path.components
    }
}

impl From<PathComponent> for Path {
    fn from(component: PathComponent) -> Self {
        Path::from_component(component, false)
    }
}

impl Default for Path {
    fn default() -> Self {
        Path {
            components: Vec::default(),
            relative: false,
        }
    }
}

impl<I> std::ops::Index<I> for Path
where
    I: SliceIndex<[PathComponent]>,
{
    type Output = <I as SliceIndex<[PathComponent]>>::Output;

    fn index(&self, index: I) -> &<Path as std::ops::Index<I>>::Output {
        &self.components[index]
    }
}

impl<I> std::ops::IndexMut<I> for Path
where
    I: SliceIndex<[PathComponent]>,
{
    fn index_mut(&mut self, index: I) -> &mut <Path as std::ops::Index<I>>::Output {
        &mut self.components[index]
    }
}

impl IntoIterator for Path {
    type Item = PathComponent;
    type IntoIter = <Vec<PathComponent> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.components.into_iter()
    }
}

impl<'a> IntoIterator for &'a Path {
    type Item = &'a PathComponent;
    type IntoIter = <&'a Vec<PathComponent> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        (&self.components).into_iter()
    }
}

impl<'a> IntoIterator for &'a mut Path {
    type Item = &'a mut PathComponent;
    type IntoIter = <&'a mut Vec<PathComponent> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        (&mut self.components).into_iter()
    }
}

impl Extend<PathComponent> for Path {
    fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = PathComponent>,
    {
        for component in iter {
            self.push(component)
        }
    }
}

impl PathComponent {
    pub fn from_str(s: &str, string_arena: &mut StringArena) -> Option<PathComponent> {
        if let Ok(i) = s.parse::<u32>() {
            Some(PathComponent::Index(i))
        } else if s == "^" {
            Some(PathComponent::Parent)
        } else if !s.is_empty() && !s.contains(".") {
            Some(PathComponent::Name(string_arena.get_or_intern(s)))
        } else {
            None
        }
    }

    pub fn from_name(name: InternStr) -> PathComponent {
        PathComponent::Name(name)
    }

    pub fn from_u32(i: u32) -> PathComponent {
        PathComponent::Index(i)
    }

    pub fn parent() -> PathComponent {
        PathComponent::Parent
    }

    pub fn is_named(&self) -> bool {
        if let PathComponent::Name(_) = self {
            true
        } else {
            false
        }
    }

    pub fn as_name<'story>(&self, string_arena: &'story StringArena) -> Option<&'story str> {
        if let PathComponent::Name(s) = self {
            string_arena.resolve(*s)
        } else {
            None
        }
    }

    pub fn is_index(&self) -> bool {
        if let PathComponent::Index(_) = self {
            true
        } else {
            false
        }
    }

    pub fn as_index(&self) -> Option<u32> {
        if let PathComponent::Index(i) = self {
            Some(*i)
        } else {
            None
        }
    }

    pub fn is_parent(&self) -> bool {
        &PathComponent::Parent == self
    }

    pub fn to_string<'story>(&self, string_arena: &'story StringArena) -> Option<Cow<'story, str>> {
        match self {
            PathComponent::Name(s) => string_arena.resolve(*s).map(|s| s.into()),
            PathComponent::Index(i) => Some(i.to_string().into()),
            PathComponent::Parent => Some("^".into()),
        }
    }
}

impl From<u32> for PathComponent {
    fn from(from: u32) -> Self {
        Self::from_u32(from)
    }
}

impl From<InternStr> for PathComponent {
    fn from(from: InternStr) -> Self {
        Self::from_name(from)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parse() {
        let mut arena = StringArena::new();

        let s = "^.0.test";
        let path = Path::from_str(s, &mut arena);
        assert_eq!(path.len(), 3);
        assert!(!path.is_relative());
        assert!(!path.is_empty());
        assert!(path.contains_named_component());
        assert!(path[0].is_parent());
        assert!(path[1].is_index());
        assert_eq!(path[1].as_index(), Some(0));
        assert!(path[2].is_named());
        assert_eq!(path[2].as_name(&arena), Some("test"));
        assert_eq!(path.to_string(&arena).unwrap(), s);
        assert!(path.contains_name_str("test", &arena));

        let s = ".test.0";
        let path = Path::from_str(s, &mut arena);
        assert!(path.is_relative());
        assert_eq!(path.len(), 2);
        assert_eq!(path.to_string(&arena).unwrap(), s);
    }

    #[test]
    fn normalize() {
        let mut arena = StringArena::new();

        let mut path = Path::from_str("0.1.^.2.^.^.3", &mut arena);
        assert_eq!(path.len(), 1);
        assert_eq!(path[0].as_index(), Some(3));
        assert_eq!(path.to_string(&arena).unwrap(), "3");
        assert!(!path.contains_named_component());

        path.extend(Path::from_str(".4.5", &mut arena));
        assert_eq!(path.len(), 3);
        path.push(PathComponent::from_str("^", &mut arena).unwrap());
        assert_eq!(path.len(), 2);
        assert_eq!(path.to_string(&arena).unwrap(), "3.4");
        path.extend(Path::from_str("^.^.^.^.6", &mut arena));
        assert_eq!(path.len(), 3);
        assert_eq!(path.to_string(&arena).unwrap(), "^.^.6");
    }
}
