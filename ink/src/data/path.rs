use internship::IStr;
use std::{
    fmt::{self, Display, Formatter},
    slice::SliceIndex,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PathComponent {
    Index(u32),
    Name(IStr),
    Parent,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Path {
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

    pub fn new(components: impl IntoIterator<Item = PathComponent>, relative: bool) -> Self {
        let mut comps = Vec::new();
        for comp in components {
            Self::push_element(&mut comps, comp);
        }
        Path {
            components: comps,
            relative,
        }
    }

    pub fn new_head(head: PathComponent, tail: impl IntoIterator<Item = PathComponent>) -> Self {
        let mut components = Vec::new();
        Self::push_element(&mut components, head);
        for comp in tail {
            Self::push_element(&mut components, comp);
        }
        Path {
            components,
            relative: false,
        }
    }

    pub fn from_str(path: &str) -> Self {
        let relative = path.starts_with(".");
        let mut components: Vec<PathComponent> = Vec::new();
        for comp in path.split(".").filter_map(PathComponent::from_str) {
            Self::push_element(&mut components, comp);
        }
        Path {
            components,
            relative,
        }
    }

    pub fn is_relative(&self) -> bool {
        self.relative
    }

    pub fn contains_named_component(&self) -> bool {
        self.components.iter().any(|c| c.is_named())
    }

    pub fn contains_name(&self, name: &str) -> bool {
        self.components.iter().any(|c| match c {
            PathComponent::Name(s) => s == name,
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
}

impl From<Path> for Vec<PathComponent> {
    fn from(path: Path) -> Self {
        path.components
    }
}

impl std::str::FromStr for Path {
    type Err = ();

    fn from_str(path: &str) -> Result<Self, ()> {
        Ok(Path::from_str(path))
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

impl Display for Path {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), fmt::Error> {
        if self.relative {
            write!(fmt, ".")?;
        }
        for (i, comp) in self.components.iter().enumerate() {
            if i > 0 {
                write!(fmt, ".")?;
            }
            write!(fmt, "{}", comp)?;
        }
        Ok(())
    }
}

impl PathComponent {
    pub fn from_u32(i: u32) -> PathComponent {
        PathComponent::Index(i)
    }

    pub fn parent() -> PathComponent {
        PathComponent::Parent
    }

    pub fn from_str(s: &str) -> Option<PathComponent> {
        if let Ok(i) = s.parse::<u32>() {
            Some(PathComponent::Index(i))
        } else if s == "^" {
            Some(PathComponent::Parent)
        } else if !s.is_empty() && !s.contains(".") {
            Some(PathComponent::Name(s.into()))
        } else {
            None
        }
    }

    pub fn is_named(&self) -> bool {
        if let PathComponent::Name(_) = self {
            true
        } else {
            false
        }
    }

    pub fn as_name(&self) -> Option<&str> {
        if let PathComponent::Name(s) = self {
            Some(s)
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
}

impl Display for PathComponent {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), fmt::Error> {
        use PathComponent::*;
        match self {
            Index(i) => write!(fmt, "{}", i),
            Name(s) => write!(fmt, "{}", s),
            Parent => write!(fmt, "^"),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parse() {
        let s = "^.0.test";
        let path = Path::from_str(s);
        assert_eq!(path.len(), 3);
        assert!(!path.is_relative());
        assert!(path[0].is_parent());
        assert!(path[1].is_index());
        assert_eq!(path[1].as_index(), Some(0));
        assert!(path[2].is_named());
        assert_eq!(path[2].as_name(), Some("test"));
        assert_eq!(path.to_string(), s);
        assert!(path.contains_name("test"));

        let s = ".test.0";
        let path = Path::from_str(s);
        assert!(path.is_relative());
        assert_eq!(path.len(), 2);
        assert_eq!(path.to_string(), s);
    }

    #[test]
    fn normalize() {
        let mut path = Path::from_str("0.1.^.2.^.^.3");
        assert_eq!(path.len(), 1);
        assert_eq!(path[0].as_index(), Some(3));
        assert_eq!(path.to_string(), "3");

        path.extend(Path::from_str(".4.5"));
        assert_eq!(path.len(), 3);
        path.push(PathComponent::from_str("^").unwrap());
        assert_eq!(path.len(), 2);
        assert_eq!(path.to_string(), "3.4");
        path.extend(Path::from_str("^.^.^.^.6"));
        assert_eq!(path.len(), 3);
        assert_eq!(path.to_string(), "^.^.6");
    }
}
