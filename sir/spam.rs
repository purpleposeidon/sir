// Alas! There shall never be a knight equal to chivalry itself.

use crate::rt::*;
use std::fmt;

impl fmt::Debug for Item {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Item")
            .field("ty", &self.ty)
            .field("guards", &self.guards)
            //.field("body", &self.body)
            .finish()
    }
}
impl fmt::Debug for Body {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Body::Primitive => write!(f, "Body::Primitive"),
            Body::Struct(i) => write!(f, "Body::Struct({:?})", i),
            Body::Enum(i) => write!(f, "Body::Enum({:?})", i),
            Body::Vec(i) => write!(f, "Body::Vec({:?})", i),
            Body::Map(i) => write!(f, "Body::Map({:?})", i),
        }
    }
}
impl fmt::Debug for BodyStruct {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("BodyStruct")
            .field("body_type", &self.body_type)
            .field("fields", &self.fields)
            .finish()
    }
}
impl fmt::Debug for BodyEnum {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("BodyEnum")
            .field("variants", &self.variants)
            .finish()
    }
}
impl fmt::Debug for Field {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.ty)?;
        if !self.guards.is_empty() {
            writeln!(f, " where {{")?;
            for guard in self.guards.iter() {
                writeln!(f, "    {:?},", guard)?;
            }
            write!(f, "}}")?;
        }
        Ok(())
    }
}
impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", clean_ty_name(self.name))
    }
}
fn clean_ty_name(name: &str) -> &str {
    let pretty = &[
        ("alloc::boxed::", "Box"),
        ("alloc::collections::binary_heap::", "BinaryHeap"),
        ("alloc::collections::btree::map::", "BTreeMap"),
        ("alloc::collections::btree::set::", "BTreeSet"),
        ("alloc::collections::linked_list::", "LinkedList"),
        ("alloc::collections::vec_deque::", "VecDeque"),
        ("alloc::sync::", "Arc"),
        ("alloc::vec::", "Vec"),
        ("core::cell::", "Cell"),
        ("core::cell::", "RefCell"),
        ("core::result::", "Result"),
        ("std::collections::hash::map::", "HashMap"),
        ("std::collections::hash::set::", "HashSet"),
        ("std::sync::rwlock::", "RwLock"),
    ];
    for &(bad, good) in pretty {
        if name.starts_with(bad) {
            let name = &name[bad.len()..];
            if name.starts_with(good) {
                return name;
            }
        }
    }
    name
}
impl fmt::Debug for Ty {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}
impl fmt::Debug for Variant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Variant")
            .field("name", &self.name)
            .field("body_type", &self.body_type)
            .field("fields", &self.fields)
            .finish()
    }
}
impl fmt::Debug for BodyVec {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("BodyVec")
            .field("items", &self.items)
            .finish()
    }
}
impl fmt::Debug for BodyMap {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("BodyMap")
            .field("keys", &self.keys)
            .field("vals", &self.vals)
            .finish()
    }
}
