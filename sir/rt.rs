//! Describing the structure of types. Self-explanatory? Start with [`Sword`].

use std::fmt;
use std::sync::Arc;
use ezty::{Ty, AnyDebug};
use crate::Name;
use crate::chivalry::{SyntaxPosition, ValidAt};
use std::any::{Any, TypeId};
use crate::Blade;

/// `StdAny + Send + Sync`
pub type SSAny<'a> = &'a (dyn Any + Send + Sync);

#[derive(Clone)]
pub struct Item {
    pub ty: Ty,
    pub guards: Vec<Guard>,
    pub body: Body,
}
#[derive(Clone)]
pub enum Body {
    Primitive,
    Struct(Arc<BodyStruct>),
    Enum(Arc<BodyEnum>),
    Vec(Arc<BodyVec>),
    Map(Arc<BodyMap>),
}
#[derive(Clone)]
pub struct BodyStruct {
    pub body_type: BodyType,
    pub fields: Vec<Arc<Field>>,
    pub init: fn(AnyOptionT, &mut dyn FnMut(AnyOptionT)),
}
#[derive(Clone)]
pub struct BodyEnum {
    pub variants: Vec<Arc<Variant>>,
    /// NB: This is *NOT* Rust's discrim, it's the index into `variants`.
    pub variant_index: fn(&dyn AnyDebug) -> usize,
}
// This name isn't very good; why should it HAVE to be a Vec?
// I think the real problem is that there's just infinite varieties of data structures.
// FIXME: Should be a guard I guess.
#[derive(Clone)]
pub struct BodyVec {
    // For serialization purposes we could merge BodyVec & BodyMap. But there are other purposes, as exposed by
    // their vt's.
    pub items: Ty,
    pub vt: CollectionVec,
}
#[derive(Clone)]
pub struct BodyMap {
    pub keys: Ty,
    pub vals: Ty,
    pub vt: CollectionMap,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BodyType {
    Unit,
    Tuple,
    Struct,
}
#[derive(Clone)]
pub struct Variant {
    pub name: Name,
    pub body_type: BodyType,
    pub fields: Vec<Arc<Field>>,
    pub init: fn(AnyOptionT, &mut dyn FnMut(AnyOptionT)),
    pub guards: Vec<Guard>,
}
#[derive(Clone)]
pub struct Field {
    pub name: Name,
    /// This is a `fn` ptr because `TypeId::of` is not `const`. Some day this will change.
    pub ty: Ty,
    // FIXME: crate:field-offset? It's typed, but we could erase it.
    pub as_ref: fn(&dyn AnyDebug) -> &dyn AnyDebug,
    pub as_mut: fn(&mut dyn AnyDebug) -> &mut dyn AnyDebug,
    /// Calls the function with `&mut Option::<F>::None`. Allows initialization without allocation.
    pub with: fn(&mut dyn FnMut(AnyOptionT)),
    pub guards: Vec<Guard>,
}
impl Field {
    pub fn get_ref<'a>(&self, val: &'a dyn AnyDebug) -> &'a dyn AnyDebug {
        (self.as_ref)(val)
    }
    pub fn get_mut<'a>(&self, val: &'a mut dyn AnyDebug) -> &'a mut dyn AnyDebug {
        (self.as_mut)(val)
    }
}

/// A marker object. See [`crate::chivalry`].
#[derive(Clone)]
pub struct Guard(pub Arc<dyn AnyDebug>);
impl fmt::Debug for Guard {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}
impl Guard {
    pub fn new<Position: SyntaxPosition, G: ValidAt<Position>>(g: G) -> Self {
        Guard(Arc::new(g))
    }
    fn is_ty(expect: TypeId, any: &dyn AnyDebug) -> bool {
        // I've been writing too much Rust.
        let actual = <dyn AnyDebug>::type_id(any);
        expect == actual
    }
    pub fn is<T: AnyDebug>(&self) -> bool {
        let expect = TypeId::of::<T>();
        Self::is_ty(expect, &self.0)
    }
    pub fn as_ref<T: AnyDebug>(&self) -> Option<&T> {
        self.0.downcast_ref()
    }
}

/// Represents `&mut Option<T>`. Used to move values in or out. Initiailize by `*aot = Some(_)`,
/// take by `aot.take().unwrap()`.
pub type AnyOptionT<'a> = &'a mut dyn AnyDebug;
/// An [`AnyDebug`] parameter that is used as a `self`.
pub type AnyThis = dyn AnyDebug;
/// The key used in a Map.
pub type AnyKey<'a> = &'a dyn AnyDebug;
/// Some `&mut Option<(Key, Value)>`; used for Maps.
pub type AnyOptionPair<'a> = AnyOptionT<'a>;

/// A function that will be immediately invoked with an iterator over some Vec.
pub type VecIterFn<'a> = &'a mut dyn FnMut(
    &mut dyn Iterator<Item=&dyn AnyDebug>,
);

/// Dynamic virtual table for Vec-like types.
#[derive(Clone)]
pub struct CollectionVec {
    pub len: fn(&AnyThis) -> usize,
    pub get_ref: fn(&AnyThis, usize) -> &dyn AnyDebug,
    pub get_mut: fn(&mut AnyThis, usize) -> &mut dyn AnyDebug,
    pub reserve: fn(&mut AnyThis, usize),
    pub push: fn(&mut AnyThis, AnyOptionT),
    pub iter: fn(&AnyThis, VecIterFn),
    pub collect: fn(AnyOptionT, Option<usize> /* reserve. Does not limit callback count. */, &mut dyn FnMut(AnyOptionT)),
}

/// A function that will be immediately invoked with an iterator over some Map.
pub type MapIterFn<'a> = &'a mut dyn FnMut(
    &mut dyn Iterator<Item=(AnyKey, &dyn AnyDebug)>,
);

/// Dynamic virtual table for Map-like types.
#[derive(Clone)]
pub struct CollectionMap {
    pub len: fn(&AnyThis) -> usize,
    pub get_ref: for<'this> fn(&'this AnyThis, AnyKey) -> Option<&'this dyn AnyDebug>,
    pub get_mut: for<'this> fn(&'this mut AnyThis, AnyKey) -> Option<&'this mut dyn AnyDebug>,
    pub reserve: fn(&mut AnyThis, usize),
    pub insert: fn(&mut AnyThis, AnyOptionPair),
    pub iter_items: fn(&AnyThis, MapIterFn),
    pub collect: fn(AnyOptionT, Option<usize>, &mut dyn FnMut(AnyOptionT, AnyOptionT)),
}


/// The root of a type; points to an [`Item`].
#[derive(Clone)]
pub struct Sword {
    pub item: Arc<Item>,
}
impl Sword {
    pub fn of<T: Blade>() -> Self {
        T::sword()
    }
}

impl fmt::Debug for Sword {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.item)
    }
}

impl Variant {
    pub fn is_unit(&self) -> bool {
        self.fields.len() == 0 && self.body_type == BodyType::Unit
    }
}
