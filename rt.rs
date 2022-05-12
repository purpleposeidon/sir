use std::fmt;
use crate::{List, Name};
pub use crate::util::Ty;
use crate::util::{CowBox, AnyDebug, FieldInit, Init};
use std::any::{Any, TypeId, type_name};
use crate::Blade;

pub type SAny = &'static dyn AnyDebug;
pub type SSAny = &'static (dyn Any + Send + Sync);

#[derive(Clone)]
pub struct Item {
    pub ty: fn() -> Ty,
    pub guards: List<Guard>,
    pub body: Body,
}
#[derive(Clone)]
pub enum Body {
    Primitive,
    Struct(BodyStruct),
    Enum(BodyEnum),
    Vec(BodyVec),
    Map(BodyMap),
}
#[derive(Clone)]
pub struct BodyStruct {
    pub body_type: BodyType,
    pub fields: List<Field>,
    pub init: fn(AnyOptionT, &mut dyn FnMut(AnyOptionT)),
}
#[derive(Clone)]
pub struct BodyEnum {
    pub variants: List<Variant>,
    /// NB: This is *NOT* Rust's discrim, it's the index into the array of variants.
    // FIXME: s/discrim/variant_index
    pub discrim: fn(&dyn AnyDebug) -> usize,
}
impl fmt::Debug for BodyEnum {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("BodyEnum")
            .field("variants", &self.variants)
            .finish_non_exhaustive()
    }
}
#[derive(Clone)]
pub struct BodyVec {
    // FIXME: s/Vec/Sequence ?
    pub items: CowBox<Scabbard>,
    pub vt: CollectionVec,
}
impl fmt::Debug for BodyVec {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("BodyVec")
            .field("items", &self.items)
            .finish_non_exhaustive()
    }
}
#[derive(Clone)]
pub struct BodyMap {
    pub keys: CowBox<Item>,
    pub vals: CowBox<Item>,
    pub vt: CollectionMap,
}
impl fmt::Debug for BodyMap {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("BodyMap")
            .field("keys", &self.keys)
            .field("vals", &self.vals)
            .finish_non_exhaustive()
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum BodyType {
    Unit,
    Tuple,
    Struct,
}
#[derive(Clone)]
pub struct Variant {
    pub name: Name,
    pub body_type: BodyType,
    pub fields: List<Field>,
    pub init: fn(AnyOptionT, &mut dyn FnMut(AnyOptionT)),
    // FIXME: enum discrim
    // /// ```
    // /// #[repr(u8)]
    // /// enum Example {
    // ///     Foo = 2,
    // /// }
    // /// ```
    // pub discrim: usize,
}
#[derive(Clone)]
pub struct Field {
    pub name: Name,
    /// This is a `fn` ptr because `TypeId::of` is not `const`. Some day this will change.
    pub ty: fn() -> Ty,
    // FIXME: crate:field-offset? Not const tho...
    pub as_ref: fn(&dyn AnyDebug) -> &dyn AnyDebug,
    pub as_mut: fn(&mut dyn AnyDebug) -> &mut dyn AnyDebug,
    /// Calls the function with `&mut Option::<F>::None`. Allows initialization without allocation.
    pub with: fn(&mut dyn FnMut(AnyOptionT)),
    pub guards: List<Guard>,
}
impl Field {
    pub fn get_ref<'a>(&self, val: &'a dyn AnyDebug) -> &'a dyn AnyDebug {
        (self.as_ref)(val)
    }
    pub fn get_mut<'a>(&self, val: &'a mut dyn AnyDebug) -> &'a mut dyn AnyDebug {
        (self.as_mut)(val)
    }
}

/// A runtime meta-attribute.
#[derive(Copy, Clone)]
pub struct Guard {
    pub event: SAny,
    // FIXME: Probably either &() or &fn() -> T or &fn(A) -> T or &fn(Ctx, A) -> T
    pub handle: SSAny,
}
impl fmt::Debug for Guard {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.handle.downcast_ref::<()>().is_some() {
            write!(f, "{:?}", self.event)
        } else {
            write!(f, "{:?} => {:?}", self.event, <dyn Any>::type_id(self.handle))
        }
    }
}
impl Guard {
    fn is_ty(expect: TypeId, any: &dyn AnyDebug) -> bool {
        // I've been writing too much Rust.
        let actual = <dyn AnyDebug>::type_id(any);
        expect == actual
    }
    fn is_sty(expect: TypeId, any: SSAny) -> bool {
        let actual = <dyn Any>::type_id(any);
        expect == actual
    }
    pub fn event_is<T: AnyDebug>(&self) -> bool {
        let expect = TypeId::of::<T>();
        Self::is_ty(expect, self.event)
    }
    pub fn handle_is<In: AnyDebug, Out: AnyDebug>(&self) -> bool {
        let expect = TypeId::of::<Box<dyn Fn(In) -> Out + 'static>>();
        Self::is_sty(expect, self.handle)
    }
    pub fn event<T: AnyDebug>(&self) -> Option<&T> {
        self.event.downcast_ref()
    }
    pub fn handle<In: AnyDebug, Out: AnyDebug>(&self) -> Option<&(dyn Fn(In) -> Out + 'static)> {
        self.handle
            .downcast_ref::<Box<dyn Fn(In) -> Out + 'static>>()
            .map(|h: &Box<dyn Fn(In) -> Out + 'static>| &**h)
    }
    pub fn call<In: AnyDebug, Out: AnyDebug>(&self, arg: In) -> Out {
        if TypeId::of::<In>() == TypeId::of::<()>() {
            if let Some(f) = self.handle.downcast_ref::<Box<dyn Fn() -> Out + 'static>>() {
                return f();
            }
        }
        let f = self.handle::<In, Out>()
            .unwrap_or_else(|| panic!("Can't downcast Guard's handle to dyn Fn({}) -> {}", type_name::<In>(), type_name::<Out>()));
        f(arg)
    }
}
impl Item {
    pub fn guard<Event: AnyDebug, Handle: Any + Send + Sync>(&self) -> Option<&Handle> {
        guard0::<Event, Handle>(&self.guards)
    }
    pub fn guarded<Event: AnyDebug>(&self) -> Option<&Event> {
        self.guard::<Event, Event>()
    }
}
impl Field {
    pub fn guard<Event: AnyDebug, Handle: Any + Send + Sync>(&self) -> Option<&Handle> {
        guard0::<Event, Handle>(&self.guards)
    }
    pub fn guarded<Event: AnyDebug>(&self) -> Option<&Event> {
        self.guard::<Event, Event>()
    }
}
fn guard0<Event: AnyDebug, Handle: Any + Send + Sync>(guard: &List<Guard>) -> Option<&Handle> {
    guard1(guard, TypeId::of::<Event>()).and_then(|handle| handle.downcast_ref::<Handle>())
}
fn guard1(guard: &List<Guard>, event: TypeId) -> Option<SSAny> {
    guard
        .iter()
        .filter(|g| <dyn AnyDebug>::type_id(g.event) == event)
        .next()
        .map(|g| g.handle)
}

pub type AnyOptionT<'a> = &'a mut dyn AnyDebug;
pub type AnyThis = dyn AnyDebug;
pub type AnyKey<'a> = &'a dyn AnyDebug;
pub type AnyOptionPair<'a> = AnyOptionT<'a>;

pub type VecIterFn<'a> = &'a mut dyn FnMut(
    &mut dyn Iterator<Item=&dyn AnyDebug>,
);

#[derive(Clone)]
pub struct CollectionVec {
    pub len: fn(&AnyThis) -> usize,
    pub read: fn(&AnyThis, usize) -> &dyn AnyDebug,
    pub write: fn(&mut AnyThis, usize) -> &mut dyn AnyDebug,
    pub reserve: fn(&mut AnyThis, usize),
    pub push: fn(&mut AnyThis, AnyOptionT),
    pub iter: fn(&AnyThis, VecIterFn),
    pub collect: fn(AnyOptionT, Option<usize> /* reserve. Does not limit callback count. */, &mut dyn FnMut(AnyOptionT)),
}

pub type MapIterFn<'a> = &'a mut dyn FnMut(
    &mut dyn Iterator<Item=(AnyKey, &dyn AnyDebug)>,
);

#[derive(Clone)]
pub struct CollectionMap {
    pub len: fn(&AnyThis) -> usize,
    pub read: for<'this> fn(&'this AnyThis, AnyKey) -> Option<&'this dyn AnyDebug>,
    pub write: for<'this> fn(&'this mut AnyThis, AnyKey) -> Option<&'this mut dyn AnyDebug>,
    pub reserve: fn(&mut AnyThis, usize),
    pub insert: fn(&mut AnyThis, AnyOptionPair),
    pub iter_items: fn(&AnyThis, MapIterFn),
    pub collect: fn(AnyOptionT, Option<usize>, &mut dyn FnMut(AnyOptionT, AnyOptionT)),
}



impl fmt::Debug for Item {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Item")
            .field("ty", &(self.ty)())
            .field("guards", &self.guards)
            //.field("body", &self.body)
            .finish_non_exhaustive()
    }
}
impl fmt::Debug for Field {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.name, (self.ty)().name)?;
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
            .finish_non_exhaustive()
    }
}

#[derive(Clone)]
pub struct Scabbard {
    pub item: Item,
    pub init: List<fn(FieldInit, Init)>,
}
impl Scabbard {
    pub fn of<T: Blade>() -> Self {
        T::SCABBARD
    }
}

impl fmt::Debug for Scabbard {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.item)
    }
}

impl Item {
    pub fn type_id(&self) -> TypeId {
        (self.ty)().id
    }
}
impl Field {
    pub fn type_id(&self) -> TypeId {
        (self.ty)().id
    }
}
impl CowBox<Item> {
    pub fn type_id(&self) -> TypeId {
        self.with(|it| it.type_id())
    }
}

impl Variant {
    pub fn is_unit(&self) -> bool {
        self.fields.len() == 0 && self.body_type == BodyType::Unit
    }
}
