//! Marker types forming a common (dynamic) vocabulary for guards.
//! A well-rounded knight should use these vocabulary items.
// FIXME: "chivalry"? Could be a good description. It's less, uh, mandatory.
// FIXME: Turn on `Valid`.

use crate::Name;

/*
/// A type `Self` that implements `Valid<SyntaxContext, KeyMarker>` can be used as a guard in a
/// `blade!` macro using the syntax `where { KeyMarker => Self }`.
/// `SyntaxContext` is an item from `vocab::ctx`: `Item`, `Struct`, `Field`, `Enum`, `Variant`,
/// `Vec`, or `Map`.
pub trait Valid<C: Ctx, KeyMarker> {}
pub mod ctx {
    pub struct Item;
    pub struct Struct;
    pub struct Field;
    pub struct Enum;
    pub struct Variant;
    pub struct Vec;
    pub struct Map;
    use super::Ctx;
    impl Ctx for Item {}
    impl Ctx for Struct {}
    impl Ctx for Field {}
    impl Ctx for Enum {}
    impl Ctx for Variant {}
    impl Ctx for Vec {}
    impl Ctx for Map {}
}
pub trait Ctx {}*/

/// Provide a missing value.
#[derive(Debug, Clone, Default)]
pub struct Missing;
//impl<X, C: Ctx, R, F: Fn() -> R> Valid<C, Missing> for X {}

/// (FIXME: dubiously unsafe) Indicates that it's sound to cast `&T` to `&[u8; size_of<T>]`
#[derive(Debug, Clone, Default)]
pub struct TotalMem;
//impl Valid<ctx::Struct, Missing> for () {}
//impl Valid<ctx::Enum, Missing> for () {}

#[derive(Debug, Clone, Default)]
pub struct NonTotalMem;
//impl Valid<ctx::Struct, Missing> for () {}
//impl Valid<ctx::Enum, Missing> for () {}

/// Indicates the version number of the given element.
#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Version(pub u16);
impl Default for Version {
    fn default() -> Self { Version(0) }
}
// impl<C: Ctx> Valid<C, Version> {}

/// Modifies a value to be in some "proper" range.
#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Clamp;
// impl<X, C: Ctx, R, F: Fn(&mut R)> R> Valid<C, Clamp> for X {}

/// How an enum should be tagged in JSON-like formats.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
#[repr(u8)]
pub enum EnumTag {
    /// `{"VariantName": { …data… }}` — The default.
    External,
    /// `{ "type": "VariantName", …data… }`
    Internal {
        label_name: Name,
    },
    /// `{ "type": "VariantName", "value": { …data… }}`
    Adjacent {
        type_name: Name,
        data_name: Name,
    },
    /// `{ …data… }` — Uses the first successful parse.
    Untagged,
}
// impl Valid<ctx::Variant, EnumTag> for () {}
impl Default for EnumTag {
    fn default() -> Self { Self::External }
}
