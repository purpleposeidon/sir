//! Marker types forming a common (dynamic) vocabulary for guards.
//! A well-rounded knight should use these vocabulary items.
// FIXME: This should be per-crate for now.

use crate::{Name, rt};
use std::any::Any;
use std::{mem, fmt};

/// All guards must implement `ValidAt<Rt, V>` where `Rt` is `rt::Item` or `rt::Field`, and V is
/// the value that appears there.
pub trait ValidAt<P: SyntaxPosition> {}
pub trait SyntaxPosition {}
impl SyntaxPosition for crate::rt::Item {}
impl SyntaxPosition for crate::rt::Field {}

/// Provide a missing value.
/// The function's parameter is `&mut Option<T>`; it should set it to `Some(T::default())`.
#[derive(Clone)]
pub struct ProvideDefault(pub fn(&mut dyn Any));
impl fmt::Debug for ProvideDefault {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ProvideDefault(_)")
    }
}
impl ProvideDefault {
    pub fn from<T: 'static, F: 'static + Fn() -> T>(_f: F) -> Self {
        // This is stupid, but it works. If Box::new were const...
        assert_eq!(mem::size_of::<F>(), 0, "Expected 0-sized Fn() -> T (eg, stateless closure or simple fn)");
        ProvideDefault(|t: &mut dyn Any| {
            let t: &mut Option<T> = t.downcast_mut().expect("wrong type");
            let f = unsafe { std::mem::transmute::<&(), &F>(&()) };
            *t = Some(f());
        })
    }
    pub fn from_trait<T: 'static + Default>() -> Self {
        Self::from::<T, _>(Default::default)
    }
}
impl<P: SyntaxPosition> ValidAt<P> for ProvideDefault {}

/// Initialize `Option::<T>::Some` from raw bytes. Akin to `ptr::read`.
///
/// # Safety
/// Your implementation should use `unsafe`. This should only be used on types that are:
/// 1. `#[repr(C)]`
/// 2. Have no padding
pub struct TotalRead(pub fn(&mut dyn Any, &[u8]));
/// Cast `&T` to `&[u8]`. Ownership of `T` is *not* transfered through the `&[u8]`; this is only
/// used for quickly serializing to disk.
/// # Safety
/// Your implementation should use `unsafe`. This should only be used on types that are:
/// 1. `#[repr(C)]`
/// 2. Have no padding
pub struct TotalWrite(pub fn(&dyn Any) -> &[u8]);

impl ValidAt<crate::rt::Item> for TotalRead {}
impl ValidAt<crate::rt::Item> for TotalWrite {}

/// This is the default, rather than `TotalRead`/`TotalWrite`. Provided to enable explicit
/// configuration.
#[derive(Debug, Clone)]
pub struct NonTotalMem;
impl ValidAt<crate::rt::Item> for NonTotalMem {}

/// Indicates the version number of the given element.
#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Version(pub u16);
impl Default for Version {
    fn default() -> Self { Version(0) }
}
impl<P: SyntaxPosition> ValidAt<P> for Version {}

/// Modifies a value to be in some "proper" range.
#[derive(Clone)]
pub struct Clamp(pub fn(&mut dyn Any));
impl fmt::Debug for Clamp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Clamp(_)")
    }
}
impl Clamp {
    pub fn with<T: 'static, F: 'static + Fn(&mut T)>(_f: F) -> Clamp {
        assert_eq!(mem::size_of::<F>(), 0, "Expected 0-sized Fn() -> T (eg, stateless closure or simple fn)");
        Clamp(|t: &mut dyn Any| {
            let t: &mut T = t.downcast_mut().expect("wrong type");
            let f = unsafe { std::mem::transmute::<&(), &F>(&()) };
            f(t);
        })
    }
}
impl<P: SyntaxPosition> ValidAt<P> for Clamp {}

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
impl Default for EnumTag {
    fn default() -> Self { Self::External }
}
impl ValidAt<rt::Item> for EnumTag {}
