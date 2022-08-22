//! Common vocabulary types for guards. Knights should try to implement appropriate handling for
//! them. Any type implementing [`ValidAt`] can be a guard.

use std::any::Any;
use std::{mem, fmt};
use crate::{Name, rt};
use crate::util::{AnyDebug, Ty};
use crate::rt::Guard;

/// All guards must implement `ValidAt<Rt, V>` where `Rt` is `rt::Item` or `rt::Field`, and V is
/// the value that appears there.
pub trait ValidAt<P: SyntaxPosition>: AnyDebug {}
/// Specifies a place that a [`Guard`] may be placed.
pub trait SyntaxPosition {
    fn get_guards(&self) -> &[Guard];
}
impl SyntaxPosition for crate::rt::Item {
    fn get_guards(&self) -> &[Guard] {
        &self.guards[..]
    }
}
impl SyntaxPosition for crate::rt::Field {
    fn get_guards(&self) -> &[Guard] {
        &self.guards[..]
    }
}
impl SyntaxPosition for crate::rt::Variant {
    fn get_guards(&self) -> &[Guard] {
        &self.guards[..]
    }
}

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

impl fmt::Debug for TotalRead {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "TotalRead(_)")
    }
}
impl fmt::Debug for TotalWrite {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "TotalWrite(_)")
    }
}

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

/// Serialize to nothing. Deserialize using Default.
#[derive(Copy, Clone)]
pub struct Skip(pub fn(&mut dyn AnyDebug));
impl fmt::Debug for Skip {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Skip(0x{:x?})", self.0 as usize)
    }
}
impl Skip {
    pub fn default<T: AnyDebug + Default>() -> Self {
        Skip(|out: &mut dyn AnyDebug| {
            let ty = <dyn AnyDebug>::get_ty(out);
            let out: &mut Option<T> = out.downcast_mut().unwrap_or_else(|| {
                panic!("Skip::default should initialize Option<{:?}>, but was given {:?}", Ty::of::<T>(), ty)
            });
            *out = Some(T::default());
        })
    }
}
impl ValidAt<rt::Field> for Skip {}


// It'd need to be VariantDiscrim(Box<dyn Any>).
//#[derive(Debug)]
//pub struct VariantDiscrim<T>(pub T);
//impl<T: fmt::Debug> ValidAt<crate::rt::Variant> for VariantDiscrim<T> {}

/// A guard that only a particular knight should use.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct GuardedOnly<T> {
    pub knight: Ty,
    pub val: T,
}
impl<T> GuardedOnly<T> {
    pub fn by<K: 'static>(val: T) -> Self {
        Self {
            knight: Ty::of::<K>(), // FIXME(rust): TypeId::of not const
            val,
        }
    }
}
impl<P: SyntaxPosition, T: ValidAt<P>> ValidAt<P> for GuardedOnly<T> {}

pub fn get_guards<G: AnyDebug>(guards: &[Guard], knight: Ty) -> impl Iterator<Item=&G> {
    let mut it1 = guards.iter();
    let mut it2 = guards.iter();
    std::iter::from_fn(move || -> Option<&G> {
        while let Some(g) = it1.next() {
            if let Some(g) = g.as_ref::<GuardedOnly<G>>() {
                if g.knight == knight {
                    return Some(&g.val);
                }
            }
        }
        while let Some(g) = it2.next() {
            if let Some(g) = g.as_ref::<G>() {
                return Some(g);
            }
        }
        None
    })
}

impl crate::rt::Item {
    pub fn guard<E: AnyDebug, K: 'static>(&self) -> Option<&E> {
        self.guards::<E, K>().next()
    }
    pub fn guards<E: AnyDebug, K: 'static>(&self) -> impl Iterator<Item=&E> {
        get_guards::<E>(&self.guards, Ty::of::<K>())
    }
}
impl crate::rt::Field {
    pub fn guard<E: AnyDebug, K: 'static>(&self) -> Option<&E> {
        self.guards::<E, K>().next()
    }
    pub fn guards<E: AnyDebug, K: 'static>(&self) -> impl Iterator<Item=&E> {
        get_guards::<E>(&self.guards, Ty::of::<K>())
    }
}
impl crate::rt::Variant {
    pub fn guard<E: AnyDebug, K: 'static>(&self) -> Option<&E> {
        self.guards::<E, K>().next()
    }
    pub fn guards<E: AnyDebug, K: 'static>(&self) -> impl Iterator<Item=&E> {
        get_guards::<E>(&self.guards, Ty::of::<K>())
    }
}
