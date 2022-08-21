extern crate sir_macro;
#[doc(hidden)]
pub use sir_macro::blade_impl;

/// Implements the body of [`Blade`](trait.Blade.html).
///
/// # Syntax
///
/// The body should mirror (as appropriate) the contents of the type.
/// A `where {}` can be added on the end to place [guards](rt/struct.Guard.html),
/// which is filled with values to add to the guard. This list is mandatory on the first line.
///
/// ```rust
/// # type T = ();
/// sir::blade! {
///     enum Option::<T> where {},
///     None where {},
///     Some(T where {}) where {},
/// }
/// ```
///
/// # Debugging
/// The output can be logged to a file by setting the env var `LOG_BLADE_IMPL_MACRO` to the name (eg `Option`
/// for `std::option::Option`), or `ALL`. The output is written to `/tmp/blade_impl_%.rs`, or the path specified by the env var
/// `LOG_BLADE_IMPL_MACRO_OUTPUT` with any `%` replaced by a number.
///
/// `rustfmt` must be installed.
#[macro_export]
macro_rules! blade {
    ($($tt:tt)*) => {
        fn sword() -> $crate::rt::Sword {
            $crate::blade_impl! {
                $($tt)*
            }
        }
    };
}

/// Construct a [`Sword`](rt/struct.Sword.html).
/// See [`blade!`](macro.blade.html).
#[macro_export]
macro_rules! sword {
    ($($tt:tt)*) => {
        $crate::blade_impl!($($tt)*)
    };
}

// You can't use `$crate` in a proc_macro; this lets you use `crate::sir` instead.
#[doc(hidden)]
pub use crate as sir;

pub mod rt;
mod spam;
mod impls;
pub mod knights;

pub type Name = &'static str;

pub mod util {
    pub use ezty::{Ty, AnyDebug};
}

#[doc(hidden)]
pub mod prelude_macro {
    pub use crate::rt::*;
    pub use crate::util::{Ty, AnyDebug};
    pub use std::borrow::Cow;
    pub use std::sync::Arc;
    pub use std::any::Any as StdAny;

    #[doc(hidden)]
    #[cold]
    pub fn variant_mismatch() -> ! {
        panic!("unexpected variant mismatch")
    }
}

pub mod chivalry;

mod blade_trait {
    use crate::util::AnyDebug;
    use crate::rt::Sword;
    use std::any::type_name;

    #[cold]
    fn mismatch(t: &dyn AnyDebug, ty: &str) -> ! {
        panic!("type mismatch: expected a {}, found: {:?}", ty, t)
    }

    /// Trait providing the default [`Sword`] for a type.
    /// A Knight should allow registration of both `impl Blade` and `Sword` parameters.
    pub trait Blade: AnyDebug {
        // NOTE: Sword has a lot of Arc's. I was thinking I'd like to be able to recycle them. But
        // in fact I would not; really it's just there to make Eq easy.
        fn sword() -> Sword;

        // These two are for convenience.
        #[doc(hidden)]
        fn downcast_ref(this: &dyn AnyDebug) -> &Self
        where
            Self: Sized,
        {
            match this.downcast_ref() {
                Some(s) => s,
                _ => mismatch(this, type_name::<Self>()),
            }
        }
        #[doc(hidden)]
        fn downcast_mut(this: &mut dyn AnyDebug) -> &mut Self
        where
            Self: Sized,
        {
            Self::downcast_ref(this);
            this.downcast_mut().unwrap()
        }
    }
}
pub use self::blade_trait::Blade;

/// Create a [`Sword`] for a tuple. Field numbers must be provided.
///
/// # Usage
/// ```no_compile
/// tuple_sword(0: i32, 1: i32)
/// ```
#[macro_export]
macro_rules! tuple_sword {
    ($($n:tt: $t:ty),*) => {{
        use $crate::prelude_macro::*;
        type Tup = ($($t,)*);
        Sword {
            item: Arc::new(Item {
                ty: Ty::of::<Tup>(),
                guards: vec![],
                body: Body::Struct(Arc::new(BodyStruct {
                    body_type: BodyType::Tuple,
                    fields: vec![
                        $(Arc::new(Field {
                            name: stringify!($n),
                            ty: Ty::of::<u32>(),
                            as_ref: |d| &d.downcast_ref::<Tup>().unwrap().$n,
                            as_mut: |d| &mut d.downcast_mut::<Tup>().unwrap().$n,
                            with: |f: &mut dyn FnMut(AnyOptionT)| {
                                let mut val = Option::<$t>::None;
                                f(&mut val);
                            },
                            guards: vec![],
                        }),)*
                    ],
                    // This could almost be done w/o a macro; but `init` is the problem.
                    init: |out: AnyOptionT, each: &mut dyn FnMut(AnyOptionT)| {
                        let out: &mut Option<Tup> = out.downcast_mut().expect("wrong type (enum)");
                        let this = ($({
                            let mut v = Option::<$t>::None;
                            each(&mut v);
                            if let Some(v) = v { v } else { return }
                        },)*);
                        *out = Some(this);
                    },
                })),
            }),
        }
    }};
}

#[cfg(test)]
#[test]
fn test_tuple_sword() {
    tuple_sword!(0: i32, 1: u8);
}
