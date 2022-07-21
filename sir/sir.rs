#[macro_use]
extern crate mopa;

extern crate sir_macro;
#[doc(hidden)]
pub use sir_macro::blade_impl;

#[macro_export]
macro_rules! orphan_blade {
    ($($tt:tt)*) => {
        $crate::blade_impl!($($tt)*);
    };
}


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

// You can't use `$crate` in a proc_macro; this lets you use `crate::sir` instead.
#[doc(hidden)]
pub use crate as sir;

pub mod util;
pub mod rt;
mod spam;
mod impls;
pub mod knights;

pub type Name = &'static str;

#[doc(hidden)]
pub mod prelude_macro {
    pub use crate::rt::*;
    pub use crate::util::{Ty, AnyDebug};
    pub use std::borrow::Cow;
    pub use std::sync::Arc;

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
