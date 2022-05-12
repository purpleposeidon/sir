use crate::util::Ty;
use crate::rt::*;
use crate::Blade;
use std::borrow::Cow;
use crate::util::{FieldInit, Init};
use std::collections::HashMap;
use std::any::TypeId;
use crate::knights::{a2r, a2m};

impl<T: Blade> Blade for Option<T> {
    blade! {
        enum Option::<T> where {},
        Option::<T>::None,
        Option::<T>::Some(T),
    }
}

impl<T: Blade, E: Blade> Blade for Result<T, E> {
    blade! {
        enum Result::<T, E> where {},
        Result::<T, E>::Ok(T),
        Result::<T, E>::Err(E),
    }
}

impl<T: Blade> Blade for Box<T> {
    const SCABBARD: Scabbard = {
        use crate::prelude_macro::*;
        Scabbard {
            item: Item {
                ty: Ty::of::<Self>,
                guards: Cow::Borrowed(&[]),
                body: Body::Struct(BodyStruct {
                    body_type: BodyType::Tuple,
                    fields: Cow::Borrowed(&[Field {
                        name: "0",
                        ty: Ty::of::<T>,
                        as_ref: |s: &dyn AnyDebug| -> &dyn AnyDebug {
                            Self::downcast_ref(s) as &T
                        },
                        as_mut: |s: &mut dyn AnyDebug| -> &mut dyn AnyDebug {
                            Self::downcast_mut(s) as &mut T
                        },
                        with: |f: &mut dyn FnMut(AnyOptionT)| f(&mut Option::<Self>::None),
                        guards: Cow::Borrowed(&[]),
                    }]),
                    init: |out: AnyOptionT, each: &mut dyn FnMut(AnyOptionT)| {
                        let out: &mut Option<Box<T>> = out.downcast_mut().unwrap();
                        let mut inner = Option::<T>::None;
                        each(&mut inner);
                        if let Some(inner) = inner {
                            *out = Some(Box::new(inner));
                        }
                    },
                }),
            },
            init: Cow::Borrowed(&[|mut _field: FieldInit, init: Init| {
                let (_0, _field) = _field.unpack::<T>();
                init.with(Box::new(_0));
            }]),
        }
    };
}

fn scabbard_of<T: Blade>() -> Scabbard { <T as Blade>::SCABBARD }
impl<T: Blade> Blade for Vec<T> {
    const SCABBARD: Scabbard = {
        use crate::prelude_macro::*;
        use crate::rt::*;
        use crate::util::*;
        Scabbard {
            item: Item {
                ty: Ty::of::<Self>,
                guards: Cow::Borrowed(&[]),
                body: Body::Vec(BodyVec {
                    items: CowBox::Init(scabbard_of::<T>),
                    vt: CollectionVec {
                        len: |a: &AnyThis| a2r::<Self>(a).len(),
                        read: |a: &AnyThis, i: usize| &a2r::<Self>(a)[i],
                        write: |a: &mut AnyThis, i: usize| &mut a2m::<Self>(a)[i],
                        reserve: |a: &mut AnyThis, n: usize| a2m::<Self>(a).reserve(n),
                        push: |a: &mut AnyThis, val: AnyOptionT| {
                            let a = a2m::<Self>(a);
                            let val: &mut Option<T> = val.downcast_mut().unwrap();
                            a.push(val.take().unwrap())
                        },
                        iter: |a: &AnyThis, func: VecIterFn| {
                            let a = a2r::<Self>(a);
                            let mut it = a
                                .iter()
                                .map(|v| v as &dyn AnyDebug);
                            func(&mut it);
                        },
                        collect: |
                            output: AnyOptionT,
                            reserve: Option<usize>,
                            next: &mut dyn FnMut(AnyOptionT),
                        | {
                            let output: &mut Option<Vec<T>> = output.downcast_mut().expect("wrong type");
                            let mut out = if let Some(n) = reserve {
                                Vec::<T>::with_capacity(n)
                            } else {
                                Vec::<T>::new()
                            };
                            loop {
                                let mut hold = Option::<T>::None;
                                next(&mut hold);
                                if let Some(val) = hold {
                                    out.push(val);
                                } else {
                                    break;
                                }
                            }
                            *output = Some(out);
                        },
                    },
                }),
            },
            init: Cow::Borrowed(&[|_field: FieldInit, init: Init| {
                init.with(Self::default());
            }]),
        }
    };
}

use crate::prelude_macro::AnyDebug;

impl<K: Blade, V: Blade> Blade for HashMap<K, V>
where
    K: Eq + std::hash::Hash,
    K: AnyDebug,
    V: AnyDebug,
{
    const SCABBARD: Scabbard = {
        use crate::prelude_macro::*;
        use crate::rt::*;
        use crate::util::*;
        Scabbard {
            item: Item {
                ty: Ty::of::<Self>,
                guards: Cow::Borrowed(&[]),
                body: Body::Map(BodyMap {
                    keys: CowBox::Init(|| K::SCABBARD.item),
                    vals: CowBox::Init(|| V::SCABBARD.item),
                    vt: CollectionMap {
                        len: |a: &AnyThis| a2r::<Self>(a).len(),
                        read: |a: &AnyThis, k: AnyKey| {
                            let a = a2r::<HashMap<&K, V>>(a);
                            let k = a2r::<K>(k);
                            a.get(k).map(|v| v as &dyn AnyDebug)
                        },
                        write: |a: &mut AnyThis, k: AnyKey| {
                            let a = a2m::<HashMap<&K, V>>(a);
                            let k = a2r::<K>(k);
                            a.get_mut(k).map(|v| v as &mut dyn AnyDebug)
                        },
                        reserve: |a: &mut AnyThis, n: usize| {
                            let a = a2m::<Self>(a);
                            a.reserve(n);
                        },
                        insert: |a: &mut AnyThis, kv: AnyOptionPair| {
                            let a = a2m::<Self>(a);
                            let (k, v) = a2m::<Option<(K, V)>>(kv).take().unwrap();
                            a.insert(k, v);
                        },
                        iter_items: |a: &AnyThis, func: MapIterFn| {
                            let a = a2r::<Self>(a);
                            let mut it = a
                                .iter()
                                .map(|(k, v): (&K, &V)| {
                                    (
                                        k as &dyn AnyDebug,
                                        v as &dyn AnyDebug,
                                    )
                                });
                            func(&mut it);
                        },
                        collect: |
                            output: AnyOptionT,
                            reserve: Option<usize>,
                            next: &mut dyn FnMut(AnyOptionT, AnyOptionT),
                        | {
                            let output: &mut Option<HashMap<K, V>> = output.downcast_mut().expect("wrong type");
                            let mut out = if let Some(n) = reserve {
                                HashMap::<K, V>::with_capacity(n)
                            } else {
                                HashMap::<K, V>::new()
                            };
                            loop {
                                let mut key = Option::<K>::None;
                                let mut val = Option::<V>::None;
                                next(&mut key, &mut val);
                                if let (Some(key), Some(val)) = (key, val) {
                                    out.insert(key, val);
                                } else {
                                    break;
                                }
                            }
                            *output = Some(out);
                        },
                    },
                }),
            },
            init: Cow::Borrowed(&[|_field: FieldInit, init: Init| {
                init.with(Self::default());
            }]),
        }
    };
}

macro_rules! impl_primal_blades {
    ($($ty:ty,)*) => {
        $(
            impl Blade for $ty {
                const SCABBARD: Scabbard = Scabbard {
                    item: Item {
                        ty: Ty::of::<Self>,
                        guards: Cow::Borrowed(&[]),
                        body: Body::Primitive,
                    },
                    init: Cow::Borrowed(&[
                        |mut _field: FieldInit, init: Init| {
                            let (val, _field) = _field.unpack::<Self>();
                            init.with(val);
                        },
                    ]),
                };
            }
        )*
        pub fn register_primals() -> HashMap<TypeId, Scabbard> {
            let mut ret = HashMap::new();
            $(ret.insert(TypeId::of::<$ty>(), <$ty as Blade>::SCABBARD);)*
            ret
        }
    };
}

impl_primal_blades! {
    bool,
    i8,
    u8,
    i16,
    u16,
    i32,
    u32,
    i64,
    u64,
    i128,
    u128,
    usize,
    isize,

    String,
    &'static str,
}
