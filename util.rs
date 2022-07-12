use std::fmt;
use std::any::type_name;
use std::hash;
use crate::Name;
use std::alloc::Layout;

#[derive(Copy, Clone, Eq)]
pub struct Ty {
    pub id: NonStaticTypeId,
    pub name: Name,
    pub layout: Layout,
}
impl hash::Hash for Ty {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        hash::Hash::hash(&self.id, state)
    }
}
impl PartialEq for Ty {
    fn eq(&self, other: &Ty) -> bool {
        self.id == other.id
    }
}
impl Ty {
    pub fn of<T>() -> Ty {
        Ty {
            id: NonStaticTypeId::of::<T>(),
            name: type_name::<T>(),
            layout: Layout::new::<T>(),
        }
    }
}

pub trait AnyDebug: mopa::Any + fmt::Debug + Send + Sync {
    fn type_name<'a>(&'a self) -> &'static str {
        type_name::<Self>()
    }
    fn get_ty(&self) -> Ty;
}
mopafy!(AnyDebug);
impl<X: mopa::Any + fmt::Debug + Send + Sync> AnyDebug for X {
    fn get_ty(&self) -> Ty {
        Ty::of::<Self>()
    }
}

#[derive(PartialEq, Eq, Hash, Copy, Clone)]
pub struct NonStaticTypeId(usize);
impl NonStaticTypeId {
    pub fn of<T>() -> Self {
        Self(Self::of::<T> as usize)
    }
}
