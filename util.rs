use std::fmt;
use std::any::{type_name, Any};
use std::hash;
use crate::Name;
use std::alloc::Layout;

pub struct FieldInit<'a> {
    inner: Inner<'a>,
}
impl<'a> Default for FieldInit<'a> {
    fn default() -> Self {
        Self::end()
    }
}
impl<'a> fmt::Debug for FieldInit<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut f = f.debug_tuple("FieldInit");
        let mut top = &self.inner;
        while let Inner::Field { field, next } = top {
            f.field(field);
            top = &next.inner;
        }
        f.finish()
    }
}

enum Inner<'a> {
    End,
    Field {
        field: &'a mut dyn AnyDebug,
        next: &'a mut FieldInit<'a>,
    },
}
impl<'a> FieldInit<'a> {
    pub const fn end() -> Self {
        FieldInit { inner: Inner::End }
    }
    pub fn pack<T: AnyDebug>(field: &'a mut Option<T>, next: &'a mut FieldInit<'a>) -> Self {
        FieldInit {
            inner: Inner::Field { field, next },
        }
    }
    pub fn unpack<T: AnyDebug>(&mut self) -> (T, &mut Self) {
        match &mut self.inner {
            Inner::End => Self::early_end(),
            Inner::Field { field, next } => {
                if let Some(field) = field.downcast_mut::<Option<T>>() {
                    let field = field.take().unwrap_or_else(|| Self::taken());
                    (field, next)
                } else {
                    // FIXME: make this cold (harder than it looks)
                    panic!("Expected value of type {}, found: {:?}", type_name::<T>(), field)
                }
            },
        }
    }
    #[cold] fn taken() -> ! { panic!("Field was already taken") }
    #[cold] fn early_end() -> ! { panic!("Unexpected end of fields list") }
}

use std::mem::MaybeUninit;

pub struct Uninit<T: Any> {
    val: MaybeUninit<T>,
    // 'init' at the end so that the compiler can easily plough it over.
    init: bool,
}
impl<T: Any> Default for Uninit<T> {
    fn default() -> Self { Self::new() }
}

const _A: crate::TODO = crate::TODO;
#[allow(dead_code)] // FIXME: library
impl<T: Any> Uninit<T> {
    pub fn new() -> Self {
        Uninit {
            val: MaybeUninit::uninit(),
            init: false,
        }
    }
    /*#[must_use]
    pub fn begin(&mut self) -> Init {
        Init(self)
    }*/
    pub fn init(&mut self, t: T) {
        assert_uninit(&mut self.init);
        self.val.write(t);
    }
    pub fn take(self) -> T {
        assert_init(self.init);
        unsafe { self.val.assume_init() }
    }
}
fn assert_uninit(init: &mut bool) {
    if *init {
        panic!("already initialized");
    }
    *init = true;
}
fn assert_init(init: bool) {
    if !init {
        panic!("not initialized");
    }
}

#[must_use]
pub struct Init<'a>(&'a mut dyn Any);
impl<'a> Init<'a> {
    fn mark(b: &mut bool) {
        assert!(!*b, "already initialized");
        *b = true;
    }
    pub fn with<T: Any>(self, init: T) {
        let val: &mut Uninit<T> = self.0.downcast_mut::<Uninit<T>>()
            .expect("wrong mismatch");
        Self::mark(&mut val.init);
        val.val.write(init);
    }
}

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
