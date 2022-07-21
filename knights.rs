use crate::rt::*;
use std::collections::{HashMap, HashSet};
use crate::util::AnyDebug;
use crate::Blade;
use std::sync::Arc;
use std::any::type_name;

// Sir Jen - Debugger/Inspector
// Sir Mise - Printer
// Sir Vei - Printer
// Sir Loin
// Sir George - Surcharge? idk man
// Sir Plant
// Sir Ial - Serializer
// Sir Vive - Serializer. Or "interact w/ objects defined only dynamically"
// Sir Eel - Serializer
// Sir George (Or was it Michael?) - Slayer of dragons, hence compilers, hence does JIT. But maybe it
// should just be Saint George.

#[doc(hidden)]
pub fn a2r<R: AnyDebug>(a: &dyn AnyDebug) -> &R {
    if let Some(a) = a.downcast_ref() {
        return a;
    }
    panic!("Expected {}, got a {} = {:?}", type_name::<R>(), <dyn AnyDebug>::type_name(a), a)
}
#[doc(hidden)]
pub fn a2m<R: AnyDebug>(a: &mut dyn AnyDebug) -> &mut R {
    let name = <dyn AnyDebug>::type_name(a);
    if let Some(a) = a.downcast_mut() {
        return a;
    }
    panic!("Expected {}, got a {}", type_name::<R>(), name)
}

#[derive(Clone)]
pub struct Kingdom {
    pub swords: Arc<HashMap<Ty, Sword>>,
}
pub struct BuildKingdom {
    swords: HashMap<Ty, Sword>,
}
impl Default for BuildKingdom {
    fn default() -> Self { Self::new() }
}
impl Kingdom {
    pub fn builder() -> BuildKingdom {
        BuildKingdom::default()
    }
    pub fn empty() -> BuildKingdom {
        BuildKingdom {
            swords: Default::default(),
        }
    }
}
impl BuildKingdom {
    pub fn new() -> Self {
        Self {
            swords: crate::impls::register_primals(),
        }
    }
    pub fn add<T: Blade>(&mut self) {
        self.add_blade(Sword::of::<T>());
    }
    pub fn add_blade(&mut self, sword: Sword) {
        self.swords.insert(sword.item.ty.clone(), sword);
    }
    pub fn remove<T>(&mut self) {
        self.swords.remove(&Ty::of::<T>());
    }
    pub fn build(mut self) -> Kingdom {
        use std::cell::RefCell;
        let holes = RefCell::new(vec![]);
        let known = self.swords.keys().cloned().collect::<HashSet<Ty>>();
        for (ty, scab) in &mut self.swords {
            let sty = &scab.item.ty;
            macro_rules! hole {
                ($($tt:tt)*) => {
                    holes.borrow_mut().push(format!($($tt)*));
                };
            }
            if ty != sty {
                hole!("{} is not {:?}", ty.name, sty.name);
            }
            use crate::rt::*;
            let validate_ty = |ty: &Ty| {
                if !known.contains(ty) {
                    hole!("unregistered {} in {}", ty.name, sty.name);
                }
            };
            let validate_fields = |body_type: BodyType, fields: &Vec<Arc<Field>>| {
                if body_type == BodyType::Unit {
                    assert!(fields.is_empty());
                }
                for field in fields {
                    validate_ty(&field.ty);
                }
            };
            match &scab.item.body {
                Body::Primitive => (),
                Body::Struct(bod) => {
                    validate_fields(bod.body_type, &bod.fields);
                },
                Body::Enum(bod) => {
                    for variant in &bod.variants {
                        validate_fields(variant.body_type, &variant.fields);
                    }
                },
                Body::Vec(bod) => {
                    validate_ty(&bod.items);
                },
                Body::Map(bod) => {
                    validate_ty(&bod.keys);
                    validate_ty(&bod.vals);
                },
            }
        }
        let holes = holes.take();
        if !holes.is_empty() {
            let mut msg = format!("\nKingdom validation failed:\n");
            for h in &holes {
                use std::fmt::Write;
                writeln!(&mut msg, "    {}", h).ok();
            }
            panic!("{}", msg);
        }
        Kingdom {
            swords: Arc::new(self.swords),
        }
    }
}
impl Kingdom {
    pub fn visit<'a, E>(
        &self,
        ty: &Ty,
        visitor: &mut dyn BodyVisitor<Err=E>,
    ) -> Result<(), E> {
        let sword = self.swords.get(&ty)
            .unwrap_or_else(|| panic!("missing sword {}", ty));
        self.visit_sword(sword, visitor)
    }
    pub fn visit_sword<'a, E>(
        &self,
        sword: &Sword,
        visitor: &mut dyn BodyVisitor<Err=E>,
    ) -> Result<(), E> {
        let ty = &sword.item.ty;
        let item = &sword.item;
        match &item.body {
            Body::Primitive => {
                visitor.visit_primitive(Visit {
                    item,
                    body: &ty,
                })
            },
            Body::Struct(body) => {
                visitor.visit_struct(Visit {
                    item,
                    body,
                })
            },
            Body::Enum(body) => {
                visitor.visit_enum(Visit {
                    item,
                    body,
                })
            },
            Body::Vec(body) => {
                visitor.visit_vec(Visit {
                    item,
                    body,
                })
            },
            Body::Map(body) => {
                visitor.visit_map(Visit {
                    item,
                    body,
                })
            },
        }
    }
}

#[derive(Debug)]
pub struct Visit<'a, T> {
    pub item: &'a Item,
    pub body: &'a T,
}

pub trait BodyVisitor {
    type Err;
    fn visit_primitive(&mut self, visit: Visit<Ty>) -> Result<(), Self::Err>;
    fn visit_struct(&mut self, visit: Visit<BodyStruct>) -> Result<(), Self::Err>;
    fn visit_enum(&mut self, visit: Visit<BodyEnum>) -> Result<(), Self::Err>;
    fn visit_vec(&mut self, visit: Visit<BodyVec>) -> Result<(), Self::Err>;
    fn visit_map(&mut self, visit: Visit<BodyMap>) -> Result<(), Self::Err>;
}
