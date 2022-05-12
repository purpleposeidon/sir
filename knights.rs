#![allow(dead_code)]
use crate::rt::*;
use std::collections::{HashMap, HashSet};
use crate::util::AnyDebug;
use std::any::TypeId;
use crate::Blade;
use std::sync::Arc;
use crate::rt::AnyOptionT;
use std::any::type_name;
use crate::vocab::EnumTag;

// Sir Jen - Debugger/Inspector
// Sir Mise - Printer
// Sir Vei - Printer
// Sir Loin
// Sir George - Surcharge? idk man
// Sir Plant
// Sir Ial - Serializer
// Sir Vive - Serializer. Or "interact w/ objects defined only dynamically"
// Sir Eel - Serializer
// Sir George (Or was it Michael?) - Slayer of dragons, hence compilers, hence JIT. But maybe it
// should just be Saint George tho.

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
pub struct Armory {
    scabbards: Arc<HashMap<TypeId, Scabbard>>,
}
pub struct BuildArmory {
    scabbards: HashMap<TypeId, Scabbard>,
}
impl Default for BuildArmory {
    fn default() -> Self { Self::new() }
}
impl Armory {
    pub fn builder() -> BuildArmory {
        BuildArmory::default()
    }
}
impl BuildArmory {
    pub fn new() -> Self {
        Self {
            scabbards: crate::impls::register_primals(),
        }
    }
    pub fn add<T: Blade>(&mut self) {
        self.add_blade(Scabbard::of::<T>());
    }
    pub fn add_blade(&mut self, scabbard: Scabbard) {
        let ty = (scabbard.item.ty)().id;
        self.scabbards.insert(ty, scabbard);
    }
    pub fn build(mut self) -> Armory {
        use std::cell::RefCell;
        let holes = RefCell::new(vec![]);
        let known = self.scabbards.keys().cloned().collect::<HashSet<TypeId>>();
        for (ty, scab) in &mut self.scabbards {
            let sty = (scab.item.ty)();
            let sub = sty.id;
            macro_rules! hole {
                ($($tt:tt)*) => {
                    holes.borrow_mut().push(format!($($tt)*));
                };
            }
            if ty != &sub {
                hole!("{} is not {:?}", sty.name, sub);
            }
            use crate::rt::*;
            use crate::List;
            let validate_ty = |ty: fn() -> Ty| {
                let ty = ty();
                if !known.contains(&ty.id) {
                    hole!("unregistered {} in {}", ty.name, sty.name);
                }
            };
            let validate_fields = |body_type: BodyType, fields: &List<Field>| {
                if body_type == BodyType::Unit {
                    assert!(fields.is_empty());
                }
                for field in fields.as_ref() {
                    validate_ty(field.ty);
                }
            };
            let validate_item = |item: &Item| {
                validate_ty(item.ty);
            };
            match &mut scab.item.body {
                Body::Primitive => (),
                Body::Struct(BodyStruct { body_type, fields, .. }) => {
                    validate_fields(*body_type, &fields);
                },
                Body::Enum(BodyEnum { variants, .. }) => {
                    for variant in variants.as_ref() {
                        validate_fields(variant.body_type, &variant.fields);
                    }
                },
                Body::Vec(BodyVec { items, .. }) => {
                    validate_item(&items.as_own().item);
                },
                Body::Map(BodyMap { keys, vals, .. }) => {
                    validate_item(keys.as_own());
                    validate_item(vals.as_own());
                },
            }
        }
        let holes = holes.take();
        if !holes.is_empty() {
            let mut msg = format!("Armory validation failed:\n");
            for h in &holes {
                use std::fmt::Write;
                writeln!(&mut msg, "    {}", h).ok();
            }
            panic!("{}", msg);
        }
        Armory {
            scabbards: Arc::new(self.scabbards),
        }
    }
}
impl Armory {
    pub fn visit<'a, E>(
        &self,
        ty: TypeId,
        visitor: &mut dyn BodyVisitor<Err=E>,
    ) -> Result<(), E> {
        let scabbard = self.scabbards.get(&ty).expect("missing scabbard");
        self.visit_scabbard(scabbard, visitor)
    }
    pub fn visit_scabbard<'a, E>(
        &self,
        scabbard: &Scabbard,
        visitor: &mut dyn BodyVisitor<Err=E>,
    ) -> Result<(), E> {
        let ty = scabbard.item.type_id();
        let item = &scabbard.item;
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


// FIXME: Rename to Quest? Hmm.
pub trait BodyVisitor {
    type Err;
    fn visit_primitive(&mut self, visit: Visit<TypeId>) -> Result<(), Self::Err>;
    fn visit_struct(&mut self, visit: Visit<BodyStruct>) -> Result<(), Self::Err>;
    fn visit_enum(&mut self, visit: Visit<BodyEnum>) -> Result<(), Self::Err>;
    fn visit_vec(&mut self, visit: Visit<BodyVec>) -> Result<(), Self::Err>;
    fn visit_map(&mut self, visit: Visit<BodyMap>) -> Result<(), Self::Err>;
}


// FIXME: Improved trait? Add default methods? Would st_george like this?
// So visit() is a default method that calls enter(), exit(), and performs recursion.
/*pub trait Quest {
    type Err;
    fn enter_item(&mut self, visit: Visit<Item>) -> Result<(), Self::Err>;
    fn visit_item(&mut self, visit: Visit<Item>) -> Result<(), Self::Err>;
    fn leave_item(&mut self, visit: Visit<Item>) -> Result<(), Self::Err>;

    fn visit_primitive(&mut self, visit: Visit<TypeId>) -> Result<(), Self::Err>;
    fn visit_field(&mut self, visit: Visit<Field>) -> Result<(), Self::Err>;

    fn enter_struct(&mut self, visit: Visit<BodyStruct>) -> Result<(), Self::Err>;
    fn leave_struct(&mut self, visit: Visit<BodyStruct>) -> Result<(), Self::Err>;

    fn enter_enum(&mut self, visit: Visit<BodyEnum>) -> Result<(), Self::Err>;
    fn leave_enum(&mut self, visit: Visit<BodyEnum>) -> Result<(), Self::Err>;
    fn enter_enum_variant(&mut self, visit: Visit<Variant>) -> Result<(), Self::Err>;
    fn leave_enum_variant(&mut self, visit: Visit<Variant>) -> Result<(), Self::Err>;

    fn enter_vec(&mut self, visit: Visit<BodyVec>) -> Result<(), Self::Err>;
    fn visit_vec_item(&mut self, visit: Visit<BodyVec>) -> Result<(), Self::Err>;
    fn leave_vec(&mut self, visit: Visit<BodyVec>) -> Result<(), Self::Err>;

    fn enter_map(&mut self, visit: Visit<BodyVec>) -> Result<(), Self::Err>;
    fn visit_map_item(&mut self, visit: Visit<BodyVec>) -> Result<(), Self::Err>;
    fn leave_map(&mut self, visit: Visit<BodyVec>) -> Result<(), Self::Err>;
}*/

pub struct Vey {
    pub armory: Armory,
    pub primitives: HashMap<TypeId, fn(&dyn AnyDebug)>,
}
impl Vey {
    pub fn equip(armory: &Armory) -> Self {
        let mut primitives = HashMap::<TypeId, fn(&dyn AnyDebug)>::new();
        primitives.insert(TypeId::of::<i32>(), |x: &dyn AnyDebug| {
            let x: i32 = *a2r(x);
            print!("{}", x)
        });
        primitives.insert(TypeId::of::<&'static str>(), |x: &dyn AnyDebug| {
            let x: &str = *a2r(x);
            print!("{:?}", x)
        });
        primitives.insert(TypeId::of::<String>(), |x: &dyn AnyDebug| {
            let x: &String = a2r(x);
            print!("{:?}", x)
        });
        Vey {
            armory: armory.clone(),
            primitives,
        }
    }
    pub fn value(&self, val: &dyn AnyDebug) -> Result<(), ()> {
        let ty = <dyn AnyDebug>::type_id(val);
        let mut visitor = VeyVisitor {
            armory: &self.armory,
            depth: 0,
            val,
        };
        self.armory.visit(ty, &mut visitor)?;
        println!();
        Ok(())
    }
}

pub struct VeyVisitor<'a> {
    pub armory: &'a Armory,
    pub depth: usize,
    pub val: &'a dyn AnyDebug,
}
impl<'a> VeyVisitor<'a> {
    fn indent(&self) {
        print!("{:width$}", "", width = self.depth * 4);
    }
}
fn open_bt(bt: BodyType) -> &'static str {
    match bt {
        BodyType::Unit => "",
        BodyType::Tuple => {
            print!("(");
            ")"
        },
        BodyType::Struct => {
            print!(" {{\n");
            "}"
        },
    }
}
type VeyR = Result<(), ()>;
impl<'a> BodyVisitor for VeyVisitor<'a> {
    type Err = ();
    fn visit_primitive(&mut self, _visit: Visit<TypeId>) -> VeyR {
        // FIXME: Use a prim HashMap; AnyDebug -= Debug.
        print!("{:?}", self.val);
        Ok(())
    }
    fn visit_struct(&mut self, visit: Visit<BodyStruct>) -> VeyR {
        print!("{}", (visit.item.ty)().name);
        let closer = open_bt(visit.body.body_type);
        if closer == "" { return Ok(()); }
        let d = if visit.body.body_type == BodyType::Tuple {
            0
        } else {
            1
        };
        let depth = self.depth + d;
        let mut first = true;
        for field in visit.body.fields.iter() {
            let mut sub = VeyVisitor {
                armory: self.armory,
                depth,
                val: field.get_ref(self.val),
            };
            let ty = <dyn AnyDebug>::type_id(sub.val);
            if d == 1 {
                sub.indent();
            } else if first {
                first = false;
            } else {
                print!(", ");
            }
            if visit.body.body_type == BodyType::Struct {
                print!("{}: ", field.name);
            }
            self.armory.visit(ty, &mut sub)?;
            if d == 1 {
                println!(",");
            }
        }
        if d == 1 {
            self.indent();
        }
        print!("{}", closer);
        Ok(())
    }
    fn visit_enum(&mut self, visit: Visit<BodyEnum>) -> VeyR {
        let discrim = (visit.body.discrim)(self.val);
        let variant = &visit.body.variants[discrim];
        print!("{}", variant.name);
        let closer = open_bt(variant.body_type);
        if closer == "" { return Ok(()); }
        if let BodyType::Struct = variant.body_type {
            let depth = self.depth + 1;
            for field in variant.fields.iter() {
                let mut sub = VeyVisitor {
                    armory: self.armory,
                    depth,
                    val: field.get_ref(self.val),
                };
                let ty = <dyn AnyDebug>::type_id(sub.val);
                self.indent();
                print!("{}: ", field.name);
                self.armory.visit(ty, &mut sub)?;
                println!(",");
            }
        } else {
            let mut first = true;
            for field in variant.fields.iter() {
                if first {
                    first = false;
                } else {
                    print!(", ");
                }
                let mut sub = VeyVisitor {
                    armory: self.armory,
                    depth: self.depth,
                    val: field.get_ref(self.val),
                };
                let ty = <dyn AnyDebug>::type_id(sub.val);
                self.armory.visit(ty, &mut sub)?;
            }
        }
        print!("{}", closer);
        Ok(())
    }
    fn visit_vec(&mut self, visit: Visit<BodyVec>) -> VeyR {
        let len = (visit.body.vt.len)(self.val);
        let ty = visit.body.items.with(|w| w.item.type_id());
        if len == 0 {
            print!("vec![]");
            return Ok(());
        } else if len == 1 {
            print!("vec![");
            let val = (visit.body.vt.read)(self.val, 0);
            let mut sub = VeyVisitor {
                armory: self.armory,
                depth: self.depth,
                val,
            };
            self.armory.visit(ty, &mut sub)?;
            print!("]");
            return Ok(());
        }
        println!("vec![");
        let depth = self.depth + 1;
        for i in 0..len {
            let val = (visit.body.vt.read)(self.val, i);
            let mut sub = VeyVisitor {
                armory: self.armory,
                depth,
                val,
            };
            sub.indent();
            self.armory.visit(ty, &mut sub)?;
            println!(",");
        }
        self.indent();
        print!("]");
        Ok(())
    }
    fn visit_map(&mut self, visit: Visit<BodyMap>) -> VeyR {
        let mut ret = Ok(());
        macro_rules! tri {
            ($x:expr) => {
                match $x {
                    Ok(x) => x,
                    Err(e) => {
                        ret = Err(e);
                        return;
                    },
                }
            };
        }
        let len = (visit.body.vt.len)(self.val);
        let kty = visit.body.keys.type_id();
        let vty = visit.body.vals.type_id();
        if len == 0 {
            print!("{{}}");
            return Ok(());
        } else if len == 1 {
            (visit.body.vt.iter_items)(self.val, &mut |iter| {
                let mut sep = "";
                for (k, v) in iter {
                    print!("{}{{ ", sep);
                    let sub = &mut VeyVisitor {
                        armory: self.armory,
                        depth: self.depth,
                        val: k,
                    };
                    tri![self.armory.visit(kty, sub)];
                    print!(": ");
                    let sub = &mut VeyVisitor {
                        armory: self.armory,
                        depth: self.depth,
                        val: v,
                    };
                    tri![self.armory.visit(vty, sub)];
                    print!(" }}");
                    sep = "∪"; // Don't lie to me.
                }
            });
            return ret;
        }
        println!("{{");
        let depth = self.depth + 1;
        (visit.body.vt.iter_items)(self.val, &mut |iter| {
            for (k, v) in iter {
                let sub = &mut VeyVisitor {
                    armory: self.armory,
                    depth,
                    val: k,
                };
                sub.indent();
                tri![self.armory.visit(kty, sub)];
                print!(": ");
                let sub = &mut VeyVisitor {
                    armory: self.armory,
                    depth,
                    val: v,
                };
                tri![self.armory.visit(vty, sub)];
                println!(",");
            }
        });
        self.indent();
        print!("}}");
        ret
    }
}


#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum LuaType {
    Nil,
    Boolean,
    LightUserData,
    Integer,
    Number,
    String,
    Table,
    Function,
    Thread,
    UserData,
    Error,
}
impl LuaType {
    fn from(val: &rlua::Value) -> LuaType {
        use rlua::Value;
        match val {
            Value::Nil => LuaType::Nil,
            Value::Boolean(_) => LuaType::Boolean,
            Value::LightUserData(_) => LuaType::LightUserData,
            Value::Integer(_) => LuaType::Integer,
            Value::Number(_) => LuaType::Number,
            Value::String(_) => LuaType::String,
            Value::Table(_) => LuaType::Table,
            Value::Function(_) => LuaType::Function,
            Value::Thread(_) => LuaType::Thread,
            Value::UserData(_) => LuaType::UserData,
            Value::Error(_) => LuaType::Error,
        }
    }
    fn name(&self) -> &'static str {
        match self {
            LuaType::Nil => "Nil",
            LuaType::Boolean => "Boolean",
            LuaType::LightUserData => "LightUserData",
            LuaType::Integer => "Integer",
            LuaType::Number => "Number",
            LuaType::String => "String",
            LuaType::Table => "Table",
            LuaType::Function => "Function",
            LuaType::Thread => "Thread",
            LuaType::UserData => "UserData",
            LuaType::Error => "Error",
        }
    }
}

use rlua::FromLua;

/// Create some value from `Lua` and `LuaValue`, and replace it into `&mut Option<T>`
pub type PrimitiveConverter = Box<dyn for<'lua, 'out> Fn(&'lua rlua::Lua, rlua::Value<'lua>, AnyOptionT<'out>) -> Result<(), rlua::Error>>;
pub struct Mun {
    armory: Armory,
    convert_primitive: HashMap<
        (LuaType, TypeId),
        PrimitiveConverter,
    >, // i swear this was completely unintentional
}
impl Mun {
    pub fn new(armory: &Armory) -> Self {
        let mut ret = Mun { armory: armory.clone(), convert_primitive: Default::default() };
        ret.default_setup();
        ret
    }
    fn default_setup(&mut self) {
        //self.prim::<()>(LuaType::Nil);
        self.prim::<bool>(LuaType::Boolean);
        self.prim::<i8>(LuaType::Integer);
        self.prim::<u8>(LuaType::Integer);
        self.prim::<i16>(LuaType::Integer);
        self.prim::<u16>(LuaType::Integer);
        self.prim::<i32>(LuaType::Integer);
        self.prim::<u32>(LuaType::Integer);
        self.prim::<i64>(LuaType::Integer);
        self.prim::<u64>(LuaType::Integer);
        //self.prim::<i128>(LuaType::Integer);
        //self.prim::<u128>(LuaType::Integer);
        self.prim::<isize>(LuaType::Integer);
        self.prim::<usize>(LuaType::Integer);
        self.prim::<f32>(LuaType::Number);
        self.prim::<f64>(LuaType::Number);
        self.prim::<String>(LuaType::String);
    }
    fn prim<T>(&mut self, lt: LuaType)
    where
        for<'lua> T: AnyDebug + FromLua<'lua>,
    {
        let c = |lua: &rlua::Lua, value: rlua::Value, out: AnyOptionT| -> Result<(), rlua::Error> {
            let val: T = FromLua::from_lua(value, lua)?;
            let out: &mut Option<T> = out.downcast_mut().unwrap();
            *out = Some(val);
            Ok(())
        };
        self.convert_primitive.insert(
            (lt, TypeId::of::<T>()),
            Box::new(c),
        );
    }
    pub fn scabbard<T: AnyDebug>(&self) -> Result<&Scabbard, rlua::Error> {
        self.armory.scabbards.get(&TypeId::of::<T>())
            .ok_or_else(|| rlua::Error::RuntimeError(format!("{} has no scabbard", type_name::<T>())))
    }
    pub fn create<T: AnyDebug>(
        &self,
        lua: &rlua::Lua,
        src: rlua::Value,
    ) -> Result<T, rlua::Error> {
        let mut ret = Option::<T>::None;
        let scabbard = self.scabbard::<T>()?;
        self.create0(lua, src, &mut ret, scabbard)?;
        ret
            .ok_or_else(|| rlua::Error::RuntimeError(format!("{} value not created", type_name::<T>())))
    }
    fn create0(
        &self,
        lua: &rlua::Lua,
        src: rlua::Value,
        dst: AnyOptionT,
        scabbard: &Scabbard,
    ) -> Result<(), rlua::Error> {
        let mut visitor = MunVisitor {
            mun: self,
            lua,
            src,
            dst,
            ctx: Ctx {
                parent: None,
                part: CtxPart::Head,
            },
        };
        self.armory.visit_scabbard(scabbard, &mut visitor)
    }
}

trait Take {
    fn take(&mut self) -> Self;
}
impl<'lua> Take for rlua::Value<'lua> {
    fn take(&mut self) -> Self {
        std::mem::replace(self, rlua::Value::Nil)
    }
}

#[derive(Clone)]
struct Ctx<'a> {
    parent: Option<&'a Self>,
    part: CtxPart<'a>,
}
#[derive(Clone)]
enum CtxPart<'a> {
    Head,
    Field(&'a str),
    Variant(&'a str),
    Index(usize),
}
impl<'a> Ctx<'a> {
    fn write(&self, path: &mut String) {
        use std::fmt::Write;
        if let Some(p) = self.parent {
            p.write(path);
        };
        match self.part {
            CtxPart::Head => Ok(()),
            CtxPart::Field(f) => {
                if path.is_empty() {
                    write!(path, "{}", f)
                } else {
                    write!(path, ".{}", f)
                }
            },
            _ if {
                if path.is_empty() {
                    write!(path, "it").ok();
                }
                false
            } => Ok(()),
            CtxPart::Variant(v) => write!(path, "::{}", v),
            CtxPart::Index(i) => write!(path, "[{}]", i),
        }.ok();
    }
    fn to_string(&self) -> String {
        let mut s = String::new();
        self.write(&mut s);
        format!("path: {}", s)
    }
    fn _to_string2(&self) -> String {
        let mut s = String::new();
        self.write(&mut s);
        s
    }
    fn ctx<T>(&self, r: Result<T, rlua::Error>) -> Result<T, rlua::Error> {
        use rlua::Error::*;
        r.map_err(|e| match e {
            ToLuaConversionError { from, to, message: None } => ToLuaConversionError { from, to, message: Some(self.to_string()) },
            FromLuaConversionError { from, to, message: None } => FromLuaConversionError { from, to, message: Some(self.to_string()) },
            e => e,
        })
    }
}

struct MunVisitor<'a, 'lua, 'dst> {
    mun: &'a Mun,
    lua: &'lua rlua::Lua,
    src: rlua::Value<'lua>,
    dst: AnyOptionT<'dst>,
    ctx: Ctx<'a>,
}
impl<'a, 'lua, 'dst> BodyVisitor for MunVisitor<'a, 'lua, 'dst> {
    type Err = rlua::Error;
    fn visit_primitive(&mut self, visit: Visit<TypeId>) -> Result<(), rlua::Error> {
        let key = (
            LuaType::from(&self.src),
            visit.item.type_id(),
        );
        if let Some(prim) = self.mun.convert_primitive.get(&key) {
            let val = self.src.take();
            prim(&self.lua, val, self.dst)
        } else {
            let mut path = String::new();
            self.ctx.write(&mut path);
            Err(rlua::Error::FromLuaConversionError {
                from: key.0.name(),
                to: self.mun.armory.scabbards.get(&key.1)
                    .map(|s| (s.item.ty)().name)
                    .unwrap_or("<type not registered>"),
                message: Some(format!("in {}", path)),
            })
        }
    }
    fn visit_struct(&mut self, visit: Visit<BodyStruct>) -> Result<(), rlua::Error> {
        let table = self.ctx.ctx(rlua::Table::from_lua(self.src.take(), self.lua))?;
        let mut fields = visit.body.fields.iter();
        let mut err = Ok(());
        let mun = &self.mun;
        let lua = self.lua;
        let ctx = &self.ctx;
        (visit.body.init)(self.dst, &mut |dst: AnyOptionT| {
            if err.is_err() { return; }
            if let Some(field) = fields.next() {
                let src = match ctx.ctx(table.raw_get(field.name)) {
                    Ok(s) => s,
                    Err(e) => {
                        err = Err(e);
                        return;
                    },
                };
                let sub = Ctx {
                    parent: Some(ctx),
                    part: CtxPart::Field(field.name),
                };
                let mut sub = MunVisitor { mun, lua, src, dst, ctx: sub };
                let ty = field.type_id();
                err = ctx.ctx(mun.armory.visit(ty, &mut sub));
            }
        });
        err
    }
    fn visit_enum(&mut self, visit: Visit<BodyEnum>) -> Result<(), rlua::Error> {
        let variants = &visit.body.variants[..];
        let err = |from: &'static str, message: &str| -> Result<(), rlua::Error> {
            Err(rlua::Error::FromLuaConversionError {
                from,
                to: (visit.item.ty)().name,
                message: Some(format!("{}", message)),
            })
        };
        let tag = visit.item.guarded::<EnumTag>().unwrap_or(&EnumTag::External);
        let src = match &self.src {
            rlua::Value::Table(src) => src,
            // FIXME: `nil`-forbidding guard?
            rlua::Value::Nil => {
                // For Option::None: use the lone unit variant.
                let mut single = None;
                for variant in variants {
                    if variant.is_unit() {
                        if single.is_some() {
                            return err("nil", "multiple possible unit variants");
                        }
                        single = Some(variant);
                    }
                }
                return if let Some(variant) = single {
                    let mut visitor = MunVisitor {
                        mun: &self.mun,
                        lua: self.lua,
                        src: rlua::Value::Nil,
                        dst: self.dst,
                        ctx: Ctx {
                            parent: Some(&self.ctx),
                            part: CtxPart::Variant(variant.name),
                        },
                    };
                    visitor.visit_variant(Visit {
                        item: visit.item,
                        body: variant,
                    })
                } else {
                    err("nil", "not a table")
                };
            },
            rlua::Value::String(src) => {
                let src: &str = src.to_str()?;
                for variant in variants {
                    if !variant.is_unit() { continue; }
                    if variant.name != src { continue; }
                    let mut visitor = MunVisitor {
                        mun: &self.mun,
                        lua: self.lua,
                        src: self.src.clone(),
                        dst: self.dst,
                        ctx: Ctx {
                            parent: Some(&self.ctx),
                            part: CtxPart::Variant(variant.name),
                        },
                    };
                    return visitor.visit_variant(Visit {
                        item: visit.item,
                        body: variant,
                    });
                }
                return err("string", "does not name a unit variant");
            },
            _ => {
                // FIXME: Try a map.
                return err("<value>", "not a table");
            },
        };
        match tag {
            // {"VariantName": { …data… }}
            EnumTag::External => {
                if src.clone().pairs::<rlua::Value, rlua::Value>().count() != 1 {
                    return err("table", "expected single-element table");
                }
                for variant in variants {
                    let src: rlua::Value = src.get(variant.name)?;
                    if let rlua::Value::Nil = src {
                        continue;
                    }
                    let mut visitor = MunVisitor {
                        mun: &self.mun,
                        lua: self.lua,
                        src,
                        dst: self.dst,
                        ctx: Ctx {
                            parent: Some(&self.ctx),
                            part: CtxPart::Variant(variant.name),
                        },
                    };
                    return visitor.visit_variant(Visit {
                        item: visit.item,
                        body: variant,
                    });
                }
            },
            // { "type": "VariantName", …data… }
            EnumTag::Internal { label_name } | EnumTag::Adjacent { type_name: label_name, .. } => {
                let variant_name: rlua::String = src.get(*label_name)?;
                let variant_name = variant_name.to_str()?;
                let src: rlua::Table = if let EnumTag::Adjacent { data_name, .. } = tag {
                    src.get(*data_name)?
                } else {
                    src.clone()
                };
                let src = rlua::Value::Table(src);
                for variant in variants {
                    if variant_name != variant.name { continue; }
                    let mut visitor = MunVisitor {
                        mun: &self.mun,
                        lua: self.lua,
                        src,
                        dst: self.dst,
                        ctx: Ctx {
                            parent: Some(&self.ctx),
                            part: CtxPart::Variant(variant.name),
                        },
                    };
                    return visitor.visit_variant(Visit {
                        item: visit.item,
                        body: variant,
                    });
                }
                return err("<variant>", "unknown variant");
            },
            // { …data… }
            EnumTag::Untagged => {
                for variant in variants {
                    let mut visitor = MunVisitor {
                        mun: &self.mun,
                        lua: self.lua,
                        src: rlua::Value::Table(src.clone()),
                        dst: self.dst,
                        ctx: Ctx {
                            parent: Some(&self.ctx),
                            part: CtxPart::Variant(variant.name),
                        },
                    };
                    let ret = visitor.visit_variant(Visit {
                        item: visit.item,
                        body: variant,
                    });
                    if ret.is_ok() {
                        return ret;
                    }
                }
                return err("table", "unknown untagged variant");
            },
        }
        impl<'a, 'lua, 'dst> MunVisitor<'a, 'lua, 'dst> {
            fn visit_variant(&mut self, visit: Visit<Variant>) -> Result<(), rlua::Error> {
                if visit.body.is_unit() {
                    (visit.body.init)(self.dst, &mut |_dst: AnyOptionT| {
                        unreachable!()
                    });
                    return Ok(());
                }
                let table = self.ctx.ctx(rlua::Table::from_lua(self.src.take(), self.lua))?;
                let mut fields = visit.body.fields.iter();
                let mut err = Ok(());
                let mun = &self.mun;
                let lua = self.lua;
                let ctx = &self.ctx;
                match visit.body.body_type {
                    BodyType::Unit => (),
                    BodyType::Tuple => {
                        let mut i = 0;
                        (visit.body.init)(self.dst, &mut |dst: AnyOptionT| {
                            if err.is_err() { return; }
                            if let Some(field) = fields.next() {
                                i += 1;
                                let src = match ctx.ctx(table.raw_get(i)) {
                                    Ok(s) => s,
                                    Err(e) => {
                                        err = Err(e);
                                        return;
                                    },
                                };
                                let sub = Ctx {
                                    parent: Some(ctx),
                                    part: CtxPart::Field(field.name),
                                };
                                let mut sub = MunVisitor { mun, lua, src, dst, ctx: sub };
                                let ty = field.type_id();
                                err = ctx.ctx(mun.armory.visit(ty, &mut sub));
                            }
                        });
                    },
                    BodyType::Struct => {
                        (visit.body.init)(self.dst, &mut |dst: AnyOptionT| {
                            if err.is_err() { return; }
                            if let Some(field) = fields.next() {
                                let src = match ctx.ctx(table.raw_get(field.name)) {
                                    Ok(s) => s,
                                    Err(e) => {
                                        err = Err(e);
                                        return;
                                    },
                                };
                                let sub = Ctx {
                                    parent: Some(ctx),
                                    part: CtxPart::Field(field.name),
                                };
                                let mut sub = MunVisitor { mun, lua, src, dst, ctx: sub };
                                let ty = field.type_id();
                                err = ctx.ctx(mun.armory.visit(ty, &mut sub));
                            }
                        });
                    },
                }
                err
            }
        }
        err("<value>", "no matching variant")
    }
    fn visit_vec(&mut self, _visit: Visit<BodyVec>) -> Result<(), rlua::Error> { todo!() }
    fn visit_map(&mut self, visit: Visit<BodyMap>) -> Result<(), rlua::Error> {
        let src: rlua::Table = FromLua::from_lua(self.src.clone(), self.lua)?;
        let mut src = src.pairs();
        let kty = visit.body.keys.type_id();
        let vty = visit.body.vals.type_id();
        let ks = self.mun.armory.scabbards.get(&kty).expect("key missing scabbard");
        let vs = self.mun.armory.scabbards.get(&vty).expect("val missing scabbard");
        let mut res = Ok(());
        let mun = &self.mun;
        let lua = self.lua;
        let ctx = &self.ctx;
        let mut i = 1; // FIXME: Not very informative.
        (visit.body.vt.collect)(self.dst, None /* FIXME: reserve */, &mut |key: AnyOptionT, val: AnyOptionT| {
            if let Some(Ok(kv)) = src.next() {
                let ctx = Ctx {
                    parent: Some(ctx),
                    part: CtxPart::Index(i),
                };
                {
                    let mut visitor = MunVisitor {
                        mun, lua,
                        src: kv.0,
                        dst: key,
                        ctx: ctx.clone(),
                    };
                    res = mun.armory.visit_scabbard(ks, &mut visitor);
                }
                if res.is_err() { return; }
                {
                    let mut visitor = MunVisitor {
                        mun, lua,
                        src: kv.1,
                        dst: val,
                        ctx,
                    };
                    res = mun.armory.visit_scabbard(vs, &mut visitor);
                }
            }
            i += 1;
        });
        res
    }
}

use std::fmt;
pub struct Mark {
    armory: Armory,
    // SURMARK
    // Sur"mark`, n. (Shipbuilding)
    //
    // Defn: A mark made on the molds of a ship, when building, to show
    // where the angles of the timbers are to be placed. [Written also
    // sirmark.]
}
impl Mark {
    // Simply serialize values to lua.
    pub fn new(armory: &Armory) -> Mark {
        Mark { armory: armory.clone() }
    }
    pub fn value(&self, mut out: impl fmt::Write, var: &'static str, val: &dyn AnyDebug) -> fmt::Result {
        write!(out, "{} = ", var)?;
        let mut m = MarkVisitor {
            armory: &self.armory,
            val,
            out: &mut out,
            depth: 0,
        };
        let ty = <dyn AnyDebug>::type_id(val);
        self.armory.visit(ty, &mut m)
    }
}
struct MarkVisitor<'a, W: fmt::Write> {
    armory: &'a Armory,
    val: &'a dyn AnyDebug,
    out: &'a mut W,
    depth: usize,
}
impl<'a, W: fmt::Write> MarkVisitor<'a, W> {
    fn indent(&mut self) {
        write!(self.out, "{:width$}", "", width = self.depth * 4).ok();
    }
    fn write_variant(&mut self, variant: &Variant) -> fmt::Result {
        let mut first = true;
        let val = self.val;
        for field in variant.fields.iter() {
            if first { first = false; }
            else { write!(self.out, ", ")?; }
            self.val = (field.as_ref)(val);
            self.armory.visit(field.type_id(), self)?;
        }
        Ok(())
    }
}
impl<'a, W: fmt::Write> BodyVisitor for MarkVisitor<'a, W> {
    type Err = fmt::Error;
    fn visit_primitive(&mut self, _visit: Visit<TypeId>) -> fmt::Result {
        write!(self.out, "{:?}", self.val)
    }
    fn visit_struct(&mut self, visit: Visit<BodyStruct>) -> fmt::Result {
        let val = self.val;
        match visit.body.body_type {
            BodyType::Unit => return write!(self.out, "{{}}"), // FIXME: Is this enough?
            BodyType::Tuple => {
                write!(self.out, "{{ ")?;
                let mut first = true;
                for field in visit.body.fields.iter() {
                    if first { first = false; }
                    else { write!(self.out, ", ")?; }
                    self.val = (field.as_ref)(val);
                    self.armory.visit(field.type_id(), self)?;
                }
                self.val = val;
                write!(self.out, " }}")
            },
            BodyType::Struct => {
                writeln!(self.out, "{{")?;
                self.depth += 1;
                for field in visit.body.fields.iter() {
                    self.indent();
                    write!(self.out, "{} = ", field.name)?; // FIXME: Assumes lua likes this name.
                    self.val = (field.as_ref)(val);
                    self.armory.visit(field.type_id(), self)?;
                    writeln!(self.out, ",")?;
                }
                self.val = val;
                self.depth -= 1;
                self.indent();
                write!(self.out, "}}")
            },
        }
    }
    fn visit_enum(&mut self, visit: Visit<BodyEnum>) -> fmt::Result {
        let discrim = (visit.body.discrim)(self.val);
        let variant = &visit.body.variants[discrim];
        if variant.is_unit() {
            return write!(self.out, "{:?}", variant.name);
        }
        // FIXME: #[repr(_)] discrim = ???
        let tag = visit.item.guarded::<EnumTag>().unwrap_or(&EnumTag::External);
        // Ok(3) becomes ...
        match tag {
            // { Ok = { 3 } }
            EnumTag::External => {
                write!(self.out, "{{ {} = {{ ", variant.name)?;
                self.write_variant(variant)?;
                write!(self.out, " }} }}")
            },
            // { label_name = "Ok", 3 }
            EnumTag::Internal { label_name } => {
                write!(self.out, "{{ {} = {:?}, ", label_name, variant.name)?;
                self.write_variant(variant)?;
                write!(self.out, " }}")
            },
            // { type_name: "VariantName", data_name: { …data… }}
            EnumTag::Adjacent { type_name, data_name } => {
                write!(self.out, "{{ {} = {:?}, {} = {{", type_name, variant.name, data_name)?;
                self.write_variant(variant)?;
                write!(self.out, " }}")
            },
            // { 3 }
            EnumTag::Untagged => {
                // FIXME: Assert unamibiguity.
                write!(self.out, "{{ ")?;
                self.write_variant(variant)?;
                write!(self.out, " }}")
            },
        }
    }
    fn visit_vec(&mut self, _visit: Visit<BodyVec>) -> fmt::Result { todo!() }
    fn visit_map(&mut self, visit: Visit<BodyMap>) -> fmt::Result {
        writeln!(self.out, "{{")?;
        self.depth += 1;
        let key_ty = visit.body.keys.type_id();
        let val_ty = visit.body.vals.type_id();
        (visit.body.vt.iter_items)(self.val, &mut |iter: &mut dyn Iterator<Item=(AnyKey, &dyn AnyDebug)>| {
            for (key, val) in iter {
                self.indent();
                write!(self.out, "[").ok();
                let mut visitor = MarkVisitor {
                    armory: self.armory,
                    val: key,
                    out: self.out,
                    depth: self.depth,
                };
                self.armory.visit(key_ty, &mut visitor).ok();
                write!(self.out, "] = ").ok();
                let mut visitor = MarkVisitor {
                    armory: self.armory,
                    val: val,
                    out: self.out,
                    depth: self.depth,
                };
                self.armory.visit(val_ty, &mut visitor).ok();
                writeln!(self.out, ",").ok();
            }
        });
        self.depth -= 1;
        self.indent();
        write!(self.out, "}}")
    }
}

use std::io::{Read, Write};
use bincode::config::Config;
use bincode::de::Decode;
use bincode::enc::Encode;
use bincode::error::{DecodeError, EncodeError};
pub type DResult = Result<(), DecodeError>;
pub type EResult = Result<(), EncodeError>;
pub type ReadPrim<C, R> = for<'w, 'out> fn(C, &'w mut R, AnyOptionT<'out>) -> DResult;
pub type WritePrim<C, W> = fn(C, &mut W, &dyn AnyDebug) -> EResult;
pub struct Eel<C: Config, R: Read, W: Write> {
    armory: Armory,
    prim_read: HashMap<TypeId, ReadPrim<C, R>>,
    prim_write: HashMap<TypeId, WritePrim<C, W>>,
    // FIXME: Ought not mix read & write...
    // Eg it's very reasonable to need multiple readers but only 1 writer
}
impl<C: Config, R: Read, W: Write> Eel<C, R, W> {
    pub fn new(armory: &Armory) -> Self {
        let mut ret = Eel {
            armory: armory.clone(),
            prim_read: Default::default(),
            prim_write: Default::default(),
        };
        ret.default_setup();
        ret
    }
    fn default_setup(&mut self) {
        self.prim::<()>();
        self.prim::<bool>();
        self.prim::<i8>();
        self.prim::<u8>();
        self.prim::<i16>();
        self.prim::<u16>();
        self.prim::<i32>();
        self.prim::<u32>();
        self.prim::<i64>();
        self.prim::<u64>();
        self.prim::<i128>();
        self.prim::<u128>();
        // Too risky.
        //self.prim::<isize>();
        //self.prim::<usize>();
        self.prim::<f32>();
        self.prim::<f64>();
        self.prim::<String>();
    }
    pub fn prim<T: AnyDebug + Decode + Encode>(&mut self) {
        let f = |cfg: C, fd: &mut R, out: AnyOptionT| -> DResult {
            let out: &mut Option<T> = out.downcast_mut().expect("type mismatch");
            *out = Some(bincode::decode_from_std_read(fd, cfg)?);
            Ok(())
        };
        self.prim_read.insert(TypeId::of::<T>(), f);
        let f = |cfg: C, out: &mut W, val: &dyn AnyDebug| -> EResult {
            let val: &T = val.downcast_ref().expect("type mismatch");
            bincode::encode_into_std_write::<&T, C, W>(val, out, cfg)?;
            Ok(())
        };
        self.prim_write.insert(TypeId::of::<T>(), f);
    }
    pub fn read<T: AnyDebug>(&self, cfg: C, read: &mut R) -> Result<T, DecodeError> {
        let mut out = Option::<T>::None;
        let mut reader = DeSirEelIce {
            armory: &self.armory,
            prim_read: &self.prim_read,
            cfg,
            read,
            dst: &mut out,
        };
        let ty = TypeId::of::<T>();
        self.armory.visit(ty, &mut reader)?;
        out.ok_or_else(|| DecodeError::OtherString(format!("value not created")))
    }
    pub fn write(&self, cfg: C, write: &mut W, val: &dyn AnyDebug) -> EResult {
        let mut writer = SirEelIce {
            armory: &self.armory,
            prim_write: &self.prim_write,
            cfg,
            write,
            val,
        };
        let ty = <dyn AnyDebug>::type_id(val);
        self.armory.visit(ty, &mut writer)
    }
}
struct DeSirEelIce<'a, 'dst, C: Config, R: Read> {
    armory: &'a Armory,
    prim_read: &'a HashMap<TypeId, ReadPrim<C, R>>,
    cfg: C,
    read: &'a mut R,
    dst: AnyOptionT<'dst>,
}
impl<'a, 'dst, C: Config, R: Read> BodyVisitor for DeSirEelIce<'a, 'dst, C, R> {
    type Err = DecodeError;
    fn visit_primitive(&mut self, visit: Visit<TypeId>) -> Result<(), Self::Err> {
        let reader = self.prim_read.get(&visit.body).expect("unknown type");
        reader(self.cfg, self.read, self.dst)
    }
    fn visit_struct(&mut self, visit: Visit<BodyStruct>) -> Result<(), Self::Err> {
        let mut fields = visit.body.fields.iter();
        let mut ret = Ok(());
        let armory = self.armory;
        let prim_read = self.prim_read;
        let cfg = self.cfg;
        let read = &mut *self.read;
        (visit.body.init)(self.dst, &mut |dst: AnyOptionT| {
            if ret.is_err() { return; }
            if let Some(field) = fields.next() {
                let mut sub = DeSirEelIce { armory, prim_read, cfg, read, dst };
                let ty = field.type_id();
                ret = armory.visit(ty, &mut sub);
            }
        });
        ret
    }
    fn visit_enum(&mut self, visit: Visit<BodyEnum>) -> Result<(), Self::Err> {
        let discrim: usize = {
            let cfg = bincode::config::standard();
            bincode::decode_from_std_read(self.read, cfg)?
        };
        let variant = if let Some(variant) = visit.body.variants.get(discrim) {
            // NB/FIXME: our 'variant index' is not Rust's.
            variant
        } else {
            return Err(DecodeError::UnexpectedVariant {
                type_name: "<not implemented>",
                allowed: bincode::error::AllowedEnumVariants::Allowed(&[]),
                found: discrim as u32,
            });
        };
        let mut fields = variant.fields.iter();
        let mut ret = Ok(());
        let armory = self.armory;
        let prim_read = self.prim_read;
        let cfg = self.cfg;
        let read = &mut *self.read;
        (variant.init)(self.dst, &mut |dst: AnyOptionT| {
            if ret.is_err() { return; }
            if let Some(field) = fields.next() {
                let mut sub = DeSirEelIce { armory, prim_read, cfg, read, dst };
                let ty = field.type_id();
                ret = armory.visit(ty, &mut sub);
            }
        });
        ret
    }
    fn visit_vec(&mut self, visit: Visit<BodyVec>) -> Result<(), Self::Err> {
        let len: usize = {
            let cfg = bincode::config::standard();
            bincode::decode_from_std_read(self.read, cfg)?
        };
        let ty = visit.body.items.with(|w| w.item.type_id());
        let mut ret = Ok(());
        let mut i = 0;
        let armory = self.armory;
        let prim_read = self.prim_read;
        let cfg = self.cfg;
        let read = &mut *self.read;
        (visit.body.vt.collect)(self.dst, Some(len), &mut |dst: AnyOptionT| {
            if i == len { return; }
            i += 1;
            let mut sub = DeSirEelIce { armory, prim_read, cfg, read, dst };
            let e = armory.visit(ty, &mut sub);
            if e.is_err() {
                ret = e;
                return;
            }
        });
        ret
    }
    fn visit_map(&mut self, visit: Visit<BodyMap>) -> Result<(), Self::Err> {
        let len: usize = {
            let cfg = bincode::config::standard();
            bincode::decode_from_std_read(self.read, cfg)?
        };
        let key_ty = visit.body.keys.with(|w| (w.ty)().id);
        let val_ty = visit.body.vals.with(|w| (w.ty)().id);
        let mut ret = Ok(());
        let mut i = 0;
        let armory = self.armory;
        let prim_read = self.prim_read;
        let cfg = self.cfg;
        let read = &mut *self.read;
        (visit.body.vt.collect)(self.dst, Some(len), &mut |key: AnyOptionT, val: AnyOptionT| {
            if i == len { return; }
            i += 1;
            let mut sub = DeSirEelIce { armory, prim_read, cfg, read, dst: key };
            let e = armory.visit(key_ty, &mut sub);
            if e.is_err() {
                ret = e;
                return;
            }
            let mut sub = DeSirEelIce { armory, prim_read, cfg, read, dst: val };
            let e = armory.visit(val_ty, &mut sub);
            if e.is_err() {
                ret = e;
                return;
            }
        });
        ret
    }
}
struct SirEelIce<'a, C: Config, W: Write> {
    armory: &'a Armory,
    prim_write: &'a HashMap<TypeId, WritePrim<C, W>>,
    cfg: C,
    write: &'a mut W,
    val: &'a dyn AnyDebug,
}
impl<'a, C: Config, W: Write> BodyVisitor for SirEelIce<'a, C, W> {
    type Err = EncodeError;
    fn visit_primitive(&mut self, _visit: Visit<TypeId>) -> Result<(), Self::Err> {
        let ty = <dyn AnyDebug>::type_id(self.val);
        let writer = self.prim_write.get(&ty).expect("unknown type");
        writer(self.cfg, self.write, self.val)
    }
    fn visit_struct(&mut self, visit: Visit<BodyStruct>) -> Result<(), Self::Err> {
        for field in visit.body.fields.iter() {
            let mut sub = SirEelIce {
                armory: self.armory,
                prim_write: self.prim_write,
                cfg: self.cfg,
                write: self.write,
                val: field.get_ref(self.val),
            };
            let ty = <dyn AnyDebug>::type_id(sub.val);
            self.armory.visit(ty, &mut sub)?;
        }
        Ok(())
    }
    fn visit_enum(&mut self, visit: Visit<BodyEnum>) -> Result<(), Self::Err> {
        let discrim: usize = (visit.body.discrim)(self.val);
        {
            let cfg = bincode::config::standard();
            bincode::encode_into_std_write(discrim, self.write, cfg)
                .map(|_| ())?;
        }
        // NB/FIXME: our 'variant index' is not Rust's.
        let variant = &visit.body.variants[discrim];
        for field in variant.fields.iter() {
            let mut sub = SirEelIce {
                armory: self.armory,
                prim_write: self.prim_write,
                cfg: self.cfg,
                write: self.write,
                val: field.get_ref(self.val),
            };
            let ty = <dyn AnyDebug>::type_id(sub.val);
            self.armory.visit(ty, &mut sub)?;
        }
        Ok(())
    }
    fn visit_vec(&mut self, visit: Visit<BodyVec>) -> Result<(), Self::Err> {
        {
            let len: usize = (visit.body.vt.len)(self.val);
            let cfg = bincode::config::standard();
            bincode::encode_into_std_write(len, self.write, cfg)
                .map(|_| ())?;
        }
        let ty = visit.body.items.with(|w| w.item.type_id());
        let mut ret = Ok(());
        (visit.body.vt.iter)(self.val, &mut |iter: &mut dyn Iterator<Item=&dyn AnyDebug>| {
            for val in iter {
                let mut sub = SirEelIce {
                    armory: self.armory,
                    prim_write: self.prim_write,
                    cfg: self.cfg,
                    write: self.write,
                    val,
                };
                let e = self.armory.visit(ty, &mut sub);
                if e.is_err() {
                    ret = e;
                    return;
                }
            }
        });
        ret
    }
    fn visit_map(&mut self, visit: Visit<BodyMap>) -> Result<(), Self::Err> {
        {
            let len: usize = (visit.body.vt.len)(self.val);
            let cfg = bincode::config::standard();
            bincode::encode_into_std_write(len, self.write, cfg)
                .map(|_| ())?;
        }
        let key_ty = visit.body.keys.type_id();
        let val_ty = visit.body.vals.type_id();
        let mut ret = Ok(());
        (visit.body.vt.iter_items)(self.val, &mut |iter: &mut dyn Iterator<Item=(AnyKey, &dyn AnyDebug)>| {
            for (key, val) in iter {
                let mut sub = SirEelIce {
                    armory: self.armory,
                    prim_write: self.prim_write,
                    cfg: self.cfg,
                    write: self.write,
                    val: key,
                };
                let e = self.armory.visit(key_ty, &mut sub);
                if e.is_err() {
                    ret = e;
                    return;
                }
                let mut sub = SirEelIce {
                    armory: self.armory,
                    prim_write: self.prim_write,
                    cfg: self.cfg,
                    write: self.write,
                    val,
                };
                let e = self.armory.visit(val_ty, &mut sub);
                if e.is_err() {
                    ret = e;
                    return;
                }
            }
        });
        ret
    }
}
