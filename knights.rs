#![allow(dead_code)]
use crate::rt::*;
use std::collections::{HashMap, HashSet};
use crate::util::AnyDebug;
use crate::Blade;
use std::sync::Arc;
use crate::rt::AnyOptionT;
use std::any::type_name;
use crate::chivalry::EnumTag;

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
pub struct Armory {
    pub swords: Arc<HashMap<Ty, Sword>>,
}
pub struct BuildArmory {
    swords: HashMap<Ty, Sword>,
}
impl Default for BuildArmory {
    fn default() -> Self { Self::new() }
}
impl Armory {
    pub fn builder() -> BuildArmory {
        BuildArmory::default()
    }
    pub fn empty() -> BuildArmory {
        BuildArmory {
            swords: Default::default(),
        }
    }
}
impl BuildArmory {
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
    pub fn build(mut self) -> Armory {
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
            let mut msg = format!("\nArmory validation failed:\n");
            for h in &holes {
                use std::fmt::Write;
                writeln!(&mut msg, "    {}", h).ok();
            }
            panic!("{}", msg);
        }
        Armory {
            swords: Arc::new(self.swords),
        }
    }
}
impl Armory {
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


pub struct Vey {
    pub armory: Armory,
    pub primitives: HashMap<Ty, fn(&dyn AnyDebug)>,
}
impl Vey {
    pub fn equip(armory: &Armory) -> Self {
        let mut primitives = HashMap::<Ty, fn(&dyn AnyDebug)>::new();
        primitives.insert(Ty::of::<i32>(), |x: &dyn AnyDebug| {
            let x: i32 = *a2r(x);
            print!("{}", x)
        });
        primitives.insert(Ty::of::<&'static str>(), |x: &dyn AnyDebug| {
            let x: &str = *a2r(x);
            print!("{:?}", x)
        });
        primitives.insert(Ty::of::<String>(), |x: &dyn AnyDebug| {
            let x: &String = a2r(x);
            print!("{:?}", x)
        });
        Vey {
            armory: armory.clone(),
            primitives,
        }
    }
    pub fn value(&self, val: &dyn AnyDebug) -> Result<(), ()> {
        let ty = val.get_ty();
        let mut visitor = VeyVisitor {
            armory: &self.armory,
            depth: 0,
            val,
        };
        self.armory.visit(&ty, &mut visitor)?;
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
    fn visit_primitive(&mut self, _visit: Visit<Ty>) -> VeyR {
        print!("{:?}", self.val);
        Ok(())
    }
    fn visit_struct(&mut self, visit: Visit<BodyStruct>) -> VeyR {
        print!("{}", visit.item.ty.name);
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
            let ty = sub.val.get_ty();
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
            self.armory.visit(&ty, &mut sub)?;
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
        let discrim = (visit.body.variant_index)(self.val);
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
                let ty = sub.val.get_ty();
                self.indent();
                print!("{}: ", field.name);
                self.armory.visit(&ty, &mut sub)?;
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
                let ty = sub.val.get_ty();
                self.armory.visit(&ty, &mut sub)?;
            }
        }
        print!("{}", closer);
        Ok(())
    }
    fn visit_vec(&mut self, visit: Visit<BodyVec>) -> VeyR {
        let len = (visit.body.vt.len)(self.val);
        let ty = &visit.body.items;
        if len == 0 {
            print!("vec![]");
            return Ok(());
        } else if len == 1 {
            print!("vec![");
            let val = (visit.body.vt.get_ref)(self.val, 0);
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
            let val = (visit.body.vt.get_ref)(self.val, i);
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
        let kty = &visit.body.keys;
        let vty = &visit.body.vals;
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
        (LuaType, Ty),
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
            (lt, Ty::of::<T>()),
            Box::new(c),
        );
    }
    pub fn sword<T: AnyDebug>(&self) -> Result<&Sword, rlua::Error> {
        self.armory.swords.get(&Ty::of::<T>())
            .ok_or_else(|| rlua::Error::RuntimeError(format!("{} has no sword", type_name::<T>())))
    }
    pub fn create<T: AnyDebug>(
        &self,
        lua: &rlua::Lua,
        src: rlua::Value,
    ) -> Result<T, rlua::Error> {
        let mut ret = Option::<T>::None;
        let sword = self.sword::<T>()?;
        self.create0(lua, src, &mut ret, sword)?;
        ret
            .ok_or_else(|| rlua::Error::RuntimeError(format!("{} value not created", type_name::<T>())))
    }
    fn create0(
        &self,
        lua: &rlua::Lua,
        src: rlua::Value,
        dst: AnyOptionT,
        sword: &Sword,
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
        self.armory.visit_sword(sword, &mut visitor)
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
    fn visit_primitive(&mut self, visit: Visit<Ty>) -> Result<(), rlua::Error> {
        let key = (
            LuaType::from(&self.src),
            visit.item.ty,
        );
        if let Some(prim) = self.mun.convert_primitive.get(&key) {
            let val = self.src.take();
            prim(&self.lua, val, self.dst)
        } else {
            let mut path = String::new();
            self.ctx.write(&mut path);
            Err(rlua::Error::FromLuaConversionError {
                from: key.0.name(),
                to: self.mun.armory.swords.get(&key.1)
                    .map(|s| s.item.ty.name)
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
                err = ctx.ctx(mun.armory.visit(&field.ty, &mut sub));
            }
        });
        err
    }
    fn visit_enum(&mut self, visit: Visit<BodyEnum>) -> Result<(), rlua::Error> {
        let variants = &visit.body.variants[..];
        let err = |from: &'static str, message: &str| -> Result<(), rlua::Error> {
            Err(rlua::Error::FromLuaConversionError {
                from,
                to: visit.item.ty.name,
                message: Some(format!("{}", message)),
            })
        };
        let tag = visit.item.guard::<EnumTag>().unwrap_or(&EnumTag::External);
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
                        let mut i = 0i32;
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
                                err = ctx.ctx(mun.armory.visit(&field.ty, &mut sub));
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
                                err = ctx.ctx(mun.armory.visit(&field.ty, &mut sub));
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
        let kty = &visit.body.keys;
        let vty = &visit.body.vals;
        let ks = self.mun.armory.swords.get(kty).expect("key missing sword");
        let vs = self.mun.armory.swords.get(vty).expect("val missing sword");
        let mut res = Ok(());
        let mun = &self.mun;
        let lua = self.lua;
        let ctx = &self.ctx;
        let mut i = 1;
        let hint = src.size_hint().1;
        (visit.body.vt.collect)(self.dst, hint, &mut |key: AnyOptionT, val: AnyOptionT| {
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
                    res = mun.armory.visit_sword(ks, &mut visitor);
                }
                if res.is_err() { return; }
                {
                    let mut visitor = MunVisitor {
                        mun, lua,
                        src: kv.1,
                        dst: val,
                        ctx,
                    };
                    res = mun.armory.visit_sword(vs, &mut visitor);
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
        let ty = val.get_ty();
        let mut m = MarkVisitor {
            armory: &self.armory,
            val,
            out: &mut out,
            depth: 0,
        };
        self.armory.visit(&ty, &mut m)
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
            self.armory.visit(&field.ty, self)?;
        }
        Ok(())
    }
}
impl<'a, W: fmt::Write> BodyVisitor for MarkVisitor<'a, W> {
    type Err = fmt::Error;
    fn visit_primitive(&mut self, _visit: Visit<Ty>) -> fmt::Result {
        write!(self.out, "{:?}", self.val)
    }
    fn visit_struct(&mut self, visit: Visit<BodyStruct>) -> fmt::Result {
        let val = self.val;
        match visit.body.body_type {
            BodyType::Unit => return write!(self.out, "{{}}"),
            BodyType::Tuple => {
                write!(self.out, "{{ ")?;
                let mut first = true;
                for field in visit.body.fields.iter() {
                    if first { first = false; }
                    else { write!(self.out, ", ")?; }
                    self.val = (field.as_ref)(val);
                    self.armory.visit(&field.ty, self)?;
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
                    self.armory.visit(&field.ty, self)?;
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
        let discrim = (visit.body.variant_index)(self.val);
        let variant = &visit.body.variants[discrim];
        if variant.is_unit() {
            return write!(self.out, "{:?}", variant.name);
        }
        // FIXME: Rust supports custom representation for individual variants, eg enum A { B=3 }
        let tag = visit.item.guard::<EnumTag>().unwrap_or(&EnumTag::External);
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
        let key_ty = &visit.body.keys;
        let val_ty = &visit.body.vals;
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
