extern crate sir;
extern crate rlua;

use sir::rt::*;
use sir::chivalry::*;
use sir::knights::{Kingdom, BodyVisitor, Visit};
use sir::util::{AnyDebug, Ty};
use std::any::type_name;
use std::collections::HashMap;


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
    kingdom: Kingdom,
    convert_primitive: HashMap<
        (LuaType, Ty),
        PrimitiveConverter,
    >, // i swear this was completely unintentional
}
impl Mun {
    pub fn new(kingdom: &Kingdom) -> Self {
        let mut ret = Mun { kingdom: kingdom.clone(), convert_primitive: Default::default() };
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
        self.kingdom.swords.get(&Ty::of::<T>())
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
        ret.ok_or_else(|| rlua::Error::RuntimeError(format!("{} value not created", type_name::<T>())))
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
        self.kingdom.visit_sword(sword, &mut visitor)
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
                to: self.mun.kingdom.swords.get(&key.1)
                    .map(|s| s.item.ty.name())
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
                err = ctx.ctx(mun.kingdom.visit(&field.ty, &mut sub));
            }
        });
        err
    }
    fn visit_enum(&mut self, visit: Visit<BodyEnum>) -> Result<(), rlua::Error> {
        let variants = &visit.body.variants[..];
        let err = |from: &'static str, message: &str| -> Result<(), rlua::Error> {
            Err(rlua::Error::FromLuaConversionError {
                from,
                to: visit.item.ty.name(),
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
                                err = ctx.ctx(mun.kingdom.visit(&field.ty, &mut sub));
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
                                err = ctx.ctx(mun.kingdom.visit(&field.ty, &mut sub));
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
        let ks = self.mun.kingdom.swords.get(kty).expect("key missing sword");
        let vs = self.mun.kingdom.swords.get(vty).expect("val missing sword");
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
                    res = mun.kingdom.visit_sword(ks, &mut visitor);
                }
                if res.is_err() { return; }
                {
                    let mut visitor = MunVisitor {
                        mun, lua,
                        src: kv.1,
                        dst: val,
                        ctx,
                    };
                    res = mun.kingdom.visit_sword(vs, &mut visitor);
                }
            }
            i += 1;
        });
        res
    }
}

#[test]
fn main() {
    use sir::{blade, Blade};
    let mut kingdom = Kingdom::builder();
    kingdom.add::<Option<i32>>();
    kingdom.add::<HashMap<i32, bool>>();
    kingdom.add::<Example>();
    kingdom.add::<Nested>();
    let kingdom = &kingdom.build();

    {
        let mun = Mun::new(kingdom);
        let lua = rlua::Lua::new();
        let r: rlua::Value = lua.exec(r##"
    return {
        foo = 1,
        bar = 2,
        nested = {
            cheese = "yes please!",
        },
        map = {
            [10] = true,
            [20] = false,
            [30] = true,
        },
    }
    "##, None).unwrap();
        let r = mun.create::<Example>(&lua, r).expect("mun fail");
        println!("sir_mun = {:#?}", r);
    }

    #[derive(Default, Debug)]
    struct Example {
        foo: i8,
        bar: u64,
        nested: Nested,
        map: HashMap<i32, bool>,
        answer: Option<i32>,
    }
    impl Blade for Example {
        blade! {
            struct Example where {},
            foo: i8,
            bar: u64,
            nested: Nested,
            map: HashMap<i32, bool>,
            answer: Option<i32>,
        }
    }
    #[derive(Default, Debug)]
    struct Nested {
        cheese: String,
    }
    impl Blade for Nested {
        blade! {
            struct Nested where {},
            cheese: String,
        }
    }
}
