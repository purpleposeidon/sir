extern crate sir;

use sir::prelude_macro::*;
//use sir::quest::phase;
//use sir::quest::Visit;
//use sir::quest::Knight;
//use sir::rt;
//use std::any::Any;

use std::collections::HashMap;

use std::io::{Read, Write};
use bincode::config::Config;
use bincode::de::Decode;
use bincode::enc::Encode;
use bincode::error::{DecodeError, EncodeError};

use sir::knights::{Armory, BodyVisitor, Visit};

pub type DResult = Result<(), DecodeError>;
pub type EResult = Result<(), EncodeError>;
pub type ReadPrim<C, R> = for<'w, 'out> fn(C, &'w mut R, AnyOptionT<'out>) -> DResult;
pub type WritePrim<C, W> = fn(C, &mut W, &dyn AnyDebug) -> EResult;

pub mod cfg {
    use bincode::config::*;
    pub type Cfg = Configuration<LittleEndian, Fixint, SkipFixedArrayLength>;
    pub fn new() -> Cfg {
        bincode::config::standard()
            .skip_fixed_array_length() // NB: incompatible w/ serde
            .with_fixed_int_encoding()
            .with_little_endian() // Match x86 order. (It's the default, but we're being explicit.)
    }
}

/*struct SirEelCtx<'x, C, W> {
    value: Vec<&'x dyn Any>,
    enum_index: u8,
    config: C,
    write: W,
}
impl<'a, C: Config, W: Write> SirEelCtx<'a, C, W> {
    fn write<T: Encode>(&mut self, t: &T) -> Result<(), EncodeError> {
        bincode::encode_into_std_write::<&T, C, W>(t, self.write, self.config)
    }
}

struct SirEelBuilder<C: Config, W: Write> {
    armory: Armory,
    knight: sir::quest::Builder<SirEelCtx<'static>, Result<(), EncodeError>>,
}
*/
/*impl EelBuilder {
    fn new(armory: Armory) -> Self {
        let knight = Knight::new();
        let mut ret = SirEelBuilder { armory, knight };
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
        self.knight.on(|ctx: &mut SirEelCtx, phase::Enter, visit: Visit<rt::BodyEnum>| {
            let value: &dyn Any = unsafe { &*ctx.value.last().expect("stack underflow") };
            ctx.enum_index = visit.item.variant_index(value).try_into().expect("enum has too many variants");
            bincode::encode_into_std_write::<&u8, C, W>(&ctx.enum_index, out, cfg)
        }).on_enum(|ctx: &mut SirEelCtx, phase::PickVariant, visit: Visit<rt::BodyEnum>| {
            ctx.enum_index as usize
        });
    }
    pub fn prim<T: AnyDebug + Decode + Encode>(&mut self) {
        self.knight.on(|ctx: &mut SirEelCtx, phase::Enter, _visit: Visit<Ty, T>| {
            let val = ctx.value.pop().expect("stack underflow").downcast_ref::<T>().unwrap("type mismatch");
            bincode::encode_into_std_write::<&T, C, W>(val, out, cfg)
        });
    }
}*/

pub struct Eel<C: Config, R: Read, W: Write> {
    armory: Armory,
    prim_read: HashMap<Ty, ReadPrim<C, R>>,
    prim_write: HashMap<Ty, WritePrim<C, W>>,
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
        self.prim_read.insert(Ty::of::<T>(), f);
        let f = |cfg: C, out: &mut W, val: &dyn AnyDebug| -> EResult {
            let val: &T = val.downcast_ref().expect("type mismatch");
            bincode::encode_into_std_write::<&T, C, W>(val, out, cfg)?;
            Ok(())
        };
        self.prim_write.insert(Ty::of::<T>(), f);
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
        let ty = Ty::of::<T>();
        self.armory.visit(&ty, &mut reader)?;
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
        let ty = val.get_ty();
        self.armory.visit(&ty, &mut writer)
    }
}
struct DeSirEelIce<'a, 'dst, C: Config, R: Read> {
    armory: &'a Armory,
    prim_read: &'a HashMap<Ty, ReadPrim<C, R>>,
    cfg: C,
    read: &'a mut R,
    dst: AnyOptionT<'dst>,
}
impl<'a, 'dst, C: Config, R: Read> BodyVisitor for DeSirEelIce<'a, 'dst, C, R> {
    type Err = DecodeError;
    fn visit_primitive(&mut self, visit: Visit<Ty>) -> Result<(), Self::Err> {
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
                ret = armory.visit(&field.ty, &mut sub);
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
            // NB: our 'variant index' is not Rust's.
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
                ret = armory.visit(&field.ty, &mut sub);
            }
        });
        ret
    }
    fn visit_vec(&mut self, visit: Visit<BodyVec>) -> Result<(), Self::Err> {
        let len: usize = {
            let cfg = bincode::config::standard();
            bincode::decode_from_std_read(self.read, cfg)?
        };
        let ty = &visit.body.items;
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
        let key_ty = &visit.body.keys;
        let val_ty = &visit.body.vals;
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
    prim_write: &'a HashMap<Ty, WritePrim<C, W>>,
    cfg: C,
    write: &'a mut W,
    val: &'a dyn AnyDebug,
}
impl<'a, C: Config, W: Write> BodyVisitor for SirEelIce<'a, C, W> {
    type Err = EncodeError;
    fn visit_primitive(&mut self, _visit: Visit<Ty>) -> Result<(), Self::Err> {
        let ty = self.val.get_ty();
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
            let ty = sub.val.get_ty();
            self.armory.visit(&ty, &mut sub)?;
        }
        Ok(())
    }
    fn visit_enum(&mut self, visit: Visit<BodyEnum>) -> Result<(), Self::Err> {
        let discrim: usize = (visit.body.variant_index)(self.val);
        {
            let cfg = bincode::config::standard();
            bincode::encode_into_std_write(discrim, self.write, cfg)
                .map(|_| ())?;
        }
        // NB: our 'variant index' is not Rust's.
        let variant = &visit.body.variants[discrim];
        for field in variant.fields.iter() {
            let mut sub = SirEelIce {
                armory: self.armory,
                prim_write: self.prim_write,
                cfg: self.cfg,
                write: self.write,
                val: field.get_ref(self.val),
            };
            let ty = sub.val.get_ty();
            self.armory.visit(&ty, &mut sub)?;
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
        let ty = &visit.body.items;
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
        let key_ty = &visit.body.keys;
        let val_ty = &visit.body.vals;
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

fn sir_eel(armory: &Armory) {
    let v = std::result::Result::<i32, bool>::Err(true);
    use std::io::Cursor;
    let eel = Eel::<cfg::Cfg, Cursor<&[u8]>, Vec<u8>>::new(armory);
    let mut out = Vec::<u8>::new();
    {
        // FIXME: &mut &mut? Dumb.
        {
            eel.write(cfg::new(), &mut out, &v).unwrap();
        }
        println!("sir_eel = {:?}", out);
    }
    let mut cursor = Cursor::new(&out[..]);
    let got = eel.read::<std::result::Result::<i32, bool>>(cfg::new(), &mut cursor).unwrap();
    println!("{got:?}");
}

fn main() {
    let mut armory = sir::knights::Armory::empty();
    armory.add::<bool>();
    armory.add::<i32>();
    armory.add::<Result<i32, bool>>();
    armory.add::<Vec<i32>>();
    armory.add::<Result<bool, i32>>();
    armory.add::<HashMap<i32, bool>>();
    armory.add::<AT>();
    let armory = &armory.build();

    sir_eel(armory);
}
#[derive(Debug)]
struct AT {
    x: bool,
    data: Vec<i32>,
    foo: Result<bool, i32>,
    map: HashMap<i32, bool>,
}
impl sir::Blade for AT {
    sir::blade! {
        struct AT where {},
        x: bool,
        data: Vec<i32>,
        foo: Result<bool, i32>,
        map: HashMap<i32, bool>,
    }
}

