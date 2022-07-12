extern crate sir;

use sir::prelude_macro::*;
use sir::quest::phase;
use sir::quest::Visit;
use sir::quest::Knight;
use sir::rt;
use std::convert::TryFrom;

use std::collections::HashMap;
use std::ops::Range;

use std::io::Write;
use bincode::config::Config;
use bincode::de::Decode;
use bincode::enc::Encode;
use bincode::error::{DecodeError, EncodeError};

use sir::knights::Armory;

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

pub struct Ctx<C, W> {
    stack: Vec<*const dyn AnyDebug>,
    iter_stack: Vec<Range<usize>>,
    enum_index: usize,
    config: C,
    write: W,
}
trait Top {
    fn top(&self) -> &dyn AnyDebug;
}
impl Top for Vec<*const dyn AnyDebug> {
    fn top(&self) -> &dyn AnyDebug {
        let node: Option<*const dyn AnyDebug> = self.last().cloned();
        let node: *const dyn AnyDebug = node.unwrap();
        let node: &dyn AnyDebug = unsafe { &*node };
        node
    }
}


type Ret = Result<(), EncodeError>;

pub struct Builder<C: Config, W: Write> {
    armory: Armory,
    pub knight: sir::quest::Builder<Ctx<C, W>, Ret>,
}
impl<C: Config, W: Write> Builder<C, W> {
    pub fn new_empty(armory: Armory) -> Self {
        let knight = Knight::new();
        Builder { armory, knight }
    }
    pub fn new(armory: Armory) -> Self {
        let mut ret = Self::new_empty(armory);
        ret.default_setup();
        ret
    }
    fn default_setup(&mut self) {
        self.register_primals();
        self.register_structural();
    }
    pub fn register_large_enum<T, SpecificEnum>(&mut self)
    where
        T: Encode + TryFrom<usize>,
        <T as TryFrom<usize>>::Error: std::fmt::Debug,
    {
        // FIXME: ahead-of-time enum variant check?
        self.knight.on(|ctx: &mut Ctx<C, W>, phase::Enter, visit: Visit<rt::BodyEnum, SpecificEnum>| {
            // We have to choose the variant a bit early so that we can serialize it properly.
            let value = ctx.stack.top();
            ctx.enum_index = (visit.body.variant_index)(value);
            let n: T = T::try_from(ctx.enum_index).expect("enum has too many variants");
            bincode::encode_into_std_write::<T, C, W>(n, &mut ctx.write, ctx.config).map(|_| ())
        });
    }
    pub fn register_structural(&mut self) {
        self.knight.on(|ctx: &mut Ctx<C, W>, phase::Enter, visit: Visit<rt::Field>| {
            let node = ctx.stack.top();
            let child: &dyn AnyDebug = (visit.body.as_ref)(node);
            //println!("phase::Enter(field) {:?} // {:?} = {:?}", visit.item.ty, visit.body, child);
            let child: *const dyn AnyDebug = child;
            ctx.stack.push(child);
            Ok(())
        }).on(|ctx: &mut Ctx<C, W>, phase::Leave, _visit: Visit<rt::Field>| {
            //println!("phase::Leave(field) {:?} // {:?}", _visit.item.ty, _visit.body);
            ctx.stack.pop();
            Ok(())
        });
        self.register_large_enum::<u8, sir::quest::Generic>();
        self.knight.on_enum(|ctx: &mut Ctx<C, W>, phase::PickVariant, _visit: Visit<rt::BodyEnum>| {
            //println!("phase::PickVariant {:?} = {:?}", _visit.item.ty, ctx.enum_index);
            ctx.enum_index
        }).on(|ctx: &mut Ctx<C, W>, phase::Enter, visit: Visit<rt::BodyVec>| {
            let node = ctx.stack.top();
            let len = (visit.body.vt.len)(node);
            ctx.iter_stack.push(0..len);
            Ok(())
        }).on_iter(|ctx: &mut Ctx<C, W>, phase::Iter, visit: Visit<rt::BodyVec>| {
            let range = ctx.iter_stack.last_mut().unwrap();
            if let Some(i) = range.next() {
                if i > 0 {
                    ctx.stack.pop();
                }
                let val: &dyn AnyDebug = (visit.body.vt.get_ref)(ctx.stack.top(), i);
                let val: *const dyn AnyDebug = val;
                ctx.stack.push(val);
                true
            } else {
                ctx.iter_stack.pop();
                ctx.stack.pop();
                false
            }
        }).on(|_ctx: &mut Ctx<C, W>, phase::Enter, _visit: Visit<rt::BodyMap>| {
            //ctx.iter_stack.push(0);
            //Ok(())
            let node = ctx.stack.top();
            let len = (visit.body.vt.len)(node);
            ctx.iter_stack.push(0..len);
            Ok(())
        }).on_iter(|_ctx: &mut Ctx<C, W>, phase::Iter, _visit: Visit<rt::BodyMap>| {
            todo!()
            /*let n = ctx.iter_stack.last_mut().unwrap();
            if *n == 0 {
                ctx.iter_stack.pop();
                false
            } else {
                *n -= 1;
                true
            }*/
        });
        // [1, 2, 3]
        // begin_iter
        // | iter?
        // | | item
        // end_iter
    }
    pub fn register_primals(&mut self) {
        macro_rules! prims {
            ($($ty:ty,)*) => {
                $(self.prim::<$ty>();)*
            };
        }
        prims! {
            (),
            bool,
            i8, u8,
            i16, u16,
            i32, u32,
            i64, u64,
            // isize, usize -- Too risky.
            i128, u128,
            f32, f64,
            String,
        }
    }
    pub fn prim<T: AnyDebug + Decode + Encode>(&mut self) {
        self.knight.on(|ctx: &mut Ctx<C, W>, phase::Enter, _visit: Visit<Ty, T>| {
            //println!("phase::Enter(prim1) {} = {:?}", std::any::type_name::<T>(), ctx.stack.top());
            let val = ctx.stack.top().downcast_ref::<T>().expect("type mismatch");
            //println!("phase::Enter(prim2) {} = {:?}", std::any::type_name::<T>(), val);
            bincode::encode_into_std_write::<&T, C, W>(val, &mut ctx.write, ctx.config).map(|_| ())
        });
    }
    pub fn build(self) -> Result<SirEel<C, W, impl Fn(&mut Ctx<C, W>, &mut Ret) -> sir::quest::Flow>, String> {
        let knight = self.knight.build();
        let knight = sir::quest::compile(knight, &self.armory, sir::quest::Flow::result)?;
        Ok(SirEel { knight })
    }
}
pub struct SirEel<C: Config, W: Write, ReadFlow> {
    knight: sir::quest::Compiled<Ctx<C, W>, Ret, ReadFlow>,
}
impl<C: Config, W: Write, ReadFlow> SirEel<C, W, ReadFlow>
where
    ReadFlow: Fn(&mut Ctx<C, W>, &mut Ret) -> sir::quest::Flow,
{
    fn write(&self, config: C, write: W, val: &dyn AnyDebug) -> Option<Ret> {
        let ty = <dyn AnyDebug>::get_ty(val);
        let mut ctx = Ctx {
            stack: vec![val],
            iter_stack: vec![],
            enum_index: 0,
            config,
            write,
        };
        self.knight.run(&mut ctx, ty)
    }
}

fn write_eel<'a, 'b, ReadFlow>(
    out: &'a mut Vec<u8>,
    eel: &SirEel<cfg::Cfg, &'b mut Vec<u8>, ReadFlow>,
    v: &dyn AnyDebug,
) -> Option<Ret>
where
    ReadFlow: Fn(&mut Ctx<cfg::Cfg, &'b mut Vec<u8>>, &mut Ret) -> sir::quest::Flow,
{
    // Actually, 'a and 'b are not entangled.
    let out: &'b mut Vec<u8> = unsafe { std::mem::transmute(out) };
    eel.write(cfg::new(), out, v)
}


fn sir_eel(armory: &Armory) {
    //let v = std::result::Result::<i32, bool>::Err(true);
    //let v = std::result::Result::<i32, bool>::Ok(23);
    let v = AT {
        x: true,
        data: vec![1, 2, 3, 4, 5],
        foo: Ok(false),
        map: vec![
            (4, true),
            (3, false),
            (2, true),
        ].drain(..).collect(),
    };
    println!("size {:?}", std::mem::size_of_val(&v));
    let eel = Builder::<cfg::Cfg, &mut Vec<u8>>::new(armory.clone()).build().unwrap();

    let eel: SirEel<_, &mut Vec<u8>, _> = eel;

    let mut out = Vec::<u8>::new();
    {
        // FIXME: &mut &mut? Dumb.
        let ret = write_eel(&mut out, &eel, &v);
        println!("ret = {ret:?}");
        println!("sir_eel = {:?}", out);
    }
    //use std::io::Cursor;
    //let mut cursor = Cursor::new(&out[..]);
    //let got = eel.read::<std::result::Result::<i32, bool>>(cfg::new(), &mut cursor).unwrap();
    //println!("{got:?}");
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


