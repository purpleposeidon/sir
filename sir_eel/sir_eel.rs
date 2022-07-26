extern crate sir;
extern crate bincode;

#[macro_use] #[allow(unused_imports)] extern crate eztrace;

use std::collections::{HashMap, HashSet};
use std::convert::TryInto;
use std::io;
use std::sync::Arc;
use crate::sir::rt::{self, Body, AnyOptionT};
use crate::sir::util::{AnyDebug, Ty};

/// This is used as the `GuardedBy` marker.
pub struct SirEel;

pub trait Key {
    type R;
}
pub trait KCall<K: Key>: Fn(&CArena<K>, &mut K) -> K::R + 'static {}
impl<K, F> KCall<K> for F
where
    K: Key,
    F: Fn(&CArena<K>, &mut K) -> K::R + 'static,
{}

pub struct CArena<K: Key> {
    buf: Vec<u8>,
    seed: u32,
    _ctx: [K; 0],
    pub handle_buf: Vec<Handle>,
    pub field_as_ref: Vec<fn(&dyn AnyDebug) -> &dyn AnyDebug>,
    pub field_as_mut: Vec<fn(&mut dyn AnyDebug) -> &mut dyn AnyDebug>,
    pub field_with: Vec<fn(&mut dyn FnMut(crate::sir::rt::AnyOptionT))>,
}
impl<K: Key> CArena<K> {
    pub fn new() -> Self {
        use std::sync::atomic::AtomicU32;
        static COUNT: AtomicU32 = AtomicU32::new(1);
        let seed = COUNT.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        assert!(seed >= 1, "ohh husbant you creat too many CArena. now we are unsound.");
        CArena {
            buf: vec![0; std::mem::size_of::<usize>()], // lazy work-around for 0-sized types
            seed,
            _ctx: [],
            handle_buf: vec![],
            field_as_ref: vec![],
            field_as_mut: vec![],
            field_with: vec![],
        }
    }
    pub fn write<F: KCall<K>>(&mut self, f: F) -> Handle {
        unsafe {
            use std::mem;
            assert!(!mem::needs_drop::<F>());

            if mem::size_of::<F>() == 0 {
                let ptr: &F = &f;
                let [_ptr, vtable]: [usize; 2] = mem::transmute(ptr as &dyn KCall<K>);
                return Handle {
                    seed: self.seed,
                    index: 0,
                    vtable,
                };
            }

            let padding = 0 .. self.buf.len() % mem::size_of::<usize>();
            self.buf.extend(padding.map(|_| 0));

            let [_ptr, vtable]: [usize; 2] = mem::transmute(&f as &dyn KCall<K>);
            let handle = Handle {
                seed: self.seed,
                index: self.buf.len() as u32,
                vtable,
            };

            let f = &f as *const F as *const u8;
            let f = std::slice::from_raw_parts(f, mem::size_of::<F>());
            self.buf.extend(f);
            mem::forget(f);
            handle
        }
    }
    pub fn run<'a, 'b, 'c>(&'a self, handle: Handle, ctx: &'b mut K) -> K::R
    where
        K: 'b,
        K::R: 'c,
    {
        assert_eq!(handle.seed, self.seed, "handle for wrong generation");
        let ptr: [usize; 2] = [
            handle.index as usize + &self.buf[0] as *const u8 as usize,
            handle.vtable,
        ];
        let ptr: &dyn KCall<K> = unsafe { std::mem::transmute(ptr) };
        ptr(self, ctx)
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
#[repr(C)]
pub struct Handle {
    seed: u32,
    index: u32,
    vtable: usize,
}
impl Handle {
    #[doc(hidden)]
    pub const fn invalid() -> Handle {
        Handle {
            seed: 0,
            index: u32::MAX,
            vtable: 0,
        }
    }
}

#[derive(Hash, PartialEq, Eq)]
pub struct FnKey(usize);
impl FnKey {
    pub fn new(f: fn(&mut dyn AnyDebug)) -> Self { Self(f as usize) }
}

pub struct CC<K: Key> {
    pub arena: CArena<K>,
    pub known: HashMap<Ty, Handle>,
    pub field_skip: HashMap<FnKey, Handle>,
}
impl<K: Key> CC<K> {
    pub fn new() -> Self {
        CC {
            arena: CArena::new(),
            known: HashMap::new(),
            field_skip: HashMap::new(),
        }
    }
    pub fn write<F: KCall<K>>(&mut self, f: F) -> Handle {
        self.arena.write(f)
    }
    pub fn add<F: KCall<K>>(&mut self, ty: Ty, f: F) {
        let handle = self.write(f);
        assert!(self.known.insert(ty, handle).is_none());
    }
    pub fn get_skipper(
        &mut self,
        get_target: impl (Fn(&mut K) -> &mut dyn AnyDebug) + 'static,
        init: fn(&mut dyn AnyDebug),
        ok: impl (Fn(&CArena<K>, &mut K) -> K::R) + 'static,
    ) -> Handle {
        let arena = &mut self.arena;
        *self.field_skip
            .entry(FnKey::new(init))
            .or_insert_with(move || {
                let f = move |arena: &CArena<K>, ctx: &mut K| -> K::R {
                    init(get_target(ctx));
                    ok(arena, ctx)
                };
                arena.write(f)
            })
    }
    pub fn get(&self, ty: Ty) -> Handle {
        *self.known.get(&ty)
            .unwrap_or_else(|| panic!("asked for {:?} before it was defined", ty))
    }
}

pub type CR = *mut dyn AnyDebug;
pub type CW = *const dyn AnyDebug;
pub struct Ctx<F, C> {
    pub val: C,
    pub fd: F,
}
impl<W: io::Write> Key for Ctx<W, CW> {
    type R = EelResult;
}

use crate::bincode::error::{EncodeError, DecodeError};
pub type EelResult = Result<usize, EncodeError>;

use sir::knights::Kingdom;
fn ordered_tys(kingdom: &Kingdom) -> Vec<Ty> {
    let mut order = Vec::<Ty>::new();
    let mut seen = HashSet::<Ty>::new();
    for (ty, sword) in kingdom.swords.iter() {
        let mut put = |ty: Ty| {
            if seen.insert(ty) {
                order.push(ty);
            }
        };
        match &sword.item.body {
            Body::Primitive => (),
            Body::Struct(b) => {
                for f in &b.fields {
                    put(f.ty);
                }
            },
            Body::Enum(b) => {
                for b in &b.variants {
                    for f in &b.fields {
                        put(f.ty);
                    }
                }
            },
            Body::Vec(b) => put(b.items),
            Body::Map(b) => {
                put(b.keys);
                put(b.vals);
            },
        }
        put(*ty);
    }
    order
}

impl<W: io::Write> Ctx<W, CW> {
    fn fix_write<T: bincode::enc::Encode>(&mut self, val: &T) -> EelResult {
        let config = crate::bincode::config::standard()
            .with_little_endian()
            .with_fixed_int_encoding()
            .skip_fixed_array_length()
            ;
        crate::bincode::encode_into_std_write::<&T, _, W>(val, &mut self.fd, config)
    }
    fn var_write<T: bincode::enc::Encode>(&mut self, val: &T) -> EelResult {
        let config = crate::bincode::config::standard()
            .with_little_endian()
            .with_variable_int_encoding()
            .skip_fixed_array_length()
            ;
        crate::bincode::encode_into_std_write::<&T, _, W>(val, &mut self.fd, config)
    }
}
impl<W: io::Write> CC<Ctx<W, CW>> {
    pub fn prim<T: AnyDebug + bincode::enc::Encode>(&mut self) {
        self.add(Ty::of::<T>(), |_arena: &CArena<Ctx<W, CW>>, ctx: &mut Ctx<W, CW>| {
            let val = unsafe { &*ctx.val };
            let val: &T = val.downcast_ref().expect("wrong type");
            ctx.fix_write(val)
        });
    }
}
fn w_fields<W: io::Write>(cc: &mut CC<Ctx<W, CW>>, fields: &[Arc<rt::Field>]) -> Handle {
    let hfstart: u16 = cc.arena.handle_buf.len().try_into().unwrap();
    let rfstart: u16 = cc.arena.field_as_ref.len().try_into().unwrap();
    let mut flen = 0u16;
    for field in fields {
        if field.guard::<sir::chivalry::Skip, SirEel>().is_some() {
            continue;
        }
        cc.arena.handle_buf.push(cc.get(field.ty));
        cc.arena.field_as_ref.push(field.as_ref);
        flen += 1;
    }
    let f = move |arena: &CArena<Ctx<W, CW>>, ctx: &mut Ctx<W, CW>| -> EelResult {
        let hfstart = hfstart as usize;
        let rfstart = rfstart as usize;
        let flen = flen as usize;
        let mut n = 0;
        let val = unsafe { &*ctx.val };
        for (h, f) in (hfstart .. hfstart + flen).zip(rfstart .. rfstart + flen) {
            ctx.val = arena.field_as_ref[f](val);
            let err = arena.run(arena.handle_buf[h], ctx);
            ctx.val = val;
            n += err?;
        }
        Ok(n)
    };
    cc.write(f)
}

impl<R: io::Read> Ctx<R, CR> {
    pub fn fix_read0<T: AnyDebug + bincode::de::Decode>(&mut self) -> Result<T, DecodeError> {
        let config = crate::bincode::config::standard()
            .with_little_endian()
            .with_fixed_int_encoding()
            .skip_fixed_array_length()
            ;
        crate::bincode::decode_from_std_read(&mut self.fd, config)
    }
    pub fn var_read0<T: AnyDebug + bincode::de::Decode>(&mut self) -> Result<T, DecodeError> {
        let config = crate::bincode::config::standard()
            .with_little_endian()
            .with_variable_int_encoding()
            .skip_fixed_array_length()
            ;
        crate::bincode::decode_from_std_read(&mut self.fd, config)
    }
    fn fix_read1<T: AnyDebug + bincode::de::Decode>(&mut self, out: &mut dyn AnyDebug) -> DeelResult {
        if let Some(out) = out.downcast_mut::<Option<T>>() {
            debug_assert!(out.is_none());
            *out = Some(self.fix_read0::<T>()?);
            Ok(())
        } else {
            let expect = Ty::of::<Option<T>>();
            let actual = <dyn AnyDebug>::get_ty(out);
            panic!("wrong type: expect {:?}, got {:?}", expect, actual)
        }
    }
}
impl<R: io::Read> CC<Ctx<R, CR>> {
    pub fn prim<T: AnyDebug + bincode::de::Decode>(&mut self) {
        self.add(Ty::of::<T>(), |_arena: &CArena<Ctx<R, CR>>, ctx: &mut Ctx<R, CR>| {
            let out = unsafe { &mut *ctx.val };
            ctx.fix_read1::<T>(out)
        });
    }
}
fn r_fields<R: io::Read>(
    cc: &mut CC<Ctx<R, CR>>,
    init: fn(AnyOptionT, &mut dyn FnMut(AnyOptionT)),
    fields: &[Arc<rt::Field>],
) -> Handle {
    let fhstart: u16 = cc.arena.handle_buf.len().try_into().unwrap();
    for field in fields {
        let handle = {
            if let Some(default) = field.guard::<sir::chivalry::Skip, SirEel>() {
                cc.get_skipper(
                    |ctx: &mut Ctx<R, CR>| unsafe { &mut *ctx.val },
                    default.0,
                    |_, _| -> DeelResult { Ok(()) },
                )
            } else {
                cc.get(field.ty)
            }
        };
        cc.arena.handle_buf.push(handle);
    }
    let fhend: u16 = cc.arena.handle_buf.len().try_into().unwrap();
    let f = move |arena: &CArena<Ctx<R, CR>>, ctx: &mut Ctx<R, CR>| -> DeelResult {
        let out: &mut dyn AnyDebug = unsafe { &mut *ctx.val };
        let mut iter = arena.handle_buf[fhstart as usize .. fhend as usize].iter();
        let mut err = Ok(());
        init(out, &mut |field: &mut dyn AnyDebug| {
            if let Some(&handle) = iter.next() {
                ctx.val = field;
                err = arena.run(handle, ctx);
                if err.is_err() {
                    while iter.next().is_some() {}
                }
            }
        });
        ctx.val = out;
        err
    };
    cc.write(f)
}

pub struct EncoderBuilder<W: io::Write> {
    pub kingdom: Kingdom,
    pub cc: CC<Ctx<W, CW>>,
}
impl<W: io::Write> EncoderBuilder<W> {
    pub fn new(kingdom: Kingdom) -> Self {
        let cc = CC::<Ctx<W, CW>>::new();
        EncoderBuilder { kingdom, cc }
    }
    pub fn add_prims(&mut self) {
        self.cc.prim::<bool>();
        self.cc.prim::<char>();
        self.cc.prim::<()>();

        self.cc.prim::<u8>();
        self.cc.prim::<u16>();
        self.cc.prim::<u32>();
        self.cc.prim::<u64>();
        self.cc.prim::<u128>();
        self.cc.prim::<usize>();

        self.cc.prim::<i8>();
        self.cc.prim::<i16>();
        self.cc.prim::<i32>();
        self.cc.prim::<i64>();
        self.cc.prim::<i128>();
        self.cc.prim::<isize>();
    }

    pub fn compile(mut self, overrider: impl Fn(&mut Self, Ty) -> bool) -> CC<Ctx<W, CW>> {
        for &ty in &ordered_tys(&self.kingdom) {
            if overrider(&mut self, ty) {
                continue;
            }
            let sword = self.kingdom.swords.get(&ty).unwrap();
            match &sword.item.body {
                Body::Primitive => assert!(self.cc.known.get(&ty).is_some(), "missing primitive {:?}", ty),
                Body::Struct(body) => {
                    let handle = w_fields(&mut self.cc, &body.fields[..]);
                    self.cc.known.insert(ty, handle);
                },
                Body::Enum(body) => {
                    let variant_index = body.variant_index;
                    let mut variants = vec![];
                    assert!(!body.variants.is_empty(), "uninhabited enums not implemented");
                    for variant in &body.variants {
                        let handle = w_fields(&mut self.cc, &variant.fields[..]);
                        variants.push(handle);
                    }
                    let hvstart = self.cc.arena.handle_buf.len() as u16;
                    self.cc.arena.handle_buf.extend(variants.drain(..));
                    let f = move |arena: &CArena<Ctx<W, CW>>, ctx: &mut Ctx<W, CW>| -> EelResult {
                        let val = unsafe { &*ctx.val };
                        let vi = variant_index(val);
                        let n = ctx.var_write(&vi)?;
                        let handle = hvstart as usize + vi;
                        let handle = arena.handle_buf[handle];
                        Ok(n + arena.run(handle, ctx)?)
                    };
                    self.cc.add(ty, f);
                },
                Body::Vec(body) => {
                    let items = self.cc.get(body.items);
                    let len = body.vt.len;
                    let iter = body.vt.iter;
                    self.cc.add(ty, move |arena: &CArena<Ctx<W, CW>>, ctx: &mut Ctx<W, CW>| {
                        let container = unsafe { &*ctx.val };
                        let mut n = ctx.var_write(&len(container))?;
                        let mut err = None;
                        iter(container, &mut |iter: &mut dyn Iterator<Item=&dyn AnyDebug>| {
                            for item in iter {
                                ctx.val = item;
                                match arena.run(items, ctx) {
                                    Ok(dn) => n += dn,
                                    Err(e) => {
                                        err = Some(e);
                                        break;
                                    },
                                }
                            }
                        });
                        ctx.val = container;
                        err.map(Err).unwrap_or(Ok(n))
                    });
                },
                Body::Map(body) => {
                    let keys = self.cc.get(body.keys);
                    let vals = self.cc.get(body.vals);
                    let len = body.vt.len;
                    let iter = body.vt.iter_items;
                    self.cc.add(ty, move |arena: &CArena<Ctx<W, CW>>, ctx: &mut Ctx<W, CW>| {
                        let container = unsafe { &*ctx.val };
                        let mut n = ctx.var_write(&len(container))?;
                        let mut err = None;
                        iter(container, &mut |iter: &mut dyn Iterator<Item=(&dyn AnyDebug, &dyn AnyDebug)>| {
                            for (key, val) in iter {
                                for &(it, handle) in &[(key, keys), (val, vals)] {
                                    ctx.val = it;
                                    match arena.run(handle, ctx) {
                                        Ok(dn) => n += dn,
                                        Err(e) => {
                                            err = Some(e);
                                            break;
                                        },
                                    }
                                }
                            }
                        });
                        ctx.val = container;
                        err.map(Err).unwrap_or(Ok(n))
                    });
                },
            }
        }
        self.cc
    }
}

pub type DeelResult = Result<(), DecodeError>;
impl<R: io::Read> Key for Ctx<R, CR> {
    type R = DeelResult;
}

pub struct DecoderBuilder<R: io::Read> {
    pub kingdom: Kingdom,
    pub cc: CC<Ctx<R, CR>>,
}
impl<R: io::Read> DecoderBuilder<R> {
    pub fn new(kingdom: Kingdom) -> Self {
        let cc = CC::<Ctx<R, CR>>::new();
        DecoderBuilder { kingdom, cc }
    }
    pub fn add_prims(&mut self) {
        self.cc.prim::<bool>();
        self.cc.prim::<char>();
        self.cc.prim::<()>();

        self.cc.prim::<u8>();
        self.cc.prim::<u16>();
        self.cc.prim::<u32>();
        self.cc.prim::<u64>();
        self.cc.prim::<u128>();
        self.cc.prim::<usize>();

        self.cc.prim::<i8>();
        self.cc.prim::<i16>();
        self.cc.prim::<i32>();
        self.cc.prim::<i64>();
        self.cc.prim::<i128>();
        self.cc.prim::<isize>();
    }

    pub fn compile(mut self, overrider: impl Fn(&mut Self, Ty) -> bool) -> CC<Ctx<R, CR>> {
        for &ty in &ordered_tys(&self.kingdom) {
            if overrider(&mut self, ty) {
                continue;
            }
            let sword = self.kingdom.swords.get(&ty).unwrap();
            match &sword.item.body {
                Body::Primitive => assert!(self.cc.known.get(&ty).is_some(), "missing primitive {:?}", ty),
                Body::Struct(body) => {
                    let handle = r_fields(&mut self.cc, body.init, &body.fields[..]);
                    self.cc.known.insert(ty, handle);
                },
                Body::Enum(body) => {
                    let mut variant_entries = vec![]; // FOOLISH GOSSLING! DO NOT INTERLEAVE THE HANDLES!
                    for variant in &body.variants {
                        // &mut Option<A>
                        let handle = r_fields(&mut self.cc, variant.init, &variant.fields[..]);
                        variant_entries.push(handle);
                    }
                    let vhstart: u16 = self.cc.arena.handle_buf.len().try_into().unwrap();
                    let vcount: u16 = body.variants.len().try_into().unwrap();
                    self.cc.arena.handle_buf.extend(variant_entries);
                    let f = move |arena: &CArena<Ctx<R, CR>>, ctx: &mut Ctx<R, CR>| -> DeelResult {
                        // &mut Option<Result<A, B>>
                        let variant_index: usize = ctx.var_read0()?;
                        if variant_index > vcount as usize {
                            let err = format!("variant_index too high: {} > {}", variant_index, vcount);
                            return Err(DecodeError::OtherString(err));
                        }
                        let handle = arena.handle_buf[vhstart as usize + variant_index];
                        arena.run(handle, ctx)
                    };
                    self.cc.add(ty, f);
                },
                Body::Vec(body) => {
                    let items = self.cc.get(body.items);
                    let collect = body.vt.collect;
                    let f = move |arena: &CArena<Ctx<R, CR>>, ctx: &mut Ctx<R, CR>| -> DeelResult {
                        let out = unsafe { &mut *ctx.val };
                        let mut len: usize = ctx.var_read0()?;
                        let mut err = Ok(());
                        collect(out, Some(len), &mut |hold| {
                            if len == 0 { return; }
                            len -= 1;
                            ctx.val = hold;
                            err = arena.run(items, ctx);
                            if err.is_err() {
                                len = 0;
                            }
                        });
                        ctx.val = out;
                        err
                    };
                    self.cc.add(ty, f);
                },
                Body::Map(body) => {
                    let keys = self.cc.get(body.keys);
                    let vals = self.cc.get(body.vals);
                    let collect = body.vt.collect;
                    let f = move |arena: &CArena<Ctx<R, CR>>, ctx: &mut Ctx<R, CR>| -> DeelResult {
                        let out = unsafe { &mut *ctx.val };
                        let mut len: usize = ctx.var_read0()?;
                        let mut err = Ok(());
                        collect(out, Some(len), &mut |hold_key, hold_val| {
                            if len == 0 { return; }
                            len -= 1;
                            ctx.val = hold_key;
                            err = arena.run(keys, ctx);
                            if err.is_err() {
                                len = 0;
                                return;
                            }
                            ctx.val = hold_val;
                            err = arena.run(vals, ctx);
                            if err.is_err() {
                                len = 0;
                            }
                        });
                        ctx.val = out;
                        err
                    };
                    self.cc.add(ty, f);
                },
            }
        }
        self.cc
    }
}

#[cfg(test)]
#[test]
fn main() {
    let mut kingdom = Kingdom::empty();
    kingdom.add::<bool>();
    kingdom.add::<i32>();
    kingdom.add::<u8>();
    kingdom.add::<Result<i32, bool>>();
    kingdom.add::<Vec<i32>>();
    kingdom.add::<Result<bool, i32>>();
    kingdom.add::<HashMap<u8, bool>>();
    kingdom.add::<AT>();
    kingdom.add::<Empty>();
    let kingdom = &kingdom.build();

    type W = Vec<u8>;
    let mut builder = EncoderBuilder::<W>::new(kingdom.clone());
    builder.add_prims();
    let cc = builder.compile(|_, _| false);

    {
        let val = Empty {
            seen: 10,
            val: 42,
            also_seen: 20,
        };
        trace!(val);
        let mut ctx = Ctx {
            val: &val as *const dyn AnyDebug,
            fd: Vec::<u8>::new(),
        };
        let ty = AnyDebug::get_ty(&val);
        let handle = *cc.known.get(&ty).unwrap_or_else(|| panic!("Handling for {:?} was not compiled", ty));
        let ret = cc.arena.run(handle, &mut ctx);
        trace!(ret);
        trace!(ctx.fd);
        assert_eq!(ctx.fd.len(), 2);
        {
            let mut builder = DecoderBuilder::<R>::new(kingdom.clone());
            builder.add_prims();
            let cc = builder.compile(|_, _| false);
            let mut out = Option::<Empty>::None;
            let mut ctx = Ctx {
                val: &mut out as *mut dyn AnyDebug,
                fd: io::Cursor::new(&ctx.fd[..]),
            };
            let handle = *cc.known.get(&ty).unwrap();
            let ret = cc.arena.run(handle, &mut ctx);
            ret.unwrap();
            let mut out = out.unwrap();
            assert_eq!(out.val, 0);
            assert_eq!(val.val, 42);
            out.val = val.val;
            assert_eq!(out, val);
        }
    }

    let fd = {
        let val = AT {
            x: true,
            data: vec![1, 2, 3, 8, 9],
            foo: Ok(false),
            map: vec![(4, true), (3, false)].drain(..).collect(),
        };
        trace!(val);
        let mut ctx = Ctx {
            val: &val as *const dyn AnyDebug,
            fd: Vec::<u8>::new(),
        };
        let ty = AnyDebug::get_ty(&val);
        let handle = *cc.known.get(&ty).unwrap_or_else(|| panic!("Handling for {:?} was not compiled", ty));
        let ret = cc.arena.run(handle, &mut ctx);
        trace!(ret);
        trace!(ctx.fd);
        ctx.fd
    };

    println!();

    use std::io::Cursor;
    type R<'a> = Cursor<&'a [u8]>;
    let mut builder = DecoderBuilder::<R>::new(kingdom.clone());
    builder.add_prims();
    let cc = builder.compile(|_, _| false);
    {
        let mut out = Option::<AT>::None;
        let mut ctx = Ctx {
            val: &mut out as *mut dyn AnyDebug,
            fd: Cursor::new(&fd[..]),
        };
        let ty = Ty::of::<AT>();
        let handle = *cc.known.get(&ty).unwrap_or_else(|| panic!("Handling for {:?} was not compiled", ty));
        let ret = cc.arena.run(handle, &mut ctx);
        trace!(ret);
        trace!(out);
    }

    #[derive(Debug)]
    struct AT {
        foo: Result<bool, i32>,
        x: bool,
        data: Vec<i32>,
        map: HashMap<u8, bool>,
    }
    impl sir::Blade for AT {
        sir::blade! {
            struct AT where {},
            foo: Result<bool, i32>,
            x: bool,
            data: Vec<i32>,
            map: HashMap<u8, bool>,
        }
    }
    #[derive(Debug, PartialEq)]
    struct Empty {
        seen: u8,
        val: i32,
        also_seen: u8,
    }
    use sir::chivalry::{GuardedOnly, Skip};
    impl sir::Blade for Empty {
        sir::blade! {
            struct Empty where {},
            seen: u8,
            val: i32 where {
                GuardedOnly::by::<SirEel>(Skip::default::<i32>()),
            },
            also_seen: u8,
        }
    }
}
