use std::sync::Arc;
use std::any::{TypeId, type_name};
use std::collections::{HashMap, HashSet};
use std::marker::PhantomData;
use std::fmt::{self, Write as _};
use crate::rt;
use crate::knights::Armory;
use crate::util::{Ty, NonStaticTypeId};
use std::mem::size_of;
use std::ops::Range;
use std::panic::Location;
use std::convert::TryInto;
use std::alloc::Layout;

pub struct Generic;

pub struct Knight<Ctx, Ret> {
    events: Arc<HashMap<(Ty, Ty, Ty), Event>>,
    _cr: PhantomData<(Ctx, Ret)>,
}
impl<Ctx, Ret> Clone for Knight<Ctx, Ret> {
    fn clone(&self) -> Self {
        Knight {
            events: self.events.clone(),
            _cr: PhantomData,
        }
    }
}

impl<Ctx, Ret> Knight<Ctx, Ret> {
    pub fn new() -> Builder<Ctx, Ret> {
        Builder {
            events: vec![],
            _cr: PhantomData,
        }
    }
    pub fn choose_event(
        &self,
        errors: &mut Vec<String>,
        missing_generic_handlers: &mut Vec<Ty>,
        phase: Ty,
        rt: Ty,
        special: Ty,
    ) -> Option<&Event> {
        let generic = Ty::of::<Generic>();
        if let Some(special) = self.events.get(&(phase, rt, special)) {
            Some(special)
        } else if missing_generic_handlers.contains(&rt) {
            None
        } else if let Some(generic) = self.events.get(&(phase, rt, generic)) {
            Some(generic)
        } else {
            missing_generic_handlers.push(rt);
            let msg = if rt == Ty::of::<rt::BodyEnum>() {
                "Knight lacks knight.on_enum(|Ctx, Visit<rt::BodyEnum>| -> usize { … }) for selecting the enum variant."
            } else if rt == Ty::of::<rt::BodyVec>() {
                "Knight lacks knight.on_iter(|Ctx, Visit<rt::BodyVec>| -> bool { … }) for counting out vec elements."
            } else if rt == Ty::of::<rt::BodyMap>() {
                "Knight lacks knight.on_iter(|Ctx, Visit<rt::BodyMap>| -> bool { … }) for counting out map items."
            } else {
                // Unreachable, right? (Note also that we need to push SOMETHING for the error to be detected.)
                "Knight missing (unknown) handler."
            };
            errors.push(msg.into());
            None
        }
    }
}
#[derive(Default)]
pub struct Builder<Ctx, Ret> {
    events: Vec<Event>,
    _cr: PhantomData<(Ctx, Ret)>,
}
impl<Ctx, Ret> Builder<Ctx, Ret> {
    // on(|_ctx, phase, visit| -> Ret { … })
    #[track_caller]
    pub fn on<Phase, Body, Specific>(self, f: fn(&mut Ctx, Phase, Visit<Body, Specific>) -> Ret) -> Self {
        self.on1(f)
    }
    #[track_caller]
    fn on1<Phase, Body, Specific, Ret2>(mut self, f: fn(&mut Ctx, Phase, Visit<Body, Specific>) -> Ret2) -> Self {
        let ptr_size = size_of::<usize>();
        assert_eq!(size_of::<Phase>(), 0);
        assert_eq!(size_of::<&Body>(), ptr_size);
        assert_eq!(size_of::<Visit<Body>>(), ptr_size * 2);
        assert_eq!(size_of::<Visit<Body>>(), size_of::<Visit<Ty>>());
        assert_eq!(Layout::new::<Visit<Body>>(), Layout::new::<Visit<usize>>());
        let e = Event {
            ctx: Ty::of::<Ctx>(),
            phase: Ty::of::<Phase>(),
            visit: Ty::of::<Body>(),
            specific: Ty::of::<Specific>(),
            ret: Ty::of::<Ret2>(),
            func: f as usize,
            src: Location::caller(),
        };
        e.check();
        self.events.push(e);
        self
    }
    /// The function will be called to determine which enum variant to process.
    #[track_caller]
    pub fn on_enum<Specific>(self, f: fn(&mut Ctx, phase::PickVariant, Visit<rt::BodyEnum, Specific>) -> usize) -> Self {
        self.on1(f)
    }
    /// The function will be called repeatedly to request process the items of an iterable
    /// structure. Return `false` to indicate that there are no more items to process. `Body` must
    /// be either `rt::BodyVec` or `rt::BodyMap`.
    #[track_caller]
    pub fn on_iter<Body, Specific>(self, f: fn(&mut Ctx, phase::Iter, Visit<Body, Specific>) -> bool) -> Self {
        self.on1(f)
    }
    pub fn build(mut self) -> Knight<Ctx, Ret> {
        let events = self.events.drain(..)
            .map(|step| {
                let key = (step.phase, step.visit, step.specific);
                (key, step)
            }).collect::<HashMap<_, _>>();
        let events = Arc::new(events);
        Knight { events, _cr: self._cr }
    }
}

pub struct Event {
    ctx: Ty,
    phase: Ty,
    visit: Ty,
    specific: Ty,
    ret: Ty,
    func: usize,
    src: &'static Location<'static>,
    // These fields are not safe to expose.
}
impl fmt::Debug for Event {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let specific = if self.specific == Ty::of::<Generic>() {
            format!(", {}", self.specific)
        } else {
            "".into()
        };
        if self.ret == Ty::of::<()>() {
            write!(f, "0x{:x}: fn(&mut <{}>, {}, Visit<{}{}>)", self.func, self.ctx, self.phase, self.visit, specific)
        } else {
            write!(f, "0x{:x}: fn(&mut <{}>, {}, Visit<{}{}>) -> {}", self.func, self.ctx, self.phase, self.visit, specific, self.ret)
        }
    }
}
impl Event {
    // fn validate
    fn check(&self) {
        macro_rules! assert {
            ($bit:expr, $($tt:tt)*) => {
                if !$bit {
                    panic!("Event {:?}\n\t{}", self, format_args!($($tt)*));
                }
            };
        }
        if self.phase == Ty::of::<phase::PickVariant>() {
            assert_eq!(self.ret, Ty::of::<usize>());
            assert_eq!(self.visit, Ty::of::<rt::BodyEnum>());
            return;
        }
        if self.phase == Ty::of::<phase::LeaveVariant>() {
            assert_eq!(self.visit, Ty::of::<rt::BodyEnum>());
            return;
        }
        if self.phase == Ty::of::<phase::Iter>() {
            assert_eq!(self.ret, Ty::of::<bool>());
            let iterables = &[
                Ty::of::<rt::BodyVec>(),
                Ty::of::<rt::BodyMap>(),
            ];
            assert!(iterables.contains(&self.visit), "Can only iterate over `rt::BodyVec` or `rt::BodyMap`.");
            return;
        }
        if self.phase == Ty::of::<phase::Iter>() {
            assert_eq!(self.ret, Ty::of::<bool>());
            assert_eq!(self.visit, Ty::of::<rt::BodyMap>());
            return;
        }
        let phases = &[
            Ty::of::<phase::Enter>(),
            Ty::of::<phase::Leave>(),
        ];
        assert!(phases.contains(&self.phase), "phase must be phase::Enter, phase::Visit, phase::Exit, phase::PickVariant, or phase::LeaveVariant, not {}", self.phase);

        let bodies = &[
            Ty::of::<rt::Item>(),
            Ty::of::<rt::Field>(),
            Ty::of::<rt::Ty>(),
            Ty::of::<rt::BodyStruct>(),
            Ty::of::<rt::BodyEnum>(),
            Ty::of::<rt::Variant>(),
            Ty::of::<rt::BodyVec>(),
            Ty::of::<rt::BodyMap>(),
        ];
        assert!(bodies.contains(&self.visit), "T={} in Visit<T, _>, but it must be one of:\n\trt::{{Item, Field, Ty, BodyStruct, BodyEnum, Variant, BodyVec, BodyMap}}", self.visit);

        assert!(self.visit != Ty::of::<TypeId>() && self.visit != Ty::of::<NonStaticTypeId>(), "Use Visit<Ty>, not Visit<{}>", self.visit);
        // Well. Actually this would be okay if it's a proper field.
    }
    fn downcast_erased<Ctx, Ret>(&self) -> fn(&mut Ctx, PhaseErased, VisitErased) -> Ret {
        assert!(self.phase == Ty::of::<phase::Enter>() || self.phase == Ty::of::<phase::Leave>() || self.phase == Ty::of::<phase::LeaveVariant>());
        assert_eq!(Ty::of::<Ctx>(), self.ctx);
        assert_eq!(Ty::of::<Ret>(), self.ret);
        unsafe { std::mem::transmute(self.func) }
    }
    fn downcast<Ctx, Phase, VisitBody, Ret>(&self) -> fn(&mut Ctx, Phase, Visit<VisitBody>) -> Ret {
        assert_eq!(Ty::of::<Ctx>(), self.ctx);
        assert_eq!(Ty::of::<Phase>(), self.phase);
        assert_eq!(Ty::of::<VisitBody>(), self.visit);
        assert_eq!(Ty::of::<Ret>(), self.ret);
        unsafe { std::mem::transmute(self.func) }
    }
}

#[derive(Clone)]
#[repr(C)] // might manipulate as a [usize; 2]
pub struct Visit<'a, Body, Specific = Generic> {
    pub item: &'a rt::Item,
    pub body: &'a Body,
    specific: PhantomData<Specific>,
}
impl<'a, Body: fmt::Debug, Specific> fmt::Debug for Visit<'a, Body, Specific> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Visit {}: {} ({}) {:?}", self.item.ty.name, type_name::<Body>(), type_name::<Specific>(), self.body)
    }
}


pub mod phase {
    #[derive(Debug, Copy, Clone, Default)] pub struct Enter;
    #[derive(Debug, Copy, Clone, Default)] pub struct Leave;

    #[derive(Debug, Copy, Clone, Default)] pub struct PickVariant;
    #[derive(Debug, Copy, Clone, Default)] pub struct LeaveVariant;
    #[derive(Debug, Copy, Clone, Default)] pub struct Iter;
}

type Op = u16;
pub struct Compiled<Ctx, Ret, ReadFlow> {
    start: HashMap<Ty, Range<usize>>,
    ops: Vec<Op>,
    #[cfg(debug_assertions)]
    #[allow(unused)]
    dis: Vec<String>,
    mem: Mem<Ctx, Ret>,
    visit_data: VisitData,
    visit_data_erased: [ErasedSlice; ops::COUNT_VISIT],
    read_flow: ReadFlow,
}
struct PhaseErased;
#[repr(C)]
struct VisitErased<'a> {
    item: &'a rt::Item,
    body: *const u8 /* &'a $Body */,
}
struct Mem<Ctx, Ret> {
    event_fns: Vec<fn(&mut Ctx, PhaseErased, VisitErased) -> Ret>,
    choose_variant_fns: Vec<fn(&mut Ctx, phase::PickVariant, Visit<rt::BodyEnum>) -> usize>,
    vec_seq_fns: Vec<fn(&mut Ctx, phase::Iter, Visit<rt::BodyVec>) -> bool>,
    map_pair_fns: Vec<fn(&mut Ctx, phase::Iter, Visit<rt::BodyMap>) -> bool>,
    items: Vec<Arc<rt::Item>>,
}
impl<Ctx, Ret> Default for Mem<Ctx, Ret> {
    fn default() -> Self {
        Mem {
            event_fns: Default::default(),
            choose_variant_fns: Default::default(),
            vec_seq_fns: Default::default(),
            map_pair_fns: Default::default(),
            items: Default::default(),
        }
    }
}

#[derive(Default)]
struct VisitData {
    item: Vec<Arc<rt::Item>>,
    field: Vec<Arc<rt::Field>>,
    ty: Vec<Arc<Ty>>,
    body_struct: Vec<Arc<rt::BodyStruct>>,
    body_enum: Vec<Arc<rt::BodyEnum>>,
    variant: Vec<Arc<rt::Variant>>,
    body_vec: Vec<Arc<rt::BodyVec>>,
    body_map: Vec<Arc<rt::BodyMap>>,
    // #OpCodeVec
}
struct ErasedSlice {
    start: *const Arc<u8>,
    // I don't need `len`, but otherwise a good idea.
}
impl ErasedSlice {
    pub fn new<T>(v: &[Arc<T>]) -> ErasedSlice {
        assert_eq!(size_of::<Arc<T>>(), size_of::<Arc<u8>>());
        assert_eq!(size_of::<&Arc<T>>(), size_of::<&Arc<u8>>());
        let start = if v.is_empty() {
            std::ptr::null()
        } else {
            &v[0] as *const Arc<T> as *const Arc<u8>
        };
        ErasedSlice { start }
    }
    pub unsafe fn index(&self, i: usize) -> *const u8 {
        let ptr: *const Arc<u8> = self.start.wrapping_offset(i.try_into().unwrap());
        let ptr: &Arc<u8> = &*ptr;
        let ptr: &u8 = &*ptr;
        ptr as *const u8
    }
}

trait Match<const OP: Op> {
    fn check_op(&self, actual: Op) {
        assert_eq!(actual, OP, "Tried to use opcode {} instead of {} for {}", actual, OP, type_name::<Self>());
    }
}
impl Match<{ops::VISIT_ITEM}> for rt::Item {}
impl Match<{ops::VISIT_FIELD}> for rt::Field {}
impl Match<{ops::VISIT_TY}> for rt::Ty {}
impl Match<{ops::VISIT_BODY_STRUCT}> for rt::BodyStruct {}
impl Match<{ops::VISIT_BODY_ENUM}> for rt::BodyEnum {}
impl Match<{ops::VISIT_VARIANT}> for rt::Variant {}
impl Match<{ops::VISIT_BODY_VEC}> for rt::BodyVec {}
impl Match<{ops::VISIT_BODY_MAP}> for rt::BodyMap {}

mod ops {
    use super::Op;
    #[cfg(debug_assertions)]
    pub const NOP: Op = 0; // nothing
    pub const RETURN: Op = 1; // return one thing
    pub const CHOOSE: Op = 2; // pick 2
    pub const ENTER: Op = 3; // tres es entercero
    pub const REPEAT_SEQ: Op = 4; // four loop
    pub const REPEAT_MAP: Op = 5; // four + 1

    pub const RANGE_VISIT_START: Op = 6;
    pub const VISIT_ITEM: /* ------------ */ Op = RANGE_VISIT_START + 0 /* 5 */;
    pub const VISIT_FIELD: /* ----------- */ Op = RANGE_VISIT_START + 1 /* 6 */;
    pub const VISIT_TY: /* -------------- */ Op = RANGE_VISIT_START + 2 /* 7 */; // a primitive
    pub const VISIT_BODY_STRUCT: /* ----- */ Op = RANGE_VISIT_START + 3 /* 8 */;
    pub const VISIT_BODY_ENUM: /* ------- */ Op = RANGE_VISIT_START + 4 /* 9 */;
    pub const VISIT_VARIANT: /* --------- */ Op = RANGE_VISIT_START + 5 /* 10 */;
    pub const VISIT_BODY_VEC: /* -------- */ Op = RANGE_VISIT_START + 6 /* 11 */;
    pub const VISIT_BODY_MAP: /* -------- */ Op = RANGE_VISIT_START + 7 /* 12 */;
    pub const RANGE_VISIT_END: Op = VISIT_BODY_MAP;
    // #OpCodeVec

    #[cfg(debug_assertions)]
    pub const VISIT_NAMES: &[&str] = &[
        "VISIT_ITEM",
        "VISIT_FIELD",
        "VISIT_TY",
        "VISIT_BODY_STRUCT",
        "VISIT_BODY_ENUM",
        "VISIT_VARIANT",
        "VISIT_BODY_VEC",
        "VISIT_BODY_MAP",
    ];

    /// NB: Inclusive
    pub const COUNT_VISIT: usize = (VISIT_BODY_MAP - VISIT_ITEM) as usize + 1;
}
impl<Ctx, Ret, ReadFlow> Compiled<Ctx, Ret, ReadFlow>
where
    ReadFlow: Fn(&Ctx, &mut Ret) -> Flow,
{
    // fn exec
    pub fn run(&self, ctx: &mut Ctx, ty: Ty) -> Option<Ret> {
        let mut ret = None;
        let start = self.start.get(&ty).unwrap_or_else(|| panic!("Unknown type {}", ty)).clone();
        let mut stack = vec![start];
        //println!("RUN: {} @ {:?}", ty.name, stack);
        while let Some(ops) = stack.pop() {
            //println!("{}", self.dis[ops.start]);
            let these: &[Op] = &self.ops[ops.clone()];
            #[cfg(not(debug_assertions))]
            macro_rules! log {
                ($($tt:tt)*) => {};
            }
            #[cfg(debug_assertions)]
            macro_rules! log {
                ($($tt:tt)*) => {{
                    let _ = format!($($tt)*);
                    // print!("{:?} ", ops);
                    // println!($($tt)*);
                }};
            }
            let next = match these {
                #[cfg(debug_assertions)]
                &[ops::NOP, ref next @ ..] => {
                    log!("NOP");
                    next
                },
                &[ops::RETURN, ..] => {
                    log!("RETURN {:?}", stack);
                    continue
                },
                &[
                    ops::REPEAT_SEQ,
                    visit_element_fn_id,
                    item_index,
                    visit_index,
                    element_op_start,
                    ref next @ ..
                ] => {
                    let visit_element_fn = self.mem.vec_seq_fns[visit_element_fn_id as usize];
                    let item: &rt::Item = &*self.mem.items[item_index as usize];
                    let body: &rt::BodyVec = &*self.visit_data.body_vec[visit_index as usize];
                    let flow = visit_element_fn(ctx, phase::Iter, Visit { item, body, specific: PhantomData });
                    if flow {
                        log!("\tsequence: again");
                        stack.push(ops.clone());
                        &self.ops[element_op_start as usize .. self.ops.len()]
                    } else {
                        log!("\tsequence: done");
                        next
                    }
                },
                &[
                    ops::REPEAT_MAP,
                    visit_pair_fn_id,
                    item_index,
                    visit_index,
                    key_op_start,
                    val_op_start,
                    ref next @ ..
                ] => {
                    let visit_pair_fn = self.mem.map_pair_fns[visit_pair_fn_id as usize];
                    let item: &rt::Item = &*self.mem.items[item_index as usize];
                    let body: &rt::BodyMap = &*self.visit_data.body_map[visit_index as usize];
                    let flow = visit_pair_fn(ctx, phase::Iter, Visit { item, body, specific: PhantomData });
                    if flow {
                        log!("\tmap: again");
                        let end = self.ops.len();
                        let again = ops.clone();
                        let key_ops = key_op_start as usize .. end;
                        let val_ops = val_op_start as usize .. end;
                        stack.push(again);
                        stack.push(val_ops);
                        &self.ops[key_ops]
                    } else {
                        log!("\tmap: done");
                        next
                    }
                },
                &[
                    ops::CHOOSE,
                    select_fn_index,
                    item_index,
                    visit_index,
                    n,
                    then,
                    ref next @ ..
                ] => {
                    // Jump table. Uses the function to determine which variant to use.
                    // Much like a jump table, this is a direct jump, not a function call.
                    let choose_fn = self.mem.choose_variant_fns[select_fn_index as usize];
                    let item: &rt::Item = &*self.mem.items[item_index as usize];
                    let body: &rt::BodyEnum = &*self.visit_data.body_enum[visit_index as usize];
                    let n = n as usize;
                    log!("CHOOSE (fn#{}) item:{:?} body:{:?} n={}", select_fn_index, item.ty, body, n);
                    let variant = choose_fn(ctx, phase::PickVariant, Visit { item, body, specific: PhantomData });
                    log!("\t-> {}", variant);
                    assert!(variant < n, "enum variant {} selected, but max is {}", variant, n);
                    stack.push(then as usize .. ops.end);
                    &self.ops[next[variant] as usize..]
                },
                &[ops::ENTER, start, ref next @ ..] => {
                    let next = index_within(&self.ops, next).unwrap();
                    log!("ENTER #{} [return to = {:?}]", start, next);
                    stack.push(next);
                    &self.ops[start as usize ..]
                },
                &[
                    visit_type @ ops::RANGE_VISIT_START ..= ops::RANGE_VISIT_END,
                    event_fn_index, item_index, visit_index,
                    ref next @ ..
                ] => {
                    let visit_type = visit_type as usize - ops::RANGE_VISIT_START as usize;
                    let erased_bodies = &self.visit_data_erased[visit_type];
                    // fn(&mut Ctx, PhaseErased, VisitErased) -> Ret
                    let event_fn = self.mem.event_fns[event_fn_index as usize].clone();
                    let item: &rt::Item = &*self.mem.items[item_index as usize];
                    let body: *const u8 = unsafe { erased_bodies.index(visit_index as usize) };
                    // body_data ...
                    log!("{} {:?}", ops::VISIT_NAMES[visit_type], item.ty);
                    let mut got = event_fn(ctx, PhaseErased, VisitErased {
                        item,
                        body,
                    });
                    let flow = (self.read_flow)(ctx, &mut got);
                    ret = Some(got);
                    match flow {
                        Flow::Continue => next,
                        Flow::Break => continue,
                        Flow::Quit => return ret,
                    }
                },
                // #OpCodeVec
                e => panic!("unhandled opcode pattern {:?} (total = {})", &e[..e.len().min(20)], e.len()),
            };
            let next = index_within(&self.ops, next).expect("`next` not in `ops`");
            stack.push(next);
        }
        ret
    }
}

fn index_within<T>(full: &[T], part: &[T]) -> Option<Range<usize>> {
    let size = size_of::<T>();
    let full0 = full.as_ptr() as usize;
    let full1 = full0 + full.len() * size;
    let here = part.as_ptr() as usize;
    if full0 <= here && here < full1 {
        let part_start = (here - full0) / size;
        Some(part_start .. part_start + part.len())
    } else {
        None
    }
}

trait Stow<T> {
    fn stow(&mut self, t: T) -> Op;
}
impl<'a, T> Stow<&'a Arc<T>> for Vec<Arc<T>> {
    fn stow(&mut self, t: &'a Arc<T>) -> Op {
        for (i, v) in self.iter().enumerate() {
            if Arc::ptr_eq(v, t) { return i as u16; }
        }
        let i = self.len();
        assert!(i < 0xFF_FF);
        let i = i as Op;
        self.push(t.clone());
        i
    }
}
// This isn't something that can be reasonably implemented as a trait.
macro_rules! stow {
    ($vec:expr, $t:expr) => {{
        (|| {
            let vec = &mut $vec;
            let t = $t;
            for (i, v) in vec.iter().enumerate() {
                if *v as usize == t as usize { return i as u16; }
            }
            let i = vec.len();
            assert!(i < 0xFF_FF);
            let i = i as Op;
            vec.push(t);
            i
        })()
    }};
}

#[cfg(not(debug_assertions))]
mod ignore_dis {
    #[derive(Default)]
    pub struct IgnoreDis(String);
    impl IgnoreDis {
        pub fn push(&mut self, _: String) {}
    }
    impl std::ops::Index<usize> for IgnoreDis {
        type Output = String;
        fn index(&self, _index: usize) -> &String { &self.0 }
    }
    impl std::ops::IndexMut<usize> for IgnoreDis {
        fn index_mut(&mut self, _index: usize) -> &mut Self::Output { &mut self.0 }
    }
    impl<'a> std::iter::Iterator for &'a IgnoreDis {
        type Item = String;
        fn next(&mut self) -> Option<Self::Item> { None }
    }
}


pub fn compile<Ctx, Ret, ReadFlow>(knight: Knight<Ctx, Ret>, armory: &Armory, read_flow: ReadFlow) -> Result<Compiled<Ctx, Ret, ReadFlow>, String> {
    // Should there be guards that do stuff at compile-time? Can't think of anything that
    // couldn't be better done by editing the knight.
    let mut start = HashMap::<Ty, Range<usize>>::new();
    let mut ops = Vec::<Op>::new();
    #[cfg(debug_assertions)]
    let mut dis = vec![];
    #[cfg(not(debug_assertions))]
    let mut dis = self::ignore_dis::IgnoreDis::default();
    let mut errors = Vec::<String>::new();
    let mut mem = Mem::default();
    let mut visit_data = VisitData::default();
    let mut relocations = Vec::<(usize, Ty)>::new();
    let mut ty_arc = HashSet::<Arc<Ty>>::new();
    let mut put_ty = |ty: Ty| {
        if let Some(existing) = ty_arc.get(&ty) {
            existing.clone()
        } else {
            ty_arc.insert(Arc::new(ty));
            ty_arc.get(&ty).unwrap().clone()
        }
    };

    let _enter_phase = Ty::of::<phase::Enter>();
    let _leave_phase = Ty::of::<phase::Leave>();

    let mut missing_generic_handlers = vec![];
    use rt::Body;
    // Compile the same every time.
    let mut swords = armory.swords.iter().collect::<Vec<_>>();
    swords.sort_by_key(|k| k.0.name);
    macro_rules! error {
        (continue after: $($tt:tt)*) => {{
            let msg = format!($($tt)*);
            errors.push(msg);
            continue
        }};
    }
    for (&ty, sword) in &swords {
        let ops = &mut ops;
        if errors.len() > 50 {
            errors.push(format!("..."));
            break;
        }
        let start0 = ops.len();
        macro_rules! w {
            // This seems like something in the general direction of a very good idea for helping
            // to debug writing opcodes. But it's not quite there.
            // It might be better to have an 'Abstract Op' that holds sourcing & logging info.
            (ops::$op:ident) => {{
                let pc = ops.len();
                let op = self::ops::$op;
                let msg = format!(" {:3}. {}:{} {:3} = {:80}", pc, file!(), line!(), op, stringify!($op));
                dis.push(msg);
                ops.push(op);
                pc
            }};
            (ops::$op:ident, $($label:tt)*) => {{
                let pc = ops.len();
                let op = self::ops::$op;
                let msg = format!(" {:3}. {}:{} {:3} = {:80}", pc, file!(), line!(), op, format!($($label)*));
                dis.push(msg);
                ops.push(op);
                pc
            }};
            ($op:expr) => {{
                w!($op, "{}", stringify!($op))
            }};
            ($op:expr, $($label:tt)*) => {{
                let pc = ops.len();
                let op = $op;
                let msg = format!(" {:3}. {}:{} {:3} = | {:80}", pc, file!(), line!(), op, format!($($label)*));
                dis.push(msg);
                ops.push($op);
                pc
            }};
            (@$relocate:expr) => {{
                let ty: Ty = $relocate;
                let pc = ops.len();
                relocations.push((pc, ty));
                let msg = format!(" {:3}. {}:{} ??? = | {:80}", pc, file!(), line!(), stringify!($relocate));
                dis.push(msg);
                ops.push(8008);
            }};
        }
        #[cfg(debug_assertions)]
        w!(ops::NOP, "-------- {} --------", ty.name);

        macro_rules! visit {
            ($phase:path, $body_ty:path, $specialize:expr, ops::$op:ident, visit_data.$visit_body_field:ident += $visit_data:expr) => {{
                visit!($phase, Ty::of::<$body_ty>(), $specialize, ops::$op, visit_data.$visit_body_field += $visit_data)
            }};
            ($phase:path, $body_ty:expr, $specialize:expr, ops::$op:ident, visit_data.$visit_body_field:ident += $visit_data:expr) => {{
                assert!(ops::RANGE_VISIT_START < ops::$op && ops::$op <= ops::RANGE_VISIT_END);
                let key1 = (Ty::of::<$phase>(), $body_ty, $specialize);
                let key2 = (Ty::of::<$phase>(), $body_ty, Ty::of::<Generic>());
                for key in &[key1, key2] {
                    if let Some(visitor) = knight.events.get(key) {
                        w!(ops::$op);
                        let event_fn: fn(&mut Ctx, PhaseErased, VisitErased) -> Ret = visitor.downcast_erased::<Ctx, Ret>();
                        let event_fn_id = stow!(mem.event_fns, event_fn);
                        w!(event_fn_id, "event_fn @ {}:{}", visitor.src.file(), visitor.src.line());
                        w!(mem.items.stow(&sword.item));
                        {
                            let data = $visit_data;
                            data.check_op(ops::$op);
                            w!(visit_data.$visit_body_field.stow(&data), "visit_data.{}.stow({:?})", stringify!(visit_body_field), data);
                        }
                        break;
                    }
                }
            }};
        }

        match &sword.item.body {
            Body::Primitive => {
                let ty = sword.item.ty;
                visit!(phase::Enter, Ty, ty, ops::VISIT_TY, visit_data.ty += put_ty(ty));
                visit!(phase::Leave, Ty, ty, ops::VISIT_TY, visit_data.ty += put_ty(ty));
            },
            Body::Struct(body) => {
                visit!(phase::Enter, rt::BodyStruct, ty, ops::VISIT_BODY_STRUCT, visit_data.body_struct += body);
                for field in body.fields.iter() {
                    visit!(phase::Enter, rt::Field, ty, ops::VISIT_FIELD, visit_data.field += field);
                    {
                        // call that field!
                        w!(ops::ENTER);
                        w!(@field.ty);
                    }
                    visit!(phase::Leave, rt::Field, ty, ops::VISIT_FIELD, visit_data.field += field);
                }
                visit!(phase::Leave, rt::BodyStruct, ty, ops::VISIT_BODY_STRUCT, visit_data.body_struct += body);
            },
            Body::Enum(body) => {
                let ty = sword.item.ty;
                visit!(phase::Enter, rt::BodyEnum, ty, ops::VISIT_BODY_ENUM, visit_data.body_enum += body);
                if let Some(visitor) = knight.choose_event(
                    &mut errors,
                    &mut missing_generic_handlers,
                    Ty::of::<phase::PickVariant>(),
                    Ty::of::<rt::BodyEnum>(),
                    ty,
                ) {
                    assert!(!body.variants.is_empty(), "uninhabited enums not supported");
                    // We could skip CHOOSE if variants.len() == 1. But that would be confusing.
                    w!(ops::CHOOSE);
                    let choose_fn: fn(&mut Ctx, phase::PickVariant, Visit<rt::BodyEnum>) -> usize = visitor.downcast();
                    let choose_fn_id: Op = stow!(mem.choose_variant_fns, choose_fn);
                    w!(choose_fn_id);
                    w!(mem.items.stow(&sword.item));
                    w!(visit_data.body_enum.stow(body));
                    w!(body.variants.len() as Op);
                    let then = w!(888, "<then>");
                    let jump_table_start = ops.len();
                    for _n in 0..body.variants.len() {
                        w!(888, "variant #{}", _n);
                    }
                    for (i, variant) in body.variants.iter().enumerate() {
                        let pc: Op = ops.len().try_into().expect("op index too large");
                        let jt = jump_table_start + i;
                        ops[jt] = pc;
                        dis[jt] = format!(" {:3}. {}:{} {:3} = | | {} (relocation)", jt, file!(), line!(), pc, variant.name);
                        visit!(phase::Enter, rt::Variant, ty, ops::VISIT_VARIANT, visit_data.variant += variant);
                        for field in variant.fields.iter() {
                            visit!(phase::Enter, rt::Field, ty, ops::VISIT_FIELD, visit_data.field += field);
                            visit!(phase::Leave, rt::Field, ty, ops::VISIT_FIELD, visit_data.field += field);
                        }
                        visit!(phase::Leave, rt::Variant, ty, ops::VISIT_VARIANT, visit_data.variant += variant);
                        w!(ops::RETURN);
                    }
                    ops[then] = ops.len() as Op;
                    write!(&mut dis[then], "\n\t --> {}", ops.len()).ok();
                }
                visit!(phase::LeaveVariant, rt::BodyEnum, ty, ops::VISIT_BODY_ENUM, visit_data.body_enum += body);
                visit!(phase::Leave, rt::BodyEnum, ty, ops::VISIT_BODY_ENUM, visit_data.body_enum += body);
            },
            Body::Vec(body) => {
                // Vec::with_capacity(n);
                visit!(phase::Enter, rt::BodyVec, ty, ops::VISIT_BODY_VEC, visit_data.body_vec += body);
                // for next in iter { vec.push(next) }
                let ty = sword.item.ty;
                if let Some(visitor) = knight.choose_event(
                    &mut errors,
                    &mut missing_generic_handlers,
                    Ty::of::<phase::Iter>(),
                    Ty::of::<rt::BodyVec>(),
                    ty,
                ) {
                    // opcode that calls a fn repeatedly until it returns false
                    w!(ops::REPEAT_SEQ);
                    let visit_element_fn: fn(&mut Ctx, phase::Iter, Visit<rt::BodyVec>) -> bool = visitor.downcast();
                    let visit_element_fn_id: Op = stow!(mem.vec_seq_fns, visit_element_fn);
                    w!(visit_element_fn_id);
                    w!(mem.items.stow(&sword.item));
                    w!(visit_data.body_vec.stow(body));
                    w!(@body.items);
                }
                // vec.trim();
                visit!(phase::Leave, rt::BodyVec, ty, ops::VISIT_BODY_VEC, visit_data.body_vec += body);
            },
            Body::Map(body) => {
                visit!(phase::Enter, rt::BodyMap, ty, ops::VISIT_BODY_MAP, visit_data.body_map += body);
                let ty = sword.item.ty;
                if let Some(visitor) = knight.choose_event(
                    &mut errors,
                    &mut missing_generic_handlers,
                    Ty::of::<phase::Iter>(),
                    Ty::of::<rt::BodyMap>(),
                    ty,
                ) {
                    w!(ops::REPEAT_MAP);
                    let visit_pair_fn: fn(&mut Ctx, phase::Iter, Visit<rt::BodyMap>) -> bool = visitor.downcast();
                    let visit_pair_fn_id: Op = stow!(mem.map_pair_fns, visit_pair_fn);
                    w!(visit_pair_fn_id);
                    w!(mem.items.stow(&sword.item));
                    w!(visit_data.body_map.stow(body));
                    w!(@body.keys);
                    w!(@body.vals);
                }
                visit!(phase::Leave, rt::BodyMap, ty, ops::VISIT_BODY_MAP, visit_data.body_map += body);
            },
        }
        w!(ops::RETURN);
        let start1 = ops.len();
        start.insert(ty, start0..start1);
    }
    let mut missing = false;
    for &(pc, ty) in &relocations {
        if let Some(r) = start.get(&ty) {
            let op = r.start as Op;
            ops[pc] = op;
            dis[pc] = format!(" {:3}. {}:{} {:3} = | {} (relocation)", pc, file!(), line!(), op, ty);
        } else {
            missing = true;
            error!(continue after: " - undefined reference to {}", ty);
        }
    }
    if missing {
        errors.push(format!("Available types:"));
        for k in start.keys() {
            errors.push(format!(" - {}", k));
        }
    }
    if cfg!(debug_assertions) {
        println!("\nOPS:");
        for l in &dis {
            println!("{}", l);
        }
        println!("{} bytes", size_of::<Op>() * ops.len());
    }
    if !errors.is_empty() {
        use std::fmt::Write;
        let mut out = format!("Errors compiling Knight:\n");
        for msg in &errors {
            write!(out, "{}\n", msg).ok();
        }
        return Err(out);
    }
    // SAFETY: The opcodes contain erased pointers to values in `_knight` and `_armory`.
    // In other words, `ops` has an erased borrow of `_knight` and `_armory`.
    Ok(Compiled {
        start,
        ops,
        #[cfg(debug_assertions)]
        dis,
        mem,
        visit_data_erased: [
            ErasedSlice::new(&visit_data.item[..]),
            ErasedSlice::new(&visit_data.field[..]),
            ErasedSlice::new(&visit_data.ty[..]),
            ErasedSlice::new(&visit_data.body_struct[..]),
            ErasedSlice::new(&visit_data.body_enum[..]),
            ErasedSlice::new(&visit_data.variant[..]),
            ErasedSlice::new(&visit_data.body_vec[..]),
            ErasedSlice::new(&visit_data.body_map[..]),
            // #OpCodeVec
        ],
        visit_data,
        read_flow,
    })
}

#[allow(dead_code)] // FIXME: set library crate type
pub enum Flow {
    /// Normal flow of execution.
    Continue,
    /// Skip processing the rest of the item.
    Break,
    /// End execution immediately.
    Quit,
}


#[repr(C)]
pub struct Specialize<Body, Specialized> {
    body: Body,
    _marker: PhantomData<Specialized>,
}
impl<Body, Specialized> std::ops::Deref for Specialize<Body, Specialized> {
    type Target = Body;
    fn deref(&self) -> &Body { &self.body }
}
