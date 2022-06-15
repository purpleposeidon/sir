#[macro_use]
extern crate mopa;

extern crate sir_macro;
#[doc(hidden)]
pub use sir_macro::blade_impl;

#[macro_export]
macro_rules! orphan_blade {
    ($($tt:tt)*) => {
        $crate::blade_impl!($($tt)*);
    };
}


#[macro_export]
macro_rules! blade {
    ($($tt:tt)*) => {
        fn sword() -> $crate::rt::Sword {
            $crate::blade_impl! {
                $($tt)*
            }
        }
    };
}

// You can't use `$crate` in a proc_macro; this lets you use `crate::sir` instead.
#[doc(hidden)]
pub use crate as sir;

mod knights;
mod castle;
mod util;
mod rt;
mod spam;
mod impls;
mod quest;
//mod arena;

/// Puns. Deal with it 😎
pub use self::castle::*;

pub type Name = &'static str;

#[doc(hidden)]
pub mod prelude_macro {
    pub use crate::rt::*;
    pub use crate::util::{Ty, AnyDebug, FieldInit, Init};
    pub use std::borrow::Cow;
    pub use std::sync::Arc;

    #[doc(hidden)]
    #[cold]
    pub fn variant_mismatch() -> ! {
        panic!("unexpected variant mismatch")
    }
}

pub mod chivalry;

mod blade_trait {
    use crate::util::AnyDebug;
    use crate::rt::Sword;

    pub trait Blade: AnyDebug {
        // NOTE: Sword has a lot of Arc's. I was thinking I'd like to be able to recycle them. But
        // in fact I would not; really it's just there to make Eq easy.
        fn sword() -> Sword;

        // These two are for convenience.
        #[doc(hidden)]
        fn downcast_ref(this: &dyn AnyDebug) -> &Self
        where
            Self: Sized,
        {
            this.downcast_ref().expect("type mismatch")
        }
        #[doc(hidden)]
        fn downcast_mut(this: &mut dyn AnyDebug) -> &mut Self
        where
            Self: Sized,
        {
            this.downcast_mut().expect("type mismatch")
        }
    }
}
pub use self::blade_trait::Blade;










use self::chivalry::*;
#[derive(Clone, Debug)]
enum Knight {
    Dead,
    Menu(usize),
    At {
        level: Name,
        x: i32,
        y: i32,
    },
    Mind(MindLevel),
}
impl Blade for Knight {
    blade! {
        enum Knight where {
            NonTotalMem,
            Version(2),
        },
        Knight::Dead,
        Knight::Menu(usize where {
            Clamp::with(|v: &mut usize| *v = 0),
        }),
        Knight::At {
            // ident: ty $(= init)? $(in { })?
            level: Name where {
                ProvideDefault::from(|| "grass"),
            },
            x: i32,
            y: i32,
        },
        Knight::Mind(MindLevel),
    }
}
#[derive(Debug, Clone)]
struct MindLevel {
    name: String,
    anger: Option<i32>,
    foo: Result<i32, bool>,
    nested: Option<Box<MindLevel>>,
}
impl Blade for MindLevel {
    blade! {
        struct MindLevel where {},
        name: String,
        anger: Option<i32>,
        foo: Result<i32, bool>,
        nested: Option<Box<MindLevel>>,
    }
}

#[derive(Debug)]
struct AT {
    x: bool,
    data: Vec<i32>,
    foo: Result<bool, i32>,
    map: HashMap<i32, bool>,
}
impl Blade for AT {
    blade! {
        struct AT where {},
        x: bool,
        data: Vec<i32>,
        foo: Result<bool, i32>,
        map: HashMap<i32, bool>,
    }
}

pub struct TODO; // FIXME

#[allow(unused_imports)] #[macro_use] extern crate eztrace; // FIXME: rm

fn main() {
    let _ = TODO; // TODO: yeah, we should support compiling some guards? Yeah! It's just like uh... it's just sorta like anoter Phase.
    let mut armory = crate::knights::Armory::builder();
    /*armory.add::<Knight>();
    armory.add::<MindLevel>();
    armory.add::<Option<i32>>();
    armory.add::<Result<i32, bool>>();
    armory.add::<Option<Box<MindLevel>>>();
    armory.add::<Box<MindLevel>>();
    armory.add::<Vec<String>>();*/
    armory.add::<AT>();
    armory.add::<Vec<i32>>();
    armory.add::<Result<bool, i32>>();
    armory.add::<HashMap<i32, bool>>();
    /*armory.add::<Name>();
    armory.add::<HashMap<i32, bool>>();
    armory.add::<Example>();
    armory.add::<Nested>();*/
    let armory = &armory.build();

    let sir_vay = knights::Vey::equip(armory);
    if false /* FIXME */ {
        let lancelot = Knight::Mind(MindLevel {
            name: format!("Sir Lancelot"),
            anger: Some(2),
            nested: Some(Box::new(MindLevel {
                name: format!("Sir Lancelot's Troubled Childhood"),
                anger: None,
                nested: Some(Box::new(MindLevel {
                    name: format!("The Noodle Incident"),
                    anger: Some(99),
                    nested: None,
                    foo: Ok(2),
                })),
                foo: Err(true),
            })),
            foo: Err(false),
        });
        sir_vay.value(&lancelot).ok();

        let test: Vec<String> = vec![
            format!("hey"),
            format!("guys"),
        ];
        sir_vay.value(&test).ok();

        let mut test = HashMap::<i32, bool>::new();
        test.insert(0, true);
        test.insert(1, false);
        sir_vay.value(&test).ok();

        println!();
    }
    if false {
        let mun = knights::Mun::new(armory);
        use rlua::*;
        let lua = rlua::Lua::new();
        let r: Value = lua.exec(r##"
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
        let r = mun.create::<Example>(&lua, r);
        match &r {
            Err(e) => println!("{}", e),
            //Ok(v) => println!("{:#?}", v),
            Ok(v) => { sir_vay.value(v).ok(); },
        }

        let r = r.unwrap();
        println!("sir_mun = {:#?}", r);
        let mark = knights::Mark::new(armory);
        let mut out = format!("");
        mark.value(&mut out, "sir_mark", &r).ok();
        //println!("WRITE = {{\n    {}\n}}", out.replace("\n", "\n    "));
        println!("{}", out);
        let j: Value = lua.exec(&format!("{}\nreturn sir_mark", out), None).unwrap();
        let j = mun.create::<Example>(&lua, j);
        match &j {
            Err(e) => println!("{}", e),
            //Ok(v) => println!("{:#?}", v),
            Ok(v) => { print!("REREAD = "); sir_vay.value(v).ok(); },
        }

        use std::io::Cursor;
        let eel = knights::Eel::<cfg::Cfg, Cursor<&[u8]>, Vec<u8>>::new(armory);
        let mut out = Vec::<u8>::new();
        eel.write(cfg::new(), &mut out, &r).unwrap();
        println!("sir_eel = {:?}", out);
        let mut cursor = Cursor::new(&out[..]);
        let got = eel.read::<Example>(cfg::new(), &mut cursor).unwrap();
        trace!(got);
    }
    if false {
        //let r = std::result::Result::<i32, bool>::Ok(42);
        let r = std::result::Result::<i32, bool>::Err(true);
        println!("READ = {:#?}", r);
        let mark = knights::Mark::new(armory);
        let mut out = format!("");
        mark.value(&mut out, "config", &r).ok();
        println!("WRITE = {{\n    {}\n}}", out.replace("\n", "\n    "));
        use rlua::*;
        let lua = rlua::Lua::new();
        let mun = knights::Mun::new(armory);
        let j: Value = lua.exec(&format!("{}\nreturn config", out), None).unwrap();
        let j = mun.create::<std::result::Result::<i32, bool>>(&lua, j);
        match &j {
            Err(e) => println!("{}", e),
            //Ok(v) => println!("{:#?}", v),
            Ok(v) => { print!("REREAD = "); sir_vay.value(v).ok(); },
        }

    }
    if false {
        let v = std::result::Result::<i32, bool>::Err(true);
        use std::io::Cursor;
        let eel = knights::Eel::<cfg::Cfg, Cursor<&[u8]>, Vec<u8>>::new(armory);
        let mut out = Vec::<u8>::new();
        {
            // FIXME: &mut &mut? Dumb.
            let _ = crate::TODO;
            {
                eel.write(cfg::new(), &mut out, &v).unwrap();
            }
            println!("sir_eel = {:?}", out);
        }
        let mut cursor = Cursor::new(&out[..]);
        let got = eel.read::<std::result::Result::<i32, bool>>(cfg::new(), &mut cursor).unwrap();
        trace!(got);
    }
    {
        use crate::quest::{phase, Visit, Knight};
        use crate::util::Ty;
        impl Manuel {
            fn log(&mut self, args: std::fmt::Arguments) {
                if self.depth > 40 || self.depth < 0 {
                    print!("({}) ", self.depth)
                } else {
                    for _ in 0..self.depth {
                        print!("│ ");
                    }
                }
                println!("{}", args);
            }
        }
        macro_rules! p {
            ($ctx:expr, $($tt:tt)*) => {{
                $ctx.log(format_args!($($tt)*));
            }};
        }
        struct Manuel {
            depth: isize,
            stack: Vec<Frame>,
        }
        enum Frame {
            IterCount(usize),
        }
        let knight = Knight::<Manuel, ()>::new()
            .on(|ctx: &mut Manuel, phase::Enter, visit: Visit<Ty, i32>| {
                ctx.depth += 1;
                p!(ctx, "specialized prim: {:?}", visit.body);
                ctx.depth -= 1;
            })
            .on(|ctx: &mut Manuel, phase::Enter, visit: Visit<Ty>| {
                ctx.depth += 1;
                p!(ctx, "general prim: {:?}", visit.body);
                ctx.depth -= 1;
            })
            .on(|ctx: &mut Manuel, phase::Enter, visit: Visit<rt::BodyStruct>| {
                p!(ctx, "Enter struct {:?}", visit.item.ty);
                ctx.depth += 1;
            })
            .on(|ctx: &mut Manuel, phase::Leave, visit: Visit<rt::BodyStruct>| {
                ctx.depth -= 1;
                p!(ctx, "Leave struct {:?}", visit.item.ty);
            })
            .on(|ctx: &mut Manuel, phase::Enter, visit: Visit<rt::Field>| {
                p!(ctx, "Enter field: {:?}", visit.body);
            })
            .on(|ctx: &mut Manuel, phase::Leave, visit: Visit<rt::Field>| {
                p!(ctx, "Leave field: {:?}", visit.body);
            })
            .on(|ctx: &mut Manuel, phase::Enter, visit: Visit<rt::BodyEnum>| {
                ctx.depth += 1;
                p!(ctx, "Enter enum {:?}", visit.body);
                ctx.depth += 1;
            })
            .on(|ctx: &mut Manuel, phase::Leave, visit: Visit<rt::BodyEnum>| {
                ctx.depth -= 1;
                p!(ctx, "Leave enum {:?}", visit.body);
                ctx.depth -= 1;
            })
            .on_enum(|ctx: &mut Manuel, phase::PickVariant, visit: Visit<rt::BodyEnum>| {
                p!(ctx, "Select variant {:?}", &visit.body.variants[1].name);
                ctx.depth += 1;
                1usize
            })
            .on(|ctx: &mut Manuel, phase::LeaveVariant, _visit: Visit<rt::BodyEnum>| {
                ctx.depth -= 1;
                p!(ctx, "Close variant");
            })
            .on(|ctx: &mut Manuel, phase::Enter, visit: Visit<rt::BodyVec>| {
                ctx.depth += 1;
                p!(ctx, "Enter vec {:?}", visit.body);
                ctx.depth += 1;
                ctx.stack.push(Frame::IterCount(0));
            })
            .on_iter(|ctx: &mut Manuel, phase::Iter, visit: Visit<rt::BodyVec>| -> bool {
                let Frame::IterCount(mut n) = ctx.stack.pop().unwrap();
                p!(ctx, "#{} iter seq element {}", n, visit.item.ty);
                n += 1;
                ctx.stack.push(Frame::IterCount(n));
                n < 3
            })
            .on(|ctx: &mut Manuel, phase::Leave, visit: Visit<rt::BodyVec>| {
                ctx.depth -= 1;
                p!(ctx, "Leave vec {:?}", visit.body);
                ctx.depth -= 1;
                ctx.stack.pop();
            })
            .on_iter(|_ctx: &mut Manuel, phase::Iter, _visit: Visit<rt::BodyMap>| -> bool {
                false
            })
            .on(|ctx: &mut Manuel, phase::Enter, visit: Visit<rt::BodyMap>| {
                p!(ctx, "Enter map {:?}", visit.body);
                ctx.depth += 1;
                ctx.stack.push(Frame::IterCount(0));
            })
            .on_iter(|ctx: &mut Manuel, phase::Iter, visit: Visit<rt::BodyMap>| -> bool {
                let Frame::IterCount(mut n) = ctx.stack.pop().unwrap();
                p!(ctx, "#{} iter map pair {}", n, visit.item.ty);
                n += 1;
                ctx.stack.push(Frame::IterCount(n));
                n < 3
            })
            .on(|ctx: &mut Manuel, phase::Leave, visit: Visit<rt::BodyMap>| {
                ctx.depth -= 1;
                p!(ctx, "Leave map {:?}", visit.body);
                ctx.stack.pop();
            })
            .build();
        let comp = crate::quest::compile(knight.clone(), &armory, |_ctx: &Manuel, _ret: &mut ()| quest::Flow::Continue);
        let comp = match comp {
            Ok(c) => c,
            Err(e) => {
                println!("{}", e);
                return;
            },
        };
        println!();
        /*comp.run(&mut Manuel {
            val: &32,
        }, Ty::of::<i32>());*/
        let manny = &mut Manuel {
            depth: 0,
            stack: vec![],
        };
        comp.run(manny, Ty::of::<AT>());
        assert_eq!(manny.depth, 0);
        /*comp.run(&mut Manuel {
            val: &Result::<i32, bool>::Err(true),
        }, Ty::of::<Result<i32, bool>>());*/
    }
}

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


/*fn main() {
    let val = 40_000;
    let mut out = Vec::<u8>::new();
    mod cfg {
        use bincode::config::*;
        pub type Cfg = Configuration<LittleEndian, Fixint, SkipFixedArrayLength>;
    }
    let cfg: cfg::Cfg = bincode::config::standard()
        .skip_fixed_array_length() // NB: incompatible w/ serde
        .with_fixed_int_encoding()
        .with_little_endian() // Match x86 order. (It's the default, but we're being explicit.)
    ;
    bincode::encode_into_std_write(&val, &mut out, cfg).unwrap();
    trace!(out);

    /*use std::io::Cursor;
    let r: i32 = bincode::decode_from_std_read(&mut Cursor::new(&out[..]), cfg).unwrap();
    trace!(r);*/
}*/

use std::collections::HashMap;

#[derive(Default, Debug)]
struct Example {
    foo: i8,
    bar: u64,
    nested: Nested,
    map: HashMap<i32, bool>,
    answer: Option<i32>,
}
#[derive(Default, Debug)]
struct Nested {
    cheese: String,
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
impl Blade for Nested {
    blade! {
        struct Nested where {},
        cheese: String,
    }
}



// Look: The real deal is that we need to be able to deserialize enums.
// (... And also editing! But not as badly.)
// And the deserializing should be efficient.
//      - err, not really. *Serializing* must be efficient.
// We're deserializing arrays.
// Wait, and we're serializing arrays...?
// Our official means of serializing arrays is 'memcpy'.
// So our official means of deserializing is... uh.
// Okay but one thing... schema changes.
// Serializing is deserializing is memcpy.
// ...Yeah I think we bust out unsafe. IDK man.

// .... Waaaaitaminute. What do I want to be able to do?
// - I/O Vec<Stuff> (quickly?)
// - Debug introspection
// - I/O for console commands
// - I/O LUA
// - constraining
// - Deserialize w/ migration
// + Plus I'd need to be able to create, like, new things; like pushing a new row
//
// Also compare to FZ's DataHelper...
// - I/O NBT
// - I/O network
// - I/O cfg gui
// - I/_ validation
// - I/_ migration

// Important question?
// You have a Vec<T>. You deserialize another Vec<T> to
// add to the existing one. How does that work?
