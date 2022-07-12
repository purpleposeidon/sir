extern crate sir;

use sir::quest::{phase, Visit, Knight};
use sir::util::Ty;
use sir::knights::Armory;
use sir::rt;
use std::collections::HashMap;

fn sir_manuel(armory: &Armory) {
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
    let comp = sir::quest::compile(
        knight.clone(),
        &armory,
        |_ctx: &Manuel,
        _ret: &mut ()| sir::quest::Flow::Continue,
    ).unwrap();
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

    sir_manuel(armory);
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

