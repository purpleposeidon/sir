extern crate sir;

use sir::rt::*;
use sir::knights::{Kingdom, BodyVisitor, Visit, a2r};
use sir::util::{AnyDebug, Ty};
use std::collections::HashMap;


pub struct Vey {
    pub kingdom: Kingdom,
    pub primitives: HashMap<Ty, fn(&dyn AnyDebug)>,
}
impl Vey {
    pub fn equip(kingdom: &Kingdom) -> Self {
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
            kingdom: kingdom.clone(),
            primitives,
        }
    }
    pub fn value(&self, val: &dyn AnyDebug) -> Result<(), ()> {
        let ty = val.get_ty();
        let mut visitor = VeyVisitor {
            kingdom: &self.kingdom,
            depth: 0,
            val,
        };
        self.kingdom.visit(&ty, &mut visitor)?;
        println!();
        Ok(())
    }
}

pub struct VeyVisitor<'a> {
    pub kingdom: &'a Kingdom,
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
        print!("{}", visit.item.ty.name());
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
                kingdom: self.kingdom,
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
            self.kingdom.visit(&ty, &mut sub)?;
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
                    kingdom: self.kingdom,
                    depth,
                    val: field.get_ref(self.val),
                };
                let ty = sub.val.get_ty();
                self.indent();
                print!("{}: ", field.name);
                self.kingdom.visit(&ty, &mut sub)?;
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
                    kingdom: self.kingdom,
                    depth: self.depth,
                    val: field.get_ref(self.val),
                };
                let ty = sub.val.get_ty();
                self.kingdom.visit(&ty, &mut sub)?;
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
                kingdom: self.kingdom,
                depth: self.depth,
                val,
            };
            self.kingdom.visit(ty, &mut sub)?;
            print!("]");
            return Ok(());
        }
        println!("vec![");
        let depth = self.depth + 1;
        for i in 0..len {
            let val = (visit.body.vt.get_ref)(self.val, i);
            let mut sub = VeyVisitor {
                kingdom: self.kingdom,
                depth,
                val,
            };
            sub.indent();
            self.kingdom.visit(ty, &mut sub)?;
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
                        kingdom: self.kingdom,
                        depth: self.depth,
                        val: k,
                    };
                    tri![self.kingdom.visit(kty, sub)];
                    print!(": ");
                    let sub = &mut VeyVisitor {
                        kingdom: self.kingdom,
                        depth: self.depth,
                        val: v,
                    };
                    tri![self.kingdom.visit(vty, sub)];
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
                    kingdom: self.kingdom,
                    depth,
                    val: k,
                };
                sub.indent();
                tri![self.kingdom.visit(kty, sub)];
                print!(": ");
                let sub = &mut VeyVisitor {
                    kingdom: self.kingdom,
                    depth,
                    val: v,
                };
                tri![self.kingdom.visit(vty, sub)];
                println!(",");
            }
        });
        self.indent();
        print!("}}");
        ret
    }
}

#[test]
fn main() {
    use sir::{blade, Blade};
    let mut kingdom = Kingdom::builder();
    kingdom.add::<Option<i8>>();
    kingdom.add::<i8>();
    kingdom.add::<Vec<String>>();
    kingdom.add::<Result<i8, bool>>();
    kingdom.add::<HashMap<i32, bool>>();
    kingdom.add::<Knight>();
    kingdom.add::<MindLevel>();
    let kingdom = &kingdom.build();

    let sir_vay = Vey::equip(kingdom);
    {
        let lancelot = Knight::Mind(MindLevel {
            name: format!("Sir Lancelot"),
            anger: Some(2),
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

    #[derive(Debug)]
    enum Knight {
        Mind(MindLevel),
        Nobody,
    }
    impl Blade for Knight {
        blade! {
            enum Knight where {},
            Mind(MindLevel),
            Nobody,
        }
    }
    #[derive(Debug)]
    struct MindLevel {
        name: String,
        anger: Option<i8>,
        foo: Result<i8, bool>,
    }
    impl Blade for MindLevel {
        blade! {
            struct MindLevel where {
                sir::chivalry::NonTotalMem,
            },
            name: String,
            anger: Option<i8>,
            foo: Result<i8, bool>,
        }
    }
}
