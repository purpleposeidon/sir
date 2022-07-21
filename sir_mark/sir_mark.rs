extern crate sir;

use sir::{blade, Blade};
use sir::rt::*;
use sir::chivalry::*;
use sir::knights::{Kingdom, BodyVisitor, Visit};
use sir::util::AnyDebug;
use std::fmt;

pub struct Mark {
    kingdom: Kingdom,
    // SURMARK
    // Sur"mark`, n. (Shipbuilding)
    //
    // Defn: A mark made on the molds of a ship, when building, to show
    // where the angles of the timbers are to be placed. [Written also
    // sirmark.]
}
impl Mark {
    // Simply serialize values to lua.
    pub fn new(kingdom: &Kingdom) -> Mark {
        Mark { kingdom: kingdom.clone() }
    }
    pub fn value(&self, mut out: impl fmt::Write, var: &'static str, val: &dyn AnyDebug) -> fmt::Result {
        write!(out, "{} = ", var)?;
        let ty = val.get_ty();
        let mut m = MarkVisitor {
            kingdom: &self.kingdom,
            val,
            out: &mut out,
            depth: 0,
        };
        self.kingdom.visit(&ty, &mut m)
    }
}
struct MarkVisitor<'a, W: fmt::Write> {
    kingdom: &'a Kingdom,
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
            self.kingdom.visit(&field.ty, self)?;
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
                    self.kingdom.visit(&field.ty, self)?;
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
                    self.kingdom.visit(&field.ty, self)?;
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
    fn visit_vec(&mut self, visit: Visit<BodyVec>) -> fmt::Result {
        // Unlike visit_map, we assume the order is correct.
        if (visit.body.vt.len)(self.val) == 0 {
            return write!(self.out, "{{}}");
        }
        writeln!(self.out, "{{")?;
        self.depth += 1;
        let item_ty = &visit.body.items;
        (visit.body.vt.iter)(self.val, &mut |iter: &mut dyn Iterator<Item=&dyn AnyDebug>| {
            for item in iter {
                self.indent();
                let mut visitor = MarkVisitor {
                    kingdom: self.kingdom,
                    val: item,
                    out: self.out,
                    depth: self.depth,
                };
                self.kingdom.visit(item_ty, &mut visitor).ok();
                writeln!(self.out, ",").ok();
            }
        });
        self.depth -= 1;
        self.indent();
        write!(self.out, "}}")
    }
    fn visit_map(&mut self, visit: Visit<BodyMap>) -> fmt::Result {
        if (visit.body.vt.len)(self.val) == 0 {
            return write!(self.out, "{{}}");
        }
        writeln!(self.out, "{{")?;
        self.depth += 1;
        let key_ty = &visit.body.keys;
        let val_ty = &visit.body.vals;
        // Serialize HashMap in consistent order.
        // FIXME: This could be an OrderedHashMap. Probably needs chivalry::SortBeforeSerialize?
        let mut entries = vec![];
        (visit.body.vt.iter_items)(self.val, &mut |iter: &mut dyn Iterator<Item=(AnyKey, &dyn AnyDebug)>| {
            use std::fmt::Write as _;
            for (key, val) in iter {
                let mut out = String::new();
                write!(out, "{:width$}[", "", width = self.depth * 4).ok(); //self.indent();
                let mut visitor = MarkVisitor {
                    kingdom: self.kingdom,
                    val: key,
                    out: &mut out,
                    depth: self.depth,
                };
                self.kingdom.visit(key_ty, &mut visitor).ok();
                write!(out, "] = ").ok();
                let mut visitor = MarkVisitor {
                    kingdom: self.kingdom,
                    val: val,
                    out: &mut out,
                    depth: self.depth,
                };
                self.kingdom.visit(val_ty, &mut visitor).ok();
                writeln!(out, ",").ok();
                entries.push(out);
            }
        });
        entries.sort();
        for e in &entries {
            write!(self.out, "{}", e)?;
        }
        self.depth -= 1;
        self.indent();
        write!(self.out, "}}")
    }
}

#[test]
fn main() {
    let mut kingdom = Kingdom::builder();
    kingdom.add::<Option<i32>>();
    kingdom.add::<HashMap<i32, bool>>();
    kingdom.add::<Example>();
    kingdom.add::<Nested>();
    kingdom.add::<Vec<i32>>();
    let kingdom = &kingdom.build();

    let example = Example {
        foo: 2,
        bar: 32,
        nested: Nested { cheese: format!("yes, please!") },
        map: {
            let mut map = HashMap::new();
            map.insert(24, false);
            map.insert(32, true);
            map
        },
        answer: Some(32),
    };

    let mark = Mark::new(kingdom);
    let mut out = String::new();
    mark.value(&mut out, "out", &example).unwrap();
    println!("{}", out);

    let mut out = String::new();
    let example = vec![1, 2, 3, 4, 5];
    mark.value(&mut out, "out", &example).unwrap();
    println!("{}", out);

    let mut out = String::new();
    let example = Vec::<i32>::new();
    mark.value(&mut out, "out", &example).unwrap();
    println!("{}", out);
}

use std::collections::HashMap;

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
