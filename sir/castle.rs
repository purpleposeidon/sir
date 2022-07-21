use std::collections::HashMap;
use crate::{Blade, Name};
use crate::util::Ty;
use crate::rt::Sword;

pub struct Castle {
    reg: HashMap<Ty, Sword>,
    name2ty: HashMap<Name, Ty>,
    constraints: Vec<Box<dyn FnMut(&mut HashMap<Ty, Sword>, &HashMap<Name, Ty>) + 'static + Send + Sync>>,
    frozen: bool,
}
impl Castle {
    pub fn reg(&self) -> &HashMap<Ty, Sword> {
        &self.reg
    }
    pub fn add<B: Blade>(&mut self) {
        self.add_orphan(
            Ty::of::<B>(),
            Sword::of::<B>(),
        );
    }
    pub fn add_orphan(&mut self, ty: Ty, sword: Sword) {
        self.assert_thawed();
        self.name2ty.insert(ty.name, ty);
        self.reg.insert(ty, sword);
    }
    pub fn constrain(
        &mut self,
        f: impl FnMut(
            &mut HashMap<Ty, Sword>,
            &HashMap<Name, Ty>,
        ) + 'static + Send + Sync,
    ) {
        self.constraints.push(Box::new(f));
    }
    pub fn freeze(&mut self) {
        self.assert_thawed();
        self.frozen = true;
        for c in std::mem::take(&mut self.constraints).iter_mut() {
            c(&mut self.reg, &self.name2ty);
        }
    }
    fn assert_thawed(&self) {
        if self.frozen { panic!("registry frozen"); }
    }
}

