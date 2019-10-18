use super::Object;
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::fmt::{self, Formatter};
use std::rc::Rc;

#[derive(Eq, PartialEq, Clone)]
pub struct Environment {
    data: BTreeMap<String, Object>,
    parent: Option<Rc<RefCell<Environment>>>,
}

impl std::fmt::Debug for Environment {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{:?}", self.data)
    }
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            data: BTreeMap::new(),
            parent: None,
        }
    }

    pub fn from(parent: Rc<RefCell<Environment>>) -> Environment {
        Environment {
            data: BTreeMap::new(),
            parent: Some(parent),
        }
    }

    pub(crate) fn set(&mut self, name: &str, val: Object) {
        self.data.insert(name.to_owned(), val);
    }

    pub(crate) fn get(&self, name: &str) -> Option<Object> {
        let val = self.data.get(name).cloned();
        if val.is_none() && self.parent.is_some() {
            let parent = self.parent.clone().unwrap();
            return parent.borrow().get(name).clone();
        }

        return val;
    }
}
