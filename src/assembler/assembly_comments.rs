#![allow(dead_code)]
use std::collections::{HashMap, BTreeMap};
use once_cell::sync::Lazy;
use parking_lot::Mutex;

pub type CommentMap = HashMap<usize, String>;

pub struct AssemblyCommentsRegistry {
    lock: Mutex<BTreeMap<usize, (usize, CommentMap)>>,
}

impl AssemblyCommentsRegistry {
    pub fn new() -> Self {
        AssemblyCommentsRegistry {
            lock: Mutex::new(BTreeMap::new())
        }
    }

    pub fn singleton() -> &'static Self {
        static INSTANCE: Lazy<AssemblyCommentsRegistry> = Lazy::new(|| AssemblyCommentsRegistry::new());
        &INSTANCE
    }

    pub fn comment<'a>(&self, _in_code: *const u8) -> Option<String> {
        /*let comments = self.lock.lock();
    
        let comments = comments.lower_bound(std::ops::Bound::Excluded(&(in_code as usize)));
        
        if let Some((end, v)) = comments.peek_next().map(|x| x.1) {
            if in_code as usize > *end as usize {
                return None;
            }

            if let Some(comment) = v.get(&(in_code as usize)) {
                Some(comment.clone())
            } else {
                None
            }
        } else {
            None
        }*/
        None
    }

    pub fn unregister_code_range(&self, _start: *const u8, _end: *const u8) {
        /*let start = start as usize;
        let end = end as usize;

        let mut comments = self.lock.lock();

        let (found_end, _) = match comments.remove(&start) {
            Some(v) => v,
            None => return,
        };

        assert_eq!(found_end, end);*/
    }

    pub fn register_code_range(&self, _start: *const u8, _end: *const u8, _new_comments: CommentMap) {
        /*let start = start as usize;
        let end = end as usize;

        let mut comments = self.lock.lock();

        assert!(comments.insert(start, (end, new_comments)).is_none());*/
    }
}