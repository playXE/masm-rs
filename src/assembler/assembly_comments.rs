#![allow(dead_code)]
use once_cell::sync::Lazy;
use parking_lot::Mutex;
use std::collections::HashMap;

pub type CommentMap = HashMap<usize, String>;

pub struct AssemblyCommentsRegistry {
    lock: Mutex<Vec<(usize, usize, CommentMap)>>,
}

impl AssemblyCommentsRegistry {
    pub fn new() -> Self {
        AssemblyCommentsRegistry {
            lock: Mutex::new(Vec::new()),
        }
    }

    pub fn singleton() -> &'static Self {
        static INSTANCE: Lazy<AssemblyCommentsRegistry> =
            Lazy::new(|| AssemblyCommentsRegistry::new());
        &INSTANCE
    }

    pub fn comment<'a>(&self, in_code: *const u8) -> Option<String> {
        let comments = self.lock.lock();

        // search lower bound
        /*let (_, (end, comment_map)) = match comments.range(..=(in_code as usize)).next_back() {
            Some(v) => v,
            None => return None,
        };*/

        /*

        if in_code as usize > *end {
            return None;
        }

        if let Some(comment) = comment_map.get(&(in_code as usize)) {
            return Some(comment.clone());
        }
        None*/

        comments
            .iter()
            .find(|(start, end, _)| in_code as usize >= *start && in_code as usize <= *end)
            .and_then(|(_, _, comment_map)| comment_map.get(&(in_code as usize)).map(|s| s.clone()))
    }

    pub fn unregister_code_range(&self, start: *const u8, end: *const u8) {
        let start = start as usize;
        let end = end as usize;

        let mut comments = self.lock.lock();

        /*let (found_end, _) = match comments.remove(&start) {
            Some(v) => v,
            None => return,
        };*/
        let mut found_end = 0;

        comments.retain(|(k, v, _)| {
            if *k == start && *v == end {
                found_end = *v;
                return false;
            }
            true
        });

        assert_eq!(found_end, end);
    }

    pub fn register_code_range(&self, start: *const u8, end: *const u8, new_comments: CommentMap) {
        let start = start as usize;
        let end = end as usize;

        let mut comments = self.lock.lock();
        for (pos, comment) in new_comments.iter() {
            println!("{:x}: {}", pos, comment);
        }
        comments.push((start, end, new_comments));
    }
}
