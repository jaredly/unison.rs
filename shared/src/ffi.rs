use super::types::*;
use std::sync::Arc;

pub trait FFI {
    // NOTE: This function is responsible for doing any type validation on the returned value.
    // If it returns something with an incorrect type, undefined behavior will result.
    // TODO maybe the State should have knowledge of the types of all `abilities` that it
    // might come across?
    // If this returns `None`, that means that the request couldn't be handled synchronously,
    // e.g. we need to just bail straight out.
    fn handle_request_sync(
        &mut self,
        typ: &ABT<Type>,
        kind: &Reference,
        number: usize,
        args: &Vec<Arc<Value>>,
    ) -> Option<Value>;

    // This is used at the top level, once we've bailed.
    fn handle_request(&mut self, request: crate::state::FullRequest);

    fn handles(&self, kind: &Reference) -> bool;
}

impl dyn FFI {
    pub fn ensure_handles(&self, kinds: &[&Reference]) -> bool {
        for kind in kinds {
            if !self.handles(kind) {
                return false;
            }
        }
        return true;
    }
}
