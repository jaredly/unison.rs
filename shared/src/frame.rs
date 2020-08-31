use super::trace::Trace;
use super::types::*;
use std::rc::Rc;

#[derive(Debug, Clone, std::cmp::PartialEq, std::cmp::PartialOrd, Hash)]
pub enum Source {
    Value(Hash),
    Fn(usize, Hash),
}

#[derive(Debug, Clone)]
pub struct Frame {
    pub source: Source,
    pub stack: Vec<Rc<Value>>,
    pub marks: Vec<usize>,
    pub handler: Option<usize>,
    pub return_index: usize,
    pub bindings: Vec<(Symbol, usize, Rc<Value>)>, // the number of usages to expect
}

impl std::fmt::Display for Frame {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        fmt.write_fmt(format_args!(
            "Frame<{:?}> : handler {:?} : return_index {}",
            self.source, self.handler, self.return_index
        ))
    }
}

impl std::cmp::PartialEq for Frame {
    fn eq(&self, other: &Self) -> bool {
        self.source == other.source
    }
}

impl std::cmp::PartialOrd for Frame {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.source < other.source {
            Some(std::cmp::Ordering::Less)
        } else if self.source > other.source {
            Some(std::cmp::Ordering::Greater)
        } else {
            None
        }
    }
}

impl Frame {
    pub fn new(source: Source, return_index: usize) -> Self {
        // let span = span!(Level::TRACE, format!("{:?}", source));
        Frame {
            source,
            stack: vec![],
            marks: vec![],
            handler: None,
            return_index,
            bindings: vec![],
        }
    }
    pub fn as_trace(&self, ph: &str, ts: std::time::Duration) -> Trace {
        Trace {
            cat: "frame".to_owned(),
            ph: ph.to_owned(), // B(eginning) E(nd) or I (info)
            ts,
            name: (self.source.clone(), None),
            tid: 1,                  // thread ID I think?
            file: "main".to_owned(), // [file] // TODO
        }
    }
}
