use super::frame::Source;

pub struct Trace {
    pub cat: String,
    pub ph: String, // B(eginning) E(nd) or I (info)
    // pid: 1
    pub ts: std::time::Duration,
    pub name: (Source, Option<usize>),
    pub tid: usize,   // thread ID I think?
    pub file: String, // [file]
}
