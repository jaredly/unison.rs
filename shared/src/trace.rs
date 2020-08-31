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

pub struct Traces {
    #[cfg(not(target_arch = "wasm32"))]
    pub start: std::time::Instant,
    pub traces: Vec<Trace>,
}

impl Traces {
    pub fn new() -> Self {
        Traces {
            #[cfg(not(target_arch = "wasm32"))]
            start: std::time::Instant::now(),
            traces: vec![],
        }
    }

    pub fn push(&mut self, frame: &crate::frame::Frame, which: &str) {
        #[cfg(not(target_arch = "wasm32"))]
        self.traces
            .push(frame.as_trace(which, self.start.elapsed()))
    }

    pub fn to_file<T: std::io::Write>(&self, file: &mut T) -> std::io::Result<()> {
        file.write_all(b"[")?;
        let mut lines = vec![];
        for trace in &self.traces {
            lines.push(format!(
            r#"
            {{"cat": {:?}, "pid": 1, "ph": {:?}, "ts": {}, "name": {:?}, "tid": {}, "args": {{"[file]": {:?} }} }}
            "#,
            trace.cat,
            trace.ph,
            trace.ts.as_micros(),
            format!("{:?}", trace.name),
            trace.tid,
            trace.file
        ));
        }
        file.write_all(lines.join(",\n").as_bytes())?;
        file.write_all(b"]")?;
        Ok(())
    }
}
