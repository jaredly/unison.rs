use super::types::*;

// So I think we have a scope and a stack?
#[derive(Debug)]
pub enum IR {
    Ref(Reference),
    // Push this value onto the stack
    Value(Term),
    // lookup the symbol, and push it onto the stack
    PushSym(Symbol),
    // pop the top value off the stack and give it a name
    PopAndName(Symbol),
    // pop the top two values off the stack, call the first with the second
    Call,
    // Pop the top N values from the stack, assemble into a seq
    Seq(usize),
    JumpTo(usize),
    Mark(usize),
    // pop the last value off the stack;
    // if it's true, advance.
    /// otherwise, jump to the given mark
    If(usize),
    // If2(usize, usize),
    // hmm I might want to short-circut?
    // And,
    // Or,
    // Dup, // duplicate the top item - might not need it
    // Pop,
    // for cleaning up after blocks
    MarkBindings,
    PopBindings,
    MarkStack,
    // if false, then pop up to the stack mark.
    // if true, the following code will bind those vbls, its fine.
    IfAndPopStack(usize),
}

impl ABT<Term> {
    pub fn to_ir(&self, cmds: &mut IREnv) {
        match self {
            ABT::Var(symbol) => cmds.push(IR::PushSym(symbol.clone())),
            ABT::Tm(term) => term.to_ir(cmds),
            ABT::Cycle(_) => unimplemented!(),
            ABT::Abs(name, body) => {
                cmds.push(IR::PopAndName(name.clone()));
                body.to_ir(cmds);
                // unreachable!("Abs should have been handled? I think?")
            }
        }
    }
}

pub struct IREnv {
    pub cmds: Vec<IR>,
    pub counter: usize,
}

impl IREnv {
    pub fn new() -> Self {
        IREnv {
            cmds: vec![],
            counter: 0,
        }
    }

    fn push(&mut self, ir: IR) {
        self.cmds.push(ir)
    }

    fn mark(&mut self) -> usize {
        self.counter += 1;
        self.counter
    }
}

impl Term {
    pub fn to_ir(&self, cmds: &mut IREnv) {
        match self {
            Term::Ref(reference) => cmds.push(IR::Ref(reference.clone())),
            Term::App(one, two) => {
                one.to_ir(cmds);
                two.to_ir(cmds);
                cmds.push(IR::Call)
            }
            Term::Ann(term, _) => term.to_ir(cmds),
            Term::Sequence(terms) => {
                let ln = terms.len();
                for inner in terms {
                    inner.to_ir(cmds);
                }
                cmds.push(IR::Seq(ln))
            }
            Term::If(cond, yes, no) => {
                let no_tok = cmds.mark();
                let done_tok = cmds.mark();
                cond.to_ir(cmds);
                cmds.push(IR::If(no_tok));
                // cmds.push(IR::MarkBindings);
                yes.to_ir(cmds);
                // cmds.push(IR::PopBindings);
                cmds.push(IR::JumpTo(done_tok));
                cmds.push(IR::Mark(no_tok));
                // cmds.push(IR::MarkBindings);
                no.to_ir(cmds);
                // cmds.push(IR::PopBindings);
                cmds.push(IR::Mark(done_tok));
            }
            Term::And(a, b) => {
                let fail_tok = cmds.mark();
                let done_tok = cmds.mark();
                a.to_ir(cmds);
                cmds.push(IR::If(fail_tok));
                b.to_ir(cmds);
                cmds.push(IR::If(fail_tok));
                cmds.push(IR::Value(Term::Boolean(true)));
                cmds.push(IR::JumpTo(done_tok));
                cmds.push(IR::Mark(fail_tok));
                cmds.push(IR::Value(Term::Boolean(false)));
                cmds.push(IR::Mark(done_tok));
            }
            Term::Or(a, b) => {
                let good_tok = cmds.mark();
                let fail_tok = cmds.mark();
                let b_tok = cmds.mark();
                let done_tok = cmds.mark();
                a.to_ir(cmds);
                cmds.push(IR::If(b_tok));
                cmds.push(IR::JumpTo(good_tok));
                cmds.push(IR::Mark(b_tok));
                b.to_ir(cmds);
                cmds.push(IR::If(fail_tok));

                cmds.push(IR::Mark(good_tok));
                cmds.push(IR::Value(Term::Boolean(true)));
                cmds.push(IR::JumpTo(done_tok));

                cmds.push(IR::Mark(fail_tok));
                cmds.push(IR::Value(Term::Boolean(false)));

                cmds.push(IR::Mark(done_tok));
            }
            Term::Let(_, v, body) => {
                v.to_ir(cmds);
                cmds.push(IR::MarkBindings);
                body.to_ir(cmds);
                cmds.push(IR::PopBindings);
            }
            Term::Match(item, arms) => {
                let done_tok = cmds.mark();
                item.to_ir(cmds);
                // erm how do I say "here's the next one"
                let mut next_tok = cmds.mark();
                for MatchCase(pattern, cond, body) in arms {
                    cmds.push(IR::MarkBindings);
                    // this'll do the bindings I think?
                    // or actually maybe not
                    // the cmds body will do that.
                    // which means .. the body needs to
                    // not? yeah maybe I can prevent that.
                    pattern.to_ir(cmds);
                    cmds.push(IR::If(next_tok));
                    match cond {
                        None => (),
                        Some(cond) => {
                            cond.to_ir(cmds);
                            cmds.push(IR::If(next_tok))
                        }
                    }

                    // TODO need to prevent the body
                    // from popping off more, we have
                    // already done the bindings folks
                    body.to_ir(cmds);
                    cmds.push(IR::PopBindings);
                    cmds.push(IR::JumpTo(done_tok));

                    cmds.push(IR::Mark(next_tok));
                    next_tok = cmds.mark();
                }
                cmds.push(IR::Mark(done_tok));
            }

            _ => cmds.push(IR::Value(self.clone())),
        }
    }
}

impl Pattern {
    pub fn to_ir(&self, cmds: &mut IREnv) {
        // Ok, so when we're inspecting a pattern,
        // the value under consideration is on the stack,
        // right?
        // BUT we need to not pop it off.
        // except ... we do for sub-things
        match self {
            Unbound => cmds.push(IR::Value(Term::Boolean(true))),
            Var => cmds.push(IR::Value(Term::Boolean(true))),
            // ugh I need to think about this more.
            _ => unreachable!(),
        }
    }
}
