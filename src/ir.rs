use super::types::*;

// So I think we have a scope and a stack?
#[derive(Debug)]
pub enum IR {
    // Lookup this reference, and push it onto the stack
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
    PopUpOne,
    // for cleaning up after blocks
    MarkBindings,
    PopBindings,
    // Match the given pattern.
    // If the "has_where" flag is true, bound variables
    // will be pushed onto the stack twice
    PatternMatch(Pattern, bool),
    PatternMatchFail,
    MarkStack,
    ClearStackMark,
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
                cmds.push(IR::MarkBindings);
                cmds.push(IR::PopAndName(name.clone()));
                body.to_ir(cmds);
                cmds.push(IR::PopBindings);
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
                yes.to_ir(cmds);
                cmds.push(IR::JumpTo(done_tok));
                cmds.push(IR::Mark(no_tok));
                no.to_ir(cmds);
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
                body.to_ir(cmds);
            }
            Term::Match(item, arms) => {
                let done_tok = cmds.mark();
                item.to_ir(cmds);
                // erm how do I say "here's the next one"
                let mut next_tok = cmds.mark();
                for MatchCase(pattern, cond, body) in arms {
                    match cond {
                        None => {
                            cmds.push(IR::PatternMatch(pattern.clone(), false));
                            cmds.push(IR::If(next_tok));
                        }
                        Some(cond) => {
                            // TODO should I have an ID with these,
                            // to catch me of I pop the stack too much?
                            cmds.push(IR::MarkStack);
                            cmds.push(IR::PatternMatch(pattern.clone(), true));
                            cmds.push(IR::IfAndPopStack(next_tok));
                            cond.to_ir(cmds);
                            cmds.push(IR::IfAndPopStack(next_tok));
                            cmds.push(IR::ClearStackMark);
                        }
                    }

                    body.to_ir(cmds);
                    cmds.push(IR::JumpTo(done_tok));

                    cmds.push(IR::Mark(next_tok));
                    next_tok = cmds.mark();
                }
                cmds.push(IR::PatternMatchFail);
                cmds.push(IR::Mark(done_tok));
                cmds.push(IR::PopUpOne);
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
