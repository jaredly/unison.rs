use super::types::*;
use pretty::{Doc, RcDoc};

use Term::*;

impl ABT<Term> {
    pub fn to_pretty(&self, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc().render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }

    fn to_doc(&self) -> pretty::RcDoc<()> {
        use ABT::*;
        match self {
            Tm(t) => t.to_doc(),
            Cycle(t) => RcDoc::text("<cycle>").append(t.to_doc()),
            Var(sym) => RcDoc::text(format!("🍿({})", sym.text)),
            Abs(sym, c) => RcDoc::text(format!("👋({}) ", sym.text)).append(c.to_doc()),
        }
    }
}

impl Pattern {
    fn to_doc(&self) -> RcDoc<()> {
        use Pattern::*;
        match self {
            Unbound => RcDoc::text("_"),
            Var => RcDoc::text("<var>"),
            Boolean(i) => RcDoc::as_string(i),
            Int(i) => RcDoc::as_string(i),
            Nat(i) => RcDoc::as_string(i),
            Float(i) => RcDoc::as_string(i),
            Text(i) => RcDoc::as_string(i),
            Char(i) => RcDoc::as_string(i),
            Constructor(r, n, children) => {
                RcDoc::text(format!("{:?} # ", r)).append(RcDoc::as_string(n))
            }
            As(i) => i.to_doc(),
            EffectPure(p) => RcDoc::text("{ ")
                .append(p.to_doc())
                .append(RcDoc::text(" }")),
            EffectBind(r, n, v, k) => RcDoc::text("Effect"),
            SequenceLiteral(l) => RcDoc::text("[")
                .append(
                    RcDoc::intersperse(
                        l.iter().map(|m| m.to_doc()),
                        RcDoc::text(",").append(Doc::line()),
                    )
                    .nest(1)
                    .group(),
                )
                .append(RcDoc::text("]")),
            SequenceOp(a, op, b) => RcDoc::text("seq op"),
        }
    }
}

impl MatchCase {
    fn to_doc(&self) -> RcDoc<()> {
        let m = self.0.to_doc();
        let m = match &self.1 {
            None => m,
            Some(o) => m.append(RcDoc::text(" | ")).append(o.to_doc()),
        };
        m.append(RcDoc::text(" -> ")).append(self.2.to_doc())
    }
}

impl Term {
    pub fn to_pretty(&self, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc().render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }

    fn to_doc(&self) -> pretty::RcDoc<()> {
        match self {
            Int(i) => RcDoc::as_string(i),
            Nat(i) => RcDoc::as_string(i),
            Float(i) => RcDoc::as_string(i),
            Boolean(i) => RcDoc::as_string(i),
            Text(i) => RcDoc::as_string(i),
            Bytes(_) => RcDoc::text("<bytes>"),
            Char(i) => RcDoc::as_string(i),
            Blank => RcDoc::text("<blank>"),
            // PartialNativeApp(name, _) => f.write_fmt(format_args!("partial({})", name)),
            Ref(i) => RcDoc::text(format!("{:?}", i)),
            Constructor(i, n) => RcDoc::text(format!("[constructor]{:?}#{}", i, n)),
            Request(i, n) => RcDoc::text(format!("[request]{:?}#{}", i, n)),
            Handle(i, n) => RcDoc::text("handle")
                .append(RcDoc::space())
                .append((*i).to_doc())
                .append(RcDoc::space())
                .append(RcDoc::text("with"))
                .append(RcDoc::space())
                .append((*n).to_doc())
                .nest(1)
                .group(),
            App(i, n) => i
                .to_doc()
                .append(RcDoc::text("("))
                .append(n.to_doc().nest(1))
                .append(RcDoc::text(")"))
                .group(),
            // RcDoc::text(format!("{:?} <app> {:?}", i, n)),
            Ann(i, _) => i.to_doc(),
            // RcDoc::text(format!("t- {:?} :: {:?} -t", i, n)),
            Sequence(i) => RcDoc::text("[")
                .append(
                    RcDoc::intersperse(
                        i.iter().map(|m| (*m).to_doc()),
                        RcDoc::text(",").append(Doc::line()),
                    )
                    .nest(1)
                    .group(),
                )
                .append("]"),
            // RcDoc::text(format!("{:?}", i)),
            If(i, a, b) => RcDoc::text("if")
                .append(RcDoc::space())
                .append(i.to_doc())
                .append(RcDoc::space())
                .append(RcDoc::text("then"))
                .append(RcDoc::space())
                .append(a.to_doc().nest(1))
                .append(RcDoc::space())
                .append(RcDoc::text("else"))
                .append(RcDoc::space())
                .append(b.to_doc().nest(1)),
            // RcDoc::text(format!("if {:?} then\n{:?}\nelse\n{:?}", i, a, b)),
            And(a, b) => a.to_doc().append(RcDoc::text(" && ")).append(b.to_doc()),
            Or(a, b) => a.to_doc().append(RcDoc::text(" || ")).append(b.to_doc()),
            Lam(a) => RcDoc::text("->").append(a.to_doc()),
            LetRec(_, a, b) => RcDoc::text("let(rec)")
                .append(RcDoc::space())
                .append(RcDoc::intersperse(
                    a.iter().map(|m| m.to_doc()),
                    Doc::line(),
                ))
                .append(RcDoc::text(" in "))
                .append(RcDoc::hardline())
                .append(b.to_doc()),
            Let(_, a, b) => RcDoc::text("let")
                .append(RcDoc::space())
                .append(a.to_doc())
                .append(RcDoc::text(" in "))
                .append(RcDoc::hardline())
                .append(b.to_doc()),
            Match(a, b) => RcDoc::text("match")
                .append(RcDoc::space())
                .append(a.to_doc())
                .append(RcDoc::space())
                .append(RcDoc::text("with"))
                .append(RcDoc::hardline())
                .append(RcDoc::intersperse(b.iter().map(|b| b.to_doc()), Doc::hardline()).nest(1)),
            TermLink(a) => RcDoc::text(format!("termLink {:?}", a)),
            TypeLink(a) => RcDoc::text(format!("typeLink {:?}", a)),
        }
    }
}

impl std::fmt::Debug for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            // RequestWithArgs(i, n, _, _) => f.write_fmt(format_args!("req<{:?} - {}>", i, n)),
            // RequestWithContinuation(i, n, _, _, _) => {
            //     f.write_fmt(format_args!("req+cont<{:?} - {}>", i, n))
            // }
            // Continuation(idx, frames) => f.write_fmt(format_args!(
            //     "cont<idx: {} - frames: {}>",
            //     idx,
            //     frames.len()
            // )),
            // RequestPure(i) => f.write_fmt(format_args!("pure<{:?}>", i)),
            Int(i) => f.write_fmt(format_args!("{}", i)),
            Nat(i) => f.write_fmt(format_args!("{}", i)),
            Float(i) => f.write_fmt(format_args!("{}", i)),
            Boolean(i) => f.write_fmt(format_args!("{}", i)),
            Text(i) => f.write_fmt(format_args!("{:?}", i)),
            Bytes(i) => f.write_fmt(format_args!("{:?}", i)),
            Char(i) => f.write_fmt(format_args!("{:?}", i)),
            Blank => f.write_str("<blank>"),
            Ref(i) => f.write_fmt(format_args!("{:?}", i)),
            Constructor(i, n) => f.write_fmt(format_args!("[constructor]{:?}#{}", i, n)),
            Request(i, n) => f.write_fmt(format_args!("[request]{:?}#{}", i, n)),
            Handle(i, n) => f.write_fmt(format_args!("handle {:?} with {:?}", i, n)),
            App(i, n) => f.write_fmt(format_args!("{:?} <app> {:?}", i, n)),
            Ann(i, n) => f.write_fmt(format_args!("t- {:?} :: {:?} -t", i, n)),
            Sequence(i) => f.write_fmt(format_args!("{:?}", i)),
            If(i, a, b) => f.write_fmt(format_args!("if {:?} then\n{:?}\nelse\n{:?}", i, a, b)),
            And(a, b) => f.write_fmt(format_args!("{:?} && {:?}", a, b)),
            Or(a, b) => f.write_fmt(format_args!("{:?} || {:?}", a, b)),
            Lam(a) => f.write_fmt(format_args!("-> {:?}", a)),
            LetRec(_, a, b) => f.write_fmt(format_args!("let(rec)\n{:?}\nin {:?}", a, b)),
            Let(_, a, b) => f.write_fmt(format_args!("let {:?} in {:?}", a, b)),
            Match(a, b) => f.write_fmt(format_args!("match {:?} with {:?}", a, b)),
            TermLink(a) => f.write_fmt(format_args!("termLink {:?}", a)),
            TypeLink(a) => f.write_fmt(format_args!("typeLink {:?}", a)),
        }
    }
}
