use pretty::{Doc, RcDoc};
use shared::types::*;

use Term::*;

pub trait ToDoc {
    // TODO add names to these too
    fn to_doc(&self) -> pretty::RcDoc<()>;
}

pub trait ToPretty {
    fn to_pretty(&self, width: usize) -> String;
}

const UNIT_HASH: &'static str = "568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8";
const TUPLE_HASH: &'static str = "onbcm0qctbnuctpm57tkc5p16b8gfke8thjf19p4r4laokji0b606rd0frnhj103qb90lve3fohkoc1eda70491hot656s1m6kk3cn0";

fn unwrap_tuple(value: &Value) -> Vec<&Value> {
    use Value::*;
    match value {
        PartialConstructor(Reference::DerivedId(Id(hash, _, _)), 0, args) => {
            if hash.0 == TUPLE_HASH {
                let mut res = unwrap_tuple(&args[1]);
                res.insert(0, &args[0]);
                return res;
            } else if hash.0 == UNIT_HASH {
                return vec![];
            }
        }
        Constructor(Reference::DerivedId(Id(hash, _, _)), 0) => {
            if hash.0 == UNIT_HASH {
                return vec![];
            }
        }
        _ => (),
    };
    return vec![value];
}

#[derive(Default)]
pub struct Names {
    pub terms: std::collections::HashMap<String, Vec<String>>,
    pub types: std::collections::HashMap<String, Vec<String>>,
    pub constructors: std::collections::HashMap<(String, usize), Vec<String>>,
}

fn value_to_doc(value: &Value, names: &Names) -> pretty::RcDoc<'static, ()> {
    use Value::*;
    match value {
        Sequence(items) => RcDoc::text("[")
            .append(
                RcDoc::line()
                    .append(RcDoc::intersperse(
                        items.iter().map(|m| value_to_doc(m, names)),
                        RcDoc::text(",").append(Doc::line()),
                    ))
                    .nest(5)
                    .append(Doc::line())
                    .group(),
            )
            .append(RcDoc::text("]")),
        PartialConstructor(Reference::DerivedId(Id(hash, _, _)), num, args) => {
            if hash.0 == TUPLE_HASH {
                let mut items = unwrap_tuple(&args[1]);
                items.insert(0, &args[0]);
                RcDoc::text("(")
                    .append(
                        RcDoc::line()
                            .append(RcDoc::intersperse(
                                items.iter().map(|m| value_to_doc(m, names)),
                                RcDoc::text(",").append(Doc::line()),
                            ))
                            .nest(5)
                            .append(Doc::line())
                            .group(),
                    )
                    .append(RcDoc::text(")"))
            } else {
                let name = names
                    .constructors
                    .get(&(hash.to_string(), *num))
                    .map(|v| v.join("."))
                    .unwrap_or(hash.0.clone());
                RcDoc::text(name)
                    .append(RcDoc::text("("))
                    .append(
                        RcDoc::intersperse(
                            args.iter().map(|m| value_to_doc(m, names)),
                            RcDoc::text(",").append(Doc::line()),
                        )
                        .nest(5)
                        .group(),
                    )
                    .append(RcDoc::text(")"))
            }
        }
        Constructor(Reference::DerivedId(Id(hash, _, _)), num) => {
            if hash.0 == UNIT_HASH {
                return RcDoc::text("()");
            }
            let name = names
                .constructors
                .get(&(hash.to_string(), *num))
                .map(|v| v.join("."))
                .unwrap_or(hash.0.clone());
            RcDoc::text(name)
        }
        Boolean(i) => RcDoc::as_string(i),
        Int(i) => RcDoc::as_string(i),
        Nat(i) => RcDoc::as_string(i),
        Float(i) => RcDoc::as_string(i),
        Text(i) => RcDoc::as_string(i),
        Char(i) => RcDoc::as_string(i),
        Bytes(_) => RcDoc::as_string("<Bytes>"),
        Ref(_) => RcDoc::as_string("<Ref>"),
        x => RcDoc::as_string(format!("{:?}", x)),
    }
}

pub fn value_to_pretty(value: &Value, names: &Names, width: usize) -> String {
    let mut w = Vec::new();
    value_to_doc(value, names).render(width, &mut w).unwrap();
    String::from_utf8(w).unwrap()
}

impl ToPretty for ABT<Term> {
    fn to_pretty(&self, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc().render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl ToDoc for ABT<Term> {
    fn to_doc(&self) -> pretty::RcDoc<()> {
        use ABT::*;
        match self {
            Tm(t) => t.to_doc(),
            Cycle(t) => RcDoc::text("<cycle>").append(t.to_doc()),
            Var(sym, u) => RcDoc::text(format!("🍿({}/{} #{})", sym.text, sym.unique, u)),
            Abs(sym, u, c) => {
                RcDoc::text(format!("👋({}/{} ##{}) ", sym.text, sym.unique, u)).append(c.to_doc())
            }
        }
    }
}

impl ToDoc for Pattern {
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
            Constructor(r, n, _children) => {
                RcDoc::text(format!("{:?} # ", r)).append(RcDoc::as_string(n))
            }
            As(i) => i.to_doc(),
            EffectPure(p) => RcDoc::text("{ ")
                .append(p.to_doc())
                .append(RcDoc::text(" }")),
            EffectBind(_r, _n, _v, _k) => RcDoc::text("Effect"),
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
            SequenceOp(_a, _op, _b) => RcDoc::text("seq op"),
        }
    }
}

impl ToDoc for MatchCase {
    fn to_doc(&self) -> RcDoc<()> {
        let m = self.0.to_doc();
        let m = match &self.1 {
            None => m,
            Some(o) => m.append(RcDoc::text(" | ")).append(o.to_doc()),
        };
        m.append(RcDoc::text(" -> ")).append(self.2.to_doc())
    }
}

impl ToDoc for Term {
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
            Lam(a, free) => RcDoc::text(format!("->(capture {:?})", free)).append(a.to_doc()),
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
