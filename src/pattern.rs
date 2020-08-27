use super::types::*;

impl Pattern {
    pub fn match_(&self, term: &Value) -> Option<Vec<Value>> {
        match (self, term) {
            (Pattern::EffectPure(_), Value::RequestWithContinuation(_, _, _, _, _)) => None,
            (Pattern::EffectPure(pattern), Value::RequestPure(inner)) => pattern.match_(inner),
            (
                Pattern::EffectBind(reference, number, args, kont),
                Value::RequestWithContinuation(tref, tnum, targs, tidx, tkont),
            ) if reference == tref && number == tnum && args.len() == targs.len() => {
                let mut all = vec![];
                for i in 0..args.len() {
                    match args[i].match_(&targs[i]) {
                        None => return None,
                        Some(inner) => {
                            all.extend(inner);
                        }
                    }
                }
                match &**kont {
                    Pattern::Unbound => (),
                    Pattern::Var => all.push(Value::Continuation(*tidx, tkont.clone())),
                    _ => unreachable!("Can't match on a continuation"),
                }
                Some(all)
            }
            (Pattern::Unbound, _) => Some(vec![]),
            (Pattern::Var, t) => Some(vec![t.clone()]),
            (Pattern::Boolean(a), Value::Boolean(b)) if a == b => Some(vec![]),
            (Pattern::Int(a), Value::Int(b)) if a == b => Some(vec![]),
            (Pattern::Nat(a), Value::Nat(b)) if a == b => Some(vec![]),
            (Pattern::Float(a), Value::Float(b)) if a == b => Some(vec![]),
            (Pattern::Text(a), Value::Text(b)) if a == b => Some(vec![]),
            (Pattern::Char(a), Value::Char(b)) if a == b => Some(vec![]),
            (Pattern::As(a), term) => match a.match_(term) {
                None => None,
                Some(mut terms) => {
                    terms.insert(0, term.clone());
                    Some(terms)
                }
            },
            (Pattern::SequenceLiteral(patterns), Value::Sequence(items))
                if patterns.len() == items.len() =>
            {
                let mut all = vec![];
                for i in 0..patterns.len() {
                    match patterns[i].match_(&items[i]) {
                        None => return None,
                        Some(v) => {
                            all.extend(v);
                        }
                    }
                }
                return Some(all);
            }
            (Pattern::SequenceOp(one, op, two), Value::Sequence(contents)) => match op {
                SeqOp::Cons => {
                    if contents.len() > 0 {
                        match one.match_(&contents[0]) {
                            None => None,
                            Some(mut ones) => {
                                match two.match_(&Value::Sequence(contents[1..].to_vec())) {
                                    None => None,
                                    Some(twos) => {
                                        ones.extend(twos);
                                        Some(ones)
                                    }
                                }
                            }
                        }
                    } else {
                        None
                    }
                }
                SeqOp::Snoc => {
                    if contents.len() > 0 {
                        match one.match_(&Value::Sequence(contents[..contents.len() - 1].to_vec()))
                        {
                            None => None,
                            Some(mut ones) => match two.match_(&contents[contents.len() - 1]) {
                                None => None,
                                Some(twos) => {
                                    ones.extend(twos);
                                    Some(ones)
                                }
                            },
                        }
                    } else {
                        None
                    }
                }
                SeqOp::Concat => match (&**one, &**two) {
                    (Pattern::SequenceLiteral(patterns), two) => {
                        if contents.len() >= patterns.len() {
                            match one.match_(&Value::Sequence(contents[0..patterns.len()].to_vec()))
                            {
                                None => None,
                                Some(mut ones) => {
                                    match two.match_(&Value::Sequence(
                                        contents[patterns.len()..].to_vec(),
                                    )) {
                                        None => None,
                                        Some(twos) => {
                                            ones.extend(twos);
                                            Some(ones)
                                        }
                                    }
                                }
                            }
                        } else {
                            None
                        }
                    }
                    (_, Pattern::SequenceLiteral(patterns)) => {
                        if contents.len() >= patterns.len() {
                            let split = contents.len() - patterns.len();
                            match one.match_(&Value::Sequence(contents[0..split].to_vec())) {
                                None => None,
                                Some(mut ones) => {
                                    match two.match_(&Value::Sequence(contents[split..].to_vec())) {
                                        None => None,
                                        Some(twos) => {
                                            ones.extend(twos);
                                            Some(ones)
                                        }
                                    }
                                }
                            }
                        } else {
                            None
                        }
                    }
                    _ => unreachable!(),
                },
            },
            (Pattern::Constructor(reference, number, children), inner) => {
                let mut all = vec![];
                if children.len() > 0 {
                    match inner {
                        Value::PartialConstructor(r, n, pchildren)
                            if r == reference && n == number =>
                        {
                            if pchildren.len() != children.len() {
                                return None;
                            }
                            for i in 0..children.len() {
                                match children[i].match_(&pchildren[i]) {
                                    None => return None,
                                    Some(v) => {
                                        all.extend(v);
                                    }
                                }
                            }
                            return Some(all);
                        }
                        _ => return None,
                    }
                }

                match inner {
                    Value::Constructor(r, n) if r == reference && n == n => Some(all),
                    _ => None,
                }
                //
            }
            _ => None,
        }
    }
}
