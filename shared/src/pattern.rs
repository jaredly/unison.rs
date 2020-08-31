use super::types::*;
use std::sync::Arc;

impl Pattern {
    pub fn matches(&self, term: &Value) -> bool {
        match (self, term) {
            (Pattern::EffectPure(_), Value::RequestWithContinuation(_, _, _, _, _, _)) => false,
            (Pattern::EffectPure(pattern), Value::RequestPure(inner)) => pattern.matches(inner),
            (
                Pattern::EffectBind(reference, number, args, _),
                Value::RequestWithContinuation(tref, tnum, targs, _, _, _),
            ) if reference == tref && number == tnum && args.len() == targs.len() => {
                for i in 0..args.len() {
                    if !args[i].matches(&targs[i]) {
                        return false;
                    }
                }
                true
            }
            (Pattern::Unbound, _) => true,
            (Pattern::Var, _) => true,
            (Pattern::Boolean(a), Value::Boolean(b)) if a == b => true,
            (Pattern::Int(a), Value::Int(b)) if a == b => true,
            (Pattern::Nat(a), Value::Nat(b)) if a == b => true,
            (Pattern::Float(a), Value::Float(b)) if a == b => true,
            (Pattern::Text(a), Value::Text(b)) if a == b => true,
            (Pattern::Char(a), Value::Char(b)) if a == b => true,
            (Pattern::As(a), term) => a.matches(term),
            (Pattern::SequenceLiteral(patterns), Value::Sequence(items))
                if patterns.len() == items.len() =>
            {
                for i in 0..patterns.len() {
                    if !patterns[i].matches(&items[i]) {
                        return false;
                    }
                }
                true
            }
            (Pattern::SequenceOp(one, op, two), Value::Sequence(contents)) => match op {
                SeqOp::Cons => {
                    if contents.len() > 0 {
                        return one.matches(&contents[0])
                            && two.matches(&Value::Sequence(contents.skip(1)));
                    } else {
                        false
                    }
                }
                SeqOp::Snoc => {
                    if contents.len() > 0 {
                        return one.matches(&Value::Sequence(contents.take(contents.len() - 1)))
                            && two.matches(&contents[contents.len() - 1]);
                    } else {
                        false
                    }
                }
                SeqOp::Concat => match (&**one, &**two) {
                    (Pattern::SequenceLiteral(patterns), two) => {
                        if contents.len() >= patterns.len() {
                            return one.matches(&Value::Sequence(contents.take(patterns.len())))
                                && two.matches(&Value::Sequence(contents.skip(patterns.len())));
                        } else {
                            false
                        }
                    }
                    (_, Pattern::SequenceLiteral(patterns)) => {
                        if contents.len() >= patterns.len() {
                            let split = contents.len() - patterns.len();
                            return one.matches(&Value::Sequence(contents.take(split)))
                                && two.matches(&Value::Sequence(contents.skip(split)));
                        } else {
                            false
                        }
                    }
                    _ => unreachable!("Concat pattern not a sequence literal on either side"),
                },
            },
            (Pattern::Constructor(reference, number, children), inner) => {
                if children.len() > 0 {
                    match inner {
                        Value::PartialConstructor(r, n, pchildren)
                            if r == reference && n == number =>
                        {
                            if pchildren.len() != children.len() {
                                return false;
                            }
                            for i in 0..children.len() {
                                if !children[i].matches(&pchildren[i]) {
                                    return false;
                                }
                            }
                            true
                        }
                        _ => false,
                    }
                } else {
                    match inner {
                        Value::Constructor(r, n) if r == reference && n == n => true,
                        _ => false,
                    }
                }
                //
            }
            _ => false,
        }
    }

    pub fn match_(&self, id: &Value) -> Option<Vec<Arc<Value>>> {
        match (self.clone(), (id).clone()) {
            (Pattern::EffectPure(_), Value::RequestWithContinuation(_, _, _, _, _, _)) => None,
            (Pattern::EffectPure(pattern), Value::RequestPure(inner)) => pattern.match_(&inner),
            (
                Pattern::EffectBind(reference, number, args, kont),
                Value::RequestWithContinuation(tref, tnum, targs, tidx, mut tkont, current_idx),
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
                tkont = tkont.drain(0..current_idx + 1).collect();
                // Clear out the handler
                tkont[current_idx].handler = None;
                match *kont {
                    Pattern::Unbound => (),
                    Pattern::Var => all.push(Arc::new(Value::Continuation(tidx, tkont))),
                    _ => unreachable!("Can't match on a continuation"),
                }
                Some(all)
            }
            (Pattern::Unbound, _) => Some(vec![]),
            (Pattern::Var, t) => Some(vec![Arc::new(t)]),
            (Pattern::Boolean(a), Value::Boolean(b)) if a == b => Some(vec![]),
            (Pattern::Int(a), Value::Int(b)) if a == b => Some(vec![]),
            (Pattern::Nat(a), Value::Nat(b)) if a == b => Some(vec![]),
            (Pattern::Float(a), Value::Float(b)) if a == b => Some(vec![]),
            (Pattern::Text(a), Value::Text(b)) if a == b => Some(vec![]),
            (Pattern::Char(a), Value::Char(b)) if a == b => Some(vec![]),
            (Pattern::As(a), _term) => match a.match_(id) {
                None => None,
                Some(mut terms) => {
                    terms.insert(0, Arc::new(id.clone()));
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
                                // STOPSHIP maybe we don't need to put this ...
                                match two.match_(&Value::Sequence(contents.skip(1))) {
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
                        match one.match_(&Value::Sequence(contents.take(contents.len() - 1))) {
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
                SeqOp::Concat => match (&*one, &*two) {
                    (Pattern::SequenceLiteral(patterns), two) => {
                        if contents.len() >= patterns.len() {
                            match one.match_(&Value::Sequence(contents.take(patterns.len()))) {
                                None => None,
                                Some(mut ones) => {
                                    match two
                                        .match_(&Value::Sequence(contents.skip(patterns.len())))
                                    {
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
                            match one.match_(&Value::Sequence(contents.take(split))) {
                                None => None,
                                Some(mut ones) => {
                                    match two.match_(&Value::Sequence(contents.skip(split))) {
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
                    _ => unreachable!("Concat pattern not a sequence literal on either side"),
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
                } else {
                    match inner {
                        Value::Constructor(r, nn) if r == reference && number == nn => Some(all),
                        _ => None,
                    }
                }
                //
            }
            _ => None,
        }
    }
}
