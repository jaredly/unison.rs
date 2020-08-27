use super::types::*;

impl Pattern {
    pub fn matches(&self, term: &Value, gc: &GC) -> bool {
        match (self, term) {
            (Pattern::EffectPure(_), Value::RequestWithContinuation(_, _, _, _, _)) => false,
            (Pattern::EffectPure(pattern), Value::RequestPure(inner)) => {
                pattern.matches(gc.get(*inner), gc)
            }
            (
                Pattern::EffectBind(reference, number, args, _),
                Value::RequestWithContinuation(tref, tnum, targs, _, _),
            ) if reference == tref && number == tnum && args.len() == targs.len() => {
                for i in 0..args.len() {
                    if !args[i].matches(gc.get(targs[i]), gc) {
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
            (Pattern::As(a), term) => a.matches(term, gc),
            (Pattern::SequenceLiteral(patterns), Value::Sequence(items))
                if patterns.len() == items.len() =>
            {
                for i in 0..patterns.len() {
                    if !patterns[i].matches(gc.get(items[i]), gc) {
                        return false;
                    }
                }
                true
            }
            (Pattern::SequenceOp(one, op, two), Value::Sequence(contents)) => match op {
                SeqOp::Cons => {
                    if contents.len() > 0 {
                        return one.matches(gc.get(contents[0]), gc)
                            && two.matches(&Value::Sequence(contents.skip(1)), gc);
                    } else {
                        false
                    }
                }
                SeqOp::Snoc => {
                    if contents.len() > 0 {
                        return one
                            .matches(&Value::Sequence(contents.take(contents.len() - 1)), gc)
                            && two.matches(gc.get(contents[contents.len() - 1]), gc);
                    } else {
                        false
                    }
                }
                SeqOp::Concat => match (&**one, &**two) {
                    (Pattern::SequenceLiteral(patterns), two) => {
                        if contents.len() >= patterns.len() {
                            return one
                                .matches(&Value::Sequence(contents.take(patterns.len())), gc)
                                && two
                                    .matches(&Value::Sequence(contents.skip(patterns.len())), gc);
                        } else {
                            false
                        }
                    }
                    (_, Pattern::SequenceLiteral(patterns)) => {
                        if contents.len() >= patterns.len() {
                            let split = contents.len() - patterns.len();
                            return one.matches(&Value::Sequence(contents.take(split)), gc)
                                && two.matches(&Value::Sequence(contents.skip(split)), gc);
                        } else {
                            false
                        }
                    }
                    _ => unreachable!(),
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
                                if !children[i].matches(gc.get(pchildren[i]), gc) {
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

    pub fn match_(&self, id: usize, gc: &mut GC) -> Option<Vec<usize>> {
        match (self.clone(), gc.get(id).clone()) {
            (Pattern::EffectPure(_), Value::RequestWithContinuation(_, _, _, _, _)) => None,
            (Pattern::EffectPure(pattern), Value::RequestPure(inner)) => pattern.match_(inner, gc),
            (
                Pattern::EffectBind(reference, number, args, kont),
                Value::RequestWithContinuation(tref, tnum, targs, tidx, tkont),
            ) if reference == tref && number == tnum && args.len() == targs.len() => {
                let mut all = vec![];
                for i in 0..args.len() {
                    match args[i].match_(targs[i], gc) {
                        None => return None,
                        Some(inner) => {
                            all.extend(inner);
                        }
                    }
                }
                match *kont {
                    Pattern::Unbound => (),
                    Pattern::Var => all.push(gc.put(Value::Continuation(tidx, tkont))),
                    _ => unreachable!("Can't match on a continuation"),
                }
                Some(all)
            }
            (Pattern::Unbound, _) => Some(vec![]),
            (Pattern::Var, t) => Some(vec![id]),
            (Pattern::Boolean(a), Value::Boolean(b)) if a == b => Some(vec![]),
            (Pattern::Int(a), Value::Int(b)) if a == b => Some(vec![]),
            (Pattern::Nat(a), Value::Nat(b)) if a == b => Some(vec![]),
            (Pattern::Float(a), Value::Float(b)) if a == b => Some(vec![]),
            (Pattern::Text(a), Value::Text(b)) if a == b => Some(vec![]),
            (Pattern::Char(a), Value::Char(b)) if a == b => Some(vec![]),
            (Pattern::As(a), _term) => match a.match_(id, gc) {
                None => None,
                Some(mut terms) => {
                    terms.insert(0, id);
                    Some(terms)
                }
            },
            (Pattern::SequenceLiteral(patterns), Value::Sequence(items))
                if patterns.len() == items.len() =>
            {
                let mut all = vec![];
                for i in 0..patterns.len() {
                    match patterns[i].match_(items[i], gc) {
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
                        match one.match_(contents[0], gc) {
                            None => None,
                            Some(mut ones) => {
                                // STOPSHIP maybe we don't need to put this ...
                                match two.match_(gc.put(Value::Sequence(contents.skip(1))), gc) {
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
                        match one.match_(
                            gc.put(Value::Sequence(contents.take(contents.len() - 1))),
                            gc,
                        ) {
                            None => None,
                            Some(mut ones) => match two.match_(contents[contents.len() - 1], gc) {
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
                            match one
                                .match_(gc.put(Value::Sequence(contents.take(patterns.len()))), gc)
                            {
                                None => None,
                                Some(mut ones) => {
                                    match two.match_(
                                        gc.put(Value::Sequence(contents.skip(patterns.len()))),
                                        gc,
                                    ) {
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
                            match one.match_(gc.put(Value::Sequence(contents.take(split))), gc) {
                                None => None,
                                Some(mut ones) => {
                                    match two
                                        .match_(gc.put(Value::Sequence(contents.skip(split))), gc)
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
                                match children[i].match_(pchildren[i], gc) {
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
