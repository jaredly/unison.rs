# unison.rs

An experimental runtime for unison code, written in rust, compiled to wasm for use in the browser.

## Usage:

- download the [release binary](https://github.com/jaredly/unison.rs/releases/tag/release-2)
- run `unison.rs serve`
- go to `http://localhost:8080`, view the different namespaces that you've got, click on terms to "watch" them (including the ability to provide arguments to functions, as long as they're "primitives")
- run `unison` somewhere
- grab the web example (if you want to) `pull https://github.com/jaredly/unison-wasm-example .example`
- browse to `example.app` and click on it
- see that it's interactive!
- `cd example` and `edit counter` - in the scratch file, change `"Hello unison"` to something else
- save the scratch file, run `update` in unison, and see that the watcher auto-updates in your browser!


```
pub enum Pattern {
    Unbound,
    Var,
    Boolean(bool),
    Int(i64),
    Nat(u64),
    Float(f64),
    Text(String),
    Char(char),
    Constructor(Reference, usize, Vec<Pattern>),
    As(Box<Pattern>),
    EffectPure(Box<Pattern>),
    EffectBind(Reference, usize, Vec<Pattern>, Box<Pattern>),
    SequenceLiteral(Vec<Pattern>),
    SequenceOp(Box<Pattern>, SeqOp, Box<Pattern>),
}

can I convert a pattern into a nested let or something? with like ifs?
// so, if we keep going, things will be bound
// we also need to ...
// ...
// know whether we matched? right?

int => (= vbl 1)

(if (= 1 2)
    false
    (if (= 3 2)
        false
        (if (= 2 2)
            true
            (if (= ....)))))

match_.t_01 = match 2 with
    1 -> false
    3 -> false
    2 -> true
    4 -> false
    _ -> false

(if (equal? "ho" term)
    false
    (if (equal? "hi" term)
        true
        false))

match_.t_02 = match "hi" with
    "ho" -> false
    "hi" -> true
    _ -> false


(let [
    check-first // hm does that actually solve things?
])

(call/cc (lambda (k)
    // do your thang
))

// Ok, so that's the general structure
(let (((result check-first-arm))
    (if (eq? 'fallthrough result)
        (let (((result check-second-arm)))
            (if (eq? 'fallthrough result)
                (let (((result check-third-arm))))
                result))
        result)))

(define (check-first-arm term)
    (if (= term 36) false 'fallthrough))

(define (check-second-arm term)
    (let (((x term)))
        (if (> x 30)
            true
            fallthrough)))

(define (check-third-arm term)
    (if (= term 32) false 'fallthrough))

(define (check-fourth-arm term)
    false)


(if (equal? 35 term)
    false
    (if true
        (let ((x term))
            (if (> x 30)
                true
                'failed)
        )
        // ooh here's where I need the fallthrough. hm.
    ))

match_.t_03 = match 37 with
    36 -> false
    x | x > 30 -> true
    32 -> false
    _ -> false

match_.t_04 = match 14 with
    10 -> false
    x -> x == 14

(define (check-first-arm term)
    (if (vector? term)
        // Cons
        (if (> 0 (vector-length term))
            (let ((head (vector-ref term 0)))
                (if (= head 2)
                    // ugh
                    'fallthrough
                    ))
            'fallthrough
        )
        'fallthrough
    )
)

match [2, [3, 4], 5, 6] with
    2 +: [[x, _], y] :+ z -> x + y + z
    _ -> false

(define (check-first-arm term)
    ; (list 'Some x)
    (if (and (list? term) (= (list-length term) 2))
        (if (eq? 'Some (list-ref term 0))
            (let ((((x (list-ref term 1)))))
                (= x 3))
            'fallthrough)
        'fallthrough))



match_.t_05 = match Some 3 with
    Some x -> x == 3
    None -> false

match_.t_06 = match (3, "hi") with
    (3, _) -> true
    _ -> false
match_.t_07 = match [2,3,4] with
    3 +: _ -> false
    2 +: [3] :+ 2 -> false
    2 +: [3] :+ 4 -> true
match_.t_08 = match [2,3,4] with
    3 +: _ -> false
    2 +: l -> l == [3, 4]
match_.t_09 = match Some None with
    None -> false
    Some _ -> true
match_.t_10 = match Some None with
    None -> false
    Some (Some _) -> false
    Some None -> true
match_.t_11 = match Some (Some 4) with
    None -> false
    Some None -> false
    Some (Some x) -> x == 4
match_.t_12 = match [1,2,3] with
    a ++ [2] -> false
    [b@2] ++ a -> false
    [1] ++ [2, 1] -> false
    [a, _] ++ [3] -> a == 1


match_.t_13 = match (list 'cons 1, 2) with
    (list 'cons 1, x) | x > 2 -> false
    (list 'cons a, _) | a > 2 -> false
    _ -> true

match_.t_13 = match (1, 2) with
    (1, x) | x > 2 -> false
    (a, _) | a > 2 -> false
    _ -> true

match_.t_14 = match 3 with
    1 -> false
    b@3 -> b == 3
match_.t_15 = match [2,3] with
    [] -> false
    [2] -> false
    [a,3] -> a == 2
    _ -> false
match_.t_16 = match [] with
    2 +: [2] -> false
    [2] :+ 3 -> false
    a ++ [2] -> false
    [] -> true
    _ -> false
match_.t_17 = match [2,3] with
    2 +: [1] -> false
    1 +: [1] -> false
    2 +: [3] -> true
    _ -> false
match_.t_18 = match [2,3] with
    [2, 2, 3] ++ [] -> false
    (b@[1]) ++ [5] -> false
    a ++ [3] | a == [] -> false
    [2] :+ 1 -> false
    [1] :+ 1 -> false
    [2] :+ 3 -> true
    _ -> false

```