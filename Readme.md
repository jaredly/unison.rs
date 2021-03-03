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
ability abilities.a_01 where
    getInt : Int
abilities.f_01 = handle (abilities.a_01.getInt) with cases
    { a } -> (a, 2)
    { abilities.a_01.getInt -> k } -> handle (k +5) with cases { a } -> (a, 3)
abilities.t_01 = abilities.f_01 == (+5, 3)

(define stack '())

(define (throw-effect k effect)
    (let ((handler (car stack)))
        (set! stack (cdr stack))
        (handler (cons 'effect (cons k effect)))
    )
)

(define (rethrow eff)
    (let ((k (cadr eff))
          (ef (cddr eff)))
        (throw-effect k eff)))

(define (add-handler handler)
    (set! stack (cons handler stack)))

(define (getInt)
    (call/cc (lambda (k) (throw-effect k (list 'getInt)))))

(define (handle inner handler)
    (handler (call/cc (lambda (k)
        (add-handler k) ; TODO we'll have to pop at some point?
        (list 'pure (inner))
    )))
)

(define f_01
    (handle
        (lambda () (getInt))
        (lambda (eff)
            (match eff
                [('pure a) (list a 2)]
                [('effect k 'getInt)
                    (handle
                        (lambda () (k 5))
                        (lambda (eff)
                            (display eff)
                            (match eff
                                [('pure a) (list a 3)]
                                [_ (rethrow eff)]
                            )
                        )
                    )
                ]
                [_ (rethrow eff)]
            )
        )
    )
)

```
