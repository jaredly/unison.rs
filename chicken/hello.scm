(import matchable)

(display "Hello world\n")

(define (one a) (if (< a 10) a (two (+ a 1))))
(define (two a) (if (> a 15) (one (/ a 2)) (one (+ a 1))))

(define (plus a b) (+ a b))

(display (one 11))
(display (plus 3 4))

(display (match (list 'ho 2 3)
    [('ho x y) (+ x y)]
    [('hi x) (* x 2)]
    [('hi x y) (list x y)]
))

(define stack '())

(define (throw-effect k effect)
    (let* ((handler (car stack)) (name (car handler)) (fn (cadr handler)))
        (set! stack (cdr stack))
        (print "Calling " name)
        (fn (cons 'effect (cons k effect)))
    )
)

(define (throw-pure value)
    (let* ((handler (car stack)) (name (car handler)) (fn (cadr handler)))
        (set! stack (cdr stack))
        (print "Calling " name)
        (fn (list 'pure value))
    )
)

(define (rethrow eff)
    (let ((k (cadr eff))
          (ef (cddr eff)))
        (throw-effect k eff)))

(define (add-handler name handler)
    (print "add handler " name " " handler " " stack)
    (set! stack (cons (list name handler) stack))
    (print "now " stack)
    )

(define (getInt)
    (call/cc (lambda (k) (throw-effect k (list 'getInt)))))

(define (handle name inner handler)
    (handler (call/cc (lambda (k)
        (print "adding handler " name)
        (add-handler name k) ; TODO we'll have to pop at some point?
        (let ((value (inner)))
            (print "Got value " name " " value)
            (throw-pure value)
            ; (list 'pure value)
        )
    )))
)

(print)

(define f_01
    (handle "top"
        (lambda () (getInt))
        (lambda (eff)
            (print "top" eff)
            (match eff
                [('pure a) (list a 2)]
                [('effect k 'getInt)
                    (handle "inner"
                        (lambda () (k 5))
                        (lambda (eff)
                            (print "inner" eff)
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

(print f_01)