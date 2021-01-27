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
