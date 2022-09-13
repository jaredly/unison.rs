(define (curried a) (lambda (b) (list a b)) )
; ((curried (display "a\n")) (display "b\n"))
