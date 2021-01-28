(load "runtime_tests_match.scm")

; (use matchable)
(define runtests
    (match-lambda
    [() 10]
    [(x . y)
    
    (if (equal? #t x)
        (display "pass\n")
        (begin
            (display "fail: ")
            (display x)
            (display "\n")
        )
    )
    (runtests y)
    ]
    ))

(runtests tests)
