(load "runtime_tests.scm")

; (use matchable)
(define runtests
    (match-lambda
    [() 10]
    [(x . y)
    
    (if x
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
