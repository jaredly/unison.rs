(load "runtime_tests.scm")

(define runtests
    (match-lambda
    [() '()]
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
