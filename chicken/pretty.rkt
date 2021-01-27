#lang racket

(require racket/pretty)
(define inp (open-input-file "runtime_tests.scm"))
(define oup (open-output-file "runtime_tests_pretty.scm"))

(define (loop)
    (let [(x (read inp))]
        (if (eof-object? x)
            10
            (begin
                (write-string (pretty-format x) oup)
                (write-string "\n\n" oup)
                (loop))
        )
    ))
(loop)