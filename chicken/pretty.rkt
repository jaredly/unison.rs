#lang racket

(require racket/pretty)
(define inp (open-input-file "runtime_tests_match.scm"))
(define oup (open-output-file "runtime_tests_match_pretty.scm" #:exists 'replace))

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