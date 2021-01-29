#lang racket

(require racket/pretty)
(define inp (open-input-file "error.scm"))
(define oup (open-output-file "error_pretty.scm" #:exists 'replace))

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