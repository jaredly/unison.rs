
;;; stack is of type Array<(handler, id)>
;;; handler will be called with
;;; (list 'effect kont (list 'some-effect some-arg) current-handler)
(define stack '())

(define (throw-effect k effect)
    (if (or (not (list? effect))
            (not (procedure? k))
            (not (symbol? (car effect))))
        (begin
            (print "Invalid input to throw-effect " k " " effect)
            (abort "Invalid throw effect"))
        (if (eq? '() stack)
            (begin
                (print "Effect without handlers " k " " effect)
                (abort "no handler for effect")
                )
            (let* ((handler (car stack))
                   (f (car handler))
                   (id (cadr handler)))
                (set! stack (cdr stack))
                ; (print "---> throwing " effect " to " id)
                ; (list 'effect k 'thing abs things)
                ; (list 'effect )
                ; ok so the fact that this schema changed is probably messing with things.
                (f (list 'effect k effect handler))
                ; (f (cons 'effect (cons k effect)))
            ))
        )
)

(define (throw-pure value)
    (let* ((handler (car stack))
           (f (car handler))
           (id (cadr handler)))
        (set! stack (cdr stack))
        ; (print "Sending pure " value " to " id)
        ; STOPSHIP: do we need to keep track of the handler here?
        (f (list 'pure value))
    ))

(define (rethrow-effect full)
    ; (list 'effect k effect handler)
    (let ((k (cadr full))
          (effect (caddr full))
          (handler (cadddr full))
          )
        ; wait. so re-throw should modify K, right?
        ; such that K is now (lambda (whatever) (add-handler handler) (k whatever))
        ; yeah.
        ; (print "[re-throwing] " ef)
        (throw-effect
            (lambda (v)
                (set! stack (cons handler stack))
                (k v))
            effect)))

(define (add-handler id handler)
    (set! stack (cons (list handler id) stack))
    ; (print "now " stack)
)

(define hid 0)
(define (get-id)
    (set! hid (+ hid 1))
    hid)

(define (handle-ability inner handler)
    (let ((id (get-id)))
        (handler (call/cc (lambda (k)
            ; (print "adding handler " id)
            (add-handler id k) ; TODO we'll have to pop at some point?
            (let ((value (inner)))
                ; (print "Got value for " id " " value)
                (throw-pure value)
                ; (list 'pure value)
                ; ugh ok what's the case that breaks this?
            )
        )))
    )
)



(define (run-with-io inner)
    (handle-ability
        (lambda () (inner "Hello"))
        ; 
        (lambda (effect)
            (if (not (list? effect))
                (abort "Effect not a list")
                (if (equal? 'effect (car effect))
                    (if ; printText handle text
                        ; we just pretend it's all stdout
                        (equal? 'fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_37
                            (car (caddr effect)))
                        (begin
                            (print (caddr (caddr effect)))
                            (run-with-io (lambda (_) ((cadr effect)
                                (list 'kc92tha5f12vamultsbq93aqnphg9pnhuq3sodqvhes6st2a3h5sd2rksuptds94ptvvpg0tj0jp1rehlb73rkn0kj2r6elkdqndhjo_1
                                '())))
                            )
                        )
                        (rethrow-effect effect)
                    )
                    (if (equal? 'pure (car effect))
                        (cadr effect)
                        (begin
                            (print "Unexpected effect format " effect)
                            (abort "Unexpected effect format")
                        )
                        )
                ))
        )

    )

)