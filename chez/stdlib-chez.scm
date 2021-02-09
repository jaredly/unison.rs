;;; These are for chez
(import (srfi :67))
(import (srfi :128))
(import (srfi :133 vectors))
(import (srfi :151))

(define foldl fold-left)

(define true #t)
(define false #f)

(define abort error)
(define print (lambda args (map display args) (display "\n")))

;;;;;;;;

(define (term-link v) (list 'term-link v))
(define (type-link v) (list 'type-link v))

(define (untuple term)
    (if (and (list? term)
            (= (length term) 3)
            (equal? (car term) 'onbcm0qctbnuctpm57tkc5p16b8gfke8thjf19p4r4laokji0b606rd0frnhj103qb90lve3fohkoc1eda70491hot656s1m6kk3cn0_0))
        (cons (cadr term) (untuple (caddr term)))
        (if (equal? term Nil)
            '()
        term)))

(define (Debug.watch text)
    (lambda (v)
        (print "⚠️  " text " " (untuple v))
        v))

(define (bug v)
    (print "BUG " (untuple v))
    (abort "Found a bug!")
)

(define (print-processing name)
    ;; Uncomment this line to debug terms that are failing to process
     (print "Evaluating " name)
    '())

(define (check v name)
    (if (not v)
    (begin
        (print "❌ Test failed! " name)
        (print "Got " v)
        ; (print "Test failure")
        )
    (print "✅ passed " name)
        )
    )

(define (result-is-good result)
    (and (list? result)
        (= (length result) 2)
        (equal? (car result) 'vmc06s4f236sps61vqv35g7ridnae03uetth98aocort1825stbv7m6ncfca2j0gcane47c8db2rjtd2o6kch2lr7v2gst895pcs0m0_1)
    )
)

(define (check-results v name)
    (if (not (foldl
        (lambda (current result)
            (or (if (not (result-is-good result))
            (begin
                (print "❌ Test failed " name " " result)
                ; (abort "Test failed")
                #t
            ) #f) current))
        #f
        (vector->list v)
        ))
    (print "✅ passed " name)
    ; '()
    )
    '()
)

(define (no-match) (abort "failed to match"))

(define (f2c22r2a1sche28mn07brk1j45kp1bam3tr4k2j0un2hi1g7rbrud3f5mes2defqo1tpd9j38pqpg2f0efl3no0ede5ocl2am4bonm0 a)
    (lambda (b) (not (equal? a b)))
)

; GUID

(define (rc29vdqe019p56kupcgkg07fkib86r3oooatbmsgfbdsgpmjhsh00l307iuts3r973q5etb61vbjkes42b6adb3mkorusvmudiuorno_0 id)
    (list 'rc29vdqe019p56kupcgkg07fkib86r3oooatbmsgfbdsgpmjhsh00l307iuts3r973q5etb61vbjkes42b6adb3mkorusvmudiuorno_0 id))

(define (Bytes.fromList bytes) bytes) ; yolo

; base.Map

(define (7di5ureqgi60ue42886240kbovfhko0fg85rp2thpkl8af699upsl0os1btk27te1cjdmuerad5oi9bdd04me6mjh2m25djbj236fbo_0 k)
    (lambda (v) (list '7di5ureqgi60ue42886240kbovfhko0fg85rp2thpkl8af699upsl0os1btk27te1cjdmuerad5oi9bdd04me6mjh2m25djbj236fbo_0 k v)))

; Cons/Nil

(define (Cons one)
    (lambda (two) (list 'onbcm0qctbnuctpm57tkc5p16b8gfke8thjf19p4r4laokji0b606rd0frnhj103qb90lve3fohkoc1eda70491hot656s1m6kk3cn0_0 one two)))

(define Nil
    '568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8_0)

; (define Cons onbcm0qctbnuctpm57tkc5p16b8gfke8thjf19p4r4laokji0b606rd0frnhj103qb90lve3fohkoc1eda70491hot656s1m6kk3cn0_0 )
; (define Nil 568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8_0)


; Some/None
(define None
    '5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_0)

(define (Some arg)
    (list '5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_1 arg))

; (define None 5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_0)
; (define Some 5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_1)



(define (m7uplgfko92kqdmm6u898j5h4n86587f44u7fq1vjcad1f68n35r8j2mdfdbjta5hq9o699dgn2aphteditp30g34hsh3gru68593j0 a)
    (lambda (b) (- a b)))

(define (Nat.drop a)
    (lambda (b)
        (max 0 (- a b))))

(define (Nat.+ a)
    (lambda (b)
        (if (not (number? a))
        (begin
        (print "A is " a " " (symbol? a) " " (string? a))
        (abort "a is not a number")
        )
        )
        (natLoop (+ a b))))

(define (natLoop num)
    (if (> num maxNat) (- num 1 maxNat) num))

(define (intLoop num)
    (if (> num maxInt)
        (+ minInt (- num 1 maxInt))
        (if (< num minInt)
            (+ maxInt (- num -1 minInt))
            num
        )
    ))


(define maxNat 18446744073709551615)
(define maxInt +9223372036854775807)

(define s9h25aadei68iscfiu60eldfhe9uvh0pk3knd9m965gqlejvc5jlcqs9gfcgpgvfv85n2pefvee4ca2n7mepcoqamou73g7ilscf450
    maxNat)

(define p9og3s2h41natoslfjoi1do0omp82s4jiethebfd4j5p99ltbdmcua2egbiehs9tq9k65744cvugibiqdkgip21t7se4e8faktnl3k0
  -9223372036854775808)

(define d75vubeoep5o8ph72v0v9qdm36n17up0d7bsbdckjapcs7k9g1kv5mnbpp3444u8fmvo2h3benmk7o3sd09g1lkrrvk4q93vv8u2n3g
    maxInt)

(define minInt p9og3s2h41natoslfjoi1do0omp82s4jiethebfd4j5p99ltbdmcua2egbiehs9tq9k65744cvugibiqdkgip21t7se4e8faktnl3k0)

(define (Nat.xor a)
    (lambda (b) (bitwise-xor a b)))

(define (Nat.pow a)
    (lambda (b) (expt a b)))

(define (Float.* a) (lambda (b) (* a b)))
(define (Float./ a) (lambda (b) (/ a (exact->inexact b))))
(define (Float.- a) (lambda (b) (- a b)))
(define (Float.+ a) (lambda (b) (+ a b)))
(define (Float.fromText a) 
    (let ((v (string->number a)))
        (if (equal? #f v)
            None
            (Some (exact->inexact v))
        )
))
(define Int.toFloat exact->inexact)
(define (Float.truncate v) (inexact->exact (floor v)))
; (exact->inexact v)
; )
(define Boolean.not not)

        
(define (Int.- a)
    (lambda (b)
        (intLoop (- a b))))

        
(define (Int.+ a)
    (lambda (b)
        (intLoop (+ a b))))


        
; (define (Nat.* a)
;     (lambda (b)
;         (* a b)))

        
(define (Nat.or a)
    (lambda (b)
        (bitwise-ior a b)))

        
(define (Nat.and a)
    (lambda (b)
        (bitwise-and a b)))

(define (Nat.shiftLeft a)
    (lambda (b)
        (bitwise-and (arithmetic-shift a b) maxNat)))

        
(define (Nat.shiftRight a)
    (lambda (b)
        (arithmetic-shift a (- b))))

(define (Int.toText n) 
    (if (>= n 0)
        (string-append "+" (number->string n))
        (number->string n)
        ))
(define Int.isEven even?)
(define Int.isOdd odd?)

(define (Int.pow a) (lambda (b) (expt a b)))
(define (Int.mod a) (lambda (b) (modulo a b)))
(define (Int.complement a) (bitwise-not a))
(define (Int.truncate0 a) (max 0 a))
(define (Nat.complement a) (- maxNat a))
(define (Int.or a) (lambda (b) (bitwise-ior a b)))
(define (Int.and a) (lambda (b) (bitwise-and a b)))
(define (Int.xor a) (lambda (b) (bitwise-xor a b)))
(define (Int.increment a) (+ a 1))
(define (Int.decrement a) (- a 1))
(define (Int./ a) (lambda (b) (quotient a b)))
(define (Int.* a) (lambda (b) (* a b)))
(define (Int.negate a) (- a))

(define Nat.toText number->string)
(define (Nat.toInt x) x)
(define (Nat.sub a) (lambda (b) (- a b)))
(define Nat.isEven even?)
(define Nat.isOdd odd?)
(define (Nat.mod a) (lambda (b) (modulo a b)))
(define (Nat.increment a) (natLoop (+ a 1)))
(define (Nat.decrement a) (max 0 (- a 1)))
(define (Nat./ a) (lambda (b) (floor (/ a b))))
(define (Nat.* a) (lambda (b) (* a b)))

(define (Int.shiftLeft a)
    (lambda (b)
        (arithmetic-shift a b)))

(define (Int.shiftRight a)
    (lambda (b)
        (arithmetic-shift a (- b))))


(define default-comparator (make-default-comparator))

(define (Universal.> a) (lambda (b) (>? default-comparator a b)))
(define (Universal.>= a) (lambda (b) (>=? default-comparator a b)))
(define (Universal.<= a) (lambda (b) (<=? default-comparator a b)))
(define (Universal.< a) (lambda (b) (<? default-comparator a b)))
(define (Universal.== a) (lambda (b) (=? default-comparator a b)))
(define (Universal.compare a) (lambda (b) (comparator-if<=> default-comparator a b -1 0 1)))

; --- lists ---

;; using vectors
(define List.size vector-length)
(define (List.cons item) (lambda (vec)
    (let ((dest (make-vector (+ 1 (vector-length vec)))))
        (vector-copy! dest 1 vec)
        (vector-set! dest 0 item)
        dest)))
(define (List.snoc vec) (lambda (item)
    (let ((dest (make-vector (+ 1 (vector-length vec)))))
        (vector-copy! dest 0 vec)
        (vector-set! dest (vector-length vec) item)
        dest)))
(define (List.++ a) (lambda (b) (vector-append a b))) 
(define (List.drop count) (lambda (vec)
    (let ((count (min (vector-length vec) count)))
        (let ((ln (- (vector-length vec) count)))
            (let ((dest (make-vector ln)))
                (vector-copy! dest 0 vec count)
                dest)))))
(define (List.at a) (lambda (b)
    (if (< a (vector-length b))
        (Some (vector-ref b a))
        None)))
(define (List.take ln) (lambda (vec)
    (let ((ln_ (min ln (vector-length vec))))
        (let ((dest (make-vector ln_)))
            (vector-copy! dest 0 vec 0 ln_)
            dest))))

;; using linked lists
; (define List.size length)
; (define List.cons cons)
; (define (List.++ a) (lambda (b) (append a b))) 
; (define (List.drop a) (lambda (b) (list-tail b a)))
; (define (List.at a) (lambda (b) (list-ref b a)))

; --- text stdlib ---

(define (Text.fromCharList lst)
    (apply string (vector->list lst)))
(define (Text.toCharList text)
    (list->vector (string->list text))
)
(define Char.fromNat integer->char)
(define Char.toNat char->integer)

(define (Text.uncons t)
    (if (> (string-length t) 0)
        (Some
            (
                (Cons (string-ref t 0))
                ((Cons (substring t 1 (string-length t))) Nil)
            )
        )
        None
        )
)
(define (Text.!= a) (lambda (b) (not (equal? a b))))
(define (Text.++ a) (lambda (b) (string-append a b)))
(define Text.size string-length)
(define (Text.take count) (lambda (str)
    (let ((count (min count (string-length str))))
        (substring str 0 count))))
(define (Text.drop count) (lambda (str)
    (let ((count (min count (string-length str))))
        (substring str count (string-length str)))))

; --- abilties ---


;;; Ok, so basic idea:
;;; We maintain a stack of handlers
;;; and if you fall through an evaluation, then the handler gets put back on the stack.
;;; and if you add a handler, it gets added at the place where the handler stack pointer is at.
;;; 
;;; but when you call the continuation, we reset the pointer to the top, right?

;; Ok, nother stress test.
;; While we're partway down the handler stack, do a jump down & back & stuff.
;; how do we deal?
;;
;; So like, what if the `k` continuation to jump back just keeps track of the handlers to put back on the handler stack?
;; that way, we can handle nested jumps & back.
;;
;; Yeah that's a much better setup.




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

