




;;; These are for gerbil
(import :std/srfi/128)
(import :std/srfi/133)
(import :std/srfi/151)


;;; These are for chez
; (import (srfi :67))
; (import (srfi :128))
; (import (srfi :133 vectors))
; (import (srfi :151))

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

(define (to-json v) v)
    ; (let ((o (open-output-string)))
    ;     (json-write v o)
    ;     (get-output-string o)
    ; ))

(define (Debug.watch text)
    (lambda (v)
        (print "⚠️  " text " " (to-json (untuple v)))
        v))

(define (bug v)
    (print "BUG " (to-json (untuple v)))
    (abort "Found a bug!")
)

(define (print-processing name)
    ;; Uncomment this line to debug terms that are failing to process
     ; (print "Evaluating " name)
    '())

(define (check v name)
    (if (not v)
    (begin
        (print "❌ Test failed! " name)
        (print "Got " v)
        ; (print "Test failure")
        )
    ; (print "✅ passed " name)
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
                (print "❌ Test failed " name " " (to-json result))
                ; (abort "Test failed")
                #t
            ) #f) current))
        #f
        (vector->list v)
        ))
    ; (print "✅ passed " name)
    '()
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
                ((Cons (substring t 1 (- (string-length t) 1))) Nil)
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
        (substring str count (- (string-length str) count)))))

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


; unison_random_mersenne.random.mersenne.State

; Data(DataDecl { modifier: Structural, bound: [], constructors: [(🔣State.State/0, Arrow(App(Ref(Sequence), Ref(#d97e0jhkmd)), Ref(#80gg7kgilb)))] })

; State.State

(define 80gg7kgilboe8aq4q8qj3oq9cfjj795m5pmgc7k5r2ne76qn79e33e8gk9q7pvgjpkompfv6po8k0rhi2u1g3r6agcgkl6etlv202no_0 (lambda (arg_0) (list '80gg7kgilboe8aq4q8qj3oq9cfjj795m5pmgc7k5r2ne76qn79e33e8gk9q7pvgjpkompfv6po8k0rhi2u1g3r6agcgkl6etlv202no_0 arg_0)))

; base.io.IO

; Effect(DataDecl { modifier: Structural, bound: [], constructors: [(🔣io.IO.getFileSize_/0, Arrow(Ref(#53ai5nivms), Effect(Effects([Ref(#fgaevis4bl)]), App(App(Ref(#kc92tha5f1), Ref(#jkhagbadav)), Ref(Nat))))), (🔣io.IO.kill_/0, Arrow(Ref(#jbn673ovtd), Effect(Effects([Ref(#fgaevis4bl)]), App(App(Ref(#kc92tha5f1), Ref(#jkhagbadav)), Ref(#568rsi7o3g))))), (🔣io.IO.send_/0, Arrow(Ref(#3n6ctj9fne), Arrow(Ref(Bytes), Effect(Effects([Ref(#fgaevis4bl)]), App(App(Ref(#kc92tha5f1), Ref(#jkhagbadav)), Ref(#568rsi7o3g)))))), (🔣io.IO.bracket_/0, Forall(|a/0 #0|(Forall(|b/0 #0|(Forall(|c/0 #0|(Arrow(Arrow(Ref(#568rsi7o3g), Effect(Effects([Ref(#fgaevis4bl)]), vara (#0))), Arrow(Arrow(vara (#0), Effect(Effects([Ref(#fgaevis4bl)]), varb (#0))), Arrow(Arrow(vara (#0), Effect(Effects([Ref(#fgaevis4bl)]), varc (#0))), Effect(Effects([Ref(#fgaevis4bl)]), App(App(Ref(#kc92tha5f1), Ref(#jkhagbadav)), varc (#0))))))))))))), (🔣io.IO.getLine_/0, Arrow(Ref(#b7kh3q81n1), Effect(Effects([Ref(#fgaevis4bl)]), App(App(Ref(#kc92tha5f1), Ref(#jkhagbadav)), Ref(Text))))), (🔣io.IO.getText_/0, Arrow(Ref(#b7kh3q81n1), Effect(Effects([Ref(#fgaevis4bl)]), App(App(Ref(#kc92tha5f1), Ref(#jkhagbadav)), Ref(Text))))), (🔣io.IO.getFileTimestamp_/0, Arrow(Ref(#53ai5nivms), Effect(Effects([Ref(#fgaevis4bl)]), App(App(Ref(#kc92tha5f1), Ref(#jkhagbadav)), Ref(#kg74r0md7j))))), (🔣io.IO.closeFile_/0, Arrow(Ref(#b7kh3q81n1), Effect(Effects([Ref(#fgaevis4bl)]), App(App(Ref(#kc92tha5f1), Ref(#jkhagbadav)), Ref(#568rsi7o3g))))), (🔣io.IO.getTemporaryDirectory_/0, Effect(Effects([Ref(#fgaevis4bl)]), App(App(Ref(#kc92tha5f1), Ref(#jkhagbadav)), Ref(#53ai5nivms)))), (🔣io.IO.getCurrentDirectory_/0, Effect(Effects([Ref(#fgaevis4bl)]), App(App(Ref(#kc92tha5f1), Ref(#jkhagbadav)), Ref(#53ai5nivms)))), (🔣io.IO.renameDirectory_/0, Arrow(Ref(#53ai5nivms), Arrow(Ref(#53ai5nivms), Effect(Effects([Ref(#fgaevis4bl)]), App(App(Ref(#kc92tha5f1), Ref(#jkhagbadav)), Ref(#568rsi7o3g)))))), (🔣io.IO.renameFile_/0, Arrow(Ref(#53ai5nivms), Arrow(Ref(#53ai5nivms), Effect(Effects([Ref(#fgaevis4bl)]), App(App(Ref(#kc92tha5f1), Ref(#jkhagbadav)), Ref(#568rsi7o3g)))))), (🔣io.IO.receive_/0, Arrow(Ref(#3n6ctj9fne), Arrow(Ref(Nat), Effect(Effects([Ref(#fgaevis4bl)]), App(App(Ref(#kc92tha5f1), Ref(#jkhagbadav)), App(Ref(#5isltsdct9), Ref(Bytes))))))), (🔣io.IO.fileExists_/0, Arrow(Ref(#53ai5nivms), Effect(Effects([Ref(#fgaevis4bl)]), App(App(Ref(#kc92tha5f1), Ref(#jkhagbadav)), Ref(Boolean))))), (🔣io.IO.isDirectory_/0, Arrow(Ref(#53ai5nivms), Effect(Effects([Ref(#fgaevis4bl)]), App(App(Ref(#kc92tha5f1), Ref(#jkhagbadav)), Ref(Boolean))))), (🔣io.IO.directoryContents_/0, Arrow(Ref(#53ai5nivms), Effect(Effects([Ref(#fgaevis4bl)]), App(App(Ref(#kc92tha5f1), Ref(#jkhagbadav)), App(Ref(Sequence), Ref(#53ai5nivms)))))), (🔣io.IO.listen_/0, Arrow(Ref(#3n6ctj9fne), Effect(Effects([Ref(#fgaevis4bl)]), App(App(Ref(#kc92tha5f1), Ref(#jkhagbadav)), Ref(#568rsi7o3g))))), (🔣io.IO.closeSocket_/0, Arrow(Ref(#3n6ctj9fne), Effect(Effects([Ref(#fgaevis4bl)]), App(App(Ref(#kc92tha5f1), Ref(#jkhagbadav)), Ref(#568rsi7o3g))))), (🔣io.IO.clientSocket_/0, Arrow(Ref(#4a63ca3c83), Arrow(Ref(#9giu4jv486), Effect(Effects([Ref(#fgaevis4bl)]), App(App(Ref(#kc92tha5f1), Ref(#jkhagbadav)), Ref(#3n6ctj9fne)))))), (🔣io.IO.delay_/0, Arrow(Ref(Nat), Effect(Effects([Ref(#fgaevis4bl)]), App(App(Ref(#kc92tha5f1), Ref(#jkhagbadav)), Ref(#568rsi7o3g))))), (🔣io.IO.seek_/0, Arrow(Ref(#b7kh3q81n1), Arrow(Ref(#ep1qo0ujpu), Arrow(Ref(Int), Effect(Effects([Ref(#fgaevis4bl)]), App(App(Ref(#kc92tha5f1), Ref(#jkhagbadav)), Ref(#568rsi7o3g))))))), (🔣io.IO.serverSocket_/0, Arrow(App(Ref(#5isltsdct9), Ref(#4a63ca3c83)), Arrow(Ref(#9giu4jv486), Effect(Effects([Ref(#fgaevis4bl)]), App(App(Ref(#kc92tha5f1), Ref(#jkhagbadav)), Ref(#3n6ctj9fne)))))), (🔣io.IO.accept_/0, Arrow(Ref(#3n6ctj9fne), Effect(Effects([Ref(#fgaevis4bl)]), App(App(Ref(#kc92tha5f1), Ref(#jkhagbadav)), Ref(#3n6ctj9fne))))), (🔣io.IO.setBuffering_/0, Arrow(Ref(#b7kh3q81n1), Arrow(App(Ref(#5isltsdct9), Ref(#u934c4e54d)), Effect(Effects([Ref(#fgaevis4bl)]), App(App(Ref(#kc92tha5f1), Ref(#jkhagbadav)), Ref(#568rsi7o3g)))))), (🔣io.IO.openFile_/0, Arrow(Ref(#53ai5nivms), Arrow(Ref(#drqrm3fo7q), Effect(Effects([Ref(#fgaevis4bl)]), App(App(Ref(#kc92tha5f1), Ref(#jkhagbadav)), Ref(#b7kh3q81n1)))))), (🔣io.IO.throw/0, Forall(|a/0 #0|(Arrow(Ref(#jkhagbadav), Effect(Effects([Ref(#fgaevis4bl)]), vara (#0)))))), (🔣io.IO.fork_/0, Forall(|a/0 #0|(Arrow(Arrow(Ref(#568rsi7o3g), Effect(Effects([Ref(#fgaevis4bl)]), vara (#0))), Effect(Effects([Ref(#fgaevis4bl)]), App(App(Ref(#kc92tha5f1), Ref(#jkhagbadav)), Ref(#jbn673ovtd))))))), (🔣io.IO.getBuffering_/0, Arrow(Ref(#b7kh3q81n1), Effect(Effects([Ref(#fgaevis4bl)]), App(App(Ref(#kc92tha5f1), Ref(#jkhagbadav)), App(Ref(#5isltsdct9), Ref(#u934c4e54d)))))), (🔣io.IO.position_/0, Arrow(Ref(#b7kh3q81n1), Effect(Effects([Ref(#fgaevis4bl)]), App(App(Ref(#kc92tha5f1), Ref(#jkhagbadav)), Ref(Int))))), (🔣io.IO.setCurrentDirectory_/0, Arrow(Ref(#53ai5nivms), Effect(Effects([Ref(#fgaevis4bl)]), App(App(Ref(#kc92tha5f1), Ref(#jkhagbadav)), Ref(#568rsi7o3g))))), (🔣io.IO.createDirectory_/0, Arrow(Ref(#53ai5nivms), Effect(Effects([Ref(#fgaevis4bl)]), App(App(Ref(#kc92tha5f1), Ref(#jkhagbadav)), Ref(#568rsi7o3g))))), (🔣io.IO.removeDirectory_/0, Arrow(Ref(#53ai5nivms), Effect(Effects([Ref(#fgaevis4bl)]), App(App(Ref(#kc92tha5f1), Ref(#jkhagbadav)), Ref(#568rsi7o3g))))), (🔣io.IO.removeFile_/0, Arrow(Ref(#53ai5nivms), Effect(Effects([Ref(#fgaevis4bl)]), App(App(Ref(#kc92tha5f1), Ref(#jkhagbadav)), Ref(#568rsi7o3g))))), (🔣io.IO.systemTime_/0, Effect(Effects([Ref(#fgaevis4bl)]), App(App(Ref(#kc92tha5f1), Ref(#jkhagbadav)), Ref(#kg74r0md7j)))), (🔣io.IO.isFileEOF_/0, Arrow(Ref(#b7kh3q81n1), Effect(Effects([Ref(#fgaevis4bl)]), App(App(Ref(#kc92tha5f1), Ref(#jkhagbadav)), Ref(Boolean))))), (🔣io.IO.isFileOpen_/0, Arrow(Ref(#b7kh3q81n1), Effect(Effects([Ref(#fgaevis4bl)]), App(App(Ref(#kc92tha5f1), Ref(#jkhagbadav)), Ref(Boolean))))), (🔣io.IO.isSeekable_/0, Arrow(Ref(#b7kh3q81n1), Effect(Effects([Ref(#fgaevis4bl)]), App(App(Ref(#kc92tha5f1), Ref(#jkhagbadav)), Ref(Boolean))))), (🔣io.IO.putText_/0, Arrow(Ref(#b7kh3q81n1), Arrow(Ref(Text), Effect(Effects([Ref(#fgaevis4bl)]), App(App(Ref(#kc92tha5f1), Ref(#jkhagbadav)), Ref(#568rsi7o3g))))))] })

; io.IO.getFileSize_

(define fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_0 (lambda (arg_0) (call/cc (lambda (k) (throw-effect k (list 'fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_0 arg_0))))))

; io.IO.kill_

(define fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_1 (lambda (arg_0) (call/cc (lambda (k) (throw-effect k (list 'fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_1 arg_0))))))

; io.IO.send_

(define fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_2 (lambda (arg_0) (lambda (arg_1) (call/cc (lambda (k) (throw-effect k (list 'fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_2 arg_0 arg_1)))))))

; io.IO.bracket_

(define fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_3 (lambda (arg_0) (lambda (arg_1) (lambda (arg_2) (call/cc (lambda (k) (throw-effect k (list 'fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_3 arg_0 arg_1 arg_2))))))))

; io.IO.getLine_

(define fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_4 (lambda (arg_0) (call/cc (lambda (k) (throw-effect k (list 'fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_4 arg_0))))))

; io.IO.getText_

(define fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_5 (lambda (arg_0) (call/cc (lambda (k) (throw-effect k (list 'fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_5 arg_0))))))

; io.IO.getFileTimestamp_

(define fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_6 (lambda (arg_0) (call/cc (lambda (k) (throw-effect k (list 'fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_6 arg_0))))))

; io.IO.closeFile_

(define fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_7 (lambda (arg_0) (call/cc (lambda (k) (throw-effect k (list 'fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_7 arg_0))))))

; io.IO.getTemporaryDirectory_

(define fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_8 (lambda () (call/cc (lambda (k) (throw-effect k (list 'fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_8))))))

; io.IO.getCurrentDirectory_

(define fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_9 (lambda () (call/cc (lambda (k) (throw-effect k (list 'fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_9))))))

; io.IO.renameDirectory_

(define fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_10 (lambda (arg_0) (lambda (arg_1) (call/cc (lambda (k) (throw-effect k (list 'fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_10 arg_0 arg_1)))))))

; io.IO.renameFile_

(define fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_11 (lambda (arg_0) (lambda (arg_1) (call/cc (lambda (k) (throw-effect k (list 'fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_11 arg_0 arg_1)))))))

; io.IO.receive_

(define fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_12 (lambda (arg_0) (lambda (arg_1) (call/cc (lambda (k) (throw-effect k (list 'fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_12 arg_0 arg_1)))))))

; io.IO.fileExists_

(define fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_13 (lambda (arg_0) (call/cc (lambda (k) (throw-effect k (list 'fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_13 arg_0))))))

; io.IO.isDirectory_

(define fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_14 (lambda (arg_0) (call/cc (lambda (k) (throw-effect k (list 'fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_14 arg_0))))))

; io.IO.directoryContents_

(define fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_15 (lambda (arg_0) (call/cc (lambda (k) (throw-effect k (list 'fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_15 arg_0))))))

; io.IO.listen_

(define fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_16 (lambda (arg_0) (call/cc (lambda (k) (throw-effect k (list 'fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_16 arg_0))))))

; io.IO.closeSocket_

(define fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_17 (lambda (arg_0) (call/cc (lambda (k) (throw-effect k (list 'fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_17 arg_0))))))

; io.IO.clientSocket_

(define fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_18 (lambda (arg_0) (lambda (arg_1) (call/cc (lambda (k) (throw-effect k (list 'fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_18 arg_0 arg_1)))))))

; io.IO.delay_

(define fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_19 (lambda (arg_0) (call/cc (lambda (k) (throw-effect k (list 'fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_19 arg_0))))))

; io.IO.seek_

(define fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_20 (lambda (arg_0) (lambda (arg_1) (lambda (arg_2) (call/cc (lambda (k) (throw-effect k (list 'fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_20 arg_0 arg_1 arg_2))))))))

; io.IO.serverSocket_

(define fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_21 (lambda (arg_0) (lambda (arg_1) (call/cc (lambda (k) (throw-effect k (list 'fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_21 arg_0 arg_1)))))))

; io.IO.accept_

(define fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_22 (lambda (arg_0) (call/cc (lambda (k) (throw-effect k (list 'fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_22 arg_0))))))

; io.IO.setBuffering_

(define fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_23 (lambda (arg_0) (lambda (arg_1) (call/cc (lambda (k) (throw-effect k (list 'fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_23 arg_0 arg_1)))))))

; io.IO.openFile_

(define fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_24 (lambda (arg_0) (lambda (arg_1) (call/cc (lambda (k) (throw-effect k (list 'fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_24 arg_0 arg_1)))))))

; io.IO.throw

(define fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_25 (lambda (arg_0) (call/cc (lambda (k) (throw-effect k (list 'fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_25 arg_0))))))

; io.IO.fork_

(define fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_26 (lambda (arg_0) (call/cc (lambda (k) (throw-effect k (list 'fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_26 arg_0))))))

; io.IO.getBuffering_

(define fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_27 (lambda (arg_0) (call/cc (lambda (k) (throw-effect k (list 'fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_27 arg_0))))))

; io.IO.position_

(define fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_28 (lambda (arg_0) (call/cc (lambda (k) (throw-effect k (list 'fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_28 arg_0))))))

; io.IO.setCurrentDirectory_

(define fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_29 (lambda (arg_0) (call/cc (lambda (k) (throw-effect k (list 'fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_29 arg_0))))))

; io.IO.createDirectory_

(define fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_30 (lambda (arg_0) (call/cc (lambda (k) (throw-effect k (list 'fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_30 arg_0))))))

; io.IO.removeDirectory_

(define fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_31 (lambda (arg_0) (call/cc (lambda (k) (throw-effect k (list 'fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_31 arg_0))))))

; io.IO.removeFile_

(define fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_32 (lambda (arg_0) (call/cc (lambda (k) (throw-effect k (list 'fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_32 arg_0))))))

; io.IO.systemTime_

(define fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_33 (lambda () (call/cc (lambda (k) (throw-effect k (list 'fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_33))))))

; io.IO.isFileEOF_

(define fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_34 (lambda (arg_0) (call/cc (lambda (k) (throw-effect k (list 'fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_34 arg_0))))))

; io.IO.isFileOpen_

(define fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_35 (lambda (arg_0) (call/cc (lambda (k) (throw-effect k (list 'fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_35 arg_0))))))

; io.IO.isSeekable_

(define fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_36 (lambda (arg_0) (call/cc (lambda (k) (throw-effect k (list 'fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_36 arg_0))))))

; io.IO.putText_

(define fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_37 (lambda (arg_0) (lambda (arg_1) (call/cc (lambda (k) (throw-effect k (list 'fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_37 arg_0 arg_1)))))))

; base.Tuple

; Data(DataDecl { modifier: Structural, bound: [🔣a/0, 🔣b/0], constructors: [(🔣Tuple.Cons/0, Forall(|a/0 #0|(Forall(|b/0 #0|(Arrow(vara (#0), Arrow(varb (#0), App(App(Ref(#onbcm0qctb), vara (#0)), varb (#0)))))))))] })

; Tuple.Cons

(define onbcm0qctbnuctpm57tkc5p16b8gfke8thjf19p4r4laokji0b606rd0frnhj103qb90lve3fohkoc1eda70491hot656s1m6kk3cn0_0 (lambda (arg_0) (lambda (arg_1) (list 'onbcm0qctbnuctpm57tkc5p16b8gfke8thjf19p4r4laokji0b606rd0frnhj103qb90lve3fohkoc1eda70491hot656s1m6kk3cn0_0 arg_0 arg_1))))

; base.Optional

; Data(DataDecl { modifier: Structural, bound: [🔣a/0], constructors: [(🔣Optional.None/0, Forall(|a/0 #0|(App(Ref(#5isltsdct9), vara (#0))))), (🔣Optional.Some/0, Forall(|a/0 #0|(Arrow(vara (#0), App(Ref(#5isltsdct9), vara (#0))))))] })

; Optional.None

(define 5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_0 '5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_0)

; Optional.Some

(define 5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_1 (lambda (arg_0) (list '5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_1 arg_0)))

; unison_random_mersenne._base_additions.Nat32

; Data(DataDecl { modifier: Structural, bound: [], constructors: [(🔣Nat32.Nat32/0, Arrow(Ref(Nat), Ref(#d97e0jhkmd)))] })

; Nat32.Nat32

(define d97e0jhkmdospt1d5n3pdrl78ufb3m32hr06uhijaq8s2vdhmaehr7rv0fsm8b3p469cru224jqs46vlqesbaf5a41vlstv87gempqo_0 (lambda (arg_0) (list 'd97e0jhkmdospt1d5n3pdrl78ufb3m32hr06uhijaq8s2vdhmaehr7rv0fsm8b3p469cru224jqs46vlqesbaf5a41vlstv87gempqo_0 arg_0)))

; base.Store

; Effect(DataDecl { modifier: Structural, bound: [🔣a/0], constructors: [(🔣Store.Store.put/0, Forall(|a/0 #0|(Arrow(vara (#0), Effect(Effects([App(Ref(#tkpo8b6903), vara (#0))]), Ref(#568rsi7o3g)))))), (🔣Store.Store.get/0, Forall(|a/0 #0|(Effect(Effects([App(Ref(#tkpo8b6903), vara (#0))]), vara (#0)))))] })

; Store.Store.put

(define tkpo8b6903kdcggkdolc9kgsq2175dhg0drn17134edkambevl7aahadvueaqml9pemnttm81t7569jbrifmrg82ogjnlfv5gbc2q6o_0 (lambda (arg_0) (call/cc (lambda (k) (throw-effect k (list 'tkpo8b6903kdcggkdolc9kgsq2175dhg0drn17134edkambevl7aahadvueaqml9pemnttm81t7569jbrifmrg82ogjnlfv5gbc2q6o_0 arg_0))))))

; Store.Store.get

(define tkpo8b6903kdcggkdolc9kgsq2175dhg0drn17134edkambevl7aahadvueaqml9pemnttm81t7569jbrifmrg82ogjnlfv5gbc2q6o_1 (lambda () (call/cc (lambda (k) (throw-effect k (list 'tkpo8b6903kdcggkdolc9kgsq2175dhg0drn17134edkambevl7aahadvueaqml9pemnttm81t7569jbrifmrg82ogjnlfv5gbc2q6o_1))))))

; base.io.Handle

; Data(DataDecl { modifier: Unique("d4597403ec40fd4fbee57c62b8096f9c3d382dff01f20108546fe3530a927e86"), bound: [], constructors: [(🔣io.Handle.Handle/0, Arrow(Ref(Text), Ref(#b7kh3q81n1)))] })

; io.Handle.Handle

(define b7kh3q81n1htidjsbuuk80a7kmm6qi62lidg0hbg8o0nph7e6eubqq6k43n50qaurghvgv8p1on925980ft1jsl3pd1snq0jtj86d4o_0 (lambda (arg_0) (list 'b7kh3q81n1htidjsbuuk80a7kmm6qi62lidg0hbg8o0nph7e6eubqq6k43n50qaurghvgv8p1on925980ft1jsl3pd1snq0jtj86d4o_0 arg_0)))

; base.Unit

; Data(DataDecl { modifier: Structural, bound: [], constructors: [(🔣Unit.Unit/0, Ref(#568rsi7o3g))] })

; Unit.Unit

(define 568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8_0 '568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8_0)

; base.Doc

; Data(DataDecl { modifier: Unique("c63a75b845e4f7d01107d852e4c2485c51a50aaaa94fc61995e71bbee983a2ac3713831264adb47fb6bd1e058d5f004"), bound: [], constructors: [(🔣Doc.Link/0, Arrow(Ref(#quhghs26t7), Ref(#v00j3buk6m))), (🔣Doc.Source/0, Arrow(Ref(#quhghs26t7), Ref(#v00j3buk6m))), (🔣Doc.Blob/0, Arrow(Ref(Text), Ref(#v00j3buk6m))), (🔣Doc.Join/0, Arrow(App(Ref(Sequence), Ref(#v00j3buk6m)), Ref(#v00j3buk6m))), (🔣Doc.Signature/0, Arrow(Ref(Link.Term), Ref(#v00j3buk6m))), (🔣Doc.Evaluate/0, Arrow(Ref(Link.Term), Ref(#v00j3buk6m)))] })

; Doc.Link

(define v00j3buk6m0588orsna0oeuukfdh7g3sandn22qmld0m2fr2jvm7hjd6bpm5jh6db6hom5ag14hqmg4emhlm4urdj9ibom7s2m5417o_0 (lambda (arg_0) (list 'v00j3buk6m0588orsna0oeuukfdh7g3sandn22qmld0m2fr2jvm7hjd6bpm5jh6db6hom5ag14hqmg4emhlm4urdj9ibom7s2m5417o_0 arg_0)))

; Doc.Source

(define v00j3buk6m0588orsna0oeuukfdh7g3sandn22qmld0m2fr2jvm7hjd6bpm5jh6db6hom5ag14hqmg4emhlm4urdj9ibom7s2m5417o_1 (lambda (arg_0) (list 'v00j3buk6m0588orsna0oeuukfdh7g3sandn22qmld0m2fr2jvm7hjd6bpm5jh6db6hom5ag14hqmg4emhlm4urdj9ibom7s2m5417o_1 arg_0)))

; Doc.Blob

(define v00j3buk6m0588orsna0oeuukfdh7g3sandn22qmld0m2fr2jvm7hjd6bpm5jh6db6hom5ag14hqmg4emhlm4urdj9ibom7s2m5417o_2 (lambda (arg_0) (list 'v00j3buk6m0588orsna0oeuukfdh7g3sandn22qmld0m2fr2jvm7hjd6bpm5jh6db6hom5ag14hqmg4emhlm4urdj9ibom7s2m5417o_2 arg_0)))

; Doc.Join

(define v00j3buk6m0588orsna0oeuukfdh7g3sandn22qmld0m2fr2jvm7hjd6bpm5jh6db6hom5ag14hqmg4emhlm4urdj9ibom7s2m5417o_3 (lambda (arg_0) (list 'v00j3buk6m0588orsna0oeuukfdh7g3sandn22qmld0m2fr2jvm7hjd6bpm5jh6db6hom5ag14hqmg4emhlm4urdj9ibom7s2m5417o_3 arg_0)))

; Doc.Signature

(define v00j3buk6m0588orsna0oeuukfdh7g3sandn22qmld0m2fr2jvm7hjd6bpm5jh6db6hom5ag14hqmg4emhlm4urdj9ibom7s2m5417o_4 (lambda (arg_0) (list 'v00j3buk6m0588orsna0oeuukfdh7g3sandn22qmld0m2fr2jvm7hjd6bpm5jh6db6hom5ag14hqmg4emhlm4urdj9ibom7s2m5417o_4 arg_0)))

; Doc.Evaluate

(define v00j3buk6m0588orsna0oeuukfdh7g3sandn22qmld0m2fr2jvm7hjd6bpm5jh6db6hom5ag14hqmg4emhlm4urdj9ibom7s2m5417o_5 (lambda (arg_0) (list 'v00j3buk6m0588orsna0oeuukfdh7g3sandn22qmld0m2fr2jvm7hjd6bpm5jh6db6hom5ag14hqmg4emhlm4urdj9ibom7s2m5417o_5 arg_0)))

; unison_random_mersenne._base_additions.Nat32.truncate32 : Some(Forall(|𝕖/0 #0|(Arrow(Ref(Nat), Effect(Effects([var𝕖 (#0)]), Ref(#d97e0jhkmd))))))

(print-processing "unison_random_mersenne._base_additions.Nat32.truncate32")

(define 0cf8bvomh8t2e93miojt6arf0sdotejc0mqr4ra547qiqkkabd38b3t2pa3vhrv819ecjnvvc500ce7hs0empnhd813isvs961nt940
  (lambda
   (n)
   (d97e0jhkmdospt1d5n3pdrl78ufb3m32hr06uhijaq8s2vdhmaehr7rv0fsm8b3p469cru224jqs46vlqesbaf5a41vlstv87gempqo_0
    ((Nat.shiftRight ((Nat.shiftLeft n) 32)) 32))))

; /end unison_random_mersenne._base_additions.Nat32.truncate32

(define unison_random_mersenne._base_additions.Nat32.truncate32 0cf8bvomh8t2e93miojt6arf0sdotejc0mqr4ra547qiqkkabd38b3t2pa3vhrv819ecjnvvc500ce7hs0empnhd813isvs961nt940)

; continuations._external.base.M1l.Store.withInitialValue.handler : Some(Forall(|a/0 #0|(Forall(|e/0 #0|(Forall(|v/0 #0|(Arrow(vara (#0), Effect(Effects([]), Arrow(App(App(Ref(Effect), App(Ref(#tkpo8b6903), vara (#0))), varv (#0)), Effect(Effects([vare (#0)]), varv (#0))))))))))))

(print-processing "continuations._external.base.M1l.Store.withInitialValue.handler")

(define 0mo0glpslbgs6mtu2u9a9lt36hpeo4fofeoq8herfsq05heqntt8oo2nbkhopbh7es04bfn9ck0grsvhkt3m5d4jhe6gp4t5lrif0u8
  (lambda
   (init)
   (lambda
    (p0forqan6n)
    (let
     ((tmp-match-head p0forqan6n))
     (let
      ((result
        (if
         (and (list? tmp-match-head) (equal? (length tmp-match-head) 2) (equal? (car tmp-match-head) 'pure))
         (let ((v (cadr tmp-match-head))) v)
         'fallthrough)))
      (if
       (equal? 'fallthrough result)
       (let
        ((result
          (if
           (and
            (list? tmp-match-head)
            (equal? (length tmp-match-head) 4)
            (equal? (car tmp-match-head) 'effect)
            (equal? (length (caddr tmp-match-head)) 1)
            (equal?
             (car (caddr tmp-match-head))
             'tkpo8b6903kdcggkdolc9kgsq2175dhg0drn17134edkambevl7aahadvueaqml9pemnttm81t7569jbrifmrg82ogjnlfv5gbc2q6o_1))
           (let
            ((k (cadr tmp-match-head)))
            (handle-ability
             (lambda () (k init))
             (0mo0glpslbgs6mtu2u9a9lt36hpeo4fofeoq8herfsq05heqntt8oo2nbkhopbh7es04bfn9ck0grsvhkt3m5d4jhe6gp4t5lrif0u8
              init)))
           'fallthrough)))
        (if
         (equal? 'fallthrough result)
         (let
          ((result
            (if
             (and
              (list? tmp-match-head)
              (equal? (length tmp-match-head) 4)
              (equal? (car tmp-match-head) 'effect)
              (equal? (length (caddr tmp-match-head)) 2)
              (equal?
               (car (caddr tmp-match-head))
               'tkpo8b6903kdcggkdolc9kgsq2175dhg0drn17134edkambevl7aahadvueaqml9pemnttm81t7569jbrifmrg82ogjnlfv5gbc2q6o_0))
             (let
              ((v (list-ref (caddr tmp-match-head) 1)))
              (let
               ((k (cadr tmp-match-head)))
               (handle-ability
                (lambda
                 ()
                 (k
                  568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8_0))
                (0mo0glpslbgs6mtu2u9a9lt36hpeo4fofeoq8herfsq05heqntt8oo2nbkhopbh7es04bfn9ck0grsvhkt3m5d4jhe6gp4t5lrif0u8
                 v))))
             'fallthrough)))
          (if (equal? 'fallthrough result) (rethrow-effect tmp-match-head) result))
         result))
       result))))))

; /end continuations._external.base.M1l.Store.withInitialValue.handler

(define continuations._external.base.M1l.Store.withInitialValue.handler 0mo0glpslbgs6mtu2u9a9lt36hpeo4fofeoq8herfsq05heqntt8oo2nbkhopbh7es04bfn9ck0grsvhkt3m5d4jhe6gp4t5lrif0u8)

; unison_random_mersenne._base_additions.Nat32.shiftRight : Some(Forall(|𝕖/0 #0|(Forall(|𝕖/0 #0|(Arrow(Ref(#d97e0jhkmd), Effect(Effects([var𝕖 (#0)]), Arrow(Ref(Nat), Effect(Effects([var𝕖 (#0)]), Ref(#d97e0jhkmd))))))))))

(print-processing "unison_random_mersenne._base_additions.Nat32.shiftRight")

(define 443ei922o2d3i84c4o75e0odmok3vd5eh30beipei7akos818ko68m4j8tjh3i3f6ipc4dhhcq3s0gso59mu8suga042dvier4l8ih8
  (lambda
   (n)
   (lambda
    (b)
    (let
     ((tmp-match-head n))
     (let
      ((result
        (let
         ((tmp-0 tmp-match-head))
         (if
          (and
           (list? tmp-0)
           (equal?
            'd97e0jhkmdospt1d5n3pdrl78ufb3m32hr06uhijaq8s2vdhmaehr7rv0fsm8b3p469cru224jqs46vlqesbaf5a41vlstv87gempqo_0
            (list-ref tmp-0 0))
           (equal? (length tmp-0) 2))
          (let
           ((n-quot (list-ref tmp-0 1)))
           (d97e0jhkmdospt1d5n3pdrl78ufb3m32hr06uhijaq8s2vdhmaehr7rv0fsm8b3p469cru224jqs46vlqesbaf5a41vlstv87gempqo_0
            ((Nat.shiftRight n-quot) b)))
          'fallthrough))))
      (if (equal? 'fallthrough result) (no-match) result))))))

; /end unison_random_mersenne._base_additions.Nat32.shiftRight

(define unison_random_mersenne._base_additions.Nat32.shiftRight 443ei922o2d3i84c4o75e0odmok3vd5eh30beipei7akos818ko68m4j8tjh3i3f6ipc4dhhcq3s0gso59mu8suga042dvier4l8ih8)

; unison_random_mersenne._base_additions.Nat32.xor : Some(Forall(|𝕖/0 #0|(Forall(|𝕖/0 #0|(Arrow(Ref(#d97e0jhkmd), Effect(Effects([var𝕖 (#0)]), Arrow(Ref(#d97e0jhkmd), Effect(Effects([var𝕖 (#0)]), Ref(#d97e0jhkmd))))))))))

(print-processing "unison_random_mersenne._base_additions.Nat32.xor")

(define 4c6fis38j5nc86vcriefqvjs7fqfu5mdvqg6fkkltjgnqf6r000vggkof5l2569q87n2hnc7e8uakgfe690j53jckriu3ik8sbgpbi0
  (lambda
   (a)
   (lambda
    (b)
    (let
     ((tmp-match-head a))
     (let
      ((result
        (let
         ((tmp-0 tmp-match-head))
         (if
          (and
           (list? tmp-0)
           (equal?
            'd97e0jhkmdospt1d5n3pdrl78ufb3m32hr06uhijaq8s2vdhmaehr7rv0fsm8b3p469cru224jqs46vlqesbaf5a41vlstv87gempqo_0
            (list-ref tmp-0 0))
           (equal? (length tmp-0) 2))
          (let
           ((a-quot (list-ref tmp-0 1)))
           (let
            ((tmp-match-head b))
            (let
             ((result
               (let
                ((tmp-0 tmp-match-head))
                (if
                 (and
                  (list? tmp-0)
                  (equal?
                   'd97e0jhkmdospt1d5n3pdrl78ufb3m32hr06uhijaq8s2vdhmaehr7rv0fsm8b3p469cru224jqs46vlqesbaf5a41vlstv87gempqo_0
                   (list-ref tmp-0 0))
                  (equal? (length tmp-0) 2))
                 (let
                  ((b-quot (list-ref tmp-0 1)))
                  (d97e0jhkmdospt1d5n3pdrl78ufb3m32hr06uhijaq8s2vdhmaehr7rv0fsm8b3p469cru224jqs46vlqesbaf5a41vlstv87gempqo_0
                   ((Nat.xor a-quot) b-quot)))
                 'fallthrough))))
             (if (equal? 'fallthrough result) (no-match) result))))
          'fallthrough))))
      (if (equal? 'fallthrough result) (no-match) result))))))

; /end unison_random_mersenne._base_additions.Nat32.xor

(define unison_random_mersenne._base_additions.Nat32.xor 4c6fis38j5nc86vcriefqvjs7fqfu5mdvqg6fkkltjgnqf6r000vggkof5l2569q87n2hnc7e8uakgfe690j53jckriu3ik8sbgpbi0)

; unison_random_mersenne._base_additions.Nat32.+ : Some(Forall(|𝕖/0 #0|(Forall(|𝕖/0 #0|(Arrow(Ref(#d97e0jhkmd), Effect(Effects([var𝕖 (#0)]), Arrow(Ref(#d97e0jhkmd), Effect(Effects([var𝕖 (#0)]), Ref(#d97e0jhkmd))))))))))

(print-processing "unison_random_mersenne._base_additions.Nat32.+")

(define 4font96ujkk61k20294c82c2m432233i6akg6017q2j4u9ng48c8su2hqd35rjmucnkodhljp1vnkqr3h6mnv0bj8ej8eqk3ikqaja8
  (lambda
   (a)
   (lambda
    (b)
    (let
     ((tmp-match-head a))
     (let
      ((result
        (let
         ((tmp-0 tmp-match-head))
         (if
          (and
           (list? tmp-0)
           (equal?
            'd97e0jhkmdospt1d5n3pdrl78ufb3m32hr06uhijaq8s2vdhmaehr7rv0fsm8b3p469cru224jqs46vlqesbaf5a41vlstv87gempqo_0
            (list-ref tmp-0 0))
           (equal? (length tmp-0) 2))
          (let
           ((a-quot (list-ref tmp-0 1)))
           (let
            ((tmp-match-head b))
            (let
             ((result
               (let
                ((tmp-0 tmp-match-head))
                (if
                 (and
                  (list? tmp-0)
                  (equal?
                   'd97e0jhkmdospt1d5n3pdrl78ufb3m32hr06uhijaq8s2vdhmaehr7rv0fsm8b3p469cru224jqs46vlqesbaf5a41vlstv87gempqo_0
                   (list-ref tmp-0 0))
                  (equal? (length tmp-0) 2))
                 (let
                  ((b-quot (list-ref tmp-0 1)))
                  (0cf8bvomh8t2e93miojt6arf0sdotejc0mqr4ra547qiqkkabd38b3t2pa3vhrv819ecjnvvc500ce7hs0empnhd813isvs961nt940
                   ((Nat.+ a-quot) b-quot)))
                 'fallthrough))))
             (if (equal? 'fallthrough result) (no-match) result))))
          'fallthrough))))
      (if (equal? 'fallthrough result) (no-match) result))))))

; /end unison_random_mersenne._base_additions.Nat32.+

(define unison_random_mersenne._base_additions.Nat32.+ 4font96ujkk61k20294c82c2m432233i6akg6017q2j4u9ng48c8su2hqd35rjmucnkodhljp1vnkqr3h6mnv0bj8ej8eqk3ikqaja8)

; base.List.unfold : Some(Forall(|𝕖/0 #0|(Forall(|𝕖111/0 #0|(Forall(|a/0 #0|(Forall(|s/0 #0|(Arrow(vars (#0), Effect(Effects([var𝕖111 (#0)]), Arrow(Arrow(vars (#0), Effect(Effects([var𝕖 (#0)]), App(Ref(#5isltsdct9), App(App(Ref(#onbcm0qctb), vara (#0)), App(App(Ref(#onbcm0qctb), vars (#0)), Ref(#568rsi7o3g)))))), Effect(Effects([var𝕖 (#0)]), App(Ref(Sequence), vara (#0)))))))))))))))

(print-processing "base.List.unfold")

(define 4omhgrnp2n42sku6t4kpg38rpe4qbvqc7l909a9v5650msj11ropb7s3ndptev3bimrh23fh120vajcu8h86o43pbofnd6b9dqtv6do
  (lambda
   (s0)
   (lambda
    (f)
    (letrec
     ((go
       (lambda
        (f)
        (lambda
         (s)
         (lambda
          (acc)
          (let
           ((tmp-match-head (f s)))
           (let
            ((result
              (if
               (equal?
                '5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_0
                tmp-match-head)
               acc
               'fallthrough)))
            (if
             (equal? 'fallthrough result)
             (let
              ((result
                (let
                 ((tmp-0 tmp-match-head))
                 (if
                  (and
                   (list? tmp-0)
                   (equal?
                    '5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_1
                    (list-ref tmp-0 0))
                   (equal? (length tmp-0) 2))
                  (let
                   ((tmp-1 (list-ref tmp-0 1)))
                   (if
                    (and
                     (list? tmp-1)
                     (equal?
                      'onbcm0qctbnuctpm57tkc5p16b8gfke8thjf19p4r4laokji0b606rd0frnhj103qb90lve3fohkoc1eda70491hot656s1m6kk3cn0_0
                      (list-ref tmp-1 0))
                     (equal? (length tmp-1) 3))
                    (let
                     ((a (list-ref tmp-1 1)))
                     (let
                      ((tmp-2 (list-ref tmp-1 2)))
                      (if
                       (and
                        (list? tmp-2)
                        (equal?
                         'onbcm0qctbnuctpm57tkc5p16b8gfke8thjf19p4r4laokji0b606rd0frnhj103qb90lve3fohkoc1eda70491hot656s1m6kk3cn0_0
                         (list-ref tmp-2 0))
                        (equal? (length tmp-2) 3))
                       (let
                        ((s (list-ref tmp-2 1)))
                        (if
                         (equal?
                          '568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8_0
                          (list-ref tmp-2 2))
                         (((go f) s) ((List.snoc acc) a))
                         'fallthrough))
                       'fallthrough)))
                    'fallthrough))
                  'fallthrough))))
              (if (equal? 'fallthrough result) (no-match) result))
             result))))))))
     (((go f) s0) (vector ))))))

; /end base.List.unfold

(define base.List.unfold 4omhgrnp2n42sku6t4kpg38rpe4qbvqc7l909a9v5650msj11ropb7s3ndptev3bimrh23fh120vajcu8h86o43pbofnd6b9dqtv6do)

; unison_random_mersenne._base_additions.Nat32.* : Some(Forall(|𝕖/0 #0|(Forall(|𝕖/0 #0|(Arrow(Ref(#d97e0jhkmd), Effect(Effects([var𝕖 (#0)]), Arrow(Ref(#d97e0jhkmd), Effect(Effects([var𝕖 (#0)]), Ref(#d97e0jhkmd))))))))))

(print-processing "unison_random_mersenne._base_additions.Nat32.*")

(define 62rt8ihbsbb2cklq0cn9hbo7nisvbetfmos19i9ndrlmf2r87ksnp95covpns5th7hnau9b0n5bgu0lupe8s512c5lnl56ke4lc2hf0
  (lambda
   (a)
   (lambda
    (b)
    (let
     ((tmp-match-head a))
     (let
      ((result
        (let
         ((tmp-0 tmp-match-head))
         (if
          (and
           (list? tmp-0)
           (equal?
            'd97e0jhkmdospt1d5n3pdrl78ufb3m32hr06uhijaq8s2vdhmaehr7rv0fsm8b3p469cru224jqs46vlqesbaf5a41vlstv87gempqo_0
            (list-ref tmp-0 0))
           (equal? (length tmp-0) 2))
          (let
           ((a-quot (list-ref tmp-0 1)))
           (let
            ((tmp-match-head b))
            (let
             ((result
               (let
                ((tmp-0 tmp-match-head))
                (if
                 (and
                  (list? tmp-0)
                  (equal?
                   'd97e0jhkmdospt1d5n3pdrl78ufb3m32hr06uhijaq8s2vdhmaehr7rv0fsm8b3p469cru224jqs46vlqesbaf5a41vlstv87gempqo_0
                   (list-ref tmp-0 0))
                  (equal? (length tmp-0) 2))
                 (let
                  ((b-quot (list-ref tmp-0 1)))
                  (0cf8bvomh8t2e93miojt6arf0sdotejc0mqr4ra547qiqkkabd38b3t2pa3vhrv819ecjnvvc500ce7hs0empnhd813isvs961nt940
                   ((Nat.* a-quot) b-quot)))
                 'fallthrough))))
             (if (equal? 'fallthrough result) (no-match) result))))
          'fallthrough))))
      (if (equal? 'fallthrough result) (no-match) result))))))

; /end unison_random_mersenne._base_additions.Nat32.*

(define unison_random_mersenne._base_additions.Nat32.* 62rt8ihbsbb2cklq0cn9hbo7nisvbetfmos19i9ndrlmf2r87ksnp95covpns5th7hnau9b0n5bgu0lupe8s512c5lnl56ke4lc2hf0)

; unison_random_mersenne._base_additions.Nat32.<= : Some(Forall(|𝕖/0 #0|(Forall(|𝕖/0 #0|(Arrow(Ref(#d97e0jhkmd), Effect(Effects([var𝕖 (#0)]), Arrow(Ref(#d97e0jhkmd), Effect(Effects([var𝕖 (#0)]), Ref(Boolean))))))))))

(print-processing "unison_random_mersenne._base_additions.Nat32.<=")

(define 7ucnfm0idg5vbks422mptn66a8fja8eqndi34bfe1eej9e8d1t3tcs7i4d6g14ilhlo05i1av4rn6uep8rbt2pe64c3vrp3ak677e08
  (lambda
   (a)
   (lambda
    (b)
    (let
     ((tmp-match-head a))
     (let
      ((result
        (let
         ((tmp-0 tmp-match-head))
         (if
          (and
           (list? tmp-0)
           (equal?
            'd97e0jhkmdospt1d5n3pdrl78ufb3m32hr06uhijaq8s2vdhmaehr7rv0fsm8b3p469cru224jqs46vlqesbaf5a41vlstv87gempqo_0
            (list-ref tmp-0 0))
           (equal? (length tmp-0) 2))
          (let
           ((a-quot (list-ref tmp-0 1)))
           (let
            ((tmp-match-head b))
            (let
             ((result
               (let
                ((tmp-0 tmp-match-head))
                (if
                 (and
                  (list? tmp-0)
                  (equal?
                   'd97e0jhkmdospt1d5n3pdrl78ufb3m32hr06uhijaq8s2vdhmaehr7rv0fsm8b3p469cru224jqs46vlqesbaf5a41vlstv87gempqo_0
                   (list-ref tmp-0 0))
                  (equal? (length tmp-0) 2))
                 (let ((b-quot (list-ref tmp-0 1))) ((Universal.<= a-quot) b-quot))
                 'fallthrough))))
             (if (equal? 'fallthrough result) (no-match) result))))
          'fallthrough))))
      (if (equal? 'fallthrough result) (no-match) result))))))

; /end unison_random_mersenne._base_additions.Nat32.<=

(define unison_random_mersenne._base_additions.Nat32.<= 7ucnfm0idg5vbks422mptn66a8fja8eqndi34bfe1eej9e8d1t3tcs7i4d6g14ilhlo05i1av4rn6uep8rbt2pe64c3vrp3ak677e08)

; unison_random_mersenne.random.mersenne.seed : Some(Arrow(Ref(#d97e0jhkmd), Effect(Effects([App(Ref(#tkpo8b6903), Ref(#80gg7kgilb))]), Ref(#568rsi7o3g))))

(print-processing "unison_random_mersenne.random.mersenne.seed")

(define al7m174na7iu5al6p9je0uhjal73ek183q41m8nr9gn543ck63qsnbcej8qea6m8qdraccgpg9pg439oe4p677vb2f0hpkst0r6qsd8
  (lambda
   (s)
   (let
    ((next
      (lambda
       (index)
       (lambda
        (last)
        ((4font96ujkk61k20294c82c2m432233i6akg6017q2j4u9ng48c8su2hqd35rjmucnkodhljp1vnkqr3h6mnv0bj8ej8eqk3ikqaja8
          ((62rt8ihbsbb2cklq0cn9hbo7nisvbetfmos19i9ndrlmf2r87ksnp95covpns5th7hnau9b0n5bgu0lupe8s512c5lnl56ke4lc2hf0
            (d97e0jhkmdospt1d5n3pdrl78ufb3m32hr06uhijaq8s2vdhmaehr7rv0fsm8b3p469cru224jqs46vlqesbaf5a41vlstv87gempqo_0
             1812433253))
           ((4c6fis38j5nc86vcriefqvjs7fqfu5mdvqg6fkkltjgnqf6r000vggkof5l2569q87n2hnc7e8uakgfe690j53jckriu3ik8sbgpbi0
             last)
            ((443ei922o2d3i84c4o75e0odmok3vd5eh30beipei7akos818ko68m4j8tjh3i3f6ipc4dhhcq3s0gso59mu8suga042dvier4l8ih8
              last)
             30))))
         index)))))
    (let
     ((next-quot
       (lambda
        (tckbftuo85)
        (let
         ((tmp-match-head tckbftuo85))
         (let
          ((result
            (let
             ((tmp-0 tmp-match-head))
             (if
              (and
               (list? tmp-0)
               (equal?
                'onbcm0qctbnuctpm57tkc5p16b8gfke8thjf19p4r4laokji0b606rd0frnhj103qb90lve3fohkoc1eda70491hot656s1m6kk3cn0_0
                (list-ref tmp-0 0))
               (equal? (length tmp-0) 3))
              (let
               ((index (list-ref tmp-0 1)))
               (let
                ((tmp-1 (list-ref tmp-0 2)))
                (if
                 (and
                  (list? tmp-1)
                  (equal?
                   'onbcm0qctbnuctpm57tkc5p16b8gfke8thjf19p4r4laokji0b606rd0frnhj103qb90lve3fohkoc1eda70491hot656s1m6kk3cn0_0
                   (list-ref tmp-1 0))
                  (equal? (length tmp-1) 3))
                 (let
                  ((last (list-ref tmp-1 1)))
                  (if
                   (equal?
                    '568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8_0
                    (list-ref tmp-1 2))
                   (if
                    ((7ucnfm0idg5vbks422mptn66a8fja8eqndi34bfe1eej9e8d1t3tcs7i4d6g14ilhlo05i1av4rn6uep8rbt2pe64c3vrp3ak677e08
                      index)
                     (d97e0jhkmdospt1d5n3pdrl78ufb3m32hr06uhijaq8s2vdhmaehr7rv0fsm8b3p469cru224jqs46vlqesbaf5a41vlstv87gempqo_0
                      624))
                    (5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_1
                     ((onbcm0qctbnuctpm57tkc5p16b8gfke8thjf19p4r4laokji0b606rd0frnhj103qb90lve3fohkoc1eda70491hot656s1m6kk3cn0_0
                       last)
                      ((onbcm0qctbnuctpm57tkc5p16b8gfke8thjf19p4r4laokji0b606rd0frnhj103qb90lve3fohkoc1eda70491hot656s1m6kk3cn0_0
                        ((onbcm0qctbnuctpm57tkc5p16b8gfke8thjf19p4r4laokji0b606rd0frnhj103qb90lve3fohkoc1eda70491hot656s1m6kk3cn0_0
                          ((4font96ujkk61k20294c82c2m432233i6akg6017q2j4u9ng48c8su2hqd35rjmucnkodhljp1vnkqr3h6mnv0bj8ej8eqk3ikqaja8
                            index)
                           (d97e0jhkmdospt1d5n3pdrl78ufb3m32hr06uhijaq8s2vdhmaehr7rv0fsm8b3p469cru224jqs46vlqesbaf5a41vlstv87gempqo_0
                            1)))
                         ((onbcm0qctbnuctpm57tkc5p16b8gfke8thjf19p4r4laokji0b606rd0frnhj103qb90lve3fohkoc1eda70491hot656s1m6kk3cn0_0
                           ((next index) last))
                          568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8_0)))
                       568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8_0)))
                    5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_0)
                   'fallthrough))
                 'fallthrough)))
              'fallthrough))))
          (if (equal? 'fallthrough result) (no-match) result))))))
     (let
      ((state
        ((4omhgrnp2n42sku6t4kpg38rpe4qbvqc7l909a9v5650msj11ropb7s3ndptev3bimrh23fh120vajcu8h86o43pbofnd6b9dqtv6do
          ((onbcm0qctbnuctpm57tkc5p16b8gfke8thjf19p4r4laokji0b606rd0frnhj103qb90lve3fohkoc1eda70491hot656s1m6kk3cn0_0
            (d97e0jhkmdospt1d5n3pdrl78ufb3m32hr06uhijaq8s2vdhmaehr7rv0fsm8b3p469cru224jqs46vlqesbaf5a41vlstv87gempqo_0
             1))
           ((onbcm0qctbnuctpm57tkc5p16b8gfke8thjf19p4r4laokji0b606rd0frnhj103qb90lve3fohkoc1eda70491hot656s1m6kk3cn0_0
             s)
            568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8_0)))
         next-quot)))
      (tkpo8b6903kdcggkdolc9kgsq2175dhg0drn17134edkambevl7aahadvueaqml9pemnttm81t7569jbrifmrg82ogjnlfv5gbc2q6o_0
       (80gg7kgilboe8aq4q8qj3oq9cfjj795m5pmgc7k5r2ne76qn79e33e8gk9q7pvgjpkompfv6po8k0rhi2u1g3r6agcgkl6etlv202no_0
        state)))))))

; /end unison_random_mersenne.random.mersenne.seed

(define unison_random_mersenne.random.mersenne.seed al7m174na7iu5al6p9je0uhjal73ek183q41m8nr9gn543ck63qsnbcej8qea6m8qdraccgpg9pg439oe4p677vb2f0hpkst0r6qsd8)

; unison_random_mersenne._base_additions.iterate : Some(Forall(|𝕖/0 #0|(Forall(|a/0 #0|(Forall(|e/0 #0|(Arrow(Ref(Nat), Effect(Effects([var𝕖 (#0)]), Arrow(Arrow(Ref(#568rsi7o3g), Effect(Effects([vare (#0)]), vara (#0))), Effect(Effects([vare (#0)]), App(Ref(Sequence), vara (#0)))))))))))))

(print-processing "unison_random_mersenne._base_additions.iterate")

(define apd2pd0ob7k20kqui8q43ns0840dbubc8p5tuh183n2q4d44qmqnfnhk3vpga0m6r8ci4e6jlhlu52vbpkj1p28tsgchm2m7tss620o
  (lambda
   (n)
   (lambda
    (op)
    ((4omhgrnp2n42sku6t4kpg38rpe4qbvqc7l909a9v5650msj11ropb7s3ndptev3bimrh23fh120vajcu8h86o43pbofnd6b9dqtv6do
      n)
     (lambda
      (n)
      (if
       ((Universal.== n) 0)
       5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_0
       (5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_1
        ((onbcm0qctbnuctpm57tkc5p16b8gfke8thjf19p4r4laokji0b606rd0frnhj103qb90lve3fohkoc1eda70491hot656s1m6kk3cn0_0
          (op
           568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8_0))
         ((onbcm0qctbnuctpm57tkc5p16b8gfke8thjf19p4r4laokji0b606rd0frnhj103qb90lve3fohkoc1eda70491hot656s1m6kk3cn0_0
           ((Nat.drop n) 1))
          568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8_0)))))))))

; /end unison_random_mersenne._base_additions.iterate

(define unison_random_mersenne._base_additions.iterate apd2pd0ob7k20kqui8q43ns0840dbubc8p5tuh183n2q4d44qmqnfnhk3vpga0m6r8ci4e6jlhlu52vbpkj1p28tsgchm2m7tss620o)

; unison_random_mersenne.random.mersenne.collect : Some(Forall(|𝕖/0 #0|(Forall(|𝕖/0 #0|(Forall(|𝕖/0 #0|(Forall(|x/0 #0|(Arrow(Arrow(Ref(#568rsi7o3g), Effect(Effects([App(Ref(#tkpo8b6903), Ref(#80gg7kgilb))]), varx (#0))), Effect(Effects([var𝕖 (#0)]), Arrow(Ref(#d97e0jhkmd), Effect(Effects([var𝕖 (#0)]), Arrow(Ref(Nat), Effect(Effects([var𝕖 (#0)]), App(Ref(Sequence), varx (#0)))))))))))))))))

(print-processing "unison_random_mersenne.random.mersenne.collect")

(define 49dfoqk8ri2sa1n5hrd1m1hc7vqdkorl4jqnvbsmht268lsnpopt6tsrpd4ll4rg93fmmuh1g660n5oujo66h8tnqi2fagp3nk451r0
  (lambda
   (next-quot)
   (lambda
    (s)
    (lambda
     (n)
     (handle-ability
      (lambda
       ()
       (let
        ((_1
          (al7m174na7iu5al6p9je0uhjal73ek183q41m8nr9gn543ck63qsnbcej8qea6m8qdraccgpg9pg439oe4p677vb2f0hpkst0r6qsd8
           s)))
        ((apd2pd0ob7k20kqui8q43ns0840dbubc8p5tuh183n2q4d44qmqnfnhk3vpga0m6r8ci4e6jlhlu52vbpkj1p28tsgchm2m7tss620o
          n)
         next-quot)))
      (0mo0glpslbgs6mtu2u9a9lt36hpeo4fofeoq8herfsq05heqntt8oo2nbkhopbh7es04bfn9ck0grsvhkt3m5d4jhe6gp4t5lrif0u8
       (80gg7kgilboe8aq4q8qj3oq9cfjj795m5pmgc7k5r2ne76qn79e33e8gk9q7pvgjpkompfv6po8k0rhi2u1g3r6agcgkl6etlv202no_0
        (vector ))))))))

; /end unison_random_mersenne.random.mersenne.collect

(define unison_random_mersenne.random.mersenne.collect 49dfoqk8ri2sa1n5hrd1m1hc7vqdkorl4jqnvbsmht268lsnpopt6tsrpd4ll4rg93fmmuh1g660n5oujo66h8tnqi2fagp3nk451r0)

; unison_random_mersenne._base_additions.Nat32.complement : Some(Forall(|𝕖/0 #0|(Arrow(Ref(#d97e0jhkmd), Effect(Effects([var𝕖 (#0)]), Ref(#d97e0jhkmd))))))

(print-processing "unison_random_mersenne._base_additions.Nat32.complement")

(define aq040co1g4nrou525j1snc68l0os6nkj9bu9k5ssdm9q21gpn1uhc771ropk1lpmngh840736ho58a5csu1querjlgejo6k1j4iusvg
  (lambda
   (dhdh9c313i)
   (let
    ((tmp-match-head dhdh9c313i))
    (let
     ((result
       (let
        ((tmp-0 tmp-match-head))
        (if
         (and
          (list? tmp-0)
          (equal?
           'd97e0jhkmdospt1d5n3pdrl78ufb3m32hr06uhijaq8s2vdhmaehr7rv0fsm8b3p469cru224jqs46vlqesbaf5a41vlstv87gempqo_0
           (list-ref tmp-0 0))
          (equal? (length tmp-0) 2))
         (let
          ((a-quot (list-ref tmp-0 1)))
          (0cf8bvomh8t2e93miojt6arf0sdotejc0mqr4ra547qiqkkabd38b3t2pa3vhrv819ecjnvvc500ce7hs0empnhd813isvs961nt940
           (Nat.complement a-quot)))
         'fallthrough))))
     (if (equal? 'fallthrough result) (no-match) result)))))

; /end unison_random_mersenne._base_additions.Nat32.complement

(define unison_random_mersenne._base_additions.Nat32.complement aq040co1g4nrou525j1snc68l0os6nkj9bu9k5ssdm9q21gpn1uhc771ropk1lpmngh840736ho58a5csu1querjlgejo6k1j4iusvg)

; unison_random_mersenne._base_additions.Nat32.shiftLeft : Some(Forall(|𝕖/0 #0|(Forall(|𝕖/0 #0|(Arrow(Ref(#d97e0jhkmd), Effect(Effects([var𝕖 (#0)]), Arrow(Ref(Nat), Effect(Effects([var𝕖 (#0)]), Ref(#d97e0jhkmd))))))))))

(print-processing "unison_random_mersenne._base_additions.Nat32.shiftLeft")

(define vubdg5ln9astm9mmlo7ev20kkjj38af12b4mg2c9n56uascubn8c87a1ckn4526krenh33cadqhhuul9nldl7bhpii648e0cku5cdao
  (lambda
   (n)
   (lambda
    (b)
    (let
     ((tmp-match-head n))
     (let
      ((result
        (let
         ((tmp-0 tmp-match-head))
         (if
          (and
           (list? tmp-0)
           (equal?
            'd97e0jhkmdospt1d5n3pdrl78ufb3m32hr06uhijaq8s2vdhmaehr7rv0fsm8b3p469cru224jqs46vlqesbaf5a41vlstv87gempqo_0
            (list-ref tmp-0 0))
           (equal? (length tmp-0) 2))
          (let
           ((n-quot (list-ref tmp-0 1)))
           (0cf8bvomh8t2e93miojt6arf0sdotejc0mqr4ra547qiqkkabd38b3t2pa3vhrv819ecjnvvc500ce7hs0empnhd813isvs961nt940
            ((Nat.shiftLeft n-quot) b)))
          'fallthrough))))
      (if (equal? 'fallthrough result) (no-match) result))))))

; /end unison_random_mersenne._base_additions.Nat32.shiftLeft

(define unison_random_mersenne._base_additions.Nat32.shiftLeft vubdg5ln9astm9mmlo7ev20kkjj38af12b4mg2c9n56uascubn8c87a1ckn4526krenh33cadqhhuul9nldl7bhpii648e0cku5cdao)

; unison_random_mersenne._base_additions.Nat32.bit : Some(Forall(|𝕖/0 #0|(Arrow(Ref(Nat), Effect(Effects([var𝕖 (#0)]), Ref(#d97e0jhkmd))))))

(print-processing "unison_random_mersenne._base_additions.Nat32.bit")

(define glts8qofgo284gk3dnovfj2dges1n3b2amd2jk45rbl4s3mjbo5gent5n1mv7jsgnlqep879vgfg5j0li39jsmffr01ldrl623tjkag
  (lambda
   (i)
   ((vubdg5ln9astm9mmlo7ev20kkjj38af12b4mg2c9n56uascubn8c87a1ckn4526krenh33cadqhhuul9nldl7bhpii648e0cku5cdao
     (d97e0jhkmdospt1d5n3pdrl78ufb3m32hr06uhijaq8s2vdhmaehr7rv0fsm8b3p469cru224jqs46vlqesbaf5a41vlstv87gempqo_0
      1))
    i)))

; /end unison_random_mersenne._base_additions.Nat32.bit

(define unison_random_mersenne._base_additions.Nat32.bit glts8qofgo284gk3dnovfj2dges1n3b2amd2jk45rbl4s3mjbo5gent5n1mv7jsgnlqep879vgfg5j0li39jsmffr01ldrl623tjkag)

; unison_random_mersenne._base_additions.Nat32.and : Some(Forall(|𝕖/0 #0|(Forall(|𝕖/0 #0|(Arrow(Ref(#d97e0jhkmd), Effect(Effects([var𝕖 (#0)]), Arrow(Ref(#d97e0jhkmd), Effect(Effects([var𝕖 (#0)]), Ref(#d97e0jhkmd))))))))))

(print-processing "unison_random_mersenne._base_additions.Nat32.and")

(define hl4e732fqm204op26k9ojkuu0fmfjksr5fcknmig1akevrthiu1quscc6jj6rlo9eb8quuku07m6c0q75q11s0o9irb9jc9nbmlscbo
  (lambda
   (a)
   (lambda
    (b)
    (let
     ((tmp-match-head a))
     (let
      ((result
        (let
         ((tmp-0 tmp-match-head))
         (if
          (and
           (list? tmp-0)
           (equal?
            'd97e0jhkmdospt1d5n3pdrl78ufb3m32hr06uhijaq8s2vdhmaehr7rv0fsm8b3p469cru224jqs46vlqesbaf5a41vlstv87gempqo_0
            (list-ref tmp-0 0))
           (equal? (length tmp-0) 2))
          (let
           ((a-quot (list-ref tmp-0 1)))
           (let
            ((tmp-match-head b))
            (let
             ((result
               (let
                ((tmp-0 tmp-match-head))
                (if
                 (and
                  (list? tmp-0)
                  (equal?
                   'd97e0jhkmdospt1d5n3pdrl78ufb3m32hr06uhijaq8s2vdhmaehr7rv0fsm8b3p469cru224jqs46vlqesbaf5a41vlstv87gempqo_0
                   (list-ref tmp-0 0))
                  (equal? (length tmp-0) 2))
                 (let
                  ((b-quot (list-ref tmp-0 1)))
                  (d97e0jhkmdospt1d5n3pdrl78ufb3m32hr06uhijaq8s2vdhmaehr7rv0fsm8b3p469cru224jqs46vlqesbaf5a41vlstv87gempqo_0
                   ((Nat.and a-quot) b-quot)))
                 'fallthrough))))
             (if (equal? 'fallthrough result) (no-match) result))))
          'fallthrough))))
      (if (equal? 'fallthrough result) (no-match) result))))))

; /end unison_random_mersenne._base_additions.Nat32.and

(define unison_random_mersenne._base_additions.Nat32.and hl4e732fqm204op26k9ojkuu0fmfjksr5fcknmig1akevrthiu1quscc6jj6rlo9eb8quuku07m6c0q75q11s0o9irb9jc9nbmlscbo)

; testbed.clearBit : Some(Forall(|𝕖/0 #0|(Forall(|𝕖/0 #0|(Arrow(Ref(#d97e0jhkmd), Effect(Effects([var𝕖 (#0)]), Arrow(Ref(Nat), Effect(Effects([var𝕖 (#0)]), Ref(#d97e0jhkmd))))))))))

(print-processing "testbed.clearBit")

(define 77hkmnfup86ippah57kadki0i2s86keupous5f2f8tq3r7n2h9cf148e0cahil650tq3ch6fonjnvj1ktc2fcb252m909jacl18877o
  (lambda
   (n)
   (lambda
    (i)
    ((hl4e732fqm204op26k9ojkuu0fmfjksr5fcknmig1akevrthiu1quscc6jj6rlo9eb8quuku07m6c0q75q11s0o9irb9jc9nbmlscbo
      n)
     (aq040co1g4nrou525j1snc68l0os6nkj9bu9k5ssdm9q21gpn1uhc771ropk1lpmngh840736ho58a5csu1querjlgejo6k1j4iusvg
      (glts8qofgo284gk3dnovfj2dges1n3b2amd2jk45rbl4s3mjbo5gent5n1mv7jsgnlqep879vgfg5j0li39jsmffr01ldrl623tjkag
       i))))))

; /end testbed.clearBit

(define testbed.clearBit 77hkmnfup86ippah57kadki0i2s86keupous5f2f8tq3r7n2h9cf148e0cahil650tq3ch6fonjnvj1ktc2fcb252m909jacl18877o)

; continuations._external.base.M1l.io.stdout : Some(Ref(#b7kh3q81n1))

(print-processing "continuations._external.base.M1l.io.stdout")

(define 7plc1gmlvn6ti9lpq4ufa4uvdal6elieppmcgvr0d0scsk9e3edakgnk09covbel3hjcjlsi7rmn8auhi935fo9iibed7krpfr337fo
  (b7kh3q81n1htidjsbuuk80a7kmm6qi62lidg0hbg8o0nph7e6eubqq6k43n50qaurghvgv8p1on925980ft1jsl3pd1snq0jtj86d4o_0
   "stdout"))

; /end continuations._external.base.M1l.io.stdout

(define continuations._external.base.M1l.io.stdout 7plc1gmlvn6ti9lpq4ufa4uvdal6elieppmcgvr0d0scsk9e3edakgnk09covbel3hjcjlsi7rmn8auhi935fo9iibed7krpfr337fo)

; base.List.unsafeAt : Some(Forall(|𝕖/0 #0|(Forall(|𝕖111/0 #0|(Forall(|a/0 #0|(Arrow(Ref(Nat), Effect(Effects([var𝕖111 (#0)]), Arrow(App(Ref(Sequence), vara (#0)), Effect(Effects([var𝕖 (#0)]), vara (#0))))))))))))

(print-processing "base.List.unsafeAt")

(define etge4adj957jmmosh03foaafoju2rjidbu8hmv0nnqkt3rg80q50hnn7ukr7ve9lmfgjjsm5ul5eeq7b8vbk1j6u4bopov2j4r3u2v8
  (lambda
   (n)
   (lambda
    (as)
    (let
     ((tmp-match-head ((List.at n) as)))
     (let
      ((result
        (let
         ((tmp-0 tmp-match-head))
         (if
          (and
           (list? tmp-0)
           (equal?
            '5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_1
            (list-ref tmp-0 0))
           (equal? (length tmp-0) 2))
          (let ((a (list-ref tmp-0 1))) a)
          'fallthrough))))
      (if
       (equal? 'fallthrough result)
       (let
        ((result
          (if
           (equal?
            '5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_0
            tmp-match-head)
           ((Debug.watch "oh noes")
            ((etge4adj957jmmosh03foaafoju2rjidbu8hmv0nnqkt3rg80q50hnn7ukr7ve9lmfgjjsm5ul5eeq7b8vbk1j6u4bopov2j4r3u2v8
              n)
             as))
           'fallthrough)))
        (if (equal? 'fallthrough result) (no-match) result))
       result))))))

; /end base.List.unsafeAt

(define base.List.unsafeAt etge4adj957jmmosh03foaafoju2rjidbu8hmv0nnqkt3rg80q50hnn7ukr7ve9lmfgjjsm5ul5eeq7b8vbk1j6u4bopov2j4r3u2v8)

; unison_random_mersenne._base_additions.Nat32.isEven : Some(Forall(|𝕖/0 #0|(Arrow(Ref(#d97e0jhkmd), Effect(Effects([var𝕖 (#0)]), Ref(Boolean))))))

(print-processing "unison_random_mersenne._base_additions.Nat32.isEven")

(define mvinivvj0em7dso9honoe8oots60ujdhhr3r5d75foglvp6l80631buqr7pc5batv3lj25faf5n50ocdkn70lalinabl66a9ijft67g
  (lambda
   (srnrn77v9k)
   (let
    ((tmp-match-head srnrn77v9k))
    (let
     ((result
       (let
        ((tmp-0 tmp-match-head))
        (if
         (and
          (list? tmp-0)
          (equal?
           'd97e0jhkmdospt1d5n3pdrl78ufb3m32hr06uhijaq8s2vdhmaehr7rv0fsm8b3p469cru224jqs46vlqesbaf5a41vlstv87gempqo_0
           (list-ref tmp-0 0))
          (equal? (length tmp-0) 2))
         (let ((a (list-ref tmp-0 1))) (Nat.isEven a))
         'fallthrough))))
     (if (equal? 'fallthrough result) (no-match) result)))))

; /end unison_random_mersenne._base_additions.Nat32.isEven

(define unison_random_mersenne._base_additions.Nat32.isEven mvinivvj0em7dso9honoe8oots60ujdhhr3r5d75foglvp6l80631buqr7pc5batv3lj25faf5n50ocdkn70lalinabl66a9ijft67g)

; unison_random_mersenne.random.mersenne.next : Some(Arrow(Ref(#568rsi7o3g), Effect(Effects([App(Ref(#tkpo8b6903), Ref(#80gg7kgilb))]), Ref(#d97e0jhkmd))))

(print-processing "unison_random_mersenne.random.mersenne.next")

(define btoj29onkc1gcpi4uao0v2eb7dd5vgv0a52hqhjnrnbok5beooh9k151kl2ui2n2opmkgdjat9vf42k7jkuh4dve61hns2nmv6ockdg
  (lambda
   (_)
   (let
    ((s
      (let
       ((tmp-match-head
         (tkpo8b6903kdcggkdolc9kgsq2175dhg0drn17134edkambevl7aahadvueaqml9pemnttm81t7569jbrifmrg82ogjnlfv5gbc2q6o_1)))
       (let
        ((result
          (let
           ((tmp-0 tmp-match-head))
           (if
            (and
             (list? tmp-0)
             (equal?
              '80gg7kgilboe8aq4q8qj3oq9cfjj795m5pmgc7k5r2ne76qn79e33e8gk9q7pvgjpkompfv6po8k0rhi2u1g3r6agcgkl6etlv202no_0
              (list-ref tmp-0 0))
             (equal? (length tmp-0) 2))
            (let ((s (list-ref tmp-0 1))) s)
            'fallthrough))))
        (if (equal? 'fallthrough result) (no-match) result)))))
    (let
     ((_2
       (v00j3buk6m0588orsna0oeuukfdh7g3sandn22qmld0m2fr2jvm7hjd6bpm5jh6db6hom5ag14hqmg4emhlm4urdj9ibom7s2m5417o_3
        (vector (v00j3buk6m0588orsna0oeuukfdh7g3sandn22qmld0m2fr2jvm7hjd6bpm5jh6db6hom5ag14hqmg4emhlm4urdj9ibom7s2m5417o_2
           "We use numbers from 3 positions in the list we\'re keeping stored.")))))
     (let
      ((a
        ((etge4adj957jmmosh03foaafoju2rjidbu8hmv0nnqkt3rg80q50hnn7ukr7ve9lmfgjjsm5ul5eeq7b8vbk1j6u4bopov2j4r3u2v8
          0)
         s)))
      (let
       ((b
         ((etge4adj957jmmosh03foaafoju2rjidbu8hmv0nnqkt3rg80q50hnn7ukr7ve9lmfgjjsm5ul5eeq7b8vbk1j6u4bopov2j4r3u2v8
           1)
          s)))
       (let
        ((c
          ((etge4adj957jmmosh03foaafoju2rjidbu8hmv0nnqkt3rg80q50hnn7ukr7ve9lmfgjjsm5ul5eeq7b8vbk1j6u4bopov2j4r3u2v8
            397)
           s)))
        (let
         ((_6
           (v00j3buk6m0588orsna0oeuukfdh7g3sandn22qmld0m2fr2jvm7hjd6bpm5jh6db6hom5ag14hqmg4emhlm4urdj9ibom7s2m5417o_3
            (vector (v00j3buk6m0588orsna0oeuukfdh7g3sandn22qmld0m2fr2jvm7hjd6bpm5jh6db6hom5ag14hqmg4emhlm4urdj9ibom7s2m5417o_2
               "Calculate the next untempered number (next\'), and the new stored list, s\'.")))))
         (let
          ((p
            ((4font96ujkk61k20294c82c2m432233i6akg6017q2j4u9ng48c8su2hqd35rjmucnkodhljp1vnkqr3h6mnv0bj8ej8eqk3ikqaja8
              ((hl4e732fqm204op26k9ojkuu0fmfjksr5fcknmig1akevrthiu1quscc6jj6rlo9eb8quuku07m6c0q75q11s0o9irb9jc9nbmlscbo
                a)
               (glts8qofgo284gk3dnovfj2dges1n3b2amd2jk45rbl4s3mjbo5gent5n1mv7jsgnlqep879vgfg5j0li39jsmffr01ldrl623tjkag
                31)))
             ((77hkmnfup86ippah57kadki0i2s86keupous5f2f8tq3r7n2h9cf148e0cahil650tq3ch6fonjnvj1ktc2fcb252m909jacl18877o
               b)
              31))))
          (let
           ((ifEven
             ((4c6fis38j5nc86vcriefqvjs7fqfu5mdvqg6fkkltjgnqf6r000vggkof5l2569q87n2hnc7e8uakgfe690j53jckriu3ik8sbgpbi0
               c)
              ((443ei922o2d3i84c4o75e0odmok3vd5eh30beipei7akos818ko68m4j8tjh3i3f6ipc4dhhcq3s0gso59mu8suga042dvier4l8ih8
                p)
               1))))
           (let
            ((next-quot
              (if
               (mvinivvj0em7dso9honoe8oots60ujdhhr3r5d75foglvp6l80631buqr7pc5batv3lj25faf5n50ocdkn70lalinabl66a9ijft67g
                p)
               ifEven
               ((4c6fis38j5nc86vcriefqvjs7fqfu5mdvqg6fkkltjgnqf6r000vggkof5l2569q87n2hnc7e8uakgfe690j53jckriu3ik8sbgpbi0
                 ifEven)
                (d97e0jhkmdospt1d5n3pdrl78ufb3m32hr06uhijaq8s2vdhmaehr7rv0fsm8b3p469cru224jqs46vlqesbaf5a41vlstv87gempqo_0
                 2567483615)))))
            (let
             ((s-quot ((List.snoc ((List.drop 1) s)) next-quot)))
             (let
              ((temper
                (lambda
                 (n)
                 (let
                  ((y1
                    ((4c6fis38j5nc86vcriefqvjs7fqfu5mdvqg6fkkltjgnqf6r000vggkof5l2569q87n2hnc7e8uakgfe690j53jckriu3ik8sbgpbi0
                      n)
                     ((443ei922o2d3i84c4o75e0odmok3vd5eh30beipei7akos818ko68m4j8tjh3i3f6ipc4dhhcq3s0gso59mu8suga042dvier4l8ih8
                       n)
                      11))))
                  (let
                   ((y2
                     ((4c6fis38j5nc86vcriefqvjs7fqfu5mdvqg6fkkltjgnqf6r000vggkof5l2569q87n2hnc7e8uakgfe690j53jckriu3ik8sbgpbi0
                       y1)
                      ((hl4e732fqm204op26k9ojkuu0fmfjksr5fcknmig1akevrthiu1quscc6jj6rlo9eb8quuku07m6c0q75q11s0o9irb9jc9nbmlscbo
                        ((vubdg5ln9astm9mmlo7ev20kkjj38af12b4mg2c9n56uascubn8c87a1ckn4526krenh33cadqhhuul9nldl7bhpii648e0cku5cdao
                          y1)
                         7))
                       (d97e0jhkmdospt1d5n3pdrl78ufb3m32hr06uhijaq8s2vdhmaehr7rv0fsm8b3p469cru224jqs46vlqesbaf5a41vlstv87gempqo_0
                        2636928640)))))
                   (let
                    ((y3
                      ((4c6fis38j5nc86vcriefqvjs7fqfu5mdvqg6fkkltjgnqf6r000vggkof5l2569q87n2hnc7e8uakgfe690j53jckriu3ik8sbgpbi0
                        y2)
                       ((hl4e732fqm204op26k9ojkuu0fmfjksr5fcknmig1akevrthiu1quscc6jj6rlo9eb8quuku07m6c0q75q11s0o9irb9jc9nbmlscbo
                         ((vubdg5ln9astm9mmlo7ev20kkjj38af12b4mg2c9n56uascubn8c87a1ckn4526krenh33cadqhhuul9nldl7bhpii648e0cku5cdao
                           y2)
                          15))
                        (d97e0jhkmdospt1d5n3pdrl78ufb3m32hr06uhijaq8s2vdhmaehr7rv0fsm8b3p469cru224jqs46vlqesbaf5a41vlstv87gempqo_0
                         4022730752)))))
                    (let
                     ((y4
                       ((4c6fis38j5nc86vcriefqvjs7fqfu5mdvqg6fkkltjgnqf6r000vggkof5l2569q87n2hnc7e8uakgfe690j53jckriu3ik8sbgpbi0
                         y3)
                        ((443ei922o2d3i84c4o75e0odmok3vd5eh30beipei7akos818ko68m4j8tjh3i3f6ipc4dhhcq3s0gso59mu8suga042dvier4l8ih8
                          y3)
                         18))))
                     y4)))))))
              (let
               ((_12
                 (v00j3buk6m0588orsna0oeuukfdh7g3sandn22qmld0m2fr2jvm7hjd6bpm5jh6db6hom5ag14hqmg4emhlm4urdj9ibom7s2m5417o_3
                  (vector (v00j3buk6m0588orsna0oeuukfdh7g3sandn22qmld0m2fr2jvm7hjd6bpm5jh6db6hom5ag14hqmg4emhlm4urdj9ibom7s2m5417o_2
                     "Update the state and return the (tempered) result.")))))
               (let
                ((_13
                  (tkpo8b6903kdcggkdolc9kgsq2175dhg0drn17134edkambevl7aahadvueaqml9pemnttm81t7569jbrifmrg82ogjnlfv5gbc2q6o_0
                   (80gg7kgilboe8aq4q8qj3oq9cfjj795m5pmgc7k5r2ne76qn79e33e8gk9q7pvgjpkompfv6po8k0rhi2u1g3r6agcgkl6etlv202no_0
                    s-quot))))
                (temper next-quot))))))))))))))))

; /end unison_random_mersenne.random.mersenne.next

(define unison_random_mersenne.random.mersenne.next btoj29onkc1gcpi4uao0v2eb7dd5vgv0a52hqhjnrnbok5beooh9k151kl2ui2n2opmkgdjat9vf42k7jkuh4dve61hns2nmv6ockdg)

; unison_random_mersenne.random.mersenne.nat.next : Some(Arrow(Ref(#568rsi7o3g), Effect(Effects([App(Ref(#tkpo8b6903), Ref(#80gg7kgilb))]), Ref(Nat))))

(print-processing "unison_random_mersenne.random.mersenne.nat.next")

(define e082cs8ace1fr1mo5gg1n1adfrhr57p8ir3ca28n0f3t2googumdruou2sohv6u9daore07qcc10e5o1sj32hb7rck40f76nihi2gug
  (lambda
   (_)
   (let
    ((top32
      (let
       ((tmp-match-head
         (btoj29onkc1gcpi4uao0v2eb7dd5vgv0a52hqhjnrnbok5beooh9k151kl2ui2n2opmkgdjat9vf42k7jkuh4dve61hns2nmv6ockdg
          568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8_0)))
       (let
        ((result
          (let
           ((tmp-0 tmp-match-head))
           (if
            (and
             (list? tmp-0)
             (equal?
              'd97e0jhkmdospt1d5n3pdrl78ufb3m32hr06uhijaq8s2vdhmaehr7rv0fsm8b3p469cru224jqs46vlqesbaf5a41vlstv87gempqo_0
              (list-ref tmp-0 0))
             (equal? (length tmp-0) 2))
            (let ((n (list-ref tmp-0 1))) n)
            'fallthrough))))
        (if (equal? 'fallthrough result) (no-match) result)))))
    (let
     ((bot32
       (let
        ((tmp-match-head
          (btoj29onkc1gcpi4uao0v2eb7dd5vgv0a52hqhjnrnbok5beooh9k151kl2ui2n2opmkgdjat9vf42k7jkuh4dve61hns2nmv6ockdg
           568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8_0)))
        (let
         ((result
           (let
            ((tmp-0 tmp-match-head))
            (if
             (and
              (list? tmp-0)
              (equal?
               'd97e0jhkmdospt1d5n3pdrl78ufb3m32hr06uhijaq8s2vdhmaehr7rv0fsm8b3p469cru224jqs46vlqesbaf5a41vlstv87gempqo_0
               (list-ref tmp-0 0))
              (equal? (length tmp-0) 2))
             (let ((n (list-ref tmp-0 1))) n)
             'fallthrough))))
         (if (equal? 'fallthrough result) (no-match) result)))))
     ((Nat.or ((Nat.shiftLeft top32) 32)) bot32)))))

; /end unison_random_mersenne.random.mersenne.nat.next

(define unison_random_mersenne.random.mersenne.nat.next e082cs8ace1fr1mo5gg1n1adfrhr57p8ir3ca28n0f3t2googumdruou2sohv6u9daore07qcc10e5o1sj32hb7rck40f76nihi2gug)

; unison_random_mersenne.random.mersenne.defaultSeed : Some(Ref(#d97e0jhkmd))

(print-processing "unison_random_mersenne.random.mersenne.defaultSeed")

(define fvfia2buu96bbhqup30hmso49aano9u5rve82ovbd2msedra176jj6t3oj2hr5rngsfpf9fo38c8jjqqs19n505t1onbkd6bs9egka8
  (d97e0jhkmdospt1d5n3pdrl78ufb3m32hr06uhijaq8s2vdhmaehr7rv0fsm8b3p469cru224jqs46vlqesbaf5a41vlstv87gempqo_0
   5489))

; /end unison_random_mersenne.random.mersenne.defaultSeed

(define unison_random_mersenne.random.mersenne.defaultSeed fvfia2buu96bbhqup30hmso49aano9u5rve82ovbd2msedra176jj6t3oj2hr5rngsfpf9fo38c8jjqqs19n505t1onbkd6bs9egka8)

; base.io.rethrow : Some(Forall(|a/0 #0|(Arrow(App(App(Ref(#kc92tha5f1), Ref(#jkhagbadav)), vara (#0)), Effect(Effects([Ref(#fgaevis4bl)]), vara (#0))))))

(print-processing "base.io.rethrow")

(define rultimjsuqimjid9ktklj6n26ejvsbn21d1d161vk54hf7a7nmic96c1aefb4jhoko520e6fbtetje3fe54i4mhdjrh3p7movr3uk70
  (lambda
   (x)
   (let
    ((tmp-match-head x))
    (let
     ((result
       (let
        ((tmp-0 tmp-match-head))
        (if
         (and
          (list? tmp-0)
          (equal?
           'kc92tha5f12vamultsbq93aqnphg9pnhuq3sodqvhes6st2a3h5sd2rksuptds94ptvvpg0tj0jp1rehlb73rkn0kj2r6elkdqndhjo_0
           (list-ref tmp-0 0))
          (equal? (length tmp-0) 2))
         (let
          ((e (list-ref tmp-0 1)))
          (fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_25
           e))
         'fallthrough))))
     (if
      (equal? 'fallthrough result)
      (let
       ((result
         (let
          ((tmp-0 tmp-match-head))
          (if
           (and
            (list? tmp-0)
            (equal?
             'kc92tha5f12vamultsbq93aqnphg9pnhuq3sodqvhes6st2a3h5sd2rksuptds94ptvvpg0tj0jp1rehlb73rkn0kj2r6elkdqndhjo_1
             (list-ref tmp-0 0))
            (equal? (length tmp-0) 2))
           (let ((a (list-ref tmp-0 1))) a)
           'fallthrough))))
       (if (equal? 'fallthrough result) (no-match) result))
      result)))))

; /end base.io.rethrow

(define base.io.rethrow rultimjsuqimjid9ktklj6n26ejvsbn21d1d161vk54hf7a7nmic96c1aefb4jhoko520e6fbtetje3fe54i4mhdjrh3p7movr3uk70)

; base.io.putText : Some(Forall(|𝕖/0 #0|(Arrow(Ref(#b7kh3q81n1), Effect(Effects([var𝕖 (#0)]), Arrow(Ref(Text), Effect(Effects([Ref(#fgaevis4bl)]), Ref(#568rsi7o3g))))))))

(print-processing "base.io.putText")

(define usjgb68ojte81tje2msm1s0t1p8advuocspasdi2l5cs3nc05vgt19pgoi7d3se228u50o5fjmbi3gvnugsgni77ptd4kh3nl39p5ho
  (lambda
   (h)
   (lambda
    (t)
    (rultimjsuqimjid9ktklj6n26ejvsbn21d1d161vk54hf7a7nmic96c1aefb4jhoko520e6fbtetje3fe54i4mhdjrh3p7movr3uk70
     ((fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_37
       h)
      t)))))

; /end base.io.putText

(define base.io.putText usjgb68ojte81tje2msm1s0t1p8advuocspasdi2l5cs3nc05vgt19pgoi7d3se228u50o5fjmbi3gvnugsgni77ptd4kh3nl39p5ho)

; base.io.printLine : Some(Arrow(Ref(Text), Effect(Effects([Ref(#fgaevis4bl)]), Ref(#568rsi7o3g))))

(print-processing "base.io.printLine")

(define ssbdakrj0hbr2rpno34bj8i64mokaaoc5rdj7sil5c87qrqeamf2996qrg0gc1thhamd4bbc40l991d7vgd35qekh5hsoiqkpccvk50
  (lambda
   (t)
   (letrec
    ((_1
      ((usjgb68ojte81tje2msm1s0t1p8advuocspasdi2l5cs3nc05vgt19pgoi7d3se228u50o5fjmbi3gvnugsgni77ptd4kh3nl39p5ho
        7plc1gmlvn6ti9lpq4ufa4uvdal6elieppmcgvr0d0scsk9e3edakgnk09covbel3hjcjlsi7rmn8auhi935fo9iibed7krpfr337fo)
       t)))
    ((usjgb68ojte81tje2msm1s0t1p8advuocspasdi2l5cs3nc05vgt19pgoi7d3se228u50o5fjmbi3gvnugsgni77ptd4kh3nl39p5ho
      7plc1gmlvn6ti9lpq4ufa4uvdal6elieppmcgvr0d0scsk9e3edakgnk09covbel3hjcjlsi7rmn8auhi935fo9iibed7krpfr337fo)
     "\n"))))

; /end base.io.printLine

(define base.io.printLine ssbdakrj0hbr2rpno34bj8i64mokaaoc5rdj7sil5c87qrqeamf2996qrg0gc1thhamd4bbc40l991d7vgd35qekh5hsoiqkpccvk50)

; testbed.run_long : Some(Arrow(Ref(#568rsi7o3g), Effect(Effects([Ref(#fgaevis4bl)]), Ref(#568rsi7o3g))))

(print-processing "testbed.run_long")

(define si3c5dmbks82jmujckto784880377lo0n6od3j7f5ht01034ae7h0muiirhi8p8s9roceeqhtnuivr5ke442vsdg0qc6f97qf7bj6fg
  (lambda
   (_)
   (ssbdakrj0hbr2rpno34bj8i64mokaaoc5rdj7sil5c87qrqeamf2996qrg0gc1thhamd4bbc40l991d7vgd35qekh5hsoiqkpccvk50
    (Nat.toText
     (List.size
      (((49dfoqk8ri2sa1n5hrd1m1hc7vqdkorl4jqnvbsmht268lsnpopt6tsrpd4ll4rg93fmmuh1g660n5oujo66h8tnqi2fagp3nk451r0
         e082cs8ace1fr1mo5gg1n1adfrhr57p8ir3ca28n0f3t2googumdruou2sohv6u9daore07qcc10e5o1sj32hb7rck40f76nihi2gug)
        fvfia2buu96bbhqup30hmso49aano9u5rve82ovbd2msedra176jj6t3oj2hr5rngsfpf9fo38c8jjqqs19n505t1onbkd6bs9egka8)
       10000))))))

; (define si3c5dmbks82jmujckto784880377lo0n6od3j7f5ht01034ae7h0muiirhi8p8s9roceeqhtnuivr5ke442vsdg0qc6f97qf7bj6fg
;   (lambda
;    (_)
;    (ssbdakrj0hbr2rpno34bj8i64mokaaoc5rdj7sil5c87qrqeamf2996qrg0gc1thhamd4bbc40l991d7vgd35qekh5hsoiqkpccvk50
;     (((49dfoqk8ri2sa1n5hrd1m1hc7vqdkorl4jqnvbsmht268lsnpopt6tsrpd4ll4rg93fmmuh1g660n5oujo66h8tnqi2fagp3nk451r0
;          e082cs8ace1fr1mo5gg1n1adfrhr57p8ir3ca28n0f3t2googumdruou2sohv6u9daore07qcc10e5o1sj32hb7rck40f76nihi2gug)
;         fvfia2buu96bbhqup30hmso49aano9u5rve82ovbd2msedra176jj6t3oj2hr5rngsfpf9fo38c8jjqqs19n505t1onbkd6bs9egka8)
;        10))))

; /end testbed.run_long

(define testbed.run_long si3c5dmbks82jmujckto784880377lo0n6od3j7f5ht01034ae7h0muiirhi8p8s9roceeqhtnuivr5ke442vsdg0qc6f97qf7bj6fg)


; (export main)
; (define (main . args)
  ; (run-with-io testbed.run_long)
; )
(run-with-io testbed.run_long)