#lang racket

(define true #t)
(define false #f)

(define abort raise)
(define print (lambda args (display args) (display "\n")))

; Cons/Nil

(define (onbcm0qctbnuctpm57tkc5p16b8gfke8thjf19p4r4laokji0b606rd0frnhj103qb90lve3fohkoc1eda70491hot656s1m6kk3cn0_0 one)
    (lambda (two) (list 'onbcm0qctbnuctpm57tkc5p16b8gfke8thjf19p4r4laokji0b606rd0frnhj103qb90lve3fohkoc1eda70491hot656s1m6kk3cn0_0 one two)))

(define 568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8_0
    '568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8_0)

(define Cons onbcm0qctbnuctpm57tkc5p16b8gfke8thjf19p4r4laokji0b606rd0frnhj103qb90lve3fohkoc1eda70491hot656s1m6kk3cn0_0 )
(define Nil 568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8_0)


; Some/None
(define 5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_0
    '5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_0)

(define (5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_1 arg)
    (list '5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_1 arg))

(define None 5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_0)
(define Some 5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_1)


; --- abilties ---

(define stack '())

(define name "wip")

(define (throw-effect k effect)
    (if (or (not (list? effect))
        (not (procedure? k))
        (not (symbol? (car effect)))
        )
        (begin
        (print "Invalid input to throw-effect " k " " effect)
        (abort "Invalid throw effect"))
    (if (eq? '() stack)
        (abort "no handler for effect")
        (let* ((handler (car stack)))
            (set! stack (cdr stack))
            (print "Calling " name)
            (handler (cons 'effect (cons k effect)))
        ))
    )
)

(define (throw-pure value)
    (let* ((handler (car stack))  )
        (set! stack (cdr stack))
        (print "Calling " name)
        (handler (list 'pure value))
    )
)

(define (rethrow-effect eff)
    (let ((k (cadr eff))
          (ef (cddr eff)))
        (throw-effect k eff)))

(define (add-handler handler)
    (print "add handler " name " " handler " " stack)
    (set! stack (cons handler stack))
    (print "now " stack)
)


(define (handle-ability inner handler)
    (handler (call/cc (lambda (k)
        (print "adding handler " name)
        (add-handler k) ; TODO we'll have to pop at some point?
        (let ((value (inner)))
            (print "Got value " name " " value)
            (throw-pure value)
        )
    )))
)


; --- shimming ability definitions ---

(define (csb691v7tp98lrjb9k3tfjme750l7n1f8gmnphq4d0132skvpi2gapbht08oup4nsmlalgkqgs89okfgg5h1q21t0bf71tkeifmfii0_0 vbl)
    (call/cc (lambda (k) (throw-effect k (list 'csb691v7tp98lrjb9k3tfjme750l7n1f8gmnphq4d0132skvpi2gapbht08oup4nsmlalgkqgs89okfgg5h1q21t0bf71tkeifmfii0_0 vbl)))))



(define (jbvl6groqo3gsiodptlckggapgclh37o4b0a2ia8v7j82v0589jfu5e6k220tcng8ds136lt7r33mlnbhjuau8ujcq0laci2pu22cr0_0)
    (call/cc (lambda (k) (throw-effect k (list 'jbvl6groqo3gsiodptlckggapgclh37o4b0a2ia8v7j82v0589jfu5e6k220tcng8ds136lt7r33mlnbhjuau8ujcq0laci2pu22cr0_0)))))

(define (1r0gt3snct0dbe4g4e42b3s6jsknadma01j28nrnm598783gnlpap9s9vd522re7iomr2qurmrpf135gis17db0lebolfslpol7p6dg_0 one)
    (print "Constructin a one" one)
    (lambda (two)
        (print "Constructin a two" one two)
        (call/cc (lambda (k) (throw-effect k (list '1r0gt3snct0dbe4g4e42b3s6jsknadma01j28nrnm598783gnlpap9s9vd522re7iomr2qurmrpf135gis17db0lebolfslpol7p6dg_0 one two))))
        ))
(define getTwo 1r0gt3snct0dbe4g4e42b3s6jsknadma01j28nrnm598783gnlpap9s9vd522re7iomr2qurmrpf135gis17db0lebolfslpol7p6dg_0)