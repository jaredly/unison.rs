(import (chicken bitwise))

(define true #t)
(define false #f)

(define (Universal.== a) (lambda (b) (equal? a b)))
(define (Text.!= a) (lambda (b) (not (equal? a b))))

(define (f2c22r2a1sche28mn07brk1j45kp1bam3tr4k2j0un2hi1g7rbrud3f5mes2defqo1tpd9j38pqpg2f0efl3no0ede5ocl2am4bonm0 a)
    (lambda (b) (not (equal? a b)))
)

(define (5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_0)
    (list '5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_0))

(define (5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_1 arg)
    (list '5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_1 arg))

(define (m7uplgfko92kqdmm6u898j5h4n86587f44u7fq1vjcad1f68n35r8j2mdfdbjta5hq9o699dgn2aphteditp30g34hsh3gru68593j0 a)
    (lambda (b) (- a b)))

(define (Nat.drop a)
    (lambda (b)
        (max 0 (- a b))))

(define (Nat.+ a)
    (lambda (b)
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


(define maxNat 18446744073709552000)
(define minInt -18446744073709552000)
(define maxInt 18446744073709552000)

(define s9h25aadei68iscfiu60eldfhe9uvh0pk3knd9m965gqlejvc5jlcqs9gfcgpgvfv85n2pefvee4ca2n7mepcoqamou73g7ilscf450
    maxNat)

(define p9og3s2h41natoslfjoi1do0omp82s4jiethebfd4j5p99ltbdmcua2egbiehs9tq9k65744cvugibiqdkgip21t7se4e8faktnl3k0
    minInt)

(define d75vubeoep5o8ph72v0v9qdm36n17up0d7bsbdckjapcs7k9g1kv5mnbpp3444u8fmvo2h3benmk7o3sd09g1lkrrvk4q93vv8u2n3g
    maxInt)

(define (Nat.xor a)
    (lambda (b) (bitwise-xor a b)))

(define (Nat.pow a)
    (lambda (b) (expt a b)))

        
(define (Float.+ a)
    (lambda (b)
        (+ a b)))

(define (Float./ a)
    (lambda (b)
        (/ a b)))

        
(define (Int.- a)
    (lambda (b)
        (intLoop (- a b))))

        
(define (Int.+ a)
    (lambda (b)
        (intLoop (+ a b))))


        
(define (Nat.* a)
    (lambda (b)
        (* a b)))

        
(define (Nat.or a)
    (lambda (b)
        (bitwise-ior a b)))

        
(define (Nat.and a)
    (lambda (b)
        (bitwise-and a b)))

(define (Nat.shiftLeft a)
    (lambda (b)
        (arithmetic-shift a b)))

        
(define (Nat.shiftRight a)
    (lambda (b)
        (arithmetic-shift a (- b))))


; (define (not-implemented _) false)