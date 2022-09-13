(load "stdlib.scm")


; continuations._external.base.M1l.Unit

; Data(DataDecl { modifier: Structural, bound: [], constructors: [(🔣Unit.Unit/0, Ref(#568rsi7o3g))] })

; Unit.Unit

(define 568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8_0 '568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8_0)

; memo.Key

; Data(DataDecl { modifier: Structural, bound: [🔣r/0], constructors: [(🔣Key.Key/0, Forall(|r/0 #0|(Arrow(Forall(|e/0 #0|(Arrow(Forall(|a/0 #0|(Arrow(vara (#0), Effect(Effects([vare (#0)]), varr (#0))))), Effect(Effects([vare (#0)]), varr (#0))))), App(Ref(#4u0ej7m4le), varr (#0))))))] })

; Key.Key

(define 4u0ej7m4le70r112tsa9ej5cg7ksrtc2slpsvtakk80aoc8ls48lpqlbanaihh97in624u0atchvhq45490tgj78i50lq5i69lqk2gg_0 (lambda (arg_0) (list '4u0ej7m4le70r112tsa9ej5cg7ksrtc2slpsvtakk80aoc8ls48lpqlbanaihh97in624u0atchvhq45490tgj78i50lq5i69lqk2gg_0 arg_0)))

; continuations._external.base.M1l.test.Test

; Data(DataDecl { modifier: Structural, bound: [], constructors: [(🔣Test.Test.Test/0, Arrow(Arrow(Ref(#c6embbsl59), Ref(#op6dq1nj40)), Ref(#fjhnoa53kd)))] })

; Test.Test.Test

(define fjhnoa53kdshescmgm65m29g183m4e65i5euipgf8s2a2lhhvolmfrahu3ggot4oe2hqk84c8uom6l7kog2l96vpkic5fjq70rjv0n8_0 (lambda (arg_0) (list 'fjhnoa53kdshescmgm65m29g183m4e65i5euipgf8s2a2lhhvolmfrahu3ggot4oe2hqk84c8uom6l7kog2l96vpkic5fjq70rjv0n8_0 arg_0)))

; continuations._external.base.M1l.Trie

; Data(DataDecl { modifier: Structural, bound: [🔣k/0, 🔣v/0], constructors: [(🔣Trie.Trie/0, Forall(|k/0 #0|(Forall(|v/0 #0|(Arrow(App(Ref(#5isltsdct9), varv (#0)), Arrow(App(App(Ref(#7di5ureqgi), vark (#0)), App(App(Ref(#ps42cgn378), vark (#0)), varv (#0))), App(App(Ref(#ps42cgn378), vark (#0)), varv (#0)))))))))] })

; Trie.Trie

(define ps42cgn3788qukbk25ea48dgb0c44i5fmov3tuqra3d8sr2lvhamngqfnr685jb5iand8jq7oa5a30tp2njh5hehnjqc9dbp2bp8c88_0 (lambda (arg_0) (lambda (arg_1) (list 'ps42cgn3788qukbk25ea48dgb0c44i5fmov3tuqra3d8sr2lvhamngqfnr685jb5iand8jq7oa5a30tp2njh5hehnjqc9dbp2bp8c88_0 arg_0 arg_1))))

; memo.Memo

; Effect(DataDecl { modifier: Unique("qrl4bclopjh33fgep6r1gqonfg22i24r"), bound: [], constructors: [(🔣Memo.cache/0, Forall(|a/0 #0|(Arrow(App(Ref(#4u0ej7m4le), vara (#0)), Arrow(vara (#0), Effect(Effects([Ref(#161o8dftc2)]), Ref(#568rsi7o3g))))))), (🔣Memo.lookup/0, Forall(|a/0 #0|(Arrow(App(Ref(#4u0ej7m4le), vara (#0)), Effect(Effects([Ref(#161o8dftc2)]), App(Ref(#5isltsdct9), vara (#0)))))))] })

; Memo.cache

(define 161o8dftc2buleq7u87u0uiq1vu11a0otf74jjr3rkimodk9el3pcakvtbmnmlv2hh7d45q4093mofhgkrcor9j9rlv8ptao24dle78_0 (lambda (arg_0) (lambda (arg_1) (call/cc (lambda (k) (throw-effect k (list '161o8dftc2buleq7u87u0uiq1vu11a0otf74jjr3rkimodk9el3pcakvtbmnmlv2hh7d45q4093mofhgkrcor9j9rlv8ptao24dle78_0 arg_0 arg_1)))))))

; Memo.lookup

(define 161o8dftc2buleq7u87u0uiq1vu11a0otf74jjr3rkimodk9el3pcakvtbmnmlv2hh7d45q4093mofhgkrcor9j9rlv8ptao24dle78_1 (lambda (arg_0) (call/cc (lambda (k) (throw-effect k (list '161o8dftc2buleq7u87u0uiq1vu11a0otf74jjr3rkimodk9el3pcakvtbmnmlv2hh7d45q4093mofhgkrcor9j9rlv8ptao24dle78_1 arg_0))))))

; continuations._external.base.M1l.Optional

; Data(DataDecl { modifier: Structural, bound: [🔣a/0], constructors: [(🔣Optional.None/0, Forall(|a/0 #0|(App(Ref(#5isltsdct9), vara (#0))))), (🔣Optional.Some/0, Forall(|a/0 #0|(Arrow(vara (#0), App(Ref(#5isltsdct9), vara (#0))))))] })

; Optional.None

(define 5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_0 '5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_0)

; Optional.Some

(define 5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_1 (lambda (arg_0) (list '5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_1 arg_0)))

; continuations._external.base.M1l.Store

; Effect(DataDecl { modifier: Structural, bound: [🔣a/0], constructors: [(🔣Store.Store.put/0, Forall(|a/0 #0|(Arrow(vara (#0), Effect(Effects([App(Ref(#tkpo8b6903), vara (#0))]), Ref(#568rsi7o3g)))))), (🔣Store.Store.get/0, Forall(|a/0 #0|(Effect(Effects([App(Ref(#tkpo8b6903), vara (#0))]), vara (#0)))))] })

; Store.Store.put

(define tkpo8b6903kdcggkdolc9kgsq2175dhg0drn17134edkambevl7aahadvueaqml9pemnttm81t7569jbrifmrg82ogjnlfv5gbc2q6o_0 (lambda (arg_0) (call/cc (lambda (k) (throw-effect k (list 'tkpo8b6903kdcggkdolc9kgsq2175dhg0drn17134edkambevl7aahadvueaqml9pemnttm81t7569jbrifmrg82ogjnlfv5gbc2q6o_0 arg_0))))))

; Store.Store.get

(define tkpo8b6903kdcggkdolc9kgsq2175dhg0drn17134edkambevl7aahadvueaqml9pemnttm81t7569jbrifmrg82ogjnlfv5gbc2q6o_1 (lambda () (call/cc (lambda (k) (throw-effect k (list 'tkpo8b6903kdcggkdolc9kgsq2175dhg0drn17134edkambevl7aahadvueaqml9pemnttm81t7569jbrifmrg82ogjnlfv5gbc2q6o_1))))))

; continuations._external.base.M1l.Test.Result

; Data(DataDecl { modifier: Unique("70621e539cd802b2ad53105697800930411a3ebc"), bound: [], constructors: [(🔣Test.Result.Fail/0, Arrow(Ref(Text), Ref(#vmc06s4f23))), (🔣Test.Result.Ok/0, Arrow(Ref(Text), Ref(#vmc06s4f23)))] })

; Test.Result.Fail

(define vmc06s4f236sps61vqv35g7ridnae03uetth98aocort1825stbv7m6ncfca2j0gcane47c8db2rjtd2o6kch2lr7v2gst895pcs0m0_0 (lambda (arg_0) (list 'vmc06s4f236sps61vqv35g7ridnae03uetth98aocort1825stbv7m6ncfca2j0gcane47c8db2rjtd2o6kch2lr7v2gst895pcs0m0_0 arg_0)))

; Test.Result.Ok

(define vmc06s4f236sps61vqv35g7ridnae03uetth98aocort1825stbv7m6ncfca2j0gcane47c8db2rjtd2o6kch2lr7v2gst895pcs0m0_1 (lambda (arg_0) (list 'vmc06s4f236sps61vqv35g7ridnae03uetth98aocort1825stbv7m6ncfca2j0gcane47c8db2rjtd2o6kch2lr7v2gst895pcs0m0_1 arg_0)))

; continuations._external.base.M1l.test.internals.v1.Test.Report

; Data(DataDecl { modifier: Structural, bound: [], constructors: [(🔣Test.Report.Report/0, Arrow(App(App(Ref(#ps42cgn378), Ref(Text)), Ref(#mo80e1n080)), Ref(#op6dq1nj40)))] })

; Test.Report.Report

(define op6dq1nj40oelkl970uve3mv5icih2a4ms65h3ihbi7lv68ffpfnd3t7d3olllqlghlnik2tt5vlr8nn471kfil7pd4pprqh5h96cig_0 (lambda (arg_0) (list 'op6dq1nj40oelkl970uve3mv5icih2a4ms65h3ihbi7lv68ffpfnd3t7d3olllqlghlnik2tt5vlr8nn471kfil7pd4pprqh5h96cig_0 arg_0)))

; continuations._external.base.M1l.test.internals.v1.Test.Status

; Data(DataDecl { modifier: Structural, bound: [], constructors: [(🔣Test.Status.Failed/0, Ref(#mo80e1n080)), (🔣Test.Status.Pending/0, Ref(#mo80e1n080)), (🔣Test.Status.Expected/0, Arrow(Ref(#md6vee7q62), Ref(#mo80e1n080))), (🔣Test.Status.Unexpected/0, Arrow(Ref(#md6vee7q62), Ref(#mo80e1n080)))] })

; Test.Status.Failed

(define mo80e1n080jd4hsln94nerrejv2bacn4cd9hoebifsabm44mjvs1km04dkvlue5o0op31vbjad9f2ld6227qeqdn787e4jdrdi89p50_0 'mo80e1n080jd4hsln94nerrejv2bacn4cd9hoebifsabm44mjvs1km04dkvlue5o0op31vbjad9f2ld6227qeqdn787e4jdrdi89p50_0)

; Test.Status.Pending

(define mo80e1n080jd4hsln94nerrejv2bacn4cd9hoebifsabm44mjvs1km04dkvlue5o0op31vbjad9f2ld6227qeqdn787e4jdrdi89p50_1 'mo80e1n080jd4hsln94nerrejv2bacn4cd9hoebifsabm44mjvs1km04dkvlue5o0op31vbjad9f2ld6227qeqdn787e4jdrdi89p50_1)

; Test.Status.Expected

(define mo80e1n080jd4hsln94nerrejv2bacn4cd9hoebifsabm44mjvs1km04dkvlue5o0op31vbjad9f2ld6227qeqdn787e4jdrdi89p50_2 (lambda (arg_0) (list 'mo80e1n080jd4hsln94nerrejv2bacn4cd9hoebifsabm44mjvs1km04dkvlue5o0op31vbjad9f2ld6227qeqdn787e4jdrdi89p50_2 arg_0)))

; Test.Status.Unexpected

(define mo80e1n080jd4hsln94nerrejv2bacn4cd9hoebifsabm44mjvs1km04dkvlue5o0op31vbjad9f2ld6227qeqdn787e4jdrdi89p50_3 (lambda (arg_0) (list 'mo80e1n080jd4hsln94nerrejv2bacn4cd9hoebifsabm44mjvs1km04dkvlue5o0op31vbjad9f2ld6227qeqdn787e4jdrdi89p50_3 arg_0)))

; continuations._external.base.M1l.Tuple

; Data(DataDecl { modifier: Structural, bound: [🔣a/0, 🔣b/0], constructors: [(🔣Tuple.Cons/0, Forall(|a/0 #0|(Forall(|b/0 #0|(Arrow(vara (#0), Arrow(varb (#0), App(App(Ref(#onbcm0qctb), vara (#0)), varb (#0)))))))))] })

; Tuple.Cons

(define onbcm0qctbnuctpm57tkc5p16b8gfke8thjf19p4r4laokji0b606rd0frnhj103qb90lve3fohkoc1eda70491hot656s1m6kk3cn0_0 (lambda (arg_0) (lambda (arg_1) (list 'onbcm0qctbnuctpm57tkc5p16b8gfke8thjf19p4r4laokji0b606rd0frnhj103qb90lve3fohkoc1eda70491hot656s1m6kk3cn0_0 arg_0 arg_1))))

; continuations._external.base.M1l.test.internals.v1.Test.Scope

; Data(DataDecl { modifier: Unique("mprni7ov1h6q608nbu2klnno05jaropc"), bound: [], constructors: [(🔣Test.Scope.Scope/0, Arrow(App(Ref(Sequence), Ref(Text)), Ref(#c6embbsl59)))] })

; Test.Scope.Scope

(define c6embbsl59hin2c7ndqboquchoi44786icggh4niodbar0sf5k4eue1qcj69ps8359u1bi5t6rr2hvcpkvego2euq6dldpcne7hlg38_0 (lambda (arg_0) (list 'c6embbsl59hin2c7ndqboquchoi44786icggh4niodbar0sf5k4eue1qcj69ps8359u1bi5t6rr2hvcpkvego2euq6dldpcne7hlg38_0 arg_0)))

; continuations._external.base.M1l.Map

; Data(DataDecl { modifier: Structural, bound: [🔣k/0, 🔣v/0], constructors: [(🔣Map.Map/0, Forall(|k/0 #0|(Forall(|v/0 #0|(Arrow(App(Ref(Sequence), vark (#0)), Arrow(App(Ref(Sequence), varv (#0)), App(App(Ref(#7di5ureqgi), vark (#0)), varv (#0)))))))))] })

; Map.Map

(define 7di5ureqgi60ue42886240kbovfhko0fg85rp2thpkl8af699upsl0os1btk27te1cjdmuerad5oi9bdd04me6mjh2m25djbj236fbo_0 (lambda (arg_0) (lambda (arg_1) (list '7di5ureqgi60ue42886240kbovfhko0fg85rp2thpkl8af699upsl0os1btk27te1cjdmuerad5oi9bdd04me6mjh2m25djbj236fbo_0 arg_0 arg_1))))

; continuations._external.base.M1l.test.internals.v1.Test.Success

; Data(DataDecl { modifier: Structural, bound: [], constructors: [(🔣Test.Success.Proved/0, Ref(#md6vee7q62)), (🔣Test.Success.Passed/0, Arrow(Ref(Nat), Ref(#md6vee7q62)))] })

; Test.Success.Proved

(define md6vee7q626dlqhdao1ecjgaa2v19dhhnq9l8mr80o2gf67qprmkqe5atbve2m997u7fur9cahnqklhst1sd3kb84b36jqtol5klbbg_0 'md6vee7q626dlqhdao1ecjgaa2v19dhhnq9l8mr80o2gf67qprmkqe5atbve2m997u7fur9cahnqklhst1sd3kb84b36jqtol5klbbg_0)

; Test.Success.Passed

(define md6vee7q626dlqhdao1ecjgaa2v19dhhnq9l8mr80o2gf67qprmkqe5atbve2m997u7fur9cahnqklhst1sd3kb84b36jqtol5klbbg_1 (lambda (arg_0) (list 'md6vee7q626dlqhdao1ecjgaa2v19dhhnq9l8mr80o2gf67qprmkqe5atbve2m997u7fur9cahnqklhst1sd3kb84b36jqtol5klbbg_1 arg_0)))

; base.. : Some(Forall(|𝕖111/0 #0|(Forall(|𝕖/0 #0|(Forall(|𝕖222/0 #0|(Forall(|a/0 #0|(Forall(|b/0 #0|(Forall(|c/0 #0|(Arrow(Arrow(varb (#0), Effect(Effects([var𝕖 (#0)]), varc (#0))), Effect(Effects([var𝕖222 (#0)]), Arrow(Arrow(vara (#0), Effect(Effects([var𝕖 (#0)]), varb (#0))), Effect(Effects([var𝕖111 (#0)]), Arrow(vara (#0), Effect(Effects([var𝕖 (#0)]), varc (#0))))))))))))))))))))

(print-processing "base..")

(define 0hoa6tis7604fo17o91lfjflsvd843uvm7sueu5rfqqn5nbneajoh9fldfbmmjnhh1h2690ktfflrbb96q9lksesuh3t5amcc1ph92g
  (lambda (f) (lambda (g) (lambda (x) (f (g x))))))

; /end base..

(define base.. 0hoa6tis7604fo17o91lfjflsvd843uvm7sueu5rfqqn5nbneajoh9fldfbmmjnhh1h2690ktfflrbb96q9lksesuh3t5amcc1ph92g)

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

; base.List.foldl : Some(Forall(|𝕖111/0 #0|(Forall(|𝕖/0 #0|(Forall(|𝕖222/0 #0|(Forall(|a/0 #0|(Forall(|b/0 #0|(Arrow(Arrow(varb (#0), Effect(Effects([var𝕖 (#0)]), Arrow(vara (#0), Effect(Effects([var𝕖 (#0)]), varb (#0))))), Effect(Effects([var𝕖222 (#0)]), Arrow(varb (#0), Effect(Effects([var𝕖111 (#0)]), Arrow(App(Ref(Sequence), vara (#0)), Effect(Effects([var𝕖 (#0)]), varb (#0))))))))))))))))))

(print-processing "base.List.foldl")

(define cqe9aa74fsjcg589r4rkiif3rm6o4trk8n06bg5d9rbhsaf1eiehmeufotfcsde4h88ldg7d21gcdi822925qlub4cr3bip7ssihrt8
  (lambda
   (f)
   (lambda
    (b)
    (lambda
     (as)
     (letrec
      ((go
        (lambda
         (b)
         (lambda
          (i)
          (let
           ((tmp-match-head (let ((f-tmp (List.at i))) (f-tmp as))))
           (let
            ((result
              (if
               (equal?
                '5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_0
                tmp-match-head)
               b
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
                   ((a (list-ref tmp-0 1)))
                   (let
                    ((f-tmp (go (let ((f-tmp (f b))) (f-tmp a)))))
                    (f-tmp (let ((f-tmp (Nat.+ i))) (f-tmp 1)))))
                  'fallthrough))))
              (if (equal? 'fallthrough result) (no-match) result))
             result)))))))
      (let ((f-tmp (go b))) (f-tmp 0)))))))

; /end base.List.foldl

(define base.List.foldl cqe9aa74fsjcg589r4rkiif3rm6o4trk8n06bg5d9rbhsaf1eiehmeufotfcsde4h88ldg7d21gcdi822925qlub4cr3bip7ssihrt8)

; continuations._external.base.M1l.List.join : Some(Forall(|𝕖/0 #0|(Forall(|a/0 #0|(Arrow(App(Ref(Sequence), App(Ref(Sequence), vara (#0))), Effect(Effects([var𝕖 (#0)]), App(Ref(Sequence), vara (#0)))))))))

(print-processing "continuations._external.base.M1l.List.join")

(define 4obsc8rgr225tst6i5jv4pik3fj91afs6542ecfbh6007mig8pkihs6i191bkqfe9kh1odn2l3o0q2jbr9947061vctkhsqaj10l20o
  (let
   ((f-tmp
     (cqe9aa74fsjcg589r4rkiif3rm6o4trk8n06bg5d9rbhsaf1eiehmeufotfcsde4h88ldg7d21gcdi822925qlub4cr3bip7ssihrt8
      List.++)))
   (f-tmp (vector ))))

; /end continuations._external.base.M1l.List.join

(define continuations._external.base.M1l.List.join 4obsc8rgr225tst6i5jv4pik3fj91afs6542ecfbh6007mig8pkihs6i191bkqfe9kh1odn2l3o0q2jbr9947061vctkhsqaj10l20o)

; continuations._external.base.M1l.List.map : Some(Forall(|𝕖/0 #0|(Forall(|𝕖111/0 #0|(Forall(|a/0 #0|(Forall(|b/0 #0|(Arrow(Arrow(vara (#0), Effect(Effects([var𝕖 (#0)]), varb (#0))), Effect(Effects([var𝕖111 (#0)]), Arrow(App(Ref(Sequence), vara (#0)), Effect(Effects([var𝕖 (#0)]), App(Ref(Sequence), varb (#0)))))))))))))))

(print-processing "continuations._external.base.M1l.List.map")

(define j1ejquc7so57gceg2fsnurckebs21napum8h7jbs58bmefncgvb8h654kcp6tdt31epf25vka01q0plg8dqbl9fat988n08i1571qtg
  (lambda
   (f)
   (lambda
    (a)
    (letrec
     ((go
       (lambda
        (i)
        (lambda
         (as)
         (lambda
          (acc)
          (let
           ((tmp-match-head (let ((f-tmp (List.at i))) (f-tmp as))))
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
                   ((a (list-ref tmp-0 1)))
                   (let
                    ((f-tmp (let ((f-tmp (go (let ((f-tmp (Nat.+ i))) (f-tmp 1))))) (f-tmp as))))
                    (f-tmp (let ((f-tmp (List.snoc acc))) (f-tmp (f a))))))
                  'fallthrough))))
              (if (equal? 'fallthrough result) (no-match) result))
             result))))))))
     (let ((f-tmp (let ((f-tmp (go 0))) (f-tmp a)))) (f-tmp (vector )))))))

; /end continuations._external.base.M1l.List.map

(define continuations._external.base.M1l.List.map j1ejquc7so57gceg2fsnurckebs21napum8h7jbs58bmefncgvb8h654kcp6tdt31epf25vka01q0plg8dqbl9fat988n08i1571qtg)

; continuations._external.base.M1l.List.flatMap : Some(Forall(|𝕖/0 #0|(Forall(|𝕖111/0 #0|(Forall(|a/0 #0|(Forall(|b/0 #0|(Arrow(Arrow(vara (#0), Effect(Effects([var𝕖 (#0)]), App(Ref(Sequence), varb (#0)))), Effect(Effects([var𝕖111 (#0)]), Arrow(App(Ref(Sequence), vara (#0)), Effect(Effects([var𝕖 (#0)]), App(Ref(Sequence), varb (#0)))))))))))))))

(print-processing "continuations._external.base.M1l.List.flatMap")

(define 12em6b2ruk6gtviraalfikch4ia40lin0kq12bgmsslt2miph795d1nt86ud2rgqi0ia3aqrb4pmu2l24j40a411gc65io037h8r23o
  (lambda
   (f)
   (lambda
    (as)
    (4obsc8rgr225tst6i5jv4pik3fj91afs6542ecfbh6007mig8pkihs6i191bkqfe9kh1odn2l3o0q2jbr9947061vctkhsqaj10l20o
     (let
      ((f-tmp
        (j1ejquc7so57gceg2fsnurckebs21napum8h7jbs58bmefncgvb8h654kcp6tdt31epf25vka01q0plg8dqbl9fat988n08i1571qtg
         f)))
      (f-tmp as))))))

; /end continuations._external.base.M1l.List.flatMap

(define continuations._external.base.M1l.List.flatMap 12em6b2ruk6gtviraalfikch4ia40lin0kq12bgmsslt2miph795d1nt86ud2rgqi0ia3aqrb4pmu2l24j40a411gc65io037h8r23o)

; continuations._external.base.M1l.List.replace : Some(Forall(|𝕖/0 #0|(Forall(|𝕖111/0 #0|(Forall(|𝕖222/0 #0|(Forall(|a/0 #0|(Arrow(Ref(Nat), Effect(Effects([var𝕖222 (#0)]), Arrow(vara (#0), Effect(Effects([var𝕖111 (#0)]), Arrow(App(Ref(Sequence), vara (#0)), Effect(Effects([var𝕖 (#0)]), App(Ref(Sequence), vara (#0)))))))))))))))))

(print-processing "continuations._external.base.M1l.List.replace")

(define 16qjsrhv7rblpv0ttfrg9lmdq3671de3k37ij3o7qijhpq85u9ob7aq2tsklo65pr1m3ab7v62ibg3ck6q4p2mf9i7mniakcefuljr0
  (lambda
   (i)
   (lambda
    (a)
    (lambda
     (as)
     (let
      ((f-tmp (List.++ (let ((f-tmp (List.++ (let ((f-tmp (List.take i))) (f-tmp as))))) (f-tmp (vector a))))))
      (f-tmp (let ((f-tmp (List.drop (let ((f-tmp (Nat.+ i))) (f-tmp 1))))) (f-tmp as))))))))

; /end continuations._external.base.M1l.List.replace

(define continuations._external.base.M1l.List.replace 16qjsrhv7rblpv0ttfrg9lmdq3671de3k37ij3o7qijhpq85u9ob7aq2tsklo65pr1m3ab7v62ibg3ck6q4p2mf9i7mniakcefuljr0)

; base.Trie.head : Some(Forall(|𝕖/0 #0|(Forall(|k/0 #0|(Forall(|v/0 #0|(Arrow(App(App(Ref(#ps42cgn378), vark (#0)), varv (#0)), Effect(Effects([var𝕖 (#0)]), App(Ref(#5isltsdct9), varv (#0)))))))))))

(print-processing "base.Trie.head")

(define 3ltku227r46636ddbf8fv2odrvg7r7ppc4ookvps73d8h9jl4p7806sgejedqkknmpt89q4ca54uufvv157bmgjo786s1u8f1b1aa18
  (lambda
   (trie)
   (let
    ((tmp-match-head trie))
    (let
     ((result
       (let
        ((tmp-0 tmp-match-head))
        (if
         (and
          (list? tmp-0)
          (equal?
           'ps42cgn3788qukbk25ea48dgb0c44i5fmov3tuqra3d8sr2lvhamngqfnr685jb5iand8jq7oa5a30tp2njh5hehnjqc9dbp2bp8c88_0
           (list-ref tmp-0 0))
          (equal? (length tmp-0) 3))
         (let ((head (list-ref tmp-0 1))) head)
         'fallthrough))))
     (if (equal? 'fallthrough result) (no-match) result)))))

; /end base.Trie.head

(define base.Trie.head 3ltku227r46636ddbf8fv2odrvg7r7ppc4ookvps73d8h9jl4p7806sgejedqkknmpt89q4ca54uufvv157bmgjo786s1u8f1b1aa18)

; continuations._external.base.M1l.Trie.tail : Some(Forall(|𝕖/0 #0|(Forall(|k/0 #0|(Forall(|v/0 #0|(Arrow(App(App(Ref(#ps42cgn378), vark (#0)), varv (#0)), Effect(Effects([var𝕖 (#0)]), App(App(Ref(#7di5ureqgi), vark (#0)), App(App(Ref(#ps42cgn378), vark (#0)), varv (#0))))))))))))

(print-processing "continuations._external.base.M1l.Trie.tail")

(define 7t8atr8n4r2kjeip1vc7korq2inmun9mgs52pk8j6dehqtk258oquiretfhea2trr85ujpu1bo62h1lshkpuehisqdjqeqt37l2nj7g
  (lambda
   (trie)
   (let
    ((tmp-match-head trie))
    (let
     ((result
       (let
        ((tmp-0 tmp-match-head))
        (if
         (and
          (list? tmp-0)
          (equal?
           'ps42cgn3788qukbk25ea48dgb0c44i5fmov3tuqra3d8sr2lvhamngqfnr685jb5iand8jq7oa5a30tp2njh5hehnjqc9dbp2bp8c88_0
           (list-ref tmp-0 0))
          (equal? (length tmp-0) 3))
         (let ((tail (list-ref tmp-0 2))) tail)
         'fallthrough))))
     (if (equal? 'fallthrough result) (no-match) result)))))

; /end continuations._external.base.M1l.Trie.tail

(define continuations._external.base.M1l.Trie.tail 7t8atr8n4r2kjeip1vc7korq2inmun9mgs52pk8j6dehqtk258oquiretfhea2trr85ujpu1bo62h1lshkpuehisqdjqeqt37l2nj7g)

; continuations._external.base.M1l.Map.keys : Some(Forall(|𝕖/0 #0|(Forall(|k/0 #0|(Forall(|v/0 #0|(Arrow(App(App(Ref(#7di5ureqgi), vark (#0)), varv (#0)), Effect(Effects([var𝕖 (#0)]), App(Ref(Sequence), vark (#0)))))))))))

(print-processing "continuations._external.base.M1l.Map.keys")

(define 7rlku6qr87ovottvi42q3e13428n403nr411hr6u9g3kkkmc10n10gpfi9g0qm9o1hcbkq38njkkcnm8td4pr8lokb5p38tjiums3i0
  (lambda
   (m)
   (let
    ((tmp-match-head m))
    (let
     ((result
       (let
        ((tmp-0 tmp-match-head))
        (if
         (and
          (list? tmp-0)
          (equal?
           '7di5ureqgi60ue42886240kbovfhko0fg85rp2thpkl8af699upsl0os1btk27te1cjdmuerad5oi9bdd04me6mjh2m25djbj236fbo_0
           (list-ref tmp-0 0))
          (equal? (length tmp-0) 3))
         (let ((ks (list-ref tmp-0 1))) ks)
         'fallthrough))))
     (if (equal? 'fallthrough result) (no-match) result)))))

; /end continuations._external.base.M1l.Map.keys

(define continuations._external.base.M1l.Map.keys 7rlku6qr87ovottvi42q3e13428n403nr411hr6u9g3kkkmc10n10gpfi9g0qm9o1hcbkq38njkkcnm8td4pr8lokb5p38tjiums3i0)

; base.List.zip : Some(Forall(|𝕖/0 #0|(Forall(|𝕖111/0 #0|(Forall(|a/0 #0|(Forall(|b/0 #0|(Arrow(App(Ref(Sequence), vara (#0)), Effect(Effects([var𝕖111 (#0)]), Arrow(App(Ref(Sequence), varb (#0)), Effect(Effects([var𝕖 (#0)]), App(Ref(Sequence), App(App(Ref(#onbcm0qctb), vara (#0)), App(App(Ref(#onbcm0qctb), varb (#0)), Ref(#568rsi7o3g)))))))))))))))))

(print-processing "base.List.zip")

(define ooldaeqlo45hkl73dsa121o1amqobrq05vn8spick71tr8cla30aq5109fpfoqom41bica6b0e5isic60hm6nek1avplc9cvop8hmqo
  (lambda
   (as)
   (lambda
    (bs)
    (letrec
     ((go
       (lambda
        (acc)
        (lambda
         (i)
         (let
          ((tmp-match-head
            (let
             ((f-tmp
               (onbcm0qctbnuctpm57tkc5p16b8gfke8thjf19p4r4laokji0b606rd0frnhj103qb90lve3fohkoc1eda70491hot656s1m6kk3cn0_0
                (let ((f-tmp (List.at i))) (f-tmp as)))))
             (f-tmp
              (let
               ((f-tmp
                 (onbcm0qctbnuctpm57tkc5p16b8gfke8thjf19p4r4laokji0b606rd0frnhj103qb90lve3fohkoc1eda70491hot656s1m6kk3cn0_0
                  (let ((f-tmp (List.at i))) (f-tmp bs)))))
               (f-tmp
                568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8_0))))))
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
               (if
                (equal?
                 '5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_0
                 (list-ref tmp-0 1))
                (let
                 ((tmp-1 (list-ref tmp-0 2)))
                 (if
                  (and
                   (list? tmp-1)
                   (equal?
                    'onbcm0qctbnuctpm57tkc5p16b8gfke8thjf19p4r4laokji0b606rd0frnhj103qb90lve3fohkoc1eda70491hot656s1m6kk3cn0_0
                    (list-ref tmp-1 0))
                   (equal? (length tmp-1) 3))
                  (if
                   (equal?
                    '568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8_0
                    (list-ref tmp-1 2))
                   acc
                   'fallthrough)
                  'fallthrough))
                'fallthrough)
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
                   'onbcm0qctbnuctpm57tkc5p16b8gfke8thjf19p4r4laokji0b606rd0frnhj103qb90lve3fohkoc1eda70491hot656s1m6kk3cn0_0
                   (list-ref tmp-0 0))
                  (equal? (length tmp-0) 3))
                 (let
                  ((tmp-1 (list-ref tmp-0 2)))
                  (if
                   (and
                    (list? tmp-1)
                    (equal?
                     'onbcm0qctbnuctpm57tkc5p16b8gfke8thjf19p4r4laokji0b606rd0frnhj103qb90lve3fohkoc1eda70491hot656s1m6kk3cn0_0
                     (list-ref tmp-1 0))
                    (equal? (length tmp-1) 3))
                   (if
                    (equal?
                     '5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_0
                     (list-ref tmp-1 1))
                    (if
                     (equal?
                      '568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8_0
                      (list-ref tmp-1 2))
                     acc
                     'fallthrough)
                    'fallthrough)
                   'fallthrough))
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
                     'onbcm0qctbnuctpm57tkc5p16b8gfke8thjf19p4r4laokji0b606rd0frnhj103qb90lve3fohkoc1eda70491hot656s1m6kk3cn0_0
                     (list-ref tmp-0 0))
                    (equal? (length tmp-0) 3))
                   (let
                    ((tmp-1 (list-ref tmp-0 1)))
                    (if
                     (and
                      (list? tmp-1)
                      (equal?
                       '5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_1
                       (list-ref tmp-1 0))
                      (equal? (length tmp-1) 2))
                     (let
                      ((a (list-ref tmp-1 1)))
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
                         ((tmp-2 (list-ref tmp-1 1)))
                         (if
                          (and
                           (list? tmp-2)
                           (equal?
                            '5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_1
                            (list-ref tmp-2 0))
                           (equal? (length tmp-2) 2))
                          (let
                           ((b (list-ref tmp-2 1)))
                           (if
                            (equal?
                             '568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8_0
                             (list-ref tmp-1 2))
                            (let
                             ((f-tmp
                               (go
                                (let
                                 ((f-tmp (List.snoc acc)))
                                 (f-tmp
                                  (let
                                   ((f-tmp
                                     (onbcm0qctbnuctpm57tkc5p16b8gfke8thjf19p4r4laokji0b606rd0frnhj103qb90lve3fohkoc1eda70491hot656s1m6kk3cn0_0
                                      a)))
                                   (f-tmp
                                    (let
                                     ((f-tmp
                                       (onbcm0qctbnuctpm57tkc5p16b8gfke8thjf19p4r4laokji0b606rd0frnhj103qb90lve3fohkoc1eda70491hot656s1m6kk3cn0_0
                                        b)))
                                     (f-tmp
                                      568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8_0)))))))))
                             (f-tmp (let ((f-tmp (Nat.+ i))) (f-tmp 1))))
                            'fallthrough))
                          'fallthrough))
                        'fallthrough)))
                     'fallthrough))
                   'fallthrough))))
               (if (equal? 'fallthrough result) (no-match) result))
              result))
            result)))))))
     (let ((f-tmp (go (vector )))) (f-tmp 0))))))

; /end base.List.zip

(define base.List.zip ooldaeqlo45hkl73dsa121o1amqobrq05vn8spick71tr8cla30aq5109fpfoqom41bica6b0e5isic60hm6nek1avplc9cvop8hmqo)

; continuations._external.base.M1l.Map.values : Some(Forall(|𝕖/0 #0|(Forall(|k/0 #0|(Forall(|v/0 #0|(Arrow(App(App(Ref(#7di5ureqgi), vark (#0)), varv (#0)), Effect(Effects([var𝕖 (#0)]), App(Ref(Sequence), varv (#0)))))))))))

(print-processing "continuations._external.base.M1l.Map.values")

(define tidndnn1taua8pe9v0vub959s3fg9j7dv7v5u03maqqbpk89g06eu3j1sji9ssifachpso83rr4hh7mmu9r3kei5oil3691hsncrs0g
  (lambda
   (m)
   (let
    ((tmp-match-head m))
    (let
     ((result
       (let
        ((tmp-0 tmp-match-head))
        (if
         (and
          (list? tmp-0)
          (equal?
           '7di5ureqgi60ue42886240kbovfhko0fg85rp2thpkl8af699upsl0os1btk27te1cjdmuerad5oi9bdd04me6mjh2m25djbj236fbo_0
           (list-ref tmp-0 0))
          (equal? (length tmp-0) 3))
         (let ((vs (list-ref tmp-0 2))) vs)
         'fallthrough))))
     (if (equal? 'fallthrough result) (no-match) result)))))

; /end continuations._external.base.M1l.Map.values

(define continuations._external.base.M1l.Map.values tidndnn1taua8pe9v0vub959s3fg9j7dv7v5u03maqqbpk89g06eu3j1sji9ssifachpso83rr4hh7mmu9r3kei5oil3691hsncrs0g)

; continuations._external.base.M1l.Map.toList : Some(Forall(|𝕖/0 #0|(Forall(|k/0 #0|(Forall(|v/0 #0|(Arrow(App(App(Ref(#7di5ureqgi), vark (#0)), varv (#0)), Effect(Effects([var𝕖 (#0)]), App(Ref(Sequence), App(App(Ref(#onbcm0qctb), vark (#0)), App(App(Ref(#onbcm0qctb), varv (#0)), Ref(#568rsi7o3g)))))))))))))

(print-processing "continuations._external.base.M1l.Map.toList")

(define ausqmo0vrtdm6lo1l2c0o6aord9i20hsap890ruk3s48mb8g6akn810inn23h46dffa0it6jhhit97un6645vocn3b9vfg93g2p8n40
  (lambda
   (m)
   (let
    ((f-tmp
      (ooldaeqlo45hkl73dsa121o1amqobrq05vn8spick71tr8cla30aq5109fpfoqom41bica6b0e5isic60hm6nek1avplc9cvop8hmqo
       (7rlku6qr87ovottvi42q3e13428n403nr411hr6u9g3kkkmc10n10gpfi9g0qm9o1hcbkq38njkkcnm8td4pr8lokb5p38tjiums3i0
        m))))
    (f-tmp
     (tidndnn1taua8pe9v0vub959s3fg9j7dv7v5u03maqqbpk89g06eu3j1sji9ssifachpso83rr4hh7mmu9r3kei5oil3691hsncrs0g
      m)))))

; /end continuations._external.base.M1l.Map.toList

(define continuations._external.base.M1l.Map.toList ausqmo0vrtdm6lo1l2c0o6aord9i20hsap890ruk3s48mb8g6akn810inn23h46dffa0it6jhhit97un6645vocn3b9vfg93g2p8n40)

; continuations._external.base.M1l.test.internals.v1.Test.Report.toCLIResult : Some(Forall(|𝕖/0 #0|(Arrow(Ref(#op6dq1nj40), Effect(Effects([var𝕖 (#0)]), App(Ref(Sequence), Ref(#vmc06s4f23)))))))

(print-processing "continuations._external.base.M1l.test.internals.v1.Test.Report.toCLIResult")

(define hc6ot5gcd985tqi7188e08ui2t4mp7jfolcie602c7sq718fu9rulh62a60hn4d0b1bacq734qfne0m0jsksk7chsmomqgiuaudts0g
  (lambda
   (r)
   (letrec
    ((descend
      (lambda
       (scope)
       (lambda
        (p)
        (let
         ((tmp-match-head p))
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
               ((k (list-ref tmp-0 1)))
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
                  ((t (list-ref tmp-1 1)))
                  (if
                   (equal?
                    '568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8_0
                    (list-ref tmp-1 2))
                   (let
                    ((f-tmp
                      (go
                       (let
                        ((f-tmp
                          (Text.++
                           (if
                            (let ((f-tmp (Text.!= scope))) (f-tmp ""))
                            (let ((f-tmp (Text.++ scope))) (f-tmp "."))
                            ""))))
                        (f-tmp k)))))
                    (f-tmp t))
                   'fallthrough))
                 'fallthrough)))
              'fallthrough))))
          (if (equal? 'fallthrough result) (no-match) result))))))
     (convert
      (lambda
       (scope)
       (lambda
        (s)
        (let
         ((tmp-match-head s))
         (let
          ((result
            (if
             (equal?
              'mo80e1n080jd4hsln94nerrejv2bacn4cd9hoebifsabm44mjvs1km04dkvlue5o0op31vbjad9f2ld6227qeqdn787e4jdrdi89p50_0
              tmp-match-head)
             (vmc06s4f236sps61vqv35g7ridnae03uetth98aocort1825stbv7m6ncfca2j0gcane47c8db2rjtd2o6kch2lr7v2gst895pcs0m0_0
              scope)
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
                  'mo80e1n080jd4hsln94nerrejv2bacn4cd9hoebifsabm44mjvs1km04dkvlue5o0op31vbjad9f2ld6227qeqdn787e4jdrdi89p50_2
                  (list-ref tmp-0 0))
                 (equal? (length tmp-0) 2))
                (let
                 ((tmp-1 (list-ref tmp-0 1)))
                 (if
                  (and
                   (list? tmp-1)
                   (equal?
                    'md6vee7q626dlqhdao1ecjgaa2v19dhhnq9l8mr80o2gf67qprmkqe5atbve2m997u7fur9cahnqklhst1sd3kb84b36jqtol5klbbg_1
                    (list-ref tmp-1 0))
                   (equal? (length tmp-1) 2))
                  (let
                   ((n (list-ref tmp-1 1)))
                   (vmc06s4f236sps61vqv35g7ridnae03uetth98aocort1825stbv7m6ncfca2j0gcane47c8db2rjtd2o6kch2lr7v2gst895pcs0m0_1
                    (let
                     ((f-tmp
                       (Text.++
                        (let
                         ((f-tmp (Text.++ (let ((f-tmp (Text.++ scope))) (f-tmp " : Passed ")))))
                         (f-tmp (Nat.toText n))))))
                     (f-tmp " tests."))))
                  'fallthrough))
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
                    'mo80e1n080jd4hsln94nerrejv2bacn4cd9hoebifsabm44mjvs1km04dkvlue5o0op31vbjad9f2ld6227qeqdn787e4jdrdi89p50_2
                    (list-ref tmp-0 0))
                   (equal? (length tmp-0) 2))
                  (if
                   (equal?
                    'md6vee7q626dlqhdao1ecjgaa2v19dhhnq9l8mr80o2gf67qprmkqe5atbve2m997u7fur9cahnqklhst1sd3kb84b36jqtol5klbbg_0
                    (list-ref tmp-0 1))
                   (vmc06s4f236sps61vqv35g7ridnae03uetth98aocort1825stbv7m6ncfca2j0gcane47c8db2rjtd2o6kch2lr7v2gst895pcs0m0_1
                    (let ((f-tmp (Text.++ scope))) (f-tmp " : Proved.")))
                   'fallthrough)
                  'fallthrough))))
              (if (equal? 'fallthrough result) (no-match) result))
             result))
           result))))))
     (go
      (lambda
       (scope)
       (lambda
        (t)
        (letrec
         ((rest
           (let
            ((f-tmp
              (12em6b2ruk6gtviraalfikch4ia40lin0kq12bgmsslt2miph795d1nt86ud2rgqi0ia3aqrb4pmu2l24j40a411gc65io037h8r23o
               (descend scope))))
            (f-tmp
             (ausqmo0vrtdm6lo1l2c0o6aord9i20hsap890ruk3s48mb8g6akn810inn23h46dffa0it6jhhit97un6645vocn3b9vfg93g2p8n40
              (7t8atr8n4r2kjeip1vc7korq2inmun9mgs52pk8j6dehqtk258oquiretfhea2trr85ujpu1bo62h1lshkpuehisqdjqeqt37l2nj7g
               t))))))
         (let
          ((tmp-match-head
            (3ltku227r46636ddbf8fv2odrvg7r7ppc4ookvps73d8h9jl4p7806sgejedqkknmpt89q4ca54uufvv157bmgjo786s1u8f1b1aa18
             t)))
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
                ((status (list-ref tmp-0 1)))
                (let ((f-tmp (List.cons (let ((f-tmp (convert scope))) (f-tmp status))))) (f-tmp rest)))
               'fallthrough))))
           (if
            (equal? 'fallthrough result)
            (let
             ((result
               (if
                (equal?
                 '5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_0
                 tmp-match-head)
                rest
                'fallthrough)))
             (if (equal? 'fallthrough result) (no-match) result))
            result))))))))
    (let
     ((tmp-match-head r))
     (let
      ((result
        (let
         ((tmp-0 tmp-match-head))
         (if
          (and
           (list? tmp-0)
           (equal?
            'op6dq1nj40oelkl970uve3mv5icih2a4ms65h3ihbi7lv68ffpfnd3t7d3olllqlghlnik2tt5vlr8nn471kfil7pd4pprqh5h96cig_0
            (list-ref tmp-0 0))
           (equal? (length tmp-0) 2))
          (let ((t (list-ref tmp-0 1))) (let ((f-tmp (go ""))) (f-tmp t)))
          'fallthrough))))
      (if (equal? 'fallthrough result) (no-match) result))))))

; /end continuations._external.base.M1l.test.internals.v1.Test.Report.toCLIResult

(define continuations._external.base.M1l.test.internals.v1.Test.Report.toCLIResult hc6ot5gcd985tqi7188e08ui2t4mp7jfolcie602c7sq718fu9rulh62a60hn4d0b1bacq734qfne0m0jsksk7chsmomqgiuaudts0g)

; base.test.internals.v1.Test.report : Some(Forall(|𝕖/0 #0|(Arrow(Ref(#fjhnoa53kd), Effect(Effects([var𝕖 (#0)]), Ref(#op6dq1nj40))))))

(print-processing "base.test.internals.v1.Test.report")

(define mo3sn64t3a39or7ieeugfn6i7dc4k3uvgcll0mcvjdn9aovp7sdtv107j6l4fp2qt3sfb34g6lujpfufecgirbs4okgscsd7lh42b7o
  (lambda
   (t)
   (let
    ((tmp-match-head t))
    (let
     ((result
       (let
        ((tmp-0 tmp-match-head))
        (if
         (and
          (list? tmp-0)
          (equal?
           'fjhnoa53kdshescmgm65m29g183m4e65i5euipgf8s2a2lhhvolmfrahu3ggot4oe2hqk84c8uom6l7kog2l96vpkic5fjq70rjv0n8_0
           (list-ref tmp-0 0))
          (equal? (length tmp-0) 2))
         (let
          ((k (list-ref tmp-0 1)))
          (k
           (c6embbsl59hin2c7ndqboquchoi44786icggh4niodbar0sf5k4eue1qcj69ps8359u1bi5t6rr2hvcpkvego2euq6dldpcne7hlg38_0
            (vector ))))
         'fallthrough))))
     (if (equal? 'fallthrough result) (no-match) result)))))

; /end base.test.internals.v1.Test.report

(define base.test.internals.v1.Test.report mo3sn64t3a39or7ieeugfn6i7dc4k3uvgcll0mcvjdn9aovp7sdtv107j6l4fp2qt3sfb34g6lujpfufecgirbs4okgscsd7lh42b7o)

; continuations._external.base.M1l.test.internals.v1.Test.run : Some(Forall(|𝕖/0 #0|(Arrow(Ref(#fjhnoa53kd), Effect(Effects([var𝕖 (#0)]), App(Ref(Sequence), Ref(#vmc06s4f23)))))))

(print-processing "continuations._external.base.M1l.test.internals.v1.Test.run")

(define cf8c1a0tu8qvuihlroohh6gn3hidudkgb939359hh1v6f7im7b745ne9qh6bseo1jkr0p35tb57siau069r22vof3c023ng5pdflvo0
  (let
   ((f-tmp
     (0hoa6tis7604fo17o91lfjflsvd843uvm7sueu5rfqqn5nbneajoh9fldfbmmjnhh1h2690ktfflrbb96q9lksesuh3t5amcc1ph92g
      hc6ot5gcd985tqi7188e08ui2t4mp7jfolcie602c7sq718fu9rulh62a60hn4d0b1bacq734qfne0m0jsksk7chsmomqgiuaudts0g)))
   (f-tmp
    mo3sn64t3a39or7ieeugfn6i7dc4k3uvgcll0mcvjdn9aovp7sdtv107j6l4fp2qt3sfb34g6lujpfufecgirbs4okgscsd7lh42b7o)))

; /end continuations._external.base.M1l.test.internals.v1.Test.run

(define continuations._external.base.M1l.test.internals.v1.Test.run cf8c1a0tu8qvuihlroohh6gn3hidudkgb939359hh1v6f7im7b745ne9qh6bseo1jkr0p35tb57siau069r22vof3c023ng5pdflvo0)

; base.test.internals.v1.foldScope : Some(Forall(|𝕖/0 #0|(Forall(|𝕖111/0 #0|(Forall(|r/0 #0|(Arrow(Arrow(App(Ref(Sequence), Ref(Text)), Effect(Effects([var𝕖 (#0)]), varr (#0))), Effect(Effects([var𝕖111 (#0)]), Arrow(Ref(#c6embbsl59), Effect(Effects([var𝕖 (#0)]), varr (#0))))))))))))

(print-processing "base.test.internals.v1.foldScope")

(define fcgl4gvu81v618gdhlses5kbd72rdnia4ioangn10q5o5st6tmo6ga0an9neolgtuo14d2mjv79snj472emp5nkas4i09omoao2dih8
  (lambda
   (k)
   (lambda
    (s)
    (let
     ((tmp-match-head s))
     (let
      ((result
        (let
         ((tmp-0 tmp-match-head))
         (if
          (and
           (list? tmp-0)
           (equal?
            'c6embbsl59hin2c7ndqboquchoi44786icggh4niodbar0sf5k4eue1qcj69ps8359u1bi5t6rr2hvcpkvego2euq6dldpcne7hlg38_0
            (list-ref tmp-0 0))
           (equal? (length tmp-0) 2))
          (let ((ss (list-ref tmp-0 1))) (k ss))
          'fallthrough))))
      (if (equal? 'fallthrough result) (no-match) result))))))

; /end base.test.internals.v1.foldScope

(define base.test.internals.v1.foldScope fcgl4gvu81v618gdhlses5kbd72rdnia4ioangn10q5o5st6tmo6ga0an9neolgtuo14d2mjv79snj472emp5nkas4i09omoao2dih8)

; base.Map.empty : Some(Forall(|k/0 #0|(Forall(|v/0 #0|(App(App(Ref(#7di5ureqgi), vark (#0)), varv (#0)))))))

(print-processing "base.Map.empty")

(define gn13vdupub60p22r3lc5107bc3jq1vl9s0l93hq80naemb9e4f7bqdavfc7pr0uvug8on0qqcrk23cbo8pam08b2flffi8j98h5duq8
  (let
   ((f-tmp
     (7di5ureqgi60ue42886240kbovfhko0fg85rp2thpkl8af699upsl0os1btk27te1cjdmuerad5oi9bdd04me6mjh2m25djbj236fbo_0
      (vector ))))
   (f-tmp (vector ))))

; /end base.Map.empty

(define base.Map.empty gn13vdupub60p22r3lc5107bc3jq1vl9s0l93hq80naemb9e4f7bqdavfc7pr0uvug8on0qqcrk23cbo8pam08b2flffi8j98h5duq8)

; continuations._external.base.M1l.List.insert : Some(Forall(|𝕖/0 #0|(Forall(|𝕖111/0 #0|(Forall(|𝕖222/0 #0|(Forall(|a/0 #0|(Arrow(Ref(Nat), Effect(Effects([var𝕖222 (#0)]), Arrow(vara (#0), Effect(Effects([var𝕖111 (#0)]), Arrow(App(Ref(Sequence), vara (#0)), Effect(Effects([var𝕖 (#0)]), App(Ref(Sequence), vara (#0)))))))))))))))))

(print-processing "continuations._external.base.M1l.List.insert")

(define 76q0f96nprvosr72m4afr4csvn6e3smh05agbtn9dkm71b6heormn8ueoa03bkfaklsiqp90f7nqoef8ubbdqq28bbedd394lqbs628
  (lambda
   (i)
   (lambda
    (a)
    (lambda
     (as)
     (let
      ((f-tmp (List.++ (let ((f-tmp (List.++ (let ((f-tmp (List.take i))) (f-tmp as))))) (f-tmp (vector a))))))
      (f-tmp (let ((f-tmp (List.drop i))) (f-tmp as))))))))

; /end continuations._external.base.M1l.List.insert

(define continuations._external.base.M1l.List.insert 76q0f96nprvosr72m4afr4csvn6e3smh05agbtn9dkm71b6heormn8ueoa03bkfaklsiqp90f7nqoef8ubbdqq28bbedd394lqbs628)

; continuations._external.base.M1l.Search.lub : Some(Forall(|𝕖111/0 #0|(Forall(|𝕖/0 #0|(Forall(|𝕖222/0 #0|(Arrow(Arrow(Ref(Nat), Effect(Effects([var𝕖 (#0)]), Ref(Int))), Effect(Effects([var𝕖222 (#0)]), Arrow(Ref(Nat), Effect(Effects([var𝕖111 (#0)]), Arrow(Ref(Nat), Effect(Effects([var𝕖 (#0)]), Ref(Nat))))))))))))))

(print-processing "continuations._external.base.M1l.Search.lub")

(define 4pibi55cis86prlfggcaprd2s0md4l7m3mlmqobrjdds4qjva640qbge2hbtee04110e4nv21p1c0qln9rpha8tltbs8kau937vlfdg
  (lambda
   (hit)
   (lambda
    (bot)
    (lambda
     (top)
     (if
      (let ((f-tmp (Universal.>= bot))) (f-tmp top))
      top
      (letrec
       ((mid (let ((f-tmp (Nat./ (let ((f-tmp (Nat.+ bot))) (f-tmp top))))) (f-tmp 2))))
       (let
        ((tmp-match-head (hit mid)))
        (let
         ((result (if (equal? tmp-match-head 0) mid 'fallthrough)))
         (if
          (equal? 'fallthrough result)
          (let
           ((result
             (if
              (equal? tmp-match-head -1)
              (let
               ((f-tmp
                 (let
                  ((f-tmp
                    (4pibi55cis86prlfggcaprd2s0md4l7m3mlmqobrjdds4qjva640qbge2hbtee04110e4nv21p1c0qln9rpha8tltbs8kau937vlfdg
                     hit)))
                  (f-tmp bot))))
               (f-tmp mid))
              'fallthrough)))
           (if
            (equal? 'fallthrough result)
            (let
             ((result
               (if
                (equal? tmp-match-head 1)
                (let
                 ((f-tmp
                   (let
                    ((f-tmp
                      (4pibi55cis86prlfggcaprd2s0md4l7m3mlmqobrjdds4qjva640qbge2hbtee04110e4nv21p1c0qln9rpha8tltbs8kau937vlfdg
                       hit)))
                    (f-tmp (let ((f-tmp (Nat.+ mid))) (f-tmp 1))))))
                 (f-tmp top))
                'fallthrough)))
             (if (equal? 'fallthrough result) (no-match) result))
            result))
          result)))))))))

; /end continuations._external.base.M1l.Search.lub

(define continuations._external.base.M1l.Search.lub 4pibi55cis86prlfggcaprd2s0md4l7m3mlmqobrjdds4qjva640qbge2hbtee04110e4nv21p1c0qln9rpha8tltbs8kau937vlfdg)

; base.Search.lubIndexOf' : Some(Forall(|𝕖/0 #0|(Forall(|𝕖111/0 #0|(Forall(|𝕖222/0 #0|(Forall(|a/0 #0|(Arrow(vara (#0), Effect(Effects([var𝕖222 (#0)]), Arrow(Ref(Nat), Effect(Effects([var𝕖111 (#0)]), Arrow(App(Ref(Sequence), vara (#0)), Effect(Effects([var𝕖 (#0)]), Ref(Nat))))))))))))))))

(print-processing "base.Search.lubIndexOf\'")

(define v6q3nv802g3lfosq1lhn1976udu03kvu4i97ml4upbtu7d8dd6f3u7rhejgpjpgnbjlgrhfb483suoarv5qu3dmh1u7gk3imoaqcdmg
  (lambda
   (a)
   (lambda
    (start)
    (lambda
     (s)
     (letrec
      ((ao
        (5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_1
         a)))
      (let
       ((f-tmp
         (let
          ((f-tmp
            (4pibi55cis86prlfggcaprd2s0md4l7m3mlmqobrjdds4qjva640qbge2hbtee04110e4nv21p1c0qln9rpha8tltbs8kau937vlfdg
             (lambda (i) (let ((f-tmp (Universal.compare ao))) (f-tmp (let ((f-tmp (List.at i))) (f-tmp s))))))))
          (f-tmp start))))
       (f-tmp (List.size s))))))))

; /end base.Search.lubIndexOf'

(define base.Search.lubIndexOf-quot v6q3nv802g3lfosq1lhn1976udu03kvu4i97ml4upbtu7d8dd6f3u7rhejgpjpgnbjlgrhfb483suoarv5qu3dmh1u7gk3imoaqcdmg)

; continuations._external.base.M1l.Search.lubIndexOf : Some(Forall(|𝕖/0 #0|(Forall(|𝕖111/0 #0|(Forall(|a/0 #0|(Arrow(vara (#0), Effect(Effects([var𝕖111 (#0)]), Arrow(App(Ref(Sequence), vara (#0)), Effect(Effects([var𝕖 (#0)]), Ref(Nat))))))))))))

(print-processing "continuations._external.base.M1l.Search.lubIndexOf")

(define 95oa9uosupnsibr1c0ijfnbb0o7bhvb04gvqdqfa0rr4rjtclprgp5pll3h87v13alfvqvoa3c32gaiaqfiepv8hhjg43tppnqgl8eg
  (lambda
   (a)
   (lambda
    (s)
    (let
     ((f-tmp
       (let
        ((f-tmp
          (v6q3nv802g3lfosq1lhn1976udu03kvu4i97ml4upbtu7d8dd6f3u7rhejgpjpgnbjlgrhfb483suoarv5qu3dmh1u7gk3imoaqcdmg
           a)))
        (f-tmp 0))))
     (f-tmp s)))))

; /end continuations._external.base.M1l.Search.lubIndexOf

(define continuations._external.base.M1l.Search.lubIndexOf 95oa9uosupnsibr1c0ijfnbb0o7bhvb04gvqdqfa0rr4rjtclprgp5pll3h87v13alfvqvoa3c32gaiaqfiepv8hhjg43tppnqgl8eg)

; base.Map.insert : Some(Forall(|𝕖/0 #0|(Forall(|𝕖111/0 #0|(Forall(|𝕖222/0 #0|(Forall(|k/0 #0|(Forall(|v/0 #0|(Arrow(vark (#0), Effect(Effects([var𝕖222 (#0)]), Arrow(varv (#0), Effect(Effects([var𝕖111 (#0)]), Arrow(App(App(Ref(#7di5ureqgi), vark (#0)), varv (#0)), Effect(Effects([var𝕖 (#0)]), App(App(Ref(#7di5ureqgi), vark (#0)), varv (#0)))))))))))))))))))

(print-processing "base.Map.insert")

(define 74k2cq7jl2ppfnvlrapkepi82nb0pi587lpmijg2543buendea51uah8j1dd8irf7ikpgifafspp2s2lldivhivfiglajand844ton0
  (lambda
   (k)
   (lambda
    (v)
    (lambda
     (m)
     (let
      ((tmp-match-head m))
      (let
       ((result
         (let
          ((tmp-0 tmp-match-head))
          (if
           (and
            (list? tmp-0)
            (equal?
             '7di5ureqgi60ue42886240kbovfhko0fg85rp2thpkl8af699upsl0os1btk27te1cjdmuerad5oi9bdd04me6mjh2m25djbj236fbo_0
             (list-ref tmp-0 0))
            (equal? (length tmp-0) 3))
           (let
            ((ks (list-ref tmp-0 1)))
            (let
             ((vs (list-ref tmp-0 2)))
             (letrec
              ((i
                (let
                 ((f-tmp
                   (95oa9uosupnsibr1c0ijfnbb0o7bhvb04gvqdqfa0rr4rjtclprgp5pll3h87v13alfvqvoa3c32gaiaqfiepv8hhjg43tppnqgl8eg
                    k)))
                 (f-tmp ks))))
              (let
               ((tmp-match-head (let ((f-tmp (List.at i))) (f-tmp ks))))
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
                     ((k-quot (list-ref tmp-0 1)))
                     (if
                      (let ((f-tmp (Universal.== k))) (f-tmp k-quot))
                      (let
                       ((f-tmp
                         (7di5ureqgi60ue42886240kbovfhko0fg85rp2thpkl8af699upsl0os1btk27te1cjdmuerad5oi9bdd04me6mjh2m25djbj236fbo_0
                          ks)))
                       (f-tmp
                        (let
                         ((f-tmp
                           (let
                            ((f-tmp
                              (16qjsrhv7rblpv0ttfrg9lmdq3671de3k37ij3o7qijhpq85u9ob7aq2tsklo65pr1m3ab7v62ibg3ck6q4p2mf9i7mniakcefuljr0
                               i)))
                            (f-tmp v))))
                         (f-tmp vs))))
                      (let
                       ((f-tmp
                         (7di5ureqgi60ue42886240kbovfhko0fg85rp2thpkl8af699upsl0os1btk27te1cjdmuerad5oi9bdd04me6mjh2m25djbj236fbo_0
                          (let
                           ((f-tmp
                             (let
                              ((f-tmp
                                (76q0f96nprvosr72m4afr4csvn6e3smh05agbtn9dkm71b6heormn8ueoa03bkfaklsiqp90f7nqoef8ubbdqq28bbedd394lqbs628
                                 i)))
                              (f-tmp k))))
                           (f-tmp ks)))))
                       (f-tmp
                        (let
                         ((f-tmp
                           (let
                            ((f-tmp
                              (76q0f96nprvosr72m4afr4csvn6e3smh05agbtn9dkm71b6heormn8ueoa03bkfaklsiqp90f7nqoef8ubbdqq28bbedd394lqbs628
                               i)))
                            (f-tmp v))))
                         (f-tmp vs))))))
                    'fallthrough))))
                (if
                 (equal? 'fallthrough result)
                 (let
                  ((result
                    (if
                     (equal?
                      '5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_0
                      tmp-match-head)
                     (let
                      ((f-tmp
                        (7di5ureqgi60ue42886240kbovfhko0fg85rp2thpkl8af699upsl0os1btk27te1cjdmuerad5oi9bdd04me6mjh2m25djbj236fbo_0
                         (let ((f-tmp (List.snoc ks))) (f-tmp k)))))
                      (f-tmp (let ((f-tmp (List.snoc vs))) (f-tmp v))))
                     'fallthrough)))
                  (if (equal? 'fallthrough result) (no-match) result))
                 result))))))
           'fallthrough))))
       (if (equal? 'fallthrough result) (no-match) result)))))))

; /end base.Map.insert

(define base.Map.insert 74k2cq7jl2ppfnvlrapkepi82nb0pi587lpmijg2543buendea51uah8j1dd8irf7ikpgifafspp2s2lldivhivfiglajand844ton0)

; continuations._external.base.M1l.Map.fromList : Some(Forall(|𝕖/0 #0|(Forall(|k/0 #0|(Forall(|v/0 #0|(Arrow(App(Ref(Sequence), App(App(Ref(#onbcm0qctb), vark (#0)), App(App(Ref(#onbcm0qctb), varv (#0)), Ref(#568rsi7o3g)))), Effect(Effects([var𝕖 (#0)]), App(App(Ref(#7di5ureqgi), vark (#0)), varv (#0)))))))))))

(print-processing "continuations._external.base.M1l.Map.fromList")

(define ptf65b6dcb2khdo5ev4gkcq1cl5902kmam67s52gbp54bi8fj52eiap6ilrs206n48an0uo3o982rlduaujbnpcsdf6vs14k0iqm5ho
  (lambda
   (kvs)
   (letrec
    ((go
      (lambda
       (acc)
       (lambda
        (i)
        (let
         ((tmp-match-head (let ((f-tmp (List.at i))) (f-tmp kvs))))
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
                   ((k (list-ref tmp-1 1)))
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
                      ((v (list-ref tmp-2 1)))
                      (if
                       (equal?
                        '568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8_0
                        (list-ref tmp-2 2))
                       (let
                        ((f-tmp
                          (go
                           (let
                            ((f-tmp
                              (let
                               ((f-tmp
                                 (74k2cq7jl2ppfnvlrapkepi82nb0pi587lpmijg2543buendea51uah8j1dd8irf7ikpgifafspp2s2lldivhivfiglajand844ton0
                                  k)))
                               (f-tmp v))))
                            (f-tmp acc)))))
                        (f-tmp (let ((f-tmp (Nat.+ i))) (f-tmp 1))))
                       'fallthrough))
                     'fallthrough)))
                  'fallthrough))
                'fallthrough))))
            (if (equal? 'fallthrough result) (no-match) result))
           result)))))))
    (let
     ((f-tmp
       (go
        gn13vdupub60p22r3lc5107bc3jq1vl9s0l93hq80naemb9e4f7bqdavfc7pr0uvug8on0qqcrk23cbo8pam08b2flffi8j98h5duq8)))
     (f-tmp 0)))))

; /end continuations._external.base.M1l.Map.fromList

(define continuations._external.base.M1l.Map.fromList ptf65b6dcb2khdo5ev4gkcq1cl5902kmam67s52gbp54bi8fj52eiap6ilrs206n48an0uo3o982rlduaujbnpcsdf6vs14k0iqm5ho)

; continuations._external.base.M1l.Trie.singleton : Some(Forall(|𝕖/0 #0|(Forall(|𝕖111/0 #0|(Forall(|k/0 #0|(Forall(|v/0 #0|(Arrow(App(Ref(Sequence), vark (#0)), Effect(Effects([var𝕖111 (#0)]), Arrow(varv (#0), Effect(Effects([var𝕖 (#0)]), App(App(Ref(#ps42cgn378), vark (#0)), varv (#0)))))))))))))))

(print-processing "continuations._external.base.M1l.Trie.singleton")

(define g1j9ts6meqmu64m7c75vngast51suk7snf9ahadfjn06b57v2298qels9eo7gesqc5nfhsno0a1dqubtoo15s8ijoihcmblgo8m8a2o
  (lambda
   (path)
   (lambda
    (v)
    (let
     ((tmp-match-head path))
     (let
      ((result
        (let
         ((tmp-vec-0 tmp-match-head))
         (if
          (= (vector-length tmp-vec-0) 0)
          (let
           ((f-tmp
             (ps42cgn3788qukbk25ea48dgb0c44i5fmov3tuqra3d8sr2lvhamngqfnr685jb5iand8jq7oa5a30tp2njh5hehnjqc9dbp2bp8c88_0
              (5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_1
               v))))
           (f-tmp
            gn13vdupub60p22r3lc5107bc3jq1vl9s0l93hq80naemb9e4f7bqdavfc7pr0uvug8on0qqcrk23cbo8pam08b2flffi8j98h5duq8))
          'fallthrough))))
      (if
       (equal? 'fallthrough result)
       (let
        ((result
          (let
           ((snoc-tmp-0 tmp-match-head))
           (if
            (>= (vector-length snoc-tmp-0) 1)
            (let
             ((k (vector-ref snoc-tmp-0 0)))
             (let
              ((ks ((List.drop 1) snoc-tmp-0)))
              (let
               ((f-tmp
                 (ps42cgn3788qukbk25ea48dgb0c44i5fmov3tuqra3d8sr2lvhamngqfnr685jb5iand8jq7oa5a30tp2njh5hehnjqc9dbp2bp8c88_0
                  5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_0)))
               (f-tmp
                (ptf65b6dcb2khdo5ev4gkcq1cl5902kmam67s52gbp54bi8fj52eiap6ilrs206n48an0uo3o982rlduaujbnpcsdf6vs14k0iqm5ho
                 (vector (let
                    ((f-tmp
                      (onbcm0qctbnuctpm57tkc5p16b8gfke8thjf19p4r4laokji0b606rd0frnhj103qb90lve3fohkoc1eda70491hot656s1m6kk3cn0_0
                       k)))
                    (f-tmp
                     (let
                      ((f-tmp
                        (onbcm0qctbnuctpm57tkc5p16b8gfke8thjf19p4r4laokji0b606rd0frnhj103qb90lve3fohkoc1eda70491hot656s1m6kk3cn0_0
                         (let
                          ((f-tmp
                            (g1j9ts6meqmu64m7c75vngast51suk7snf9ahadfjn06b57v2298qels9eo7gesqc5nfhsno0a1dqubtoo15s8ijoihcmblgo8m8a2o
                             ks)))
                          (f-tmp v)))))
                      (f-tmp
                       568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8_0))))))))))
            'fallthrough))))
        (if (equal? 'fallthrough result) (no-match) result))
       result))))))

; /end continuations._external.base.M1l.Trie.singleton

(define continuations._external.base.M1l.Trie.singleton g1j9ts6meqmu64m7c75vngast51suk7snf9ahadfjn06b57v2298qels9eo7gesqc5nfhsno0a1dqubtoo15s8ijoihcmblgo8m8a2o)

; base.test.internals.v1.Test.finished : Some(Forall(|𝕖/0 #0|(Arrow(Ref(#mo80e1n080), Effect(Effects([var𝕖 (#0)]), Ref(#fjhnoa53kd))))))

(print-processing "base.test.internals.v1.Test.finished")

(define lsfrcr8jqtnjpc5g1n3541rpstc68prmpitmjjem15u81qsiahqtthvelvv2ihnet4sk7dcjkjlusflnp7fmrj2u4f465crcvlum1a0
  (lambda
   (st)
   (fjhnoa53kdshescmgm65m29g183m4e65i5euipgf8s2a2lhhvolmfrahu3ggot4oe2hqk84c8uom6l7kog2l96vpkic5fjq70rjv0n8_0
    (let
     ((f-tmp
       (0hoa6tis7604fo17o91lfjflsvd843uvm7sueu5rfqqn5nbneajoh9fldfbmmjnhh1h2690ktfflrbb96q9lksesuh3t5amcc1ph92g
        op6dq1nj40oelkl970uve3mv5icih2a4ms65h3ihbi7lv68ffpfnd3t7d3olllqlghlnik2tt5vlr8nn471kfil7pd4pprqh5h96cig_0)))
     (f-tmp
      (fcgl4gvu81v618gdhlses5kbd72rdnia4ioangn10q5o5st6tmo6ga0an9neolgtuo14d2mjv79snj472emp5nkas4i09omoao2dih8
       (lambda
        (sc)
        (let
         ((f-tmp
           (g1j9ts6meqmu64m7c75vngast51suk7snf9ahadfjn06b57v2298qels9eo7gesqc5nfhsno0a1dqubtoo15s8ijoihcmblgo8m8a2o
            sc)))
         (f-tmp st)))))))))

; /end base.test.internals.v1.Test.finished

(define base.test.internals.v1.Test.finished lsfrcr8jqtnjpc5g1n3541rpstc68prmpitmjjem15u81qsiahqtthvelvv2ihnet4sk7dcjkjlusflnp7fmrj2u4f465crcvlum1a0)

; base.<| : Some(Forall(|𝕖/0 #0|(Forall(|𝕖111/0 #0|(Forall(|a/0 #0|(Forall(|b/0 #0|(Arrow(Arrow(vara (#0), Effect(Effects([var𝕖 (#0)]), varb (#0))), Effect(Effects([var𝕖111 (#0)]), Arrow(vara (#0), Effect(Effects([var𝕖 (#0)]), varb (#0))))))))))))))

(print-processing "base.<|")

(define oujnf8a703ha04c80ktnmub0rqg53mrrrvoreaacpu2ar9af43qq6r59d3rgm0j3jfogcih1u0c6k8ddqkfbr6mkf5vkrgdviiv3628
  (lambda (f) (lambda (a) (f a))))

; /end base.<|

(define base.<-bar- oujnf8a703ha04c80ktnmub0rqg53mrrrvoreaacpu2ar9af43qq6r59d3rgm0j3jfogcih1u0c6k8ddqkfbr6mkf5vkrgdviiv3628)

; base.test.internals.v1.Test.passed : Some(Ref(#fjhnoa53kd))

(print-processing "base.test.internals.v1.Test.passed")

(define 5lvhdh49khj6973rrcf4sogledj4sntp173m19n21f3lphvfmboc17n34l49dgnaq73qjl0vmraddrqsreqk4ftic8nh9lqiitotip8
  (let
   ((f-tmp
     (oujnf8a703ha04c80ktnmub0rqg53mrrrvoreaacpu2ar9af43qq6r59d3rgm0j3jfogcih1u0c6k8ddqkfbr6mkf5vkrgdviiv3628
      (let
       ((f-tmp
         (0hoa6tis7604fo17o91lfjflsvd843uvm7sueu5rfqqn5nbneajoh9fldfbmmjnhh1h2690ktfflrbb96q9lksesuh3t5amcc1ph92g
          lsfrcr8jqtnjpc5g1n3541rpstc68prmpitmjjem15u81qsiahqtthvelvv2ihnet4sk7dcjkjlusflnp7fmrj2u4f465crcvlum1a0)))
       (f-tmp
        mo80e1n080jd4hsln94nerrejv2bacn4cd9hoebifsabm44mjvs1km04dkvlue5o0op31vbjad9f2ld6227qeqdn787e4jdrdi89p50_2)))))
   (f-tmp
    (md6vee7q626dlqhdao1ecjgaa2v19dhhnq9l8mr80o2gf67qprmkqe5atbve2m997u7fur9cahnqklhst1sd3kb84b36jqtol5klbbg_1
     1))))

; /end base.test.internals.v1.Test.passed

(define base.test.internals.v1.Test.passed 5lvhdh49khj6973rrcf4sogledj4sntp173m19n21f3lphvfmboc17n34l49dgnaq73qjl0vmraddrqsreqk4ftic8nh9lqiitotip8)

; continuations._external.base.M1l.test.internals.v1.Test.failed : Some(Ref(#fjhnoa53kd))

(print-processing "continuations._external.base.M1l.test.internals.v1.Test.failed")

(define hsq13at6ubcamump5dvur313leb15t7egpjb5v3t6hqngi8u9q88c4jrd4pvfh967jdsvt6jppg0m9bfh8lmead5243mjg27gb09gug
  (lsfrcr8jqtnjpc5g1n3541rpstc68prmpitmjjem15u81qsiahqtthvelvv2ihnet4sk7dcjkjlusflnp7fmrj2u4f465crcvlum1a0
   mo80e1n080jd4hsln94nerrejv2bacn4cd9hoebifsabm44mjvs1km04dkvlue5o0op31vbjad9f2ld6227qeqdn787e4jdrdi89p50_0))

; /end continuations._external.base.M1l.test.internals.v1.Test.failed

(define continuations._external.base.M1l.test.internals.v1.Test.failed hsq13at6ubcamump5dvur313leb15t7egpjb5v3t6hqngi8u9q88c4jrd4pvfh967jdsvt6jppg0m9bfh8lmead5243mjg27gb09gug)

; base.test.expect : Some(Forall(|𝕖/0 #0|(Arrow(Ref(Boolean), Effect(Effects([var𝕖 (#0)]), Ref(#fjhnoa53kd))))))

(print-processing "base.test.expect")

(define e9srlu54g5vi14cejiep5vrjrfahiciqu3rffr6bl4pqvh5o06j5rv31h613gstlmve6qdjl0ki1q38fp5gaotgpjpbatk0ep97n95g
  (lambda
   (b)
   (if
    b
    5lvhdh49khj6973rrcf4sogledj4sntp173m19n21f3lphvfmboc17n34l49dgnaq73qjl0vmraddrqsreqk4ftic8nh9lqiitotip8
    hsq13at6ubcamump5dvur313leb15t7egpjb5v3t6hqngi8u9q88c4jrd4pvfh967jdsvt6jppg0m9bfh8lmead5243mjg27gb09gug)))

; /end base.test.expect

(define base.test.expect e9srlu54g5vi14cejiep5vrjrfahiciqu3rffr6bl4pqvh5o06j5rv31h613gstlmve6qdjl0ki1q38fp5gaotgpjpbatk0ep97n95g)

; base.repeat : Some(Forall(|a/0 #0|(Forall(|e/0 #0|(Arrow(Ref(Nat), Effect(Effects([]), Arrow(Arrow(Ref(#568rsi7o3g), Effect(Effects([vare (#0)]), vara (#0))), Effect(Effects([vare (#0)]), Ref(#568rsi7o3g))))))))))

(print-processing "base.repeat")

(define fui79mou2a3a1dn48f2fmcttsu8jua0aa4r2r94mvea9pnq71utjosapcb9ifubc3t020uol9eqrb6ladf0si3ghbpgptd4tqbuv0lo
  (lambda
   (n)
   (lambda
    (op)
    (if
     (let ((f-tmp (Universal.== n))) (f-tmp 0))
     568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8_0
     (let
      ((_1
        (op
         568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8_0)))
      (let
       ((f-tmp
         (fui79mou2a3a1dn48f2fmcttsu8jua0aa4r2r94mvea9pnq71utjosapcb9ifubc3t020uol9eqrb6ladf0si3ghbpgptd4tqbuv0lo
          (let ((f-tmp (Nat.drop n))) (f-tmp 1)))))
       (f-tmp op)))))))

; /end base.repeat

(define base.repeat fui79mou2a3a1dn48f2fmcttsu8jua0aa4r2r94mvea9pnq71utjosapcb9ifubc3t020uol9eqrb6ladf0si3ghbpgptd4tqbuv0lo)

; memo.Memo.memo : Some(Forall(|a/0 #0|(Forall(|e/0 #0|(Arrow(Arrow(Ref(#568rsi7o3g), Effect(Effects([vare (#0)]), vara (#0))), Effect(Effects([vare (#0), Ref(#161o8dftc2)]), vara (#0))))))))

(print-processing "memo.Memo.memo")

(define m43pi6djvd07egpqnq5beb7lrm3263tn809fn4vp3jt15gng7fgu0enbdjmai37fq99eor4ao9h091htm07gbvaq8ru14vcbs0rr07o
  (lambda
   (a)
   (let
    ((key
      (4u0ej7m4le70r112tsa9ej5cg7ksrtc2slpsvtakk80aoc8ls48lpqlbanaihh97in624u0atchvhq45490tgj78i50lq5i69lqk2gg_0
       (lambda (k) (k a)))))
    (let
     ((tmp-match-head
       (161o8dftc2buleq7u87u0uiq1vu11a0otf74jjr3rkimodk9el3pcakvtbmnmlv2hh7d45q4093mofhgkrcor9j9rlv8ptao24dle78_1
        key)))
     (let
      ((result
        (if
         (equal?
          '5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_0
          tmp-match-head)
         (let
          ((result
            (a
             568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8_0)))
          (let
           ((_2
             (let
              ((f-tmp
                (161o8dftc2buleq7u87u0uiq1vu11a0otf74jjr3rkimodk9el3pcakvtbmnmlv2hh7d45q4093mofhgkrcor9j9rlv8ptao24dle78_0
                 key)))
              (f-tmp result))))
           result))
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
            (let ((a (list-ref tmp-0 1))) a)
            'fallthrough))))
        (if (equal? 'fallthrough result) (no-match) result))
       result))))))

; /end memo.Memo.memo

(define memo.Memo.memo m43pi6djvd07egpqnq5beb7lrm3263tn809fn4vp3jt15gng7fgu0enbdjmai37fq99eor4ao9h091htm07gbvaq8ru14vcbs0rr07o)

; base.Search.exact : Some(Forall(|𝕖111/0 #0|(Forall(|𝕖/0 #0|(Forall(|𝕖222/0 #0|(Arrow(Arrow(Ref(Nat), Effect(Effects([var𝕖 (#0)]), Ref(Int))), Effect(Effects([var𝕖222 (#0)]), Arrow(Ref(Nat), Effect(Effects([var𝕖111 (#0)]), Arrow(Ref(Nat), Effect(Effects([var𝕖 (#0)]), App(Ref(#5isltsdct9), Ref(Nat)))))))))))))))

(print-processing "base.Search.exact")

(define cuv5ji02gjv98sapna3lgk1siqivi5pj64k9l7qjl3td4mcrvfjg7mhsjepucj24dpm0r0atmq07mp9gob3plv0q2t4918l1c88pa50
  (lambda
   (hit)
   (lambda
    (bot)
    (lambda
     (top)
     (if
      (let ((f-tmp (Universal.>= bot))) (f-tmp top))
      5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_0
      (letrec
       ((mid (let ((f-tmp (Nat./ (let ((f-tmp (Nat.+ bot))) (f-tmp top))))) (f-tmp 2))))
       (let
        ((tmp-match-head (hit mid)))
        (let
         ((result
           (if
            (equal? tmp-match-head 0)
            (5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_1
             mid)
            'fallthrough)))
         (if
          (equal? 'fallthrough result)
          (let
           ((result
             (if
              (equal? tmp-match-head -1)
              (let
               ((f-tmp
                 (let
                  ((f-tmp
                    (cuv5ji02gjv98sapna3lgk1siqivi5pj64k9l7qjl3td4mcrvfjg7mhsjepucj24dpm0r0atmq07mp9gob3plv0q2t4918l1c88pa50
                     hit)))
                  (f-tmp bot))))
               (f-tmp mid))
              'fallthrough)))
           (if
            (equal? 'fallthrough result)
            (let
             ((result
               (if
                (equal? tmp-match-head 1)
                (let
                 ((f-tmp
                   (let
                    ((f-tmp
                      (cuv5ji02gjv98sapna3lgk1siqivi5pj64k9l7qjl3td4mcrvfjg7mhsjepucj24dpm0r0atmq07mp9gob3plv0q2t4918l1c88pa50
                       hit)))
                    (f-tmp (let ((f-tmp (Nat.+ mid))) (f-tmp 1))))))
                 (f-tmp top))
                'fallthrough)))
             (if (equal? 'fallthrough result) (no-match) result))
            result))
          result)))))))))

; /end base.Search.exact

(define base.Search.exact cuv5ji02gjv98sapna3lgk1siqivi5pj64k9l7qjl3td4mcrvfjg7mhsjepucj24dpm0r0atmq07mp9gob3plv0q2t4918l1c88pa50)

; base.Search.anyIndexOf : Some(Forall(|𝕖/0 #0|(Forall(|𝕖111/0 #0|(Forall(|a/0 #0|(Arrow(vara (#0), Effect(Effects([var𝕖111 (#0)]), Arrow(App(Ref(Sequence), vara (#0)), Effect(Effects([var𝕖 (#0)]), App(Ref(#5isltsdct9), Ref(Nat)))))))))))))

(print-processing "base.Search.anyIndexOf")

(define 9s4hdo67co9gdb16tbnlbe5rh9j94vnovcmlp92hle76d53272qqtar34npgi6h74hri8ntoa80tbmthns4sd4ai7d1een12lgf50b0
  (lambda
   (a)
   (lambda
    (s)
    (letrec
     ((ao
       (5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_1
        a)))
     (let
      ((f-tmp
        (let
         ((f-tmp
           (cuv5ji02gjv98sapna3lgk1siqivi5pj64k9l7qjl3td4mcrvfjg7mhsjepucj24dpm0r0atmq07mp9gob3plv0q2t4918l1c88pa50
            (lambda (i) (let ((f-tmp (Universal.compare ao))) (f-tmp (let ((f-tmp (List.at i))) (f-tmp s))))))))
         (f-tmp 0))))
      (f-tmp (List.size s)))))))

; /end base.Search.anyIndexOf

(define base.Search.anyIndexOf 9s4hdo67co9gdb16tbnlbe5rh9j94vnovcmlp92hle76d53272qqtar34npgi6h74hri8ntoa80tbmthns4sd4ai7d1een12lgf50b0)

; continuations._external.base.M1l.Map.lookup : Some(Forall(|𝕖/0 #0|(Forall(|𝕖111/0 #0|(Forall(|k/0 #0|(Forall(|v/0 #0|(Arrow(vark (#0), Effect(Effects([var𝕖111 (#0)]), Arrow(App(App(Ref(#7di5ureqgi), vark (#0)), varv (#0)), Effect(Effects([var𝕖 (#0)]), App(Ref(#5isltsdct9), varv (#0)))))))))))))))

(print-processing "continuations._external.base.M1l.Map.lookup")

(define j514dideqrl9nlj431g127g8h3upaunkc6av1dmv5bt7rg4ob5l8krf4jf6h570apu3suelf86dr13f6q9pt9rntpojfqqdcpkr8198
  (lambda
   (k)
   (lambda
    (m)
    (let
     ((tmp-match-head m))
     (let
      ((result
        (let
         ((tmp-0 tmp-match-head))
         (if
          (and
           (list? tmp-0)
           (equal?
            '7di5ureqgi60ue42886240kbovfhko0fg85rp2thpkl8af699upsl0os1btk27te1cjdmuerad5oi9bdd04me6mjh2m25djbj236fbo_0
            (list-ref tmp-0 0))
           (equal? (length tmp-0) 3))
          (let
           ((ks (list-ref tmp-0 1)))
           (let
            ((vs (list-ref tmp-0 2)))
            (let
             ((tmp-match-head
               (let
                ((f-tmp
                  (9s4hdo67co9gdb16tbnlbe5rh9j94vnovcmlp92hle76d53272qqtar34npgi6h74hri8ntoa80tbmthns4sd4ai7d1een12lgf50b0
                   k)))
                (f-tmp ks))))
             (let
              ((result
                (if
                 (equal?
                  '5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_0
                  tmp-match-head)
                 5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_0
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
                    (let ((i (list-ref tmp-0 1))) (let ((f-tmp (List.at i))) (f-tmp vs)))
                    'fallthrough))))
                (if (equal? 'fallthrough result) (no-match) result))
               result)))))
          'fallthrough))))
      (if (equal? 'fallthrough result) (no-match) result))))))

; /end continuations._external.base.M1l.Map.lookup

(define continuations._external.base.M1l.Map.lookup j514dideqrl9nlj431g127g8h3upaunkc6av1dmv5bt7rg4ob5l8krf4jf6h570apu3suelf86dr13f6q9pt9rntpojfqqdcpkr8198)

; memo.Memo.run : Some(Forall(|a/0 #0|(Forall(|e/0 #0|(Arrow(Arrow(Ref(#568rsi7o3g), Effect(Effects([vare (#0), Ref(#161o8dftc2)]), vara (#0))), Effect(Effects([vare (#0)]), vara (#0))))))))

(print-processing "memo.Memo.run")

(define p7f4bjb0rk7qi24qki5vmg8gh1avqc7v7kko8g2mve0cfgf6bi5lb81ag7kq4qrkijv5ngr4i39qeiuiknpm39bdiqgk2andlr579k0
  (lambda
   (r)
   (letrec
    ((h
      (lambda
       (cache)
       (lambda
        (g7dthltmco)
        (let
         ((tmp-match-head g7dthltmco))
         (let
          ((result
            (if
             (and
              (list? tmp-match-head)
              (equal? (length tmp-match-head) 2)
              (equal? (car tmp-match-head) 'pure))
             (let ((r (cadr tmp-match-head))) r)
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
                 '161o8dftc2buleq7u87u0uiq1vu11a0otf74jjr3rkimodk9el3pcakvtbmnmlv2hh7d45q4093mofhgkrcor9j9rlv8ptao24dle78_1))
               (let
                ((key (list-ref (caddr tmp-match-head) 1)))
                (let
                 ((k (cadr tmp-match-head)))
                 (handle-ability
                  (lambda
                   ()
                   (k
                    (let
                     ((f-tmp
                       (j514dideqrl9nlj431g127g8h3upaunkc6av1dmv5bt7rg4ob5l8krf4jf6h570apu3suelf86dr13f6q9pt9rntpojfqqdcpkr8198
                        key)))
                     (f-tmp cache))))
                  (h cache))))
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
                  (equal? (length (caddr tmp-match-head)) 3)
                  (equal?
                   (car (caddr tmp-match-head))
                   '161o8dftc2buleq7u87u0uiq1vu11a0otf74jjr3rkimodk9el3pcakvtbmnmlv2hh7d45q4093mofhgkrcor9j9rlv8ptao24dle78_0))
                 (let
                  ((key (list-ref (caddr tmp-match-head) 1)))
                  (let
                   ((result (list-ref (caddr tmp-match-head) 2)))
                   (let
                    ((k (cadr tmp-match-head)))
                    (handle-ability
                     (lambda
                      ()
                      (k
                       568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8_0))
                     (h
                      (let
                       ((f-tmp
                         (let
                          ((f-tmp
                            (74k2cq7jl2ppfnvlrapkepi82nb0pi587lpmijg2543buendea51uah8j1dd8irf7ikpgifafspp2s2lldivhivfiglajand844ton0
                             key)))
                          (f-tmp result))))
                       (f-tmp cache)))))))
                 'fallthrough)))
              (if (equal? 'fallthrough result) (rethrow-effect tmp-match-head) result))
             result))
           result)))))))
    (handle-ability
     (lambda
      ()
      (r
       568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8_0))
     (h
      gn13vdupub60p22r3lc5107bc3jq1vl9s0l93hq80naemb9e4f7bqdavfc7pr0uvug8on0qqcrk23cbo8pam08b2flffi8j98h5duq8)))))

; /end memo.Memo.run

(define memo.Memo.run p7f4bjb0rk7qi24qki5vmg8gh1avqc7v7kko8g2mve0cfgf6bi5lb81ag7kq4qrkijv5ngr4i39qeiuiknpm39bdiqgk2andlr579k0)

; base.Store.modify : Some(Forall(|a/0 #0|(Forall(|g/0 #0|(Arrow(Arrow(vara (#0), Effect(Effects([varg (#0)]), vara (#0))), Effect(Effects([varg (#0), App(Ref(#tkpo8b6903), vara (#0))]), Ref(#568rsi7o3g))))))))

(print-processing "base.Store.modify")

(define q43hpq78pa3tdik7n22dfot4a9ju8s7eomgj0qfv27brtesmi5l79becetiiqklq774n6icofb2jts27qs29k4qg1nlvsn6efipg4t0
  (lambda
   (f)
   (tkpo8b6903kdcggkdolc9kgsq2175dhg0drn17134edkambevl7aahadvueaqml9pemnttm81t7569jbrifmrg82ogjnlfv5gbc2q6o_0
    (f
     (tkpo8b6903kdcggkdolc9kgsq2175dhg0drn17134edkambevl7aahadvueaqml9pemnttm81t7569jbrifmrg82ogjnlfv5gbc2q6o_1)))))

; /end base.Store.modify

(define base.Store.modify q43hpq78pa3tdik7n22dfot4a9ju8s7eomgj0qfv27brtesmi5l79becetiiqklq774n6icofb2jts27qs29k4qg1nlvsn6efipg4t0)

; base.Store.withInitialValue : Some(Forall(|𝕖/0 #0|(Forall(|a/0 #0|(Forall(|g/0 #0|(Forall(|v/0 #0|(Arrow(vara (#0), Effect(Effects([var𝕖 (#0)]), Arrow(Arrow(Ref(#568rsi7o3g), Effect(Effects([varg (#0), App(Ref(#tkpo8b6903), vara (#0))]), varv (#0))), Effect(Effects([varg (#0)]), varv (#0))))))))))))))

(print-processing "base.Store.withInitialValue")

(define sdvlti5vf2e4q4st3c02cgtk0v9rqfjisokds1d6oi8og2dgcvclar9m3g6pmjocu37g0748g12fo74vu91nh4qe4oae09t4vjps8go
  (lambda
   (init)
   (lambda
    (thunk)
    (handle-ability
     (lambda
      ()
      (thunk
       568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8_0))
     (0mo0glpslbgs6mtu2u9a9lt36hpeo4fofeoq8herfsq05heqntt8oo2nbkhopbh7es04bfn9ck0grsvhkt3m5d4jhe6gp4t5lrif0u8
      init)))))

; /end base.Store.withInitialValue

(define base.Store.withInitialValue sdvlti5vf2e4q4st3c02cgtk0v9rqfjisokds1d6oi8og2dgcvclar9m3g6pmjocu37g0748g12fo74vu91nh4qe4oae09t4vjps8go)

; memo.Memo.run.test : Some(App(Ref(Sequence), Ref(#vmc06s4f23)))

(print-processing "memo.Memo.run.test")

(define 1ntq68sntskdtovf4iagc6n8fno8hkph4av6tl7digjgd3i7ttt2vk1ur0ovhf7u811csj70privh3cp278ie4vqr23h89kk46tkieo
  (cf8c1a0tu8qvuihlroohh6gn3hidudkgb939359hh1v6f7im7b745ne9qh6bseo1jkr0p35tb57siau069r22vof3c023ng5pdflvo0
   (let
    ((finalState
      (let
       ((f-tmp
         (sdvlti5vf2e4q4st3c02cgtk0v9rqfjisokds1d6oi8og2dgcvclar9m3g6pmjocu37g0748g12fo74vu91nh4qe4oae09t4vjps8go
          0)))
       (f-tmp
        (lambda
         (_)
         (let
          ((f
            (lambda
             (_)
             (m43pi6djvd07egpqnq5beb7lrm3263tn809fn4vp3jt15gng7fgu0enbdjmai37fq99eor4ao9h091htm07gbvaq8ru14vcbs0rr07o
              (lambda
               (_)
               (q43hpq78pa3tdik7n22dfot4a9ju8s7eomgj0qfv27brtesmi5l79becetiiqklq774n6icofb2jts27qs29k4qg1nlvsn6efipg4t0
                (lambda (x) (let ((f-tmp (Nat.+ x))) (f-tmp 1)))))))))
          (let
           ((_2
             (p7f4bjb0rk7qi24qki5vmg8gh1avqc7v7kko8g2mve0cfgf6bi5lb81ag7kq4qrkijv5ngr4i39qeiuiknpm39bdiqgk2andlr579k0
              (lambda
               (_)
               (let
                ((f-tmp
                  (fui79mou2a3a1dn48f2fmcttsu8jua0aa4r2r94mvea9pnq71utjosapcb9ifubc3t020uol9eqrb6ladf0si3ghbpgptd4tqbuv0lo
                   3)))
                (f-tmp f))))))
           (tkpo8b6903kdcggkdolc9kgsq2175dhg0drn17134edkambevl7aahadvueaqml9pemnttm81t7569jbrifmrg82ogjnlfv5gbc2q6o_1))))))))
    (e9srlu54g5vi14cejiep5vrjrfahiciqu3rffr6bl4pqvh5o06j5rv31h613gstlmve6qdjl0ki1q38fp5gaotgpjpbatk0ep97n95g
     (let ((f-tmp (Universal.== finalState))) (f-tmp 1))))))

; /end memo.Memo.run.test

(define memo.Memo.run.test 1ntq68sntskdtovf4iagc6n8fno8hkph4av6tl7digjgd3i7ttt2vk1ur0ovhf7u811csj70privh3cp278ie4vqr23h89kk46tkieo)

(check-results 1ntq68sntskdtovf4iagc6n8fno8hkph4av6tl7digjgd3i7ttt2vk1ur0ovhf7u811csj70privh3cp278ie4vqr23h89kk46tkieo "memo.Memo.run.test")

; testbed.test : Some(App(Ref(Sequence), Ref(#vmc06s4f23)))

(print-processing "testbed.test")

(define u1b0pko850ncn328b69tv1k916ku62mefajvttmthp69njboj8jjr7oncn0qsvfd13f1rbsujuupbkcc95ft24dq4d66fp23ar53h9o
  1ntq68sntskdtovf4iagc6n8fno8hkph4av6tl7digjgd3i7ttt2vk1ur0ovhf7u811csj70privh3cp278ie4vqr23h89kk46tkieo)

; /end testbed.test

(define testbed.test u1b0pko850ncn328b69tv1k916ku62mefajvttmthp69njboj8jjr7oncn0qsvfd13f1rbsujuupbkcc95ft24dq4d66fp23ar53h9o)

(check-results u1b0pko850ncn328b69tv1k916ku62mefajvttmthp69njboj8jjr7oncn0qsvfd13f1rbsujuupbkcc95ft24dq4d66fp23ar53h9o "testbed.test")