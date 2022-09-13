(load "stdlib.scm")


; base.Store

; Effect(DataDecl { modifier: Structural, bound: [🔣a/0], constructors: [(🔣Store.Store.put/0, Forall(|a/0 #0|(Arrow(vara (#0), Effect(Effects([App(Ref(#tkpo8b6903), vara (#0))]), Ref(#568rsi7o3g)))))), (🔣Store.Store.get/0, Forall(|a/0 #0|(Effect(Effects([App(Ref(#tkpo8b6903), vara (#0))]), vara (#0)))))] })

; Store.Store.put

(define tkpo8b6903kdcggkdolc9kgsq2175dhg0drn17134edkambevl7aahadvueaqml9pemnttm81t7569jbrifmrg82ogjnlfv5gbc2q6o_0 (lambda (arg_0) (call/cc (lambda (k) (throw-effect k (list 'tkpo8b6903kdcggkdolc9kgsq2175dhg0drn17134edkambevl7aahadvueaqml9pemnttm81t7569jbrifmrg82ogjnlfv5gbc2q6o_0 arg_0))))))

; Store.Store.get

(define tkpo8b6903kdcggkdolc9kgsq2175dhg0drn17134edkambevl7aahadvueaqml9pemnttm81t7569jbrifmrg82ogjnlfv5gbc2q6o_1 (lambda () (call/cc (lambda (k) (throw-effect k (list 'tkpo8b6903kdcggkdolc9kgsq2175dhg0drn17134edkambevl7aahadvueaqml9pemnttm81t7569jbrifmrg82ogjnlfv5gbc2q6o_1))))))

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

; unison_random_mersenne._base_additions.Nat32

; Data(DataDecl { modifier: Structural, bound: [], constructors: [(🔣Nat32.Nat32/0, Arrow(Ref(Nat), Ref(#d97e0jhkmd)))] })

; Nat32.Nat32

(define d97e0jhkmdospt1d5n3pdrl78ufb3m32hr06uhijaq8s2vdhmaehr7rv0fsm8b3p469cru224jqs46vlqesbaf5a41vlstv87gempqo_0 (lambda (arg_0) (list 'd97e0jhkmdospt1d5n3pdrl78ufb3m32hr06uhijaq8s2vdhmaehr7rv0fsm8b3p469cru224jqs46vlqesbaf5a41vlstv87gempqo_0 arg_0)))

; base.io.Handle

; Data(DataDecl { modifier: Unique("d4597403ec40fd4fbee57c62b8096f9c3d382dff01f20108546fe3530a927e86"), bound: [], constructors: [(🔣io.Handle.Handle/0, Arrow(Ref(Text), Ref(#b7kh3q81n1)))] })

; io.Handle.Handle

(define b7kh3q81n1htidjsbuuk80a7kmm6qi62lidg0hbg8o0nph7e6eubqq6k43n50qaurghvgv8p1on925980ft1jsl3pd1snq0jtj86d4o_0 (lambda (arg_0) (list 'b7kh3q81n1htidjsbuuk80a7kmm6qi62lidg0hbg8o0nph7e6eubqq6k43n50qaurghvgv8p1on925980ft1jsl3pd1snq0jtj86d4o_0 arg_0)))

; base.Optional

; Data(DataDecl { modifier: Structural, bound: [🔣a/0], constructors: [(🔣Optional.None/0, Forall(|a/0 #0|(App(Ref(#5isltsdct9), vara (#0))))), (🔣Optional.Some/0, Forall(|a/0 #0|(Arrow(vara (#0), App(Ref(#5isltsdct9), vara (#0))))))] })

; Optional.None

(define 5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_0 '5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_0)

; Optional.Some

(define 5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_1 (lambda (arg_0) (list '5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_1 arg_0)))

; base.Unit

; Data(DataDecl { modifier: Structural, bound: [], constructors: [(🔣Unit.Unit/0, Ref(#568rsi7o3g))] })

; Unit.Unit

(define 568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8_0 '568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8_0)

; base.Tuple

; Data(DataDecl { modifier: Structural, bound: [🔣a/0, 🔣b/0], constructors: [(🔣Tuple.Cons/0, Forall(|a/0 #0|(Forall(|b/0 #0|(Arrow(vara (#0), Arrow(varb (#0), App(App(Ref(#onbcm0qctb), vara (#0)), varb (#0)))))))))] })

; Tuple.Cons

(define onbcm0qctbnuctpm57tkc5p16b8gfke8thjf19p4r4laokji0b606rd0frnhj103qb90lve3fohkoc1eda70491hot656s1m6kk3cn0_0 (lambda (arg_0) (lambda (arg_1) (list 'onbcm0qctbnuctpm57tkc5p16b8gfke8thjf19p4r4laokji0b606rd0frnhj103qb90lve3fohkoc1eda70491hot656s1m6kk3cn0_0 arg_0 arg_1))))

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
    (let ((f-tmp (Nat.shiftRight (let ((f-tmp (Nat.shiftLeft n))) (f-tmp 32))))) (f-tmp 32)))))

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
            (let ((f-tmp (Nat.shiftRight n-quot))) (f-tmp b))))
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
                   (let ((f-tmp (Nat.xor a-quot))) (f-tmp b-quot))))
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
                   (let ((f-tmp (Nat.+ a-quot))) (f-tmp b-quot))))
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
                         (let
                          ((f-tmp (let ((f-tmp (go f))) (f-tmp s))))
                          (f-tmp (let ((f-tmp (List.snoc acc))) (f-tmp a))))
                         'fallthrough))
                       'fallthrough)))
                    'fallthrough))
                  'fallthrough))))
              (if (equal? 'fallthrough result) (no-match) result))
             result))))))))
     (let ((f-tmp (let ((f-tmp (go f))) (f-tmp s0)))) (f-tmp (vector )))))))

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
                   (let ((f-tmp (Nat.* a-quot))) (f-tmp b-quot))))
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
                 (let ((b-quot (list-ref tmp-0 1))) (let ((f-tmp (Universal.<= a-quot))) (f-tmp b-quot)))
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
        (let
         ((f-tmp
           (4font96ujkk61k20294c82c2m432233i6akg6017q2j4u9ng48c8su2hqd35rjmucnkodhljp1vnkqr3h6mnv0bj8ej8eqk3ikqaja8
            (let
             ((f-tmp
               (62rt8ihbsbb2cklq0cn9hbo7nisvbetfmos19i9ndrlmf2r87ksnp95covpns5th7hnau9b0n5bgu0lupe8s512c5lnl56ke4lc2hf0
                (d97e0jhkmdospt1d5n3pdrl78ufb3m32hr06uhijaq8s2vdhmaehr7rv0fsm8b3p469cru224jqs46vlqesbaf5a41vlstv87gempqo_0
                 1812433253))))
             (f-tmp
              (let
               ((f-tmp
                 (4c6fis38j5nc86vcriefqvjs7fqfu5mdvqg6fkkltjgnqf6r000vggkof5l2569q87n2hnc7e8uakgfe690j53jckriu3ik8sbgpbi0
                  last)))
               (f-tmp
                (let
                 ((f-tmp
                   (443ei922o2d3i84c4o75e0odmok3vd5eh30beipei7akos818ko68m4j8tjh3i3f6ipc4dhhcq3s0gso59mu8suga042dvier4l8ih8
                    last)))
                 (f-tmp 30)))))))))
         (f-tmp index))))))
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
                    (let
                     ((f-tmp
                       (7ucnfm0idg5vbks422mptn66a8fja8eqndi34bfe1eej9e8d1t3tcs7i4d6g14ilhlo05i1av4rn6uep8rbt2pe64c3vrp3ak677e08
                        index)))
                     (f-tmp
                      (d97e0jhkmdospt1d5n3pdrl78ufb3m32hr06uhijaq8s2vdhmaehr7rv0fsm8b3p469cru224jqs46vlqesbaf5a41vlstv87gempqo_0
                       624)))
                    (5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_1
                     (let
                      ((f-tmp
                        (onbcm0qctbnuctpm57tkc5p16b8gfke8thjf19p4r4laokji0b606rd0frnhj103qb90lve3fohkoc1eda70491hot656s1m6kk3cn0_0
                         last)))
                      (f-tmp
                       (let
                        ((f-tmp
                          (onbcm0qctbnuctpm57tkc5p16b8gfke8thjf19p4r4laokji0b606rd0frnhj103qb90lve3fohkoc1eda70491hot656s1m6kk3cn0_0
                           (let
                            ((f-tmp
                              (onbcm0qctbnuctpm57tkc5p16b8gfke8thjf19p4r4laokji0b606rd0frnhj103qb90lve3fohkoc1eda70491hot656s1m6kk3cn0_0
                               (let
                                ((f-tmp
                                  (4font96ujkk61k20294c82c2m432233i6akg6017q2j4u9ng48c8su2hqd35rjmucnkodhljp1vnkqr3h6mnv0bj8ej8eqk3ikqaja8
                                   index)))
                                (f-tmp
                                 (d97e0jhkmdospt1d5n3pdrl78ufb3m32hr06uhijaq8s2vdhmaehr7rv0fsm8b3p469cru224jqs46vlqesbaf5a41vlstv87gempqo_0
                                  1))))))
                            (f-tmp
                             (let
                              ((f-tmp
                                (onbcm0qctbnuctpm57tkc5p16b8gfke8thjf19p4r4laokji0b606rd0frnhj103qb90lve3fohkoc1eda70491hot656s1m6kk3cn0_0
                                 (let ((f-tmp (next index))) (f-tmp last)))))
                              (f-tmp
                               568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8_0)))))))
                        (f-tmp
                         568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8_0)))))
                    5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_0)
                   'fallthrough))
                 'fallthrough)))
              'fallthrough))))
          (if (equal? 'fallthrough result) (no-match) result))))))
     (let
      ((state
        (let
         ((f-tmp
           (4omhgrnp2n42sku6t4kpg38rpe4qbvqc7l909a9v5650msj11ropb7s3ndptev3bimrh23fh120vajcu8h86o43pbofnd6b9dqtv6do
            (let
             ((f-tmp
               (onbcm0qctbnuctpm57tkc5p16b8gfke8thjf19p4r4laokji0b606rd0frnhj103qb90lve3fohkoc1eda70491hot656s1m6kk3cn0_0
                (d97e0jhkmdospt1d5n3pdrl78ufb3m32hr06uhijaq8s2vdhmaehr7rv0fsm8b3p469cru224jqs46vlqesbaf5a41vlstv87gempqo_0
                 1))))
             (f-tmp
              (let
               ((f-tmp
                 (onbcm0qctbnuctpm57tkc5p16b8gfke8thjf19p4r4laokji0b606rd0frnhj103qb90lve3fohkoc1eda70491hot656s1m6kk3cn0_0
                  s)))
               (f-tmp
                568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8_0)))))))
         (f-tmp next-quot))))
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
    (let
     ((f-tmp
       (4omhgrnp2n42sku6t4kpg38rpe4qbvqc7l909a9v5650msj11ropb7s3ndptev3bimrh23fh120vajcu8h86o43pbofnd6b9dqtv6do
        n)))
     (f-tmp
      (lambda
       (n)
       (if
        (let ((f-tmp (Universal.== n))) (f-tmp 0))
        5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_0
        (5isltsdct9fhcrvud9gju8u0l9g0k9d3lelkksea3a8jdgs1uqrs5mm9p7bajj84gg8l9c9jgv9honakghmkb28fucoeb2p4v9ukmu8_1
         (let
          ((f-tmp
            (onbcm0qctbnuctpm57tkc5p16b8gfke8thjf19p4r4laokji0b606rd0frnhj103qb90lve3fohkoc1eda70491hot656s1m6kk3cn0_0
             (op
              568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8_0))))
          (f-tmp
           (let
            ((f-tmp
              (onbcm0qctbnuctpm57tkc5p16b8gfke8thjf19p4r4laokji0b606rd0frnhj103qb90lve3fohkoc1eda70491hot656s1m6kk3cn0_0
               (let ((f-tmp (Nat.drop n))) (f-tmp 1)))))
            (f-tmp
             568rsi7o3ghq8mmbea2sf8msdk20ohasob5s2rvjtqg2lr0vs39l1hm98urrjemsr3vo3fa52pibqu0maluq7g8sfg3h5f5re6vitj8_0))))))))))))

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
        (let
         ((f-tmp
           (apd2pd0ob7k20kqui8q43ns0840dbubc8p5tuh183n2q4d44qmqnfnhk3vpga0m6r8ci4e6jlhlu52vbpkj1p28tsgchm2m7tss620o
            n)))
         (f-tmp next-quot))))
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
            (let ((f-tmp (Nat.shiftLeft n-quot))) (f-tmp b))))
          'fallthrough))))
      (if (equal? 'fallthrough result) (no-match) result))))))

; /end unison_random_mersenne._base_additions.Nat32.shiftLeft

(define unison_random_mersenne._base_additions.Nat32.shiftLeft vubdg5ln9astm9mmlo7ev20kkjj38af12b4mg2c9n56uascubn8c87a1ckn4526krenh33cadqhhuul9nldl7bhpii648e0cku5cdao)

; unison_random_mersenne._base_additions.Nat32.bit : Some(Forall(|𝕖/0 #0|(Arrow(Ref(Nat), Effect(Effects([var𝕖 (#0)]), Ref(#d97e0jhkmd))))))

(print-processing "unison_random_mersenne._base_additions.Nat32.bit")

(define glts8qofgo284gk3dnovfj2dges1n3b2amd2jk45rbl4s3mjbo5gent5n1mv7jsgnlqep879vgfg5j0li39jsmffr01ldrl623tjkag
  (lambda
   (i)
   (let
    ((f-tmp
      (vubdg5ln9astm9mmlo7ev20kkjj38af12b4mg2c9n56uascubn8c87a1ckn4526krenh33cadqhhuul9nldl7bhpii648e0cku5cdao
       (d97e0jhkmdospt1d5n3pdrl78ufb3m32hr06uhijaq8s2vdhmaehr7rv0fsm8b3p469cru224jqs46vlqesbaf5a41vlstv87gempqo_0
        1))))
    (f-tmp i))))

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
                   (let ((f-tmp (Nat.and a-quot))) (f-tmp b-quot))))
                 'fallthrough))))
             (if (equal? 'fallthrough result) (no-match) result))))
          'fallthrough))))
      (if (equal? 'fallthrough result) (no-match) result))))))

; /end unison_random_mersenne._base_additions.Nat32.and

(define unison_random_mersenne._base_additions.Nat32.and hl4e732fqm204op26k9ojkuu0fmfjksr5fcknmig1akevrthiu1quscc6jj6rlo9eb8quuku07m6c0q75q11s0o9irb9jc9nbmlscbo)

; unison_random_mersenne._base_additions.Nat32.clearBit : Some(Forall(|𝕖/0 #0|(Forall(|𝕖/0 #0|(Arrow(Ref(#d97e0jhkmd), Effect(Effects([var𝕖 (#0)]), Arrow(Ref(Nat), Effect(Effects([var𝕖 (#0)]), Ref(#d97e0jhkmd))))))))))

(print-processing "unison_random_mersenne._base_additions.Nat32.clearBit")

(define 77hkmnfup86ippah57kadki0i2s86keupous5f2f8tq3r7n2h9cf148e0cahil650tq3ch6fonjnvj1ktc2fcb252m909jacl18877o
  (lambda
   (n)
   (lambda
    (i)
    (let
     ((f-tmp
       (hl4e732fqm204op26k9ojkuu0fmfjksr5fcknmig1akevrthiu1quscc6jj6rlo9eb8quuku07m6c0q75q11s0o9irb9jc9nbmlscbo
        n)))
     (f-tmp
      (aq040co1g4nrou525j1snc68l0os6nkj9bu9k5ssdm9q21gpn1uhc771ropk1lpmngh840736ho58a5csu1querjlgejo6k1j4iusvg
       (glts8qofgo284gk3dnovfj2dges1n3b2amd2jk45rbl4s3mjbo5gent5n1mv7jsgnlqep879vgfg5j0li39jsmffr01ldrl623tjkag
        i)))))))

; /end unison_random_mersenne._base_additions.Nat32.clearBit

(define unison_random_mersenne._base_additions.Nat32.clearBit 77hkmnfup86ippah57kadki0i2s86keupous5f2f8tq3r7n2h9cf148e0cahil650tq3ch6fonjnvj1ktc2fcb252m909jacl18877o)

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
     ((tmp-match-head (let ((f-tmp (List.at n))) (f-tmp as))))
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
           (let
            ((f-tmp (Debug.watch "oh noes")))
            (f-tmp
             (let
              ((f-tmp
                (etge4adj957jmmosh03foaafoju2rjidbu8hmv0nnqkt3rg80q50hnn7ukr7ve9lmfgjjsm5ul5eeq7b8vbk1j6u4bopov2j4r3u2v8
                 n)))
              (f-tmp as))))
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
        (let
         ((f-tmp
           (etge4adj957jmmosh03foaafoju2rjidbu8hmv0nnqkt3rg80q50hnn7ukr7ve9lmfgjjsm5ul5eeq7b8vbk1j6u4bopov2j4r3u2v8
            0)))
         (f-tmp s))))
      (let
       ((b
         (let
          ((f-tmp
            (etge4adj957jmmosh03foaafoju2rjidbu8hmv0nnqkt3rg80q50hnn7ukr7ve9lmfgjjsm5ul5eeq7b8vbk1j6u4bopov2j4r3u2v8
             1)))
          (f-tmp s))))
       (let
        ((c
          (let
           ((f-tmp
             (etge4adj957jmmosh03foaafoju2rjidbu8hmv0nnqkt3rg80q50hnn7ukr7ve9lmfgjjsm5ul5eeq7b8vbk1j6u4bopov2j4r3u2v8
              397)))
           (f-tmp s))))
        (let
         ((_6
           (v00j3buk6m0588orsna0oeuukfdh7g3sandn22qmld0m2fr2jvm7hjd6bpm5jh6db6hom5ag14hqmg4emhlm4urdj9ibom7s2m5417o_3
            (vector (v00j3buk6m0588orsna0oeuukfdh7g3sandn22qmld0m2fr2jvm7hjd6bpm5jh6db6hom5ag14hqmg4emhlm4urdj9ibom7s2m5417o_2
               "Calculate the next untempered number (next\'), and the new stored list, s\'.")))))
         (let
          ((p
            (let
             ((f-tmp
               (4font96ujkk61k20294c82c2m432233i6akg6017q2j4u9ng48c8su2hqd35rjmucnkodhljp1vnkqr3h6mnv0bj8ej8eqk3ikqaja8
                (let
                 ((f-tmp
                   (hl4e732fqm204op26k9ojkuu0fmfjksr5fcknmig1akevrthiu1quscc6jj6rlo9eb8quuku07m6c0q75q11s0o9irb9jc9nbmlscbo
                    a)))
                 (f-tmp
                  (glts8qofgo284gk3dnovfj2dges1n3b2amd2jk45rbl4s3mjbo5gent5n1mv7jsgnlqep879vgfg5j0li39jsmffr01ldrl623tjkag
                   31))))))
             (f-tmp
              (let
               ((f-tmp
                 (77hkmnfup86ippah57kadki0i2s86keupous5f2f8tq3r7n2h9cf148e0cahil650tq3ch6fonjnvj1ktc2fcb252m909jacl18877o
                  b)))
               (f-tmp 31))))))
          (let
           ((ifEven
             (let
              ((f-tmp
                (4c6fis38j5nc86vcriefqvjs7fqfu5mdvqg6fkkltjgnqf6r000vggkof5l2569q87n2hnc7e8uakgfe690j53jckriu3ik8sbgpbi0
                 c)))
              (f-tmp
               (let
                ((f-tmp
                  (443ei922o2d3i84c4o75e0odmok3vd5eh30beipei7akos818ko68m4j8tjh3i3f6ipc4dhhcq3s0gso59mu8suga042dvier4l8ih8
                   p)))
                (f-tmp 1))))))
           (let
            ((next-quot
              (if
               (mvinivvj0em7dso9honoe8oots60ujdhhr3r5d75foglvp6l80631buqr7pc5batv3lj25faf5n50ocdkn70lalinabl66a9ijft67g
                p)
               ifEven
               (let
                ((f-tmp
                  (4c6fis38j5nc86vcriefqvjs7fqfu5mdvqg6fkkltjgnqf6r000vggkof5l2569q87n2hnc7e8uakgfe690j53jckriu3ik8sbgpbi0
                   ifEven)))
                (f-tmp
                 (d97e0jhkmdospt1d5n3pdrl78ufb3m32hr06uhijaq8s2vdhmaehr7rv0fsm8b3p469cru224jqs46vlqesbaf5a41vlstv87gempqo_0
                  2567483615))))))
            (let
             ((s-quot (let ((f-tmp (List.snoc (let ((f-tmp (List.drop 1))) (f-tmp s))))) (f-tmp next-quot))))
             (let
              ((temper
                (lambda
                 (n)
                 (let
                  ((y1
                    (let
                     ((f-tmp
                       (4c6fis38j5nc86vcriefqvjs7fqfu5mdvqg6fkkltjgnqf6r000vggkof5l2569q87n2hnc7e8uakgfe690j53jckriu3ik8sbgpbi0
                        n)))
                     (f-tmp
                      (let
                       ((f-tmp
                         (443ei922o2d3i84c4o75e0odmok3vd5eh30beipei7akos818ko68m4j8tjh3i3f6ipc4dhhcq3s0gso59mu8suga042dvier4l8ih8
                          n)))
                       (f-tmp 11))))))
                  (let
                   ((y2
                     (let
                      ((f-tmp
                        (4c6fis38j5nc86vcriefqvjs7fqfu5mdvqg6fkkltjgnqf6r000vggkof5l2569q87n2hnc7e8uakgfe690j53jckriu3ik8sbgpbi0
                         y1)))
                      (f-tmp
                       (let
                        ((f-tmp
                          (hl4e732fqm204op26k9ojkuu0fmfjksr5fcknmig1akevrthiu1quscc6jj6rlo9eb8quuku07m6c0q75q11s0o9irb9jc9nbmlscbo
                           (let
                            ((f-tmp
                              (vubdg5ln9astm9mmlo7ev20kkjj38af12b4mg2c9n56uascubn8c87a1ckn4526krenh33cadqhhuul9nldl7bhpii648e0cku5cdao
                               y1)))
                            (f-tmp 7)))))
                        (f-tmp
                         (d97e0jhkmdospt1d5n3pdrl78ufb3m32hr06uhijaq8s2vdhmaehr7rv0fsm8b3p469cru224jqs46vlqesbaf5a41vlstv87gempqo_0
                          2636928640)))))))
                   (let
                    ((y3
                      (let
                       ((f-tmp
                         (4c6fis38j5nc86vcriefqvjs7fqfu5mdvqg6fkkltjgnqf6r000vggkof5l2569q87n2hnc7e8uakgfe690j53jckriu3ik8sbgpbi0
                          y2)))
                       (f-tmp
                        (let
                         ((f-tmp
                           (hl4e732fqm204op26k9ojkuu0fmfjksr5fcknmig1akevrthiu1quscc6jj6rlo9eb8quuku07m6c0q75q11s0o9irb9jc9nbmlscbo
                            (let
                             ((f-tmp
                               (vubdg5ln9astm9mmlo7ev20kkjj38af12b4mg2c9n56uascubn8c87a1ckn4526krenh33cadqhhuul9nldl7bhpii648e0cku5cdao
                                y2)))
                             (f-tmp 15)))))
                         (f-tmp
                          (d97e0jhkmdospt1d5n3pdrl78ufb3m32hr06uhijaq8s2vdhmaehr7rv0fsm8b3p469cru224jqs46vlqesbaf5a41vlstv87gempqo_0
                           4022730752)))))))
                    (let
                     ((y4
                       (let
                        ((f-tmp
                          (4c6fis38j5nc86vcriefqvjs7fqfu5mdvqg6fkkltjgnqf6r000vggkof5l2569q87n2hnc7e8uakgfe690j53jckriu3ik8sbgpbi0
                           y3)))
                        (f-tmp
                         (let
                          ((f-tmp
                            (443ei922o2d3i84c4o75e0odmok3vd5eh30beipei7akos818ko68m4j8tjh3i3f6ipc4dhhcq3s0gso59mu8suga042dvier4l8ih8
                             y3)))
                          (f-tmp 18))))))
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
     (let ((f-tmp (Nat.or (let ((f-tmp (Nat.shiftLeft top32))) (f-tmp 32))))) (f-tmp bot32))))))

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

; continuations._external.base.M1l.io.putText : Some(Forall(|𝕖/0 #0|(Arrow(Ref(#b7kh3q81n1), Effect(Effects([var𝕖 (#0)]), Arrow(Ref(Text), Effect(Effects([Ref(#fgaevis4bl)]), Ref(#568rsi7o3g))))))))

(print-processing "continuations._external.base.M1l.io.putText")

(define usjgb68ojte81tje2msm1s0t1p8advuocspasdi2l5cs3nc05vgt19pgoi7d3se228u50o5fjmbi3gvnugsgni77ptd4kh3nl39p5ho
  (lambda
   (h)
   (lambda
    (t)
    (rultimjsuqimjid9ktklj6n26ejvsbn21d1d161vk54hf7a7nmic96c1aefb4jhoko520e6fbtetje3fe54i4mhdjrh3p7movr3uk70
     (let
      ((f-tmp
        (fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g_37
         h)))
      (f-tmp t))))))

; /end continuations._external.base.M1l.io.putText

(define continuations._external.base.M1l.io.putText usjgb68ojte81tje2msm1s0t1p8advuocspasdi2l5cs3nc05vgt19pgoi7d3se228u50o5fjmbi3gvnugsgni77ptd4kh3nl39p5ho)

; continuations._external.base.M1l.io.printLine : Some(Arrow(Ref(Text), Effect(Effects([Ref(#fgaevis4bl)]), Ref(#568rsi7o3g))))

(print-processing "continuations._external.base.M1l.io.printLine")

(define ssbdakrj0hbr2rpno34bj8i64mokaaoc5rdj7sil5c87qrqeamf2996qrg0gc1thhamd4bbc40l991d7vgd35qekh5hsoiqkpccvk50
  (lambda
   (t)
   (letrec
    ((_1
      (let
       ((f-tmp
         (usjgb68ojte81tje2msm1s0t1p8advuocspasdi2l5cs3nc05vgt19pgoi7d3se228u50o5fjmbi3gvnugsgni77ptd4kh3nl39p5ho
          7plc1gmlvn6ti9lpq4ufa4uvdal6elieppmcgvr0d0scsk9e3edakgnk09covbel3hjcjlsi7rmn8auhi935fo9iibed7krpfr337fo)))
       (f-tmp t))))
    (let
     ((f-tmp
       (usjgb68ojte81tje2msm1s0t1p8advuocspasdi2l5cs3nc05vgt19pgoi7d3se228u50o5fjmbi3gvnugsgni77ptd4kh3nl39p5ho
        7plc1gmlvn6ti9lpq4ufa4uvdal6elieppmcgvr0d0scsk9e3edakgnk09covbel3hjcjlsi7rmn8auhi935fo9iibed7krpfr337fo)))
     (f-tmp "\n")))))

; /end continuations._external.base.M1l.io.printLine

(define continuations._external.base.M1l.io.printLine ssbdakrj0hbr2rpno34bj8i64mokaaoc5rdj7sil5c87qrqeamf2996qrg0gc1thhamd4bbc40l991d7vgd35qekh5hsoiqkpccvk50)

; testbed.run_long : Some(Arrow(Ref(#568rsi7o3g), Effect(Effects([Ref(#fgaevis4bl)]), Ref(#568rsi7o3g))))

(print-processing "testbed.run_long")

(define h0iqiac1s4dqkn3mujrv1l80674infpp5g853oh3l3h153e9m8r6ii9ijtiklj7fd1vc2jvpp5ht60rp7iekbk6p8c5cbbck374re7g
  (lambda
   (_)
   (ssbdakrj0hbr2rpno34bj8i64mokaaoc5rdj7sil5c87qrqeamf2996qrg0gc1thhamd4bbc40l991d7vgd35qekh5hsoiqkpccvk50
    (Nat.toText
     (List.size
      (let
       ((f-tmp
         (let
          ((f-tmp
            (49dfoqk8ri2sa1n5hrd1m1hc7vqdkorl4jqnvbsmht268lsnpopt6tsrpd4ll4rg93fmmuh1g660n5oujo66h8tnqi2fagp3nk451r0
             e082cs8ace1fr1mo5gg1n1adfrhr57p8ir3ca28n0f3t2googumdruou2sohv6u9daore07qcc10e5o1sj32hb7rck40f76nihi2gug)))
          (f-tmp
           fvfia2buu96bbhqup30hmso49aano9u5rve82ovbd2msedra176jj6t3oj2hr5rngsfpf9fo38c8jjqqs19n505t1onbkd6bs9egka8))))
       (f-tmp 10001)))))))

; /end testbed.run_long

(define testbed.run_long h0iqiac1s4dqkn3mujrv1l80674infpp5g853oh3l3h153e9m8r6ii9ijtiklj7fd1vc2jvpp5ht60rp7iekbk6p8c5cbbck374re7g)
(run-with-io h0iqiac1s4dqkn3mujrv1l80674infpp5g853oh3l3h153e9m8r6ii9ijtiklj7fd1vc2jvpp5ht60rp7iekbk6p8c5cbbck374re7g)