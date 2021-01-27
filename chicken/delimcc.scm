;	delimcc: Control operators for delimited continuations
;
; This library implements the variety of delimited control operators
; for R5RS Scheme. The code implements the superset of the interface
; proposed by Dybvig, Sabry, and Peyton-Jones.
; This library is the transcription into Scheme of the delimcc library of OCaml:
; http://okmij.org/ftp/Computation/Continuations.html#caml-shift
;
; Although the present code should work on any R5RS Scheme system,
; good performance should be expected only on the systems that implement
; call/cc efficiently, such as Chez Scheme, Scheme48, Gambit, Larceny.
;
;
; The library interface, based on delimcc.mli, is as follows:
;
; procedure new-prompt: 
;  (new-prompt) returns a fresh prompt, eq? only to itself.
;
; syntax: push-prompt
;  (push-prompt p e1 e2 ...)
; sets the prompt p and evaluates the sequence of expressions e1 e2 ...
; returning the result of the last one (unless take_subcont was executed)
;
; syntax: abortP
;  (abort p e) flushes the stack up to, and including, the dynamically closest
;  push-prompt with the prompt p; expression e is evaluated in the
;  remaining context.
;
; syntax take-subcont:
;  (take-subcont p sk e1 e2 ...)
; captures the continuation up to the dynamically closest push-prompt with
; the prompt p, and binds the captured delimited continuation object
; to the variable sk; the prompt is unset. The sequence of
; expressions e1 e2 ... is evaluated in the remaining context.
;
; syntax push-subcont:
;  (push-subcont sk e1 e2 ...)
; reinstates the delimited continuation represented by the object sk
; and then evaluates the sequence of expressions e1 e2 ...
;
; syntax push-delim-subcont:
;  (push-delim-subcont sk e1 e2 ...)
; is like (push-subcont sk e1 e2 ...) but inserts push-prompt
; underneath of the reinstated sk.
; 
; syntax shift:
;  (shift p f e1 e2 ...)
; is a multi-prompt shift. The captured continuation is reified as a function
; and bound to the variable f.
;
; syntax shift0:
;  (shift0 p f e1 e2 ...)
; is multi-prompt shift0. The captured continuation is reified as a function
; and bound to the variable f. After the continuation is captured,
; push-prompt p is removed.
;
; syntax control:
;  (control p f e1 e2 ...)
; is a multi-prompt control. The captured continuation is reified as a function
; and bound to the variable f. The captured continuation is not delimited
; by push-prompt.
;
; procedure prompt-set?:
;  (prompt-set? p) returns a boolean value indicating if the current context
; contains push-prompt p.
;
; The code is a straightforward re-implementation of delimcc.ml.
; Scheme trivially supports scAPI: exception handling is done with call/cc.
; In addition, systems like Chez Scheme or Scheme48 (with the hybrid
; stack/heap or segmented stack strategies) do handle control stack overflow.
; Continuation capture is quite like control stack overflow.
;
; We can attempt to use the dynamic-wind mechanism to maintain
; the pstack. In that case, a prompt could be a ref cell holding
; push-prompt's recent continuation, and dynamic-wind would
; take care of maintaining the invariant that prompt contains the continuation
; of the closest push-prompt.
; OTH, that seems quite a complex mechanism.
; In the following, we go for clarity, and for similarity with the
; the OCaml implementation.

; This ought to be a call-with-unwinding-continuation, if an
; implementation provides such a thing.
(define call/cc call-with-current-continuation)


; pstack is an associative list of (prompt . k), just like in OCaml
(define pstack '())

; Execute a thunk in the empty environment -- at the bottom of the stack --
; and pass the result, too encapsulated as a thunk, to the
; continuation at the top of pstack. The latest pstack frame is
; removed.
;
; We rely on the insight that the capture of a delimited continuation
; can be reduced to the capture of the undelimited one. We invoke 
; (go th) to execute the thunk th in the delimited context. 
; The call to 'go' is evaluated almost in the empty context
; (near the `bottom of the stack'). Therefore,
; any call/cc operation encountered during the evaluation of th
; will capture at most the context established by the 'go' call, NOT
; including the context of go's caller. Informally, invoking (go th)
; creates a new stack segment; continuations captured by call/cc
; cannot span the segment boundaries, and are hence delimited.
; 
; This emulation of delimited control is efficient providing that
; call/cc is implemented efficiently, with the hybrid heap/stack or
; stack segment strategies.

; The corresponding OCaml code, from delimcc, is as follows.
; Please see delimcc.ml for explanations.
;; let push_prompt (p : 'a prompt) (body : unit -> 'a) : 'a =
;;   try
;;     push_prompt_aux p body
;;   with
;;   | DelimCCE -> (match !ptop with
;;     | h::t -> assert (h.pfr_mark == p.mark); ptop := t; mbox_receive p
;;     | _ -> dbg_fatal_error "push_prompt: empty pstack on DelimCCE")
;;   | e -> match !ptop with
;;     | h::t -> assert (h.pfr_mark == p.mark); ptop := t; 
;; 	dbg_note "propagating exc"; raise e
;;     | _ -> dbg_fatal_error "push_prompt: empty pstack on other exc"

(define go #f)
(let ((v
	(call/cc
	  (lambda (k)
	    (set! go k)
	    (k #f)))))
  (if v
    (let* ((r (v))
	   (h (car pstack))
	   (_ (set! pstack (cdr pstack))))
      ((cdr h) (lambda () r)))	; does not return
    ))

; As in OCaml, a prompt is a ref unit. We rely on generativity of ref cells
(define (new-prompt) (list #f))

;; let push_prompt_aux (p : 'a prompt) (body : unit -> 'a) : 'a =
;;   let ek = get_ek () in
;;   let pframe = {pfr_mark = p.mark; pfr_ek = ek} in
;;   let () = ptop := pframe :: (!ptop) in
;;   let res = body () in
;;   let () = p.mbox := fun () -> res in
;;   raise DelimCCE

(define (push-prompt* p th)
  ((call/cc
     (lambda (k)
       (set! pstack (cons (cons p k) pstack))
       (go th)))))			; does not return

;; let rec unwind acc mark = function
;;   | []   -> failwith "No prompt was set" 
;;   | h::t as s -> if h.pfr_mark == mark (* Physical equality ! *)
;;                  then (h,s,acc) else unwind (h::acc) mark t

(define (unwind acc p pstack)
  (if (null? pstack) (error "No prompt was set")
    (if (eq? p (caar pstack))
      (cons pstack acc)
      (unwind (cons (car pstack) acc) p (cdr pstack)))))

; The same as above, but the removed frames are disregarded
(define (unwind-abort p pstack)
  (if (null? pstack) (error "No prompt was set")
    (if (eq? p (caar pstack))
      pstack
      (unwind-abort p (cdr pstack)))))

;; let take_subcont (p : 'b prompt) (f : ('a,'b) subcont -> unit -> 'b) : 'a =
;;   let pa = new_prompt () in
;;   push_prompt_simple pa
;;     (fun () ->
;;       let (h,s,subcontchain) = unwind [] p.mark !ptop in
;;       let () = ptop := s in
;;       let ek = h.pfr_ek in
;;       let sk = get_ek () in
;;       let ekfrag = pop_stack_fragment ek sk in
;;       p.mbox := 
;;       f {subcont_ek = ekfrag; subcont_pa = pa;
;; 	 subcont_pb = p; subcont_ps = subcontchain;
;;          subcont_bs = ek})

; the captured continuation object is a vector of three elements:
;  k  -- ekfragment, the captured continuation itself
;  p  -- the prompt that delimited the continuation
;  subchain --  the part of the pstack corresponding to k,
;               in the reverse pframe order.

(define (take-SC p f)
  ((call/cc
     (lambda (k)			; stack fragment
       (let* ((subchain-pstack (unwind '() p pstack))
	      (_ (set! pstack (car subchain-pstack)))
	      (subchain (cdr subchain-pstack)))
	 (go (f (vector k p subchain)))))))) ; returns when k is invoked

;; let push_subcont (sk : ('a,'b) subcont) (m : unit -> 'a) : 'b =
;;   let pb = sk.subcont_pb in
;;   push_prompt_simple pb (fun () ->
;;     let base = sk.subcont_bs in
;;     let ek = get_ek () in
;;     List.iter (fun pframe ->
;;       ptop := {pframe with pfr_ek = add_ek ek (sub_ek pframe.pfr_ek base)} ::
;; 	    !ptop) sk.subcont_ps;
;;     sk.subcont_pa.mbox := m;
;;     push_stack_fragment sk.subcont_ek)

(define (push-SC sk m)
  ((call/cc
     (lambda (k)
       (let ((p** (new-prompt))
	     (ekfragment (vector-ref sk 0))
	     (subchain (vector-ref sk 2)))
       (set! pstack (cons (cons p** k) pstack))
       (for-each
	 (lambda (frame)
	   (set! pstack (cons frame pstack)))
	 subchain)
       (ekfragment m))))))

(define (push-delim-SC sk m)
  ((call/cc
     (lambda (k)
       (let ((p (vector-ref sk 1))
	     (ekfragment (vector-ref sk 0))
	     (subchain (vector-ref sk 2)))
       (set! pstack (cons (cons p k) pstack))
       (for-each
	 (lambda (frame)
	   (set! pstack (cons frame pstack)))
	 subchain)
       (ekfragment m))))))

; A more efficient variation of take-SC, which does not capture
; any continuation.
(define (abort* p th)
  (let* ((pstack-new (unwind-abort p pstack))
	 (h (car pstack-new)))
    (set! pstack (cdr pstack-new))
    ((cdr h) th)))			; does not return

; Check to see if a prompt is set
(define (prompt-set? p)
  (assq p pstack))

; ------------------------------- Syntactic sugar

(define-syntax push-prompt
  (syntax-rules ()
    ((_ p e1 e2 ...) (push-prompt* p (lambda () e1 e2 ...)))))

(define-syntax abortP
  (syntax-rules ()
    ((_ p e) (abort* p (lambda () e)))))


(define-syntax take-subcont
  (syntax-rules ()
    ((_ p sk e1 e2 ...)
      (take-SC p (lambda (sk) (lambda () e1 e2 ...))))))

(define-syntax push-subcont
  (syntax-rules ()
    ((_ sk e1 e2 ...)
      (push-SC sk (lambda () e1 e2 ...)))))

(define-syntax push-delim-subcont
  (syntax-rules ()
    ((_ sk e1 e2 ...)
      (push-delim-SC sk (lambda () e1 e2 ...)))))

; The reified continuation takes a value rather than an action
(define-syntax shift
  (syntax-rules ()
    ((_ p f e1 e2 ...)
      (take-subcont p sk
	(let ((f (lambda (v) (push-delim-subcont sk v))))
	  (push-prompt p e1 e2 ...))))))

(define-syntax shift0
  (syntax-rules ()
    ((_ p f e1 e2 ...)
      (take-subcont p sk
	(let ((f (lambda (v) (push-delim-subcont sk v))))
	  e1 e2 ...)))))

(define-syntax control
  (syntax-rules ()
    ((_ p f e1 e2 ...)
      (take-subcont p sk
	(let ((f (lambda (v) (push-subcont sk v))))
	  (push-prompt p e1 e2 ...))))))

(display 10)