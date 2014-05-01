; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form init-env)))

; eval-exp is the main component of the interpreter

(define eval-exp
  (lambda (exp env)
    (cases expression exp
      [lit-exp (datum) datum]
      [var-exp (id)
        (apply-env env id; look up its value.
		   (lambda (x) x) ; procedure to call if id is in the environment 
		   (lambda () 
		     (apply-env init-env id
				(lambda (x) x)
				(lambda () (eopl:error 'apply-env ; procedure to call if id not in env
						       "variable not found in environment: ~s"
						       id)))))] 
      [app-exp (rator rands)
        (let ([proc-value (eval-exp rator env)]
              [args (eval-rands rands env)])
          (apply-proc proc-value args))]
      [if-exp (condition if-then if-else)
	(if (eval-exp condition env)
	    (eval-exp if-then env)
	    (eval-exp if-else env))]
      [if-true-exp (condition if-then)
        (if (eval-exp condition env)
	    (eval-exp if-then env))]
      [let-exp (vars exprs bodies)
	(let ([new-env (extend-env vars
				   (eval-rands exprs env)
				   env)])
	  (let loop ([bodies bodies])
	    (if (null? (cdr bodies))
		(eval-exp (car bodies) new-env)
		(begin
		  (eval-exp (car bodies) new-env)
		  (loop (cdr bodies))))))]
      [lambda-const-args-exp (vars bodies)
        (closure vars bodies env)]
      [lambda-const-var-args-exp (const-id var-id bodies)
        (closure (append const-id (list var-id)) bodies env)]
      [lambda-var-args-exp (id bodies)
	(closure (list id) bodies env)]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

; evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda (rands env)
    (map (lambda (expr) (eval-exp expr env)) rands)))

; evaluate multiple bodies
(define eval-multiple-bodies
  (lambda (bodies env)
    (if (null? (cdr bodies))
	(eval-exp (car bodies) env)
	(begin
	  (eval-exp (car bodies) env)
	  (eval-multiple-bodies (cdr bodies) env)))))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args)]
      [closure (vars bodies env)
	(let ([extended-env (extended-env-record vars args env)])
	  (eval-multiple-bodies bodies extended-env))]
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

(define *prim-proc-names* '(+ - * / add1 sub1 zero? not = < <= > >= cons car cdr))

(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc      
          *prim-proc-names*)
     (empty-env)))

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

(define apply-prim-proc
  (lambda (prim-proc args)
    (case prim-proc
      [(+) (apply + args)]
      [(-) (apply - args)]
      [(*) (apply * args)]
      [(/) (apply / args)]
      [(add1) (cond
	       [(or (null? args) (not (null? (cdr args)))) (eopl:error 'add1 "incorrect argument count in call (~s ~s)" prim-proc args)]
	       [else (+ (1st args) 1)])]
      [(sub1) (cond
	       [(or (null? args) (not (null? (cdr args)))) (eopl:error 'sub1 "incorrect argument count in call (~s ~s)" prim-proc args)]
	       [else (- (1st args) 1)])]
      [(zero?) (cond
	       [(or (null? args) (not (null? (cdr args)))) (eopl:error 'zero? "incorrect argument count in call (~s ~s)" prim-proc args)]
	       [else (zero? (1st args))])]
      [(not) (cond
	       [(or (null? args) (not (null? (cdr args)))) (eopl:error 'not "incorrect argument count in call (~s ~s)" prim-proc args)]
	       [else (not (1st args))])]
      [(=) (cond
	    [(null? args) (eopl:error '= "incorrect argument count in call (~s)" prim-proc)]
	    [else (apply = args)])]
      [(<) (cond
	    [(null? args) (eopl:error '< "incorrect argument count in call (~s)" prim-proc)]
	    [else (apply < args)])]
      [(<=) (cond
	    [(null? args) (eopl:error '<= "incorrect argument count in call (~s)" prim-proc)]
	    [else (apply <= args)])]
      [(>) (cond
	    [(null? args) (eopl:error '> "incorrect argument count in call (~s)" prim-proc)]
	    [else (apply > args)])]
      [(>=) (cond
	    [(null? args) (eopl:error '>= "incorrect argument count in call (~s)" prim-proc)]
	    [else (apply >= args)])]
      [(cons) (cond
	       [(or (null? args) (null? (cdr args)) (not (null? (cddr args)))) (eopl:error 'cons "incorrect argument count in call (~s ~s)" prim-proc args)]
	       [else (cons (1st args) (2nd args))])]
      [(car) (cond
	      [(or (null? args) (not (null? (cdr args)))) (eopl:error 'car "incorrect argument count in call (~s ~s)" prim-proc args)]
	      [else (car args)])]
      [(cdr) (cond
	      [(or (null? args) (not (null? (cdr args)))) (eopl:error 'cdr "incorrect argument count in call (~s ~s)" prim-proc args)]
	      [else (cdr args)])]
      [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-op)])))

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (parse-exp (read)))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda (x) (top-level-eval (parse-exp x))))










