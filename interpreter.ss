; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form k)
    ; later we may add things that are not expressions.
    (eval-exp form global-env k)))

; eval-exp is the main component of the interpreter

(define eval-exp
  (lambda (exp env k)
    (cases expression exp
      [lit-exp (datum) (k datum)]
      [var-exp (id)
        (apply-env env id ; look up its value.
		   k ; procedure to call if id is in the environment
		   (lambda () 
		     (apply-env global-env id
				k
				(lambda () (eopl:error 'apply-env ; procedure to call if id not in env
						       "variable not found in environment: ~s"
						       id)))))]
      [app-exp (rator rands)
		(eval-exp rator env (lambda (proc-value)
			      (eval-rands rands env (lambda (args)
						      (apply-proc proc-value args k)))))]
      [if-exp (condition if-then if-else)
		(eval-exp condition env (lambda (e-condition)
				  (if e-condition
				      (eval-exp if-then env k)
				      (eval-exp if-else env k))))]
      [if-true-exp (condition if-then)
		(eval-exp condition env (lambda (e-condition)
				  (if e-condition
				      (eval-exp if-then env k)
				      (k (void)))))]
      [let-exp (vars exprs bodies)
		(eval-rands exprs env (lambda (e-rands)
				(eval-multiple-bodies bodies (extend-env vars e-rands env) k)))]
      [lambda-const-args-exp (vars bodies)
        (k (closure-const-args vars bodies env))]
      [lambda-const-var-args-exp (const-id var-id bodies)
        (k (closure-const-var-args const-id var-id bodies env))]
      [lambda-var-args-exp (id bodies)
		(k (closure-var-args id bodies env))]
      [set!-exp (var val)
		(eval-exp val env (lambda (e-val)
			    (k (set-ref!
				(apply-env-ref env var
					       (lambda (v) v)
					       (lambda ()
						 (apply-env-ref global-env var
								(lambda (v) v)
								(lambda () (eopl:error 'apply-env-ref ; procedure to call if id not in env
										       "variable not found in environment: ~s"
										       var)))))
				e-val))))]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

; evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda (rands env k)
    (map-cps (lambda (looe k) (eval-exp (car looe) env k)) (list rands) k)))

(define map-cps
  (lambda (proc-cps lss k)
    (andmap-cps (make-cps null?) lss 
		(lambda (all-lists-null)
		  (if all-lists-null
		      (k '())
		      (proc-cps (map car lss)
				(lambda (proced-car)
				  (map-cps proc-cps (map cdr lss)
						   (lambda (mapped-cdr)
						     (k (cons proced-car mapped-cdr)))))))))))

(define make-cps
  (lambda (proc)
    (lambda (arg k)
      (k (proc arg)))))

(define andmap-cps
  (lambda (pred-cps ls k)
    (if (null? ls)
	(k #t)
	(pred-cps (car ls)
		  (lambda (v)
		    (if v
			(andmap-cps pred-cps
				    (cdr ls)
                                    k)
			(k #f)))))))

; evaluate multiple bodies
(define eval-multiple-bodies
  (lambda (bodies env k)
    (if (null? (cdr bodies))
	(eval-exp (car bodies) env k)
	(eval-exp (car bodies) env (lambda (v)
				     (eval-multiple-bodies (cdr bodies) env k))))))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args k)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args k)]
      [closure-const-args (vars bodies env)
	(let ([extended-env (extended-env-record vars (map cell args) env)])
	  (eval-multiple-bodies bodies extended-env k))]
      [closure-const-var-args (const-args var-args bodies env)
	(append-cps const-args (list var-args)
		    (lambda (appended)
		      (get-x args (length const-args)
			     (lambda (gotten)
			       (let ([extended-env (extended-env-record appended
									(map cell gotten)
									env)])
				 (eval-multiple-bodies bodies extended-env k))))))]
      [closure-var-args (var bodies env)
	(let ([extended-env (extended-env-record (list var) (list (cell args)) env)])
	  (eval-multiple-bodies bodies extended-env k))]
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

(define get-x
  (lambda (ls len k)
    (if (= 0 len)
	(k (list ls))
	(get-x (cdr ls) (sub1 len) (lambda (get-x-cdr)
				     (append-cps (list (car ls)) get-x-cdr k))))))

(define append-cps 
  (lambda (l1 l2 k)
    (if (null? l1)
	(k l2)
	(append-cps (cdr l1)
		    l2
		    (lambda (appended-cdr)
		      (k (cons (car l1)
			       appended-cdr)))))))

(define *prim-proc-names* '(+ - * / add1 sub1 zero? not = < <= > >= cons car cdr caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr list assq null? eq? equal? eqv? atom? length list->vector list? pair? procedure? vector->list vector make-vector vector-ref vector? number? symbol? set-car! set-cdr! vector-set! display newline map apply quotient list-tail void))

(define (make-init-env)         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map (lambda (ppn) (cell (prim-proc ppn)))
          *prim-proc-names*)
     (empty-env)))

(define global-env (make-init-env))

(define (reset-global-env) (set! global-env (make-init-env)))

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

(define apply-prim-proc
  (lambda (prim-proc args k)
    (case prim-proc
      [(+) (k (apply + args))]
      [(-) (k (apply - args))]
      [(*) (k (apply * args))]
      [(/) (k (apply / args))]
      [(add1) (cond
	       [(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
	       [else (k (+ (1st args) 1))])]
      [(sub1) (cond
	       [(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
	       [else (k (- (1st args) 1))])]
      [(zero?) (cond
	       [(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
	       [else (k (zero? (1st args)))])]
      [(not) (cond
	       [(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
	       [else (k (not (1st args)))])]
      [(=) (cond
	    [(null? args) (eopl:error prim-proc "incorrect argument count in call (~s)" prim-proc)]
	    [else (k (apply = args))])]
      [(<) (cond
	    [(null? args) (eopl:error prim-proc "incorrect argument count in call (~s)" prim-proc)]
	    [else (k (apply < args))])]
      [(<=) (cond
	    [(null? args) (eopl:error prim-proc "incorrect argument count in call (~s)" prim-proc)]
	    [else (k (apply <= args))])]
      [(>) (cond
	    [(null? args) (eopl:error prim-proc "incorrect argument count in call (~s)" prim-proc)]
	    [else (k (apply > args))])]
      [(>=) (cond
	    [(null? args) (eopl:error prim-proc "incorrect argument count in call (~s)" prim-proc)]
	    [else (k (apply >= args))])]
      [(cons) (cond
	       [(or (null? args) (null? (cdr args)) (not (null? (cddr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
	       [else (k (cons (1st args) (2nd args)))])]
      [(car) (cond
	      [(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
	      [else (k (car (1st args)))])]
      [(cdr) (cond
	      [(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
	      [else (k (cdr (1st args)))])]
      [(caar) (cond
	       [(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
	       [else (k (caar (1st args)))])]
      [(cdar) (cond
	       [(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
	       [else (k (cdar (1st args)))])]
      [(cadr) (cond
	       [(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
	       [else (k (cadr (1st args)))])]
      [(cddr) (cond
	       [(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
	       [else (k (cddr (1st args)))])]
      [(caaar) (cond
		[(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		[else (k (caaar (1st args)))])]
      [(caadr) (cond
		[(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		[else (k (caadr (1st args)))])]
      [(cadar) (cond
		[(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		[else (k (cadar (1st args)))])]
      [(caddr) (cond
		[(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		[else (k (cdaar (1st args)))])]
      [(cdaar) (cond
		[(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		[else (k (caddr (1st args)))])]
      [(cdadr) (cond
		[(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		[else (k (cdadr (1st args)))])]
      [(cddar) (cond
		[(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		[else (k (cddar (1st args)))])]
      [(cdddr) (cond
		[(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		[else (k (cdddr (1st args)))])]
      [(list) (k (apply list args))]
      [(assq) (cond
	       [(or (null? args) (null? (cadr args)) (not (null? (cddr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
	       [else (k (assq (car args) (cadr args)))])]
      [(null?) (cond
		[(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		[else (k (null? (car args)))])]
      [(eq?) (cond
	      [(or (null? args) (null? (cadr args)) (not (null? (cddr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
	      [else (k (eq? (car args) (cadr args)))])]
      [(equal?) (cond
		 [(or (null? args) (null? (cadr args)) (not (null? (cddr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		 [else (k (equal? (car args) (cadr args)))])]
      [(eqv?) (cond
	       [(or (null? args) (null? (cadr args)) (not (null? (cddr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		 [else (k (eqv? (car args) (cadr args)))])]
      [(atom?) (cond
		[(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		[else (k (atom? (car args)))])]
      [(length) (cond
		 [(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		 [else (k (length (car args)))])]
      [(list->vector) (cond
		       [(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		       [else (k (list->vector (car args)))])]
      [(list?) (cond
		[(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		[else (k (list? (car args)))])]
      [(pair?) (cond
		[(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		[else (k (pair? (car args)))])]
      [(procedure?) (cond
		     [(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		     [else (k (proc-val? (car args)))])]
      [(vector->list) (cond
		       [(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		       [else (k (vector->list (car args)))])]
      [(vector) (k (list->vector args))]
      [(make-vector) (cond
		      [(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		      [else (k (make-vector (car args)))])]
      [(newline) (cond
		  [(not (null? args)) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		  [else (k (newline))])]
      [(display) (cond
		  [(or (not (null? (cdr args))) (not (null? args))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		  [else (k (display (car args)))])]
      [(vector?) (cond
		  [(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		  [else (k (vector? (car args)))])]
      [(symbol?) (cond
		  [(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		  [else (k (symbol? (car args)))])]
      [(number?) (cond
		  [(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		  [else (k (number? (car args)))])]
      [(vector-ref) (cond
		     [(or (null? args) (not (null? (cddr args))) (null? (cdr args))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		     [else (k (vector-ref (car args) (cadr args)))])]
      [(set-car!) (cond
		   [(or (null? args) (not (null? (cddr args))) (null? (cdr args))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		   [else (k (set-car! (car args) (cadr args)))])]
      [(set-cdr!) (cond
		   [(or (null? args) (not (null? (cddr args))) (null? (cdr args))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		   [else (k (set-cdr! (car args) (cadr args)))])]
      [(vector-set!) (cond
		      [(or (null? args) (not (null? (cdddr args))) (null? (cdr args))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		      [else (k (vector-set! (car args) (cadr args) (caddr args)))])]
      [(map) (cond
	      [(or (null? args) (null? (cdr args))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
	      [else (map-cps (lambda (x k) (apply-proc (1st args) x k)) (cdr args) k)])]
      [(apply) (cond
		[(or (null? args) (null? (cdr args)) (not (null? (cddr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		[else (apply-proc (1st args) (2nd args) k)])]
	  [(quotient) (cond
		[(or (null? args) (null? (cdr args)) (not (null? (cddr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		[else (k (quotient (car args) (cadr args)))])]
	  [(list-tail) (cond
		[(or (null? args) (null? (cdr args)) (not (null? (cddr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		[else (k (list-tail (car args) (cadr args)))])]
	  [(void) (cond
		[(not (null? args)) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		[else (k (void))])]
      [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-proc)])))

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([line (read)])
      (if (not (equal? line '(exit)))
	  (let ([answer (top-level-eval (syntax-expand (parse-exp line)) (lambda (x) x))])
	    ;; TODO: are there answers that should display differently?
	    (eopl:pretty-print (elim-closures answer (lambda (x) x))) ;;(newline)
	    (rep))))))  ; tail-recursive, so stack doesn't grow.

(define elim-closures
  (lambda (answer k)
    (cond
     [(proc-val? answer) (k '<interpreter-procedure>)]
     [(pair? answer) (elim-closures (car answer)
				    (lambda (e-car)
				      (elim-closures (cdr answer)
						     (lambda (e-cdr)
						       (k (cons e-car e-cdr))))))]
;      (cons (elim-closures (car answer)) (elim-closures (cdr answer)))]
     [else (k answer)])))

(define eval-one-exp
  (lambda (x) (top-level-eval (syntax-expand (parse-exp x)) (lambda (evald-expression)
					      (elim-closures evald-expression (lambda (x) x))))))










