; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form k)
    ; later we may add things that are not expressions.
    (eval-exp form global-env k)))

; define-eval evaluates a definition in the global environment
(define define-eval
  (lambda (sym value k)
    (apply-env-ref global-env
		   sym
		   (lambda (ref)
		     (eval-exp value global-env (lambda (e-val) (k (set-ref! ref e-val)))))
		   (lambda ()
		     (eval-exp value global-env
			       (lambda (e-val)
				 (set-cdr! (car global-env) (vector-add-left (cdar global-env) e-val))
				 (k (set-car! (car global-env) (cons sym (caar global-env))))))))))

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
      [define-exp (symbol value) (define-eval symbol value k)]
      [app-exp (rator rands)
	(eval-exp rator env (lambda (proc-value)
			      (if (prim-proc? proc-value)
				  (map-cps (lambda (arg k) (k #f))
					   (list rands)
					   (lambda (refs)
					     (eval-rands rands refs env
							 (lambda (args)
							   (apply-proc proc-value args k)))))
				  (eval-rands rands (get-refs proc-value) env
					      (lambda (args)
						(replace-proc-refs proc-value args
								   (lambda (new-proc) (apply-proc new-proc args k))))))))]
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
      [lambda-const-args-exp (vars refs bodies)
        (k (closure-const-args vars refs bodies env))]
      [lambda-const-var-args-exp (const-id refs var-id bodies)
        (k (closure-const-var-args const-id refs var-id bodies env))]
      [lambda-var-args-exp (id bodies)
		(k (closure-var-args id bodies env))]
      [set!-exp (var val)
	(eval-exp val env (lambda (e-val)
			    (apply-env-ref env var
					   (lambda (ref) (k (set-ref! ref e-val)))
					   (lambda ()
					     (apply-env-ref global-env var
							    (lambda (ref) (k (set-ref! ref e-val)))
							    (lambda () (eopl:error 'apply-env-ref "variable not found in environment: ~s" var)))))))]
      [set!-exp-ref (ref val)
	(eval-exp val env (lambda (e-val)
			    (k (set-ref! ref e-val))))]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

; evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda (rands refs env k)
    (map-cps (lambda (looe k) 
	       (if (cadr looe)
		   (cases expression (car looe)
		     [var-exp (sym)
			      (apply-env-ref env
					     sym
					     k
					     (lambda ()
					       (apply-env-ref global-env sym k
							      (lambda ()
								(eopl:error 'apply-env-ref
									    "variable not found in environment: ~s"
									    sym)))))]
		     [else (eopl:error 'ref "Argument should be passed by reference: ~s" (car looe))])
		   (eval-exp (car looe) env k))) (list rands refs) k)))

(define map-cps
  (lambda (proc-cps lss k)
    (andmap-cps (make-cps null?) lss 
		(lambda (all-lists-null)
		  (ormap-cps (make-cps null?) lss
			     (lambda (someListNull)
			       (if (and someListNull (not all-lists-null))
				    (eopl:error 'map "lists differ in length")
				    (if all-lists-null
					(k '())
					(proc-cps (map car lss)
						  (lambda (proced-car)
						    (map-cps proc-cps (map cdr lss)
							     (lambda (mapped-cdr)
							       (k (cons proced-car mapped-cdr))))))))))))))

(define fold-left-cps
  (lambda (proc-cps init argss k)
    (andmap-cps (make-cps null?) argss
		(lambda (all-lists-null)
		  (if all-lists-null
		      (k init)
		      (map-cps (make-cps caar)
			       (list argss)
			       (lambda (mapped-car)
				 (proc-cps init mapped-car
					   (lambda (proced-car)
					     (map-cps (make-cps cdar) (list argss)
						      (lambda (mapped-cdr)
							(fold-left-cps proc-cps proced-car mapped-cdr k))))))))))))

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

(define member?-cps
  (lambda (item lst cont)
    (cond
     [(null? lst) (cont #f)]
     [(equal? item (car lst)) (cont #t)]
     [else (member?-cps item (cdr lst) cont)])))

(define ormap-cps
  (lambda (pred-cps ls k)
    (if (null? ls)
	(k #f)
	(pred-cps (car ls)
		  (lambda (isPred)
		    (if isPred
			(k #t)
			(ormap-cps pred-cps (cdr ls) k)))))))

; evaluate multiple bodies
(define eval-multiple-bodies
  (lambda (bodies env k)
    (if (null? (cdr bodies))
	(eval-exp (car bodies) env k)
	(eval-exp (car bodies) env
		  (lambda (v)
		    (eval-multiple-bodies (cdr bodies) env k))))))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args k)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args k)]
      [closure-const-args (vars refs bodies env)
	(let ([extended-env (extend-env vars (list->vector args) env)])
	  (eval-multiple-bodies bodies extended-env k))]
      [closure-const-var-args (const-args refs var-args bodies env)
	(append-cps const-args (list var-args)
		    (lambda (appended)
		      (get-x args (length const-args)
			     (lambda (gotten)
			       (let ([extended-env (extend-env appended
							       (list->vector gotten)
							       env)])
				 (eval-multiple-bodies bodies extended-env k))))))]
      [closure-var-args (var bodies env)
	(let ([extended-env (extend-env (list var) `#(,args) env)])
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

(define *prim-proc-names* '(+ - * / add1 sub1 zero? not = < <= > >= cons car cdr caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr list assq null? eq? equal? eqv? atom? length list->vector list? pair? procedure? vector->list vector make-vector vector-ref vector? number? symbol? set-car! set-cdr! vector-set! display newline map apply quotient list-tail void load append call/cc exit))

(define make-init-env
  (lambda ()             ; for now, our initial global environment only contains 
    (cons           ; procedure names.  Recall that an environment associates
     (cons *prim-proc-names*   ;  a value (not an expression) with an identifier.
	   (list->vector (map prim-proc *prim-proc-names*)))
     (empty-env))))

(define global-env (make-init-env))

(define reset-global-env (lambda () (set! global-env (make-init-env))))

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
	  [(load) (cond
		   [(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		   [else (k (let ([file (open-input-file (car args))])
			      (let loop ()
				(let ([expr (read file)])
				  (if (not (eqv? expr '#!eof))
				      (begin
					(eval-one-exp expr)
					(loop))
				      (close-input-port file))))))])]
	  [(append) (cond
		[(or (null? args) (null? (car args))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		[else (k (apply append args))])]
	  [(exit) args]
      [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-proc)])))

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    (let ([line (read)])
      (if (not (equal? line '(exit)))
	  (let ([answer (top-level-eval (syntax-expand (parse-exp line)) (lambda (x) x))])
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
     [else (k answer)])))

(define eval-one-exp
  (lambda (x) (top-level-eval (syntax-expand (parse-exp x))
			      (lambda (v) (elim-closures v (lambda (v) v))))))

(define eval-one-debug
  (lambda (x) (top-level-eval (syntax-expand (parse-exp x)) (lambda (x) x))))

(define replace-proc-refs
  (lambda (proc rands k)
    (cases proc-val proc
      [prim-proc (name) proc]
      [closure-const-args (args refs bodies env)
	(map-cps (lambda (loob k)
		   (fold-left-cps
		    (lambda (prev loair k)
		      (if (cadr loair)
			  (replace-free-refs prev (car loair) (caddr loair) k)
			  (k prev)))
		    (car loob) (list args refs rands) k))
		 (list bodies)
		 (lambda (new-bodies)
		   (k (closure-const-args args refs new-bodies env))))]
      [closure-const-var-args (const-args refs var-args bodies env)
	(map-cps (lambda (loob k)
		   (fold-left-cps
		    (lambda (prev loair k)
		      (if (cadr loair)
			  (replace-free-refs prev (car loair) (caddr loair) k)
			  (k prev)))
		    (car loob) (list const-args refs rands) k))
		 (list bodies)
		 (lambda (new-bodies)
		   (k (closure-const-var-args const-args refs var-args new-bodies env))))]
      [closure-var-args (arg bodies env)
        (map-cps (lambda (loob k)
		   (fold-left-cps
		    (lambda (prev loair k)
		      (if (cadr loair)
			  (replace-free-refs prev (car loair) (caddr loair) k)
			  (k prev)))
		    (car loob) (list (list arg) (list #f) rands) k))
		 (list bodies)
		 (lambda (new-bodies)
		   (k (closure-var-args arg new-bodies env))))])))

(define replace-free-refs
  (lambda (expr arg refarg k)
    (cases expression expr
      [var-exp (id) (if (eqv? arg id)
			(k (lit-exp (deref refarg)))
			(k (var-exp id)))]
      [lambda-const-args-exp (vars refs bodies)
	(member?-cps arg vars
		     (lambda (is-member)
		       (if is-member
			   (k expr)
			   (map-cps (lambda (loob k)
				      (replace-free-refs (car loob) arg refarg k))
				    (list bodies)
				    (lambda (new-bodies) (k (lambda-const-args-exp vars refs new-bodies)))))))]
      [lambda-const-var-args-exp (const-id refs var-id bodies)
	(member?-cps arg const-id
		     (lambda (is-member)
		       (if is-member
			   (k expr)
			   (map-cps (lambda (loob k)
				      (replace-free-refs (car loob) arg refarg k))
				    (list bodies)
				    (lambda (new-bodies) (k (lambda-const-var-args-exp const-id
										       refs
										       var-id
										       new-bodies)))))))]
      [lambda-var-args-exp (id bodies)
        (if (eqv? arg id)
	    (k expr)
	    (map-cps (lambda (loob k)
		       (replace-free-refs (car loob) arg refarg k))
		     (list bodies)
		     (lambda (new-bodies) (k (lambda-var-args-exp id new-bodies)))))]
      [if-exp (condition if-then if-else)
	(replace-free-refs condition arg refarg
			   (lambda (repd-condition)
			     (replace-free-refs if-then arg refarg
						(lambda (repd-if-then)
						  (replace-free-refs if-else arg refarg
								     (lambda (repd-if-else)
								       (k (if-exp repd-condition
										  repd-if-then
										  repd-if-else))))))))]
      [if-true-exp (condition if-then)
        (replace-free-refs condition arg refarg
			   (lambda (repd-condition)
			     (replace-free-refs if-then arg refarg
						(lambda (repd-if-then)
						  (k (if-true-exp repd-condition
								  repd-if-then))))))]
      [app-exp (rator rands)
	(replace-free-refs rator arg refarg
			   (lambda (repd-rator)
			     (map-cps (lambda (loor k)
					(replace-free-refs (car loor) arg refarg k))
				      (list rands)
				      (lambda (repd-rands)
					(k (app-exp repd-rator repd-rands))))))]
      [set!-exp (var val)
        (replace-free-refs val arg refarg
			   (lambda (repd-val)
			     (if (eqv? arg var)
				 (k (set!-exp-ref refarg repd-val))
				 (k (set!-exp var repd-val)))))]
      [set!-exp-ref (ref val)
	(replace-free-refs val arg refarg
			   (lambda (repd-val)
			     (k (set!-exp-ref ref repd-val))))]
      [else (k expr)])))
