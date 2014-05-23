; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form global-env (id-k))))

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
      [lit-exp (datum) (apply-k k datum)]
      [var-exp (id)
        (apply-env env id ; look up its value.
		   k ; procedure to call if id is in the environment
		   (apply-env-k global-env id k (error-k (list 'apply-env "variable not fount in environment: ~s" id))))]
      [define-exp (symbol value) (define-eval symbol value k)]
      [app-exp (rator rands)
	(eval-exp rator env (proc-ref-k rands env k))]
      [if-exp (condition if-then if-else)
		(eval-exp condition env (if-k if-then if-else env k))]
      [if-true-exp (condition if-then)
		(eval-exp condition env (if-true-k if-then env k))]
      [lambda-const-args-exp (vars refs bodies)
        (apply-k k (closure-const-args vars refs bodies env))]
      [lambda-const-var-args-exp (const-id refs var-id bodies)
        (apply-k k (closure-const-var-args const-id refs var-id bodies env))]
      [lambda-var-args-exp (id bodies)
		(apply-k k (closure-var-args id bodies env))]
      [set!-exp (var val)
	(eval-exp val env (set!-k env var k))]
      [set!-exp-ref (ref val)
	(eval-exp val env (set-ref!-k ref k))]
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
    (andmap-cps (make-cps null?) lss (map-cps-all-lists-null?-k proc-cps lss k))))

(define fold-left-cps
  (lambda (proc-cps init argss k)
    (andmap-cps (make-cps null?) argss (fold-left-cps-all-lists-null?-k proc-cps init argss k))))
		
(define make-cps
  (lambda (proc)
    (lambda (arg k)
      (apply-k k (proc arg)))))

(define andmap-cps
  (lambda (pred-cps ls k)
    (if (null? ls)
	(apply-k k #t)
	(pred-cps (car ls)
		  (andmap-rest-k pred-cps (cdr ls) k)))))

(define member?-cps
  (lambda (item lst k)
    (cond
     [(null? lst) (apply-k k #f)]
     [(equal? item (car lst)) (apply-k k #t)]
     [else (member?-cps item (cdr lst) k)])))

(define ormap-cps
  (lambda (pred-cps ls k)
    (if (null? ls)
	(apply-k k #f)
	(pred-cps (car ls)
		  (ormap-rest-k pred-cps (cdr ls) k)))))

; evaluate multiple bodies
(define eval-multiple-bodies
  (lambda (bodies env k)
    (if (null? (cdr bodies))
	(eval-exp (car bodies) env k)
	(eval-exp (car bodies) env (multi-body-k (cdr bodies) env k)))))

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

(define *prim-proc-names* '(+ - * / add1 sub1 zero? not = < <= > >= cons car cdr caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr list assq null? eq? equal? eqv? atom? length list->vector list? pair? procedure? vector->list vector make-vector vector-ref vector? number? symbol? set-car! set-cdr! vector-set! display newline map apply quotient list-tail void load append call/cc exit modulo))

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
      [(+) (apply-k k (apply + args))]
      [(-) (apply-k k (apply - args))]
      [(*) (apply-k k (apply * args))]
      [(/) (apply-k k (apply / args))]
      [(add1) (cond
	       [(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
	       [else (apply-k k (+ (1st args) 1))])]
      [(sub1) (cond
	       [(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
	       [else (apply-k k (- (1st args) 1))])]
      [(zero?) (cond
	       [(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
	       [else (apply-k k (zero? (1st args)))])]
      [(not) (cond
	       [(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
	       [else (apply-k k (not (1st args)))])]
      [(=) (cond
	    [(null? args) (eopl:error prim-proc "incorrect argument count in call (~s)" prim-proc)]
	    [else (apply-k k (apply = args))])]
      [(<) (cond
	    [(null? args) (eopl:error prim-proc "incorrect argument count in call (~s)" prim-proc)]
	    [else (apply-k k (apply < args))])]
      [(<=) (cond
	    [(null? args) (eopl:error prim-proc "incorrect argument count in call (~s)" prim-proc)]
	    [else (apply-k k (apply <= args))])]
      [(>) (cond
	    [(null? args) (eopl:error prim-proc "incorrect argument count in call (~s)" prim-proc)]
	    [else (apply-k k (apply > args))])]
      [(>=) (cond
	    [(null? args) (eopl:error prim-proc "incorrect argument count in call (~s)" prim-proc)]
	    [else (apply-k k (apply >= args))])]
      [(cons) (cond
	       [(or (null? args) (null? (cdr args)) (not (null? (cddr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
	       [else (apply-k k (cons (1st args) (2nd args)))])]
      [(car) (cond
	      [(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
	      [else (apply-k k (car (1st args)))])]
      [(cdr) (cond
	      [(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
	      [else (apply-k k (cdr (1st args)))])]
      [(caar) (cond
	       [(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
	       [else (apply-k k (caar (1st args)))])]
      [(cdar) (cond
	       [(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
	       [else (apply-k k (cdar (1st args)))])]
      [(cadr) (cond
	       [(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
	       [else (apply-k k (cadr (1st args)))])]
      [(cddr) (cond
	       [(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
	       [else (apply-k k (cddr (1st args)))])]
      [(caaar) (cond
		[(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		[else (apply-k k (caaar (1st args)))])]
      [(caadr) (cond
		[(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		[else (apply-k k (caadr (1st args)))])]
      [(cadar) (cond
		[(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		[else (apply-k k (cadar (1st args)))])]
      [(caddr) (cond
		[(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		[else (apply-k k (cdaar (1st args)))])]
      [(cdaar) (cond
		[(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		[else (apply-k k (caddr (1st args)))])]
      [(cdadr) (cond
		[(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		[else (apply-k k (cdadr (1st args)))])]
      [(cddar) (cond
		[(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		[else (apply-k k (cddar (1st args)))])]
      [(cdddr) (cond
		[(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		[else (apply-k k (cdddr (1st args)))])]
      [(list) (apply-k k args)]
      [(assq) (cond
	       [(or (null? args) (null? (cadr args)) (not (null? (cddr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
	       [else (apply-k k (assq (car args) (cadr args)))])]
      [(null?) (cond
		[(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		[else (apply-k k (null? (car args)))])]
      [(eq?) (cond
	      [(or (null? args) (null? (cadr args)) (not (null? (cddr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
	      [else (apply-k k (eq? (car args) (cadr args)))])]
      [(equal?) (cond
		 [(or (null? args) (null? (cadr args)) (not (null? (cddr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		 [else (apply-k k (equal? (car args) (cadr args)))])]
      [(eqv?) (cond
	       [(or (null? args) (null? (cadr args)) (not (null? (cddr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		 [else (apply-k k (eqv? (car args) (cadr args)))])]
      [(atom?) (cond
		[(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		[else (apply-k k (atom? (car args)))])]
      [(length) (cond
		 [(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		 [else (apply-k k (length (car args)))])]
      [(list->vector) (cond
		       [(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		       [else (apply-k k (list->vector (car args)))])]
      [(list?) (cond
		[(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		[else (apply-k k (list? (car args)))])]
      [(pair?) (cond
		[(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		[else (apply-k k (pair? (car args)))])]
      [(procedure?) (cond
		     [(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		     [else (apply-k k (proc-val? (car args)))])]
      [(vector->list) (cond
		       [(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		       [else (apply-k k (vector->list (car args)))])]
      [(vector) (apply-k k (list->vector args))]
      [(make-vector) (cond
		      [(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		      [else (apply-k k (make-vector (car args)))])]
      [(newline) (cond
		  [(not (null? args)) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		  [else (apply-k k (newline))])]
      [(display) (cond
		  [(or (not (null? (cdr args))) (not (null? args))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		  [else (apply-k k (display (car args)))])]
      [(vector?) (cond
		  [(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		  [else (apply-k k (vector? (car args)))])]
      [(symbol?) (cond
		  [(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		  [else (apply-k k (symbol? (car args)))])]
      [(number?) (cond
		  [(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		  [else (apply-k k (number? (car args)))])]
      [(vector-ref) (cond
		     [(or (null? args) (not (null? (cddr args))) (null? (cdr args))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		     [else (apply-k k (vector-ref (car args) (cadr args)))])]
      [(set-car!) (cond
		   [(or (null? args) (not (null? (cddr args))) (null? (cdr args))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		   [else (apply-k k (set-car! (car args) (cadr args)))])]
      [(set-cdr!) (cond
		   [(or (null? args) (not (null? (cddr args))) (null? (cdr args))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		   [else (apply-k k (set-cdr! (car args) (cadr args)))])]
      [(vector-set!) (cond
		      [(or (null? args) (not (null? (cdddr args))) (null? (cdr args))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		      [else (apply-k k (vector-set! (car args) (cadr args) (caddr args)))])]
      [(map) (cond
	      [(or (null? args) (null? (cdr args))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
	      [else (map-cps (lambda (x k) (apply-proc (1st args) x k)) (cdr args) k)])]
      [(apply) (cond
		[(or (null? args) (null? (cdr args)) (not (null? (cddr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		[else (apply-proc (1st args) (2nd args) k)])]
	  [(quotient) (cond
		[(or (null? args) (null? (cdr args)) (not (null? (cddr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		[else (apply-k k (quotient (car args) (cadr args)))])]
	  [(list-tail) (cond
		[(or (null? args) (null? (cdr args)) (not (null? (cddr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		[else (apply-k k (list-tail (car args) (cadr args)))])]
	  [(void) (cond
		[(not (null? args)) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		[else (apply-k k (void))])]
	  [(load) (cond
		   [(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		   [else (apply-k k (let ([file (open-input-file (car args))])
			      (let loop ()
				(let ([expr (read file)])
				  (if (not (eqv? expr '#!eof))
				      (begin
					(eval-one-exp expr)
					(loop))
				      (close-input-port file))))))])]
	  [(append) (cond
		[(or (null? args) (null? (car args))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		[else (apply-k k (apply append args))])]
	  [(modulo) (apply modulo args)]
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

(define elim-closures ;Never actually called by eval-exp
  (lambda (answer k)
    (cond
     [(proc-val? answer) (k '<interpreter-procedure>)]
     [(pair? answer) (elim-closures (car answer)
				    (lambda (e-car)
				      (elim-closures (cdr answer)
						     (lambda (e-cdr)
						       (k (cons e-car e-cdr))))))]
     [else (k answer)])))

(define eval-one-exp ;Never actually called by eval-exp
  (lambda (x) (elim-closures (top-level-eval (syntax-expand (parse-exp x))) (lambda (v) v))))

(define eval-one-debug ;Never actually called by eval-exp
  (lambda (x) (top-level-eval (syntax-expand (parse-exp x)))))

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
			  (apply-k k prev)))
		    (car loob) (list args refs rands) k))
		 (list bodies)
		 (replace-closure-const-args-bodies-k args refs env k))]
; START FROM HERE
      [closure-const-var-args (const-args refs var-args bodies env)
	(map-cps (lambda (loob k)
		   (fold-left-cps
		    (lambda (prev loair k)
		      (if (cadr loair)
			  (replace-free-refs prev (car loair) (caddr loair) k)
			  (apply-k k prev)))
		    (car loob) (list const-args refs rands) k))
		 (list bodies)
		 (replace-closure-const-var-args-bodies-k const-args refs var-args env k))]
      [closure-var-args (arg bodies env)
        (map-cps (lambda (loob k)
		   (fold-left-cps
		    (lambda (prev loair k)
		      (if (cadr loair)
			  (replace-free-refs prev (car loair) (caddr loair) k)
			  (apply-k k prev)))
		    (car loob) (list (list arg) (list #f) rands) k))
		 (list bodies)
		 (replace-closure-var-args-k arg env k))])))

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
