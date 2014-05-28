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
		   (define-eval-succeed-k value k)
		   (define-eval-fail-k sym value k))))

; eval-exp is the main component of the interpreter
(define eval-exp
  (lambda (exp env k)
    (case (car exp)
      [(lit-exp) (apply-k k (cadr exp))]
      [(var-exp)
       (apply-env env (cadr exp) ; look up its value.
		  k ; procedure to call if id is in the environment
		  (apply-env-k global-env (cadr exp) k (error-k (list 'apply-env "variable not found in environment: ~s" (cadr exp)))))]
      [(define-exp) (define-eval (cadr exp) (caddr exp) k)]
      [(app-exp)
       (eval-exp (cadr exp) env (proc-ref-k (caddr exp) env k))]
      [(if-exp)
       (eval-exp (cadr exp) env (if-k (caddr exp) (caddr (cdr exp)) env k))]
      [(if-true-exp)
       (eval-exp (cadr exp) env (if-true-k (caddr exp) env k))]
      [(lambda-const-args-exp)
       (apply-k k (closure-const-args (cadr exp) (caddr exp) (caddr (cdr exp)) env))]
      [(lambda-const-var-args-exp)
       (apply-k k (closure-const-var-args (cadr exp) (caddr exp) (caddr (cdr exp)) (caddr (cddr exp)) env))]
      [(lambda-var-args-exp)
       (apply-k k (closure-var-args (cadr exp) (caddr exp) env))]
      [(set!-exp)
       (eval-exp (caddr exp) env (set!-k env (cadr exp) k))]
      [(set!-exp-ref)
       (eval-exp (caddr exp) env (set-ref!-k (cadr exp) k))]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

; evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda (rands refs env k)
    (map-cps (lambda (looe k) 
	       (if (cadr looe)
		   (case (car (car looe))
		     [(var-exp)
			      (apply-env-ref env
					     (cadr (car looe))
					     k
					     (apply-env-ref-k global-env (cadr (car looe)) k
							      (error-k (list 'apply-env-ref
									     "variable not found in environment: ~s"
									     (cadr (car looe))))))]
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
    (case (car proc-value)
      [(prim-proc) (apply-prim-proc (cadr proc-value) args k)]
      [(closure-const-args)
       (let ([extended-env (extend-env (cadr proc-value) (list->vector args) (caddr (cddr proc-value)))])
	 (eval-multiple-bodies (caddr (cdr proc-value)) extended-env k))]
      [(closure-const-var-args)
       (append-cps (cadr proc-value) (list (caddr (cdr proc-value)))
		   (apply-proc-append-k args (cadr proc-value) (caddr (cddr proc-value)) (caddr (cdddr proc-value)) k))]
      [(closure-var-args)
       (let ([extended-env (extend-env (list (cadr proc-value)) (list->vector (list args)) (caddr (cdr proc-value)))])
	 (eval-multiple-bodies (caddr proc-value) extended-env k))]
      [(cont-proc)
       (apply-k (cadr proc-value) (car args))]
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

(define get-x
  (lambda (ls len k)
    (if (= 0 len)
	(apply-k k (list ls))
	(get-x (cdr ls) (sub1 len) (get-x-rest-k ls k)))))

(define append-cps 
  (lambda (l1 l2 k)
    (if (null? l1)
	(apply-k k l2)
	(append-cps (cdr l1)
		    l2
		    (append-rest-k l1 k)))))

(define *prim-proc-names* '(+ - * / add1 sub1 zero? not = < <= > >= cons car cdr caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr list assq null? eq? equal? eqv? atom? length list->vector list? pair? procedure? vector->list vector make-vector vector-ref vector? number? symbol? set-car! set-cdr! vector-set! display newline map apply quotient list-tail void load append call/cc exit modulo boolean? string? andmap list-head reverse vector-length))

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
		[else (apply-k k (caddr (1st args)))])]
      [(cdaar) (cond
		[(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		[else (apply-k k (cdaar (1st args)))])]
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
      [(vector-length) (apply-k k (apply vector-length args))]
      [(newline) (cond
		  [(not (null? args)) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		  [else (apply-k k (newline))])]
      [(display) (cond
		  [(or (null? args) (not (null? (cdr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
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
      [(andmap) (andmap-cps (lambda (x k) (apply-proc (car args) (list x) k)) (cadr args) k)]
      [(apply) (cond
		[(or (null? args) (null? (cdr args)) (not (null? (cddr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		[else (apply-proc (1st args) (2nd args) k)])]
	  [(quotient) (cond
		[(or (null? args) (null? (cdr args)) (not (null? (cddr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		[else (apply-k k (quotient (car args) (cadr args)))])]
	  [(list-tail) (cond
		[(or (null? args) (null? (cdr args)) (not (null? (cddr args)))) (eopl:error prim-proc "incorrect argument count in call (~s ~s)" prim-proc args)]
		[else (apply-k k (list-tail (car args) (cadr args)))])]
	  [(list-head) (apply-k k (apply list-head args))]
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
	  [(modulo) (apply-k k (apply modulo args))]
	  [(exit) args]
	  [(call/cc) (apply-k (call/cc-k k) (car args))]
	  [(boolean?) (apply-k k (apply boolean? args))]
	  [(string?) (apply-k k (apply string? args))]
	  [(reverse) (apply-k k (apply reverse args))]
      [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-proc)])))

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    (let ([line (read)])
      (if (not (equal? line '(exit)))
	  (let ([answer (top-level-eval (syntax-expand (parse-exp line)))])
	    (eopl:pretty-print (elim-closures answer (lambda (x) x))) ;;(newline)
	    (rep))))))  ; tail-recursive, so stack doesn't grow.

(define elim-closures ;Never actually called by eval-exp
  (lambda (answer k)
    (cond
     [(null? answer) (k answer)]
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
    (case (car proc)
      [(prim-proc) (apply-k k proc)]
      [(closure-const-args)
	(map-cps (lambda (loob kont)
		   (fold-left-cps
		    (lambda (prev loair kont)
		      (if (cadr loair)
			  (replace-free-refs prev (car loair) (caddr loair) kont)
			  (apply-k kont prev)))
		    (car loob) (list (cadr proc) (caddr proc) rands) kont))
		 (list (caddr (cdr proc)))
		 (replace-closure-const-args-bodies-k (cadr proc) (caddr proc) (caddr (cddr proc)) k))]
      [(closure-const-var-args)
	(map-cps (lambda (loob k)
		   (fold-left-cps
		    (lambda (prev loair k)
		      (if (cadr loair)
			  (replace-free-refs prev (car loair) (caddr loair) k)
			  (apply-k k prev)))
		    (car loob) (list (cadr proc) (caddr proc) (list-head rands (length (cadr proc)))) k))
		 (list (caddr (cddr proc)))
		 (replace-closure-const-var-args-bodies-k (cadr proc) (caddr proc) (caddr (cdr proc)) (caddr (cdddr proc)) k))]
      [(closure-var-args)
	(apply-k k proc)]
      [else (apply-k k proc)])))
;      [cont-proc (cont) (apply-k cont proc)])))
;        (map-cps (lambda (loob k)
;		   (fold-left-cps
;		    (lambda (prev loair k)
;		      (if (cadr loair)
;			  (replace-free-refs prev (car loair) (caddr loair) k)
;			  (apply-k k prev)))
;		    (car loob) (list (list arg) (list #f) rands) k))
;		 (list bodies)
;		 (replace-closure-var-args-k arg env k))])))

(define replace-free-refs
  (lambda (expr arg refarg k)
    (case (car expr)
      [(var-exp) (if (eqv? arg (cadr expr))
			(deref refarg (replace-free-refs-make-lit-exp-k k))
			(apply-k k (var-exp (cadr expr))))]
      [(lambda-const-args-exp)
	(member?-cps arg (cadr expr) (replace-free-refs-const-args-member?-k expr arg refarg (cadr expr) (caddr expr) (caddr (cdr expr)) k))]
      [(lambda-const-var-args-exp)
	(member?-cps arg (cadr expr) (replace-free-refs-const-var-args-member?-k expr arg refarg (cadr expr) (caddr expr) (caddr (cdr expr)) (caddr (cddr expr)) k))]
      [(lambda-var-args-exp)
        (if (eqv? arg (cadr expr))
	    (apply-k k expr)
	    (map-cps (lambda (loob k)
		       (replace-free-refs (car loob) arg refarg k))
		     (list (caddr expr))
		     (replace-free-refs-replace-var-args-bodies-k (cadr expr) k)))]
      [(if-exp)
	(replace-free-refs (cadr expr) arg refarg (replace-free-refs-if-replace-condition-k (caddr expr) (caddr (cdr expr)) arg refarg k))]
      [(if-true-exp)
        (replace-free-refs (cadr expr) arg refarg (replace-free-refs-if-true-replace-condition-k (caddr expr) arg refarg k))]
      [(app-exp)
	(replace-free-refs (cadr expr) arg refarg (replace-free-refs-app-replace-rator-k (caddr expr) arg refarg k))]
      [(set!-exp)
        (replace-free-refs (caddr expr) arg refarg (replace-free-refs-set-exp-replace-val-k (cadr expr) arg refarg k))]
      [(set!-exp-ref)
	(replace-free-refs (caddr expr) arg refarg (replace-free-refs-set-exp-ref-replace-val-k (cadr expr) k))]
      [else (apply-k k expr)])))
