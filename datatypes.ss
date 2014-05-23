
;; Parsed expression datatypes

(define-datatype expression expression? 
  [var-exp
   (id symbol?)]
  [lit-exp
   (id literal?)]
  [lambda-const-args-exp
   (id (list-of symbol?))
   (refs (list-of boolean?))
   (body (list-of expression?))]
  [lambda-const-var-args-exp
   (const-id (list-of symbol?))
   (refs (list-of boolean?))
   (var-id symbol?)
   (body (list-of expression?))]
  [lambda-var-args-exp
   (id symbol?)
   (body (list-of expression?))]
  [if-exp
   (condition expression?)
   (if-then expression?)
   (if-else expression?)]
  [if-true-exp
   (condition expression?)
   (if-then expression?)]
  [let-exp
   (vars (list-of symbol?))
   (refs (list-of boolean?))
   (exps (list-of expression?))
   (body (list-of expression?))]
  [named-let-exp
   (name symbol?)
   (vars (list-of symbol?))
   (refs (list-of boolean?))
   (exps (list-of expression?))
   (body (list-of expression?))]
  [let*-exp
   (vars (list-of symbol?))
   (refs (list-of boolean?))
   (exps (list-of expression?))
   (body (list-of expression?))]
  [letrec-exp
   (vars (list-of symbol?))
   (refs (list-of boolean?))
   (exps (list-of expression?))
   (body (list-of expression?))]
  [set!-exp
   (var symbol?)
   (val expression?)]
  [app-exp
   (operator expression?)
   (operands (list-of expression?))]
  [begin-exp
   (bodies (list-of expression?))]
  [cond-exp
   (conditions (list-of expression?))
   (if-thenss (list-of (list-of expression?)))]
  [cond-else-exp
   (conditions (list-of expression?))
   (if-thenss (list-of (list-of expression?)))
   (cond-elses (list-of expression?))]
  [and-exp
   (bools (list-of expression?))]
  [or-exp
   (bools (list-of expression?))]
  [case-exp
   (id expression?)
   (keyss (list-of (list-of expression?)))
   (exprss (list-of (list-of expression?)))]
  [case-else-exp
   (id expression?)
   (keyss (list-of (list-of expression?)))
   (exprss (list-of (list-of expression?)))
   (case-elses (list-of expression?))]
  [while-exp
   (condition expression?)
   (bodies (list-of expression?))]
  [define-exp
    (name symbol?)
    (val expression?)]
  [set!-exp-ref
   (ref reference?)
   (val expression?)])

(define symbol-or-ref?
  (lambda (v)
    (or (symbol? v) (ref? v))))

(define ref?
  (lambda (expr)
    (and
     (list? expr)
     (not (null? (cdr expr)))
     (list? (cdr expr))
     (eqv? (car expr) 'ref)
     (symbol? (cadr expr)))))

(define literal?
  (lambda (object)
    (or (number? object)
	(string? object)
	(symbol? object)
	(boolean? object)
	(vector? object)
	(pair? object)
	(null? object))))

(define let-expression-args-list?
  (lambda (lst)
    (or
     (null? lst)
     (and
      (list? lst)
      (andmap list? lst)
      (andmap (lambda (lst-item) (and (not (null? (cdr lst-item))) (null? (cddr lst-item)))) lst)
      (andmap (lambda (arg-val) (symbol-or-ref? (car arg-val))) lst)))))

(define improper-list-of
  (lambda (pred)
    (letrec ([pairandmap
	      (lambda (pred pair)
		(cond
		 [(pair? pair) (and (pred (car pair)) (pairandmap pred (cdr pair)))]
		 [else (pred pair)]))])
      (lambda (object)
	(and (pair? object) (pairandmap pred object))))))


;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define environment?
  (lambda (obj)
    (or
     (null? obj) ; An empty list is an environment
     (and
      (pair? obj)
      (pair? (car obj))
      ((list-of symbol?) (caar obj))
      (vector? (cdar obj))
      (environment? (cdr obj))))))

(define reference?
  (lambda (obj)
    (and
     (pair? obj)
     (vector? (car obj))
     (integer? (cdr obj)))))

	
; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
  [closure-const-args
   (args (list-of symbol?))
   (refs (list-of boolean?))
   (bodies (list-of expression?))
   (env environment?)]
  [closure-const-var-args
   (const-args (list-of symbol?))
   (refs (list-of boolean?))
   (var-args symbol?)
   (bodies (list-of expression?))
   (env environment?)]
  [closure-var-args
   (args symbol?)
   (bodies (list-of expression?))
   (env environment?)])

(define prim-proc?
  (lambda (proc)
    (if (not (proc-val? proc))
	#f
	(cases proc-val proc
	  [prim-proc (name) #t]
	  [else #f]))))

(define get-refs
  (lambda (proc)
    (cases proc-val proc
      [prim-proc (name) (eopl:error 'get-refs "Called with a prim-proc")]
      [closure-const-args (args refs bodies env) refs]
      [closure-const-var-args (const-args refs var-args bodies env) refs]
      [closure-var-args (args bodies env) '(#f)])))
	 
(define-datatype continuation continuation?
  [id-k] ;This is (lambda (v) v)
  [error-k (error-msg-and-arguments (lambda (v) (not (null? v))))] ;This is (eopl:error args)
  [apply-env-k
	(env environment?)
	(id symbol?)
	(succeed continuation?)
	(fail continuation?)]
  [deref-k (next-cont continuation?)]
  [eval-k-noargs 
	(exp expression?)
	(env environment?)
	(k continuation?)]
  [if-k 
	(true expression?)
	(false expression?)
	(env environment?)
	(k continuation?)]
  [if-true-k 
	(true expression?)
	(env environment?)
	(k continuation?)]
  [set-ref!-k
	(sym symbol?)
	(k continuation?)]
  [proc-ref-k
    (rands (list-of expression?))
	(env environment?)
	(k continuation?)]
  [eval-rands-k
	(rands (list-of expression?))
	(env environment?)
	(k continuation?)]
  [apply-proc-k
	(val prim-proc?)
	(k continuation?)]
  [apply-proc-newproc-k
	(args list?)
	(k continuation?)]
  [replace-proc-refs-k
    (proc proc-val?)
    (k continuation?)]
  [andmap-rest-k
    (pred-cps procedure?)
    (rest-ls list?)
    (k continuation?)]
  [ormap-rest-k
    (pred-cps procedure?)
    (rest-ls list?)
    (k continuation?)]
  [map-cps-all-lists-null?-k
    (proc-cps procedure?)
    (lss (list-of list?))
    (k continuation?)]
  [map-cps-some-list-null?-k
    (proc-cps procedure?)
    (lss (list-of list?))
    (k continuation?)]
  [map-cps-rest-k
    (proc-cps procedure?)
    (lss (list-of list?))
    (k continuation?)]
  [map-cps-consing-k
    (proced-car scheme-value?)
    (k continuation?)]
  [fold-left-cps-all-lists-null?-k
    (proc-cps procedure?)
    (init scheme-value?)
    (argss (list-of list?))
    (k continuation?)]
  [fold-left-cps-map-k
    (proc-cps procedure?)
    (init scheme-value?)
    (argss (list-of list?))
    (k continuation?)]
  [fold-left-cps-proc-k
    (proc-cps procedure?)
    (argss (list-of list?))
    (k continuation?)]
  [fold-left-cps-rest-k
    (proc-cps procedure?)
    (proced-car scheme-value?)
    (k continuation?)]
  [replace-closure-const-args-bodies-k
    (args (list-of symbol?))
    (refs (list-of boolean?))
    (env environment?)
	(k continuation?)]
  [set!-k
	(env environment?)
	(id symbol?)]
  [set!-val-k
	(val (lambda (v) (not (null? v))))
	(k continuation?)]
  [replace-closure-const-var-args-bodies-k
    (args (list-of symbol?))
	(refs (list-of symbol?))
	(var symbol?)
	(env environment?)
	(k continuation?)]
  [replace-closure-var-args-k
    (arg symbol?)
	(env environment?)
	(k continuation?)]
  )
	
(define apply-k
  (lambda (k val)
;	(begin (display k) (display (newline))
    (cases continuation k
	  [id-k () val]
	  [eval-k-noargs (exp env k)
		(eval-exp exp env k)]
	  [error-k (error-msg-and-arguments) (apply eopl:error error-msg-and-arguments)]
	  [apply-env-k (env id succeed fail)
		(apply-env env id succeed fail)]
	  [deref-k (next-continuation) (deref val next-continuation)]
	  [if-k (true false env k)
		(if val
		  (eval-exp true env k)
		  (eval-exp false env k))]
	  [if-true-k (true env k)
		(if val
		  (eval-exp true env k)
		  (apply-k k (void)))]
	  [set-ref!-k (sym k) (set-ref! sym val k)]
	  [eval-rands-k (rands env k)
		(eval-rands rands val env k)]
	  [replace-proc-refs-k (proc k)
		(replace-proc-refs proc val (apply-proc-newproc-k val k))]
	  [set!-val-k (arg k) (set-ref! val arg k)]
	  [set!-k (arg k)
		(apply-env-ref env arg (set!-val-k val k) 
		(apply-env-k global-env arg (set!-val-k val k) 
		(error-k (list 'apply-env-ref "variable not found in environment: ~s" var))))]
	  [apply-proc-k (proc k)
		(apply-proc proc val k)]
	  [apply-proc-newproc-k (args k)
		(apply-proc val args k)]
	  [proc-ref-k (rands env k)
	    (if (prim-proc? val)
		(map-cps (lambda (arg k) (apply-k k #f))
			 (list rands) 
			 (eval-rands-k rands env (apply-proc-k val k)))
		(eval-rands rands (get-refs val) env (replace-proc-refs-k val k)))]
	  [andmap-rest-k (pred-cps rest-ls k)
	    (if val
		(andmap-cps pred-cps rest-ls k)
		(apply-k k #f))]
	  [ormap-rest-k (pred-cps rest-ls k)
	    (if val
		(apply-k k #t)
		(ormap-cps pred-cps rest-ls k))]
	  [map-cps-all-lists-null?-k (proc-cps lss k)
	    (if val
		(apply-k k '())
		(ormap-cps (make-cps null?) lss (map-cps-some-list-null?-k proc-cps lss k)))]
	  [map-cps-some-list-null?-k (proc-cps lss k)
	    (if val
		(eopl:error 'map "Lists differ in length")
		(proc-cps (map car lss) (map-cps-rest-k proc-cps lss k)))]
	  [map-cps-rest-k (proc-cps lss k)
	    (map-cps proc-cps (map cdr lss) (map-cps-consing-k val k))]
	  [map-cps-consing-k (proced-car k)
	    (apply-k k (cons proced-car val))]
	  [fold-left-cps-all-lists-null?-k (proc-cps init argss k)
	    (if val
		(apply-k k init)
		(map-cps (make-cps caar) (list argss) (fold-left-cps-map-k proc-cps init argss k)))]
	  [fold-left-cps-map-k (proc-cps init argss k)
	    (proc-cps init val (fold-left-cps-proc-k proc-cps argss k))]
	  [fold-left-cps-proc-k (proc-cps argss k)
	    (map-cps (make-cps cdar) (list argss) (fold-left-cps-rest-k proc-cps val k))]
	  [fold-left-cps-rest-k (proc-cps proced-car k)
	    (fold-left-cps proc-cps proced-car val k)]
	  [replace-closure-const-args-bodies-k (args refs env k)
	    (apply-k k (closure-const-args args refs val env))]
	  [replace-closure-const-var-args-bodies-k (const ref var env k)
	    (apply-k k (closure-const-var-args const ref var val env))]
	  [replace-closure-var-args-k (arg env k)
	    (apply-k k (closure-var-args arg val env))]
	)))
  
;(define-datatype environment environment?
;  (empty-env-record)
;  (extended-env-record
;   (syms (list-of symbol?))
;   (vals (list-of scheme-value?))
;   (env environment?)))
