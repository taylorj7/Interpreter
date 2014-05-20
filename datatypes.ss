
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
  [init-k]
  [embedded-k 
    (cont)]
  [call-k
    (cont)])
	
(define apply-k
  (lambda (k call)
    (cases k
	  [init-k () call]
	  [embedded-k (cont) (cont call)]
	  [call-k (cont) (call cont)])))
  
;(define-datatype environment environment?
;  (empty-env-record)
;  (extended-env-record
;   (syms (list-of symbol?))
;   (vals (list-of scheme-value?))
;   (env environment?)))
