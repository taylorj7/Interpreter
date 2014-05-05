
;; Parsed expression datatypes

(define-datatype expression expression? 
  [var-exp
   (id symbol?)]
  [lit-exp
   (id literal?)]
  [lambda-const-args-exp
   (id (list-of symbol?))
   (body (list-of expression?))]
  [lambda-const-var-args-exp
   (const-id (list-of symbol?))
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
   (exps (list-of expression?))
   (body (list-of expression?))]
  [named-let-exp
   (name symbol?)
   (vars (list-of symbol?))
   (exps (list-of expression?))
   (body (list-of expression?))]
  [let*-exp
   (vars (list-of symbol?))
   (exps (list-of expression?))
   (body (list-of expression?))]
  [letrec-exp
   (vars (list-of symbol?))
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
   (cond-elses (list-of expression?))])

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
      (andmap (lambda (arg-val) (symbol? (car arg-val))) lst)))))

(define improper-list-of
  (lambda (pred)
    (letrec ([pairandmap
	      (lambda (pred pair)
		(cond
		 [(pair? pair) (and (pred (car pair)) (pairandmap pred (cdr pair)))]
		 [else (pred pair)]))])
      (lambda (object)
	(and (pair? object) (pairandmap pred object))))))

	
; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
  [closure-const-args
   (args (list-of symbol?))
   (bodies (list-of expression?))
   (env environment?)]
  [closure-const-var-args
   (const-args (list-of symbol?))
   (var-args symbol?)
   (bodies (list-of expression?))
   (env environment?)]
  [closure-var-args
   (args symbol?)
   (bodies (list-of expression?))
   (env environment?)])
	 
	 
	 
	
;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?)))
