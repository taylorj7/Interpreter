
;; Parsed expression datatypes

;(define-datatype expression expression? 
;  [var-exp
;   (id symbol?)]
;  [lit-exp
;   (id literal?)]
;  [lambda-const-args-exp
;   (id (list-of symbol?))
;   (refs (list-of boolean?))
;   (body (list-of expression?))]
;  [lambda-const-var-args-exp
;   (const-id (list-of symbol?))
;   (refs (list-of boolean?))
;   (var-id symbol?)
;   (body (list-of expression?))]
;  [lambda-var-args-exp
;   (id symbol?)
;   (body (list-of expression?))]
;  [if-exp
;   (condition expression?)
;   (if-then expression?)
;   (if-else expression?)]
;  [if-true-exp
;   (condition expression?)
;   (if-then expression?)]
;  [let-exp
;   (vars (list-of symbol?))
;   (refs (list-of boolean?))
;   (exps (list-of expression?))
;   (body (list-of expression?))]
;  [named-let-exp
;   (name symbol?)
;   (vars (list-of symbol?))
;   (refs (list-of boolean?))
;   (exps (list-of expression?))
;   (body (list-of expression?))]
;  [let*-exp
;   (vars (list-of symbol?))
;   (refs (list-of boolean?))
;   (exps (list-of expression?))
;   (body (list-of expression?))]
;  [letrec-exp
;   (vars (list-of symbol?))
;   (refs (list-of boolean?))
;   (exps (list-of expression?))
;   (body (list-of expression?))]
;  [set!-exp
;   (var symbol?)
;   (val expression?)]
;  [app-exp
;   (operator expression?)
;   (operands (list-of expression?))]
;  [begin-exp
;   (bodies (list-of expression?))]
;  [cond-exp
;   (conditions (list-of expression?))
;   (if-thenss (list-of (list-of expression?)))]
;  [cond-else-exp
;   (conditions (list-of expression?))
;   (if-thenss (list-of (list-of expression?)))
;   (cond-elses (list-of expression?))]
;  [and-exp
;   (bools (list-of expression?))]
;  [or-exp
;   (bools (list-of expression?))]
;  [case-exp
;   (id expression?)
;   (keyss (list-of (list-of expression?)))
;   (exprss (list-of (list-of expression?)))]
;  [case-else-exp
;   (id expression?)
;   (keyss (list-of (list-of expression?)))
;   (exprss (list-of (list-of expression?)))
;   (case-elses (list-of expression?))]
;  [while-exp
;   (condition expression?)
;   (bodies (list-of expression?))]
;  [define-exp
;    (name symbol?)
;    (val expression?)]
;  [set!-exp-ref
;   (ref reference?)
;   (val expression?)])

(define expression?
  (lambda (expr)
    (and (list? expr)
	 (case (car expr)
	   [(var-exp lit-exp lambda-const-args-exp lambda-const-var-args-exp lambda-var-args-exp if-exp
	     if-true-exp let-exp named-let-exp let*-exp letrec-exp set!-exp app-exp begin-exp cond-exp
	     cond-else-exp and-exp or-exp case-exp case-else-exp while-exp define-exp set!-exp-ref) #t]
	   [else #f]))))

(define var-exp
  (lambda (var)
    (list 'var-exp var)))

(define lit-exp
  (lambda (lit)
    (list 'lit-exp lit)))

(define lambda-const-args-exp
  (lambda (vars refs bodies)
    (list 'lambda-const-args-exp vars refs bodies)))

(define lambda-const-var-args-exp
  (lambda (const-id refs var-id bodies)
    (list 'lambda-const-var-args-exp const-id refs var-id bodies)))

(define lambda-var-args-exp
  (lambda (id bodies)
    (list 'lambda-var-args-exp id bodies)))

(define if-exp
  (lambda (condition if-then if-else)
    (list 'if-exp condition if-then if-else)))

(define if-true-exp
  (lambda (condition if-then)
    (list 'if-true-exp condition if-then)))

(define let-exp
  (lambda (vars refs exps bodies)
    (list 'let-exp vars refs exps bodies)))

(define named-let-exp
  (lambda (name vars refs exps bodies)
    (list 'named-let-exp name vars refs exps bodies)))

(define let*-exp
  (lambda (vars refs exps bodies)
    (list 'let*-exp vars refs exps bodies)))

(define letrec-exp
  (lambda (vars refs exps bodies)
    (list 'letrec-exp vars refs exps bodies)))

(define set!-exp
  (lambda (var val)
    (list 'set!-exp var val)))

(define app-exp
  (lambda (rator rands)
    (list 'app-exp rator rands)))

(define begin-exp
  (lambda (bodies)
    (list 'begin-exp bodies)))

(define cond-exp
  (lambda (conditions if-thenss)
    (list 'cond-exp conditions if-thenss)))

(define cond-else-exp
  (lambda (conditions if-thenss cond-elses)
    (list 'cond-else-exp conditions if-thenss cond-elses)))

(define and-exp
  (lambda (bools)
    (list 'and-exp bools)))

(define or-exp
  (lambda (bools)
    (list 'or-exp bools)))

(define case-exp
  (lambda (id keyss exprss)
    (list 'case-exp id keyss exprss)))

(define case-else-exp
  (lambda (id keyss exprss case-elses)
    (list 'case-else-exp id keyss exprss case-elses)))

(define while-exp
  (lambda (condition bodies)
    (list 'while-exp condition bodies)))

(define define-exp
  (lambda (name val)
    (list 'define-exp name val)))

(define set!-exp-ref
  (lambda (ref val)
    (list 'set!-exp-ref ref val)))

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

;(define-datatype proc-val proc-val?
;  [prim-proc
;   (name symbol?)]
;  [closure-const-args
;   (args (list-of symbol?))
;   (refs (list-of boolean?))
;   (bodies (list-of expression?))
;   (env environment?)]
;  [closure-const-var-args
;   (const-args (list-of symbol?))
;   (refs (list-of boolean?))
;   (var-args symbol?)
;   (bodies (list-of expression?))
;   (env environment?)]
;  [closure-var-args
;   (args symbol?)
;   (bodies (list-of expression?))
;   (env environment?)]
;  [cont-proc (k continuation?)])

(define proc-val?
  (lambda (expr)
    (and (list? expr)
	 (case (car expr)
	   [(prim-proc closure-const-args closure-const-var-args closure-var-args cont-proc) #t]
	   [else #f]))))

(define prim-proc
  (lambda (name)
    (list 'prim-proc name)))

(define closure-const-args
  (lambda (args refs bodies env)
    (list 'closure-const-args args refs bodies env)))

(define closure-const-var-args
  (lambda (const-args refs var-args bodies env)
    (list 'closure-const-var-args const-args refs var-args bodies env)))

(define closure-var-args
  (lambda (args bodies env)
    (list 'closure-var-args args bodies env)))

(define cont-proc
  (lambda (k)
    (list 'cont-proc k)))

(define prim-proc?
  (lambda (proc)
    (if (not (proc-val? proc))
	#f
	(case (car proc)
	  [(prim-proc) #t]
	  [else #f]))))

(define closure-has-var-args?
  (lambda (proc)
    (if (not (proc-val? proc))
	#f
	(case (car proc)
	  [(closure-var-args closure-const-var-args) #t]
	  [else #f]))))

(define get-refs
  (lambda (proc)
    (case (car proc)
      [(prim-proc) (eopl:error 'get-refs "Called with a prim-proc")]
      [(closure-const-args) (caddr proc)]
      [(closure-const-var-args) (caddr proc)]
      [else '(#f)])))
	 
;(define-datatype continuation continuation?
;  [id-k] ;This is (lambda (v) v)
;  [error-k (error-msg-and-arguments (lambda (v) (not (null? v))))] ;This is (eopl:error args)
;  [apply-env-k
;	(env environment?)
;	(id symbol?)
;	(succeed continuation?)
;	(fail continuation?)]
 ; [apply-env-ref-k
  ;  (env environment?)
   ; (id symbol?)
   ; (succeed continuation?)
   ; (fail continuation?)]
;  [deref-k (next-cont continuation?)]
;  [eval-k-noargs 
;	(exp expression?)
;	(env environment?)
;	(k continuation?)]
;  [if-k 
;	(true expression?)
;	(false expression?)
;	(env environment?)
;	(k continuation?)]
;  [if-true-k 
;	(true expression?)
;	(env environment?)
;	(k continuation?)]
;  [set-ref!-k
;    (ref reference?)
;    (k continuation?)]
;  [proc-ref-k
;    (rands (list-of expression?))
;	(env environment?)
;	(k continuation?)]
;  [eval-rands-k
;	(rands (list-of expression?))
;	(env environment?)
;	(k continuation?)]
;  [apply-proc-k
;	(val prim-proc?)
;	(k continuation?)]
;  [apply-proc-newproc-k
;	(args list?)
;	(k continuation?)]
;  [replace-proc-refs-k
;    (proc proc-val?)
;    (k continuation?)]
;  [andmap-rest-k
;    (pred-cps procedure?)
;    (rest-ls list?)
;    (k continuation?)]
;  [ormap-rest-k
;    (pred-cps procedure?)
;    (rest-ls list?)
;    (k continuation?)]
;  [map-cps-all-lists-null?-k
;    (proc-cps procedure?)
;    (lss (list-of list?))
;    (k continuation?)]
;  [map-cps-some-list-null?-k
;    (proc-cps procedure?)
;    (lss (list-of list?))
;    (k continuation?)]
;  [map-cps-rest-k
;    (proc-cps procedure?)
;    (lss (list-of list?))
;    (k continuation?)]
;  [map-cps-consing-k
;    (proced-car scheme-value?)
;    (k continuation?)]
;  [fold-left-cps-all-lists-null?-k
;    (proc-cps procedure?)
;    (init scheme-value?)
;    (argss (list-of list?))
;    (k continuation?)]
;  [fold-left-cps-map-k
;    (proc-cps procedure?)
;    (init scheme-value?)
;    (argss (list-of list?))
;    (k continuation?)]
;  [fold-left-cps-proc-k
;    (proc-cps procedure?)
;    (argss (list-of list?))
;    (k continuation?)]
;  [fold-left-cps-rest-k
;    (proc-cps procedure?)
;    (proced-car scheme-value?)
;    (k continuation?)]
;  [replace-closure-const-args-bodies-k
;    (args (list-of symbol?))
;    (refs (list-of boolean?))
;    (env environment?)
;	(k continuation?)]
;  [set!-k
;	(env environment?)
;	(var symbol?)
;	(k continuation?)]
;  [set!-val-k
;	(val scheme-value?)
;	(k continuation?)]
;  [replace-closure-const-var-args-bodies-k
;    (args (list-of symbol?))
;    (refs (list-of boolean?))
;    (var symbol?)
;    (env environment?)
;    (k continuation?)]
;  [replace-closure-var-args-k
;    (arg symbol?)
;	(env environment?)
;	(k continuation?)]
;  [multi-body-k
;	(bodies (list-of expression?))
;	(env environment?)
;	(k continuation?)]
;  [define-eval-succeed-k
;    (value expression?)
;    (k continuation?)]
;  [define-eval-fail-k
;    (sym symbol?)
;    (value expression?)
;    (k continuation?)]
;  [add-to-global-env-k
;   (sym symbol?)
;   (k continuation?)]
;  [replace-free-refs-const-args-member?-k
;    (expr expression?)
;    (arg symbol?)
;    (refarg reference?)
;    (vars (list-of symbol?))
;    (refs (list-of boolean?))
;    (bodies (list-of expression?))
;    (k continuation?)]
;  [replace-free-refs-replace-const-args-bodies-k
;    (vars (list-of symbol?))
;    (refs (list-of boolean?))
;    (k continuation?)]
;  [replace-free-refs-const-var-args-member?-k
;   (expr expression?)
;   (arg symbol?)
;   (refarg reference?)
;   (const-id (list-of symbol?))
;   (refs (list-of boolean?))
;   (var-id symbol?)
;   (bodies (list-of expression?))
;   (k continuation?)]
;  [replace-free-refs-replace-const-var-args-bodies-k
;   (const-id (list-of symbol?))
;   (refs (list-of boolean?))
;   (var-id symbol?)
;   (k continuation?)]
;  [replace-free-refs-replace-var-args-bodies-k
;   (id symbol?)
;   (k continuation?)]
;  [replace-free-refs-if-replace-condition-k
;   (if-then expression?)
;   (if-else expression?)
;   (arg symbol?)
;   (refarg reference?)
;   (k continuation?)]
;  [replace-free-refs-if-replace-if-then-k
;   (repd-condition expression?)
;   (if-else expression?)
;   (arg symbol?)
;   (refarg reference?)
;   (k continuation?)]
;  [replace-free-refs-if-replace-if-else-k
;   (repd-condition expression?)
;   (repd-if-then expression?)
;   (k continuation?)]
;  [replace-free-refs-if-true-replace-condition-k
;   (if-then expression?)
;   (arg symbol?)
;   (refarg reference?)
;   (k continuation?)]
;  [replace-free-refs-if-true-replace-if-then-k
;   (repd-condition expression?)
;   (k continuation?)]
;  [replace-free-refs-app-replace-rator-k
;   (rands (list-of expression?))
;   (arg symbol?)
;   (refarg reference?)
;   (k continuation?)]
;  [replace-free-refs-app-replace-rands-k
;   (repd-rator expression?)
;   (k continuation?)]
;  [replace-free-refs-set-exp-replace-val-k
;   (var symbol?)
;   (arg symbol?)
;   (refarg reference?)
;   (k continuation?)]
;  [replace-free-refs-set-exp-ref-replace-val-k
;   (ref reference?)
;   (k continuation?)]
;  [replace-free-refs-make-lit-exp-k
;   (k continuation?)]
;  [call/cc-k
;   (k continuation?)]
;  )

(define continuation?
  (lambda (cont)
    (and (list? cont)
	 (case (car cont)
	   [(id-k error-k apply-env-k apply-env-ref-k deref-k eval-k-noargs if-k if-true-k set-ref!-k proc-ref-k
	     eval-rands-k apply-proc-k apply-proc-newproc-k replace-proc-refs-k andmap-rest-k ormap-rest-k
	     map-cps-all-lists-null?-k map-cps-some-list-null?-k map-cps-rest-k map-cps-consing-k
	     fold-left-cps-all-lists-null?-k fold-left-cps-map-k fold-left-cps-proc-k fold-left-cps-rest-k
	     replace-closure-const-args-bodies-k set!-k set!-val-k replace-closure-const-var-args-bodies-k
	     replace-closure-var-args-k multi-body-k define-eval-succeed-k define-eval-fail-k add-to-global-env-k
	     replace-free-refs-const-args-member?-k replace-free-refs-replace-const-args-bodies-k
	     replace-free-refs-const-var-args-member?-k replace-free-refs-replace-const-var-args-bodies-k
	     replace-free-refs-replace-var-args-bodies-k replace-free-refs-if-replace-condition-k
	     replace-free-refs-if-replace-if-then-k replace-free-refs-if-replace-if-else-k 
	     replace-free-refs-if-true-replace-condition-k replace-free-refs-if-true-replace-if-then-k
	     replace-free-refs-app-replace-rator-k replace-free-refs-app-replace-rands-k
	     replace-free-refs-set-exp-replace-val-k replace-free-refs-set-exp-ref-replace-val-k
	     replace-free-refs-make-lit-exp-k call/cc-k append-rest-k apply-proc-append-k apply-proc-get-x-k
	     get-x-rest-k) #t]
	   [else #f]))))

(define id-k
  (lambda ()
    (list 'id-k)))

(define error-k
  (lambda (error-msg-and-arguments)
    (list 'error-k error-msg-and-arguments)))

(define apply-env-k
  (lambda (env id succeed fail)
    (list 'apply-env-k env id succeed fail)))

(define apply-env-ref-k
  (lambda (env id succeed fail)
    (list 'apply-env-ref-k env id succeed fail)))

(define deref-k
  (lambda (next-cont)
    (list 'deref-k next-cont)))

(define eval-k
  (lambda (exp env k)
    (list 'eval-k-noargs exp env k)))

(define if-k
  (lambda (true false env k)
    (list 'if-k true false env k)))

(define if-true-k
  (lambda (true env k)
    (list 'if-true-k true env k)))

(define set-ref!-k
  (lambda (ref k)
    (list 'set-ref!-k ref k)))

(define proc-ref-k
  (lambda (rands env k)
    (list 'proc-ref-k rands env k)))

(define eval-rands-k
  (lambda (rands env k)
    (list 'eval-rands-k rands env k)))

(define apply-proc-k
  (lambda (val k)
    (list 'apply-proc-k val k)))

(define apply-proc-newproc-k
  (lambda (args k)
    (list 'apply-proc-newproc-k args k)))

(define replace-proc-refs-k
  (lambda (proc k)
    (list 'replace-proc-refs-k proc k)))

(define andmap-rest-k
  (lambda (pred-cps rest-ls k)
    (list 'andmap-rest-k pred-cps rest-ls k)))

(define ormap-rest-k
  (lambda (pred-cps rest-ls k)
    (list 'ormap-rest-k pred-cps rest-ls k)))

(define map-cps-all-lists-null?-k
  (lambda (proc-cps lss k)
    (list 'map-cps-all-lists-null?-k proc-cps lss k)))

(define map-cps-some-list-null?-k
  (lambda (proc-cps lss k)
    (list 'map-cps-some-list-null?-k proc-cps lss k)))

(define map-cps-rest-k
  (lambda (proc-cps lss k)
    (list 'map-cps-rest-k proc-cps lss k)))

(define map-cps-consing-k
  (lambda (proced-car k)
    (list 'map-cps-consing-k proced-car k)))

(define fold-left-cps-all-lists-null?-k
  (lambda (proc-cps init argss k)
    (list 'fold-left-cps-all-lists-null?-k proc-cps init argss k)))

(define fold-left-cps-map-k
  (lambda (proc-cps init argss k)
    (list 'fold-left-cps-map-k proc-cps init argss k)))

(define fold-left-cps-proc-k
  (lambda (proc-cps argss k)
    (list 'fold-left-cps-proc-k proc-cps argss k)))

(define fold-left-cps-rest-k
  (lambda (proc-cps proced-car k)
    (list 'fold-left-cps-rest-k proc-cps proced-car k)))

(define replace-closure-const-args-bodies-k
  (lambda (args refs env k)
    (list 'replace-closure-const-args-bodies-k args refs env k)))

(define set!-k
  (lambda (env var k)
    (list 'set!-k env var k)))

(define set!-val-k
  (lambda (val k)
    (list 'set!-val-k val k)))

(define replace-closure-const-var-args-bodies-k
  (lambda (args refs var env k)
    (list 'replace-closure-const-var-args-bodies-k args refs var env k)))

(define replace-closure-var-args-k
  (lambda (arg env k)
    (list 'replace-closure-var-args-k arg env k)))

(define multi-body-k
  (lambda (bodies env k)
    (list 'multi-body-k bodies env k)))

(define define-eval-succeed-k
  (lambda (value k)
    (list 'define-eval-succeed-k value k)))

(define define-eval-fail-k
  (lambda (sym value k)
    (list 'define-eval-fail-k sym value k)))

(define add-to-global-env-k
  (lambda (sym k)
    (list 'add-to-global-env-k sym k)))

(define replace-free-refs-const-args-member?-k
  (lambda (expr arg refarg vars refs bodies k)
    (list 'replace-free-refs-const-args-member?-k expr arg refarg vars refs bodies k)))

(define replace-free-refs-replace-const-args-bodies-k
  (lambda (vars refs k)
    (list 'replace-free-refs-replace-const-args-bodies-k vars refs k)))

(define replace-free-refs-const-var-args-member?-k
  (lambda (expr arg refarg const-id refs var-id bodies k)
    (list 'replace-free-refs-const-var-args-member?-k expr arg refarg const-id refs var-id bodies k)))

(define replace-free-refs-replace-const-var-args-bodies-k
  (lambda (const-id refs var-id k)
    (list 'replace-free-refs-replace-const-var-args-bodies-k const-id refs var-id k)))

(define replace-free-refs-replace-var-args-bodies-k
  (lambda (id k)
    (list 'replace-free-refs-replace-var-args-bodies-k id k)))

(define replace-free-refs-if-replace-condition-k
  (lambda (if-then if-else arg refarg k)
    (list 'replace-free-refs-if-replace-condition-k if-then if-else arg refarg k)))

(define replace-free-refs-if-replace-if-then-k
  (lambda (repd-condition if-else arg refarg k)
    (list 'replace-free-refs-if-replace-if-then-k repd-condition if-else arg refarg )))

(define replace-free-refs-if-replace-if-else-k
  (lambda (repd-condition repd-if-then k)
    (list 'replace-free-refs-if-replace-if-else-k repd-condition repd-if-then k)))

(define replace-free-refs-if-true-replace-condition-k
  (lambda (if-then arg refarg k)
    (list 'replace-free-refs-if-true-replace-condition-k if-then arg refarg k)))

(define replace-free-refs-if-true-replace-if-then-k
  (lambda (repd-condition k)
    (list 'replace-free-refs-if-true-replace-if-then-k repd-condition k)))

(define replace-free-refs-app-replace-rator-k
  (lambda (rands arg refarg k)
    (list 'replace-free-refs-app-replace-rator-k rands arg refarg k)))

(define replace-free-refs-app-replace-rands-k
  (lambda (repd-rator k)
    (list 'replace-free-refs-app-replace-rands-k repd-rator k)))

(define replace-free-refs-set-exp-replace-val-k
  (lambda (var arg refarg k)
    (list 'replace-free-refs-set-exp-replace-val-k var arg refarg k)))

(define replace-free-refs-set-exp-ref-replace-val-k
  (lambda (ref k)
    (list 'replace-free-refs-set-exp-ref-replace-val-k ref k)))

(define replace-free-refs-make-lit-exp-k
  (lambda (k)
    (list 'replace-free-refs-make-lit-exp-k k)))

(define call/cc-k
  (lambda (k)
    (list 'call/cc-k k)))

(define append-rest-k
  (lambda (l1 k)
    (list 'append-rest-k l1 k)))

(define apply-proc-append-k
  (lambda (args const-args bodies env k)
    (list 'apply-proc-append-k args const-args bodies env k)))

(define apply-proc-get-x-k
  (lambda (appended env bodies k)
    (list 'apply-proc-get-x-k appended env bodies k)))

(define get-x-rest-k
  (lambda (ls k)
    (list 'get-x-rest-k ls k)))
	
(define apply-k
  (lambda (k val)
;	(begin (display k) (display (newline))
    (case (car k)
      [(id-k)
       val]
      [(eval-k-noargs)
       (eval-exp (cadr k) (caddr k) (caddr (cdr k)))]
      [(error-k)
       (apply eopl:error (cadr k))]
      [(apply-env-k)
       (apply-env (cadr k) (caddr k) (caddr (cdr k)) (caddr (cddr k)))]
      [(apply-env-ref-k)
       (apply-env-ref (cadr k) (caddr k) (caddr (cdr k)) (caddr (cddr k)))]
      [(deref-k)
       (deref val (cadr k))]
      [(if-k)
       (if val
	   (eval-exp (cadr k) (caddr (cdr k)) (caddr (cddr k)))
	   (eval-exp (caddr k) (caddr (cdr k)) (caddr (cddr k))))]
      [(if-true-k)
       (if val
	   (eval-exp (cadr k) (caddr k) (caddr (cdr k)))
	   (apply-k (caddr (cdr k)) (void)))]
      [(set-ref!-k)
       (set-ref! (cadr k) val (caddr k))]
      [(eval-rands-k)
       (eval-rands (cadr k) val (caddr k) (caddr (cdr k)))]
      [(replace-proc-refs-k)
       (replace-proc-refs (cadr k) val (apply-proc-newproc-k val (caddr k)))]
      [(set!-val-k)
       (set-ref! val (cadr k) (caddr k))]
      [(set!-k)
       (apply-env-ref (cadr k)
		      (caddr k)
		      (set!-val-k val (caddr (cdr k))) 
		      (apply-env-ref-k global-env
				       (caddr k)
				       (set!-val-k val (caddr (cdr k))) 
				       (error-k (list 'apply-env-ref "variable not found in environment: ~s" (caddr k)))))]
      [(apply-proc-k)
       (apply-proc (cadr k) val (caddr k))]
      [(apply-proc-newproc-k)
       (apply-proc val (cadr k) (caddr k))]
      [(proc-ref-k)
       (if (prim-proc? val)
	   (map-cps (lambda (arg k) (apply-k k #f))
		    (list (cadr k)) 
		    (eval-rands-k (cadr k) (caddr k) (apply-proc-k val (caddr (cdr k)))))
	   (if (closure-has-var-args? val)
	       (map-cps (lambda (arg k) (apply-k k #f))
			(list (cadr k))
			(eval-rands-k (cadr k) (caddr k) (replace-proc-refs-k val (caddr (cdr k)))))
	       (eval-rands (cadr k) (get-refs val) (caddr k) (replace-proc-refs-k val (caddr (cdr k))))))]
      [(andmap-rest-k)
       (if val
	   (andmap-cps (cadr k) (caddr k) (caddr (cdr k)))
	   (apply-k (caddr (cdr k)) #f))]
      [(ormap-rest-k)
       (if val
	   (apply-k (caddr (cdr k)) #t)
	   (ormap-cps (cadr k) (caddr k) (caddr (cdr k))))]
      [(map-cps-all-lists-null?-k)
       (if val
	   (apply-k (caddr (cdr k)) '())
	   (ormap-cps (make-cps null?) (caddr k) (map-cps-some-list-null?-k (cadr k) (caddr k) (caddr (cdr k)))))]
      [(map-cps-some-list-null?-k)
       (if val
	   (eopl:error 'map "Lists differ in length")
	   ((cadr k) (map car (caddr k)) (map-cps-rest-k (cadr k) (caddr k) (caddr (cdr k)))))]
      [(map-cps-rest-k)
       (map-cps (cadr k)
		(map cdr (caddr k))
		(map-cps-consing-k val (caddr (cdr k))))]
      [(map-cps-consing-k)
       (apply-k (caddr k) (cons (cadr k) val))]
      [(fold-left-cps-all-lists-null?-k)
       (if val
	   (apply-k (caddr (cddr k)) (caddr k))
	   (map-cps (make-cps caar)
		    (list (caddr (cdr k)))
		    (fold-left-cps-map-k (cadr k) (caddr k) (caddr (cdr k)) (caddr (cddr k)))))]
      [(fold-left-cps-map-k)
       ((cadr k) (caddr k) val (fold-left-cps-proc-k (cadr k) (caddr (cdr k)) (caddr (cddr k))))]
      [(fold-left-cps-proc-k)
       (map-cps (make-cps cdar)
		(list (caddr k))
		(fold-left-cps-rest-k (cadr k) val (caddr (cdr k))))]
      [(fold-left-cps-rest-k)
       (fold-left-cps (cadr k) (caddr k) val (caddr (cdr k)))]
      [(replace-closure-const-args-bodies-k)
       (apply-k (caddr (cddr k)) (closure-const-args (cadr k) (caddr k) val (caddr (cdr k))))]
      [(replace-closure-const-var-args-bodies-k)
       (apply-k (caddr (cdddr k)) (closure-const-var-args (cadr k) (caddr k) (caddr (cdr k)) val (caddr (cddr k))))]
      [(replace-closure-var-args-k)
       (apply-k (caddr (cdr k)) (closure-var-args (cadr k) val (caddr k)))]
      [(multi-body-k)
       (eval-multiple-bodies (cadr k) (caddr k) (caddr (cdr k)))]
      [(define-eval-succeed-k)
	(eval-exp (cadr k) global-env (set-ref!-k val (caddr k)))]
      [(define-eval-fail-k)
	(eval-exp (caddr k) global-env (add-to-global-env-k (cadr k) (caddr (cdr k))))]
      [(add-to-global-env-k)
       (set-cdr! (car global-env) (vector-add-left (cdar global-env) val))
       (apply-k (caddr k) (set-car! (car global-env) (cons (cadr k) (caar global-env))))]
      [(replace-free-refs-const-args-member?-k)
       (if val
	   (apply-k (caddr (cdddr (cddr k))) (cadr k))
	   (map-cps (lambda (loob kont) (replace-free-refs (car loob) (caddr k) (caddr (cdr k)) kont))
		    (list (caddr (cdddr (cdr k))))
		    (replace-free-refs-replace-const-args-bodies-k (caddr (cddr k)) (caddr (cdddr k)) (caddr (cdddr (cddr k))))))]
      [(replace-free-refs-replace-const-args-bodies-k)
       (apply-k (caddr (cdr k)) (lambda-const-args-exp (cadr k) (caddr k) val))]
      [(replace-free-refs-const-var-args-member?-k)
       (if val
	   (apply-k (caddr (cdddr (cdddr k))) (cadr k))
	   (map-cps (lambda (loob kont) (replace-free-refs (car loob) (caddr k) (caddr (cdr k)) kont))
		    (list (caddr (cdddr (cddr k))))
		    (replace-free-refs-replace-const-var-args-bodies-k (caddr (cddr k)) (caddr (cdddr k)) (caddr (cdddr (cdr k))) (caddr (cdddr (cdddr k))))))]
      [(replace-free-refs-replace-const-var-args-bodies-k)
       (apply-k (caddr (cddr k)) (lambda-const-var-args-exp (cadr k) (caddr k) (caddr (cdr k)) val))]
      [(replace-free-refs-replace-var-args-bodies-k)
       (apply-k (caddr k) (lambda-var-args-exp (cadr k) val))]
      [(replace-free-refs-if-replace-condition-k)
       (replace-free-refs (cadr k) (caddr (cdr k)) (caddr (cddr k)) (replace-free-refs-if-replace-if-then-k val (caddr k) (caddr (cdr k)) (caddr (cddr k)) (caddr (cdddr k))))]
      [(replace-free-refs-if-replace-if-then-k)
       (replace-free-refs (caddr k) (caddr (cdr k)) (caddr (cddr k)) (replace-free-refs-if-replace-if-else-k (cadr k) val (caddr (cdddr k))))]
      [(replace-free-refs-if-replace-if-else-k)
       (apply-k (caddr (cdr k)) (if-exp (cadr k)
					(caddr k)
					val))]
      [(replace-free-refs-if-true-replace-condition-k)
       (replace-free-refs (cadr k) (caddr k) (caddr (cdr k)) (replace-free-refs-if-then-replace-if-then-k val (caddr (cddr k))))]
      [(replace-free-refs-if-true-replace-if-then-k)
       (apply-k (caddr k) (if-true-exp (cadr k)
				       val))]
      [(replace-free-refs-app-replace-rator-k)
       (map-cps (lambda (loor kont) (replace-free-refs (car loor) (caddr k) (caddr (cdr k)) kont))
		(list (cadr k))
		(replace-free-refs-app-replace-rands-k val (caddr (cddr k))))]
      [(replace-free-refs-app-replace-rands-k)
       (apply-k (caddr k) (app-exp (cadr k) val))]
      [(replace-free-refs-set-exp-replace-val-k)
       (if (eqv? (caddr k) (cadr k))
	   (apply-k (caddr (cddr k)) (set!-exp-ref (caddr (cdr k)) val))
	   (apply-k (caddr (cddr k)) (set!-exp (cadr k) val)))]
      [(replace-free-refs-set-exp-ref-replace-val-k)
       (apply-k (caddr k) (set!-exp-ref (cadr k) val))]
      [(replace-free-refs-make-lit-exp-k)
       (apply-k (cadr k) (lit-exp val))]
      [(call/cc-k)
       (apply-proc val (list (cont-proc (cadr k))) (cadr k))]
      [(append-rest-k)
       (apply-k (caddr k) (cons (car (cadr k)) val))]
      [(apply-proc-append-k)
       (get-x (cadr k) (length (caddr k)) (apply-proc-get-x-k val (caddr (cddr k)) (caddr (cdr k)) (caddr (cdddr k))))]
      [(apply-proc-get-x-k)
       (let ([extended-env (extend-env (cadr k) (list->vector val) (caddr k))])
	 (eval-multiple-bodies (caddr (cdr k)) extended-env (caddr (cddr k))))]
      [(get-x-rest-k)
       (append-cps (list (car (cadr k))) val (caddr k))])))
;	    (cases proc-val val
;		   [closure-const-args (vars refs bodies env)
;				       (eval-multiple-bodies bodies (extend-env vars (list->vector (list (cont-proc cont))) env) cont)]
;			[else (eopl:error 'call/cc "Error Closure is wrong ~s" val)])]
;	)))
  
;(define-datatype environment environment?
;  (empty-env-record)
;  (extended-env-record
;   (syms (list-of symbol?))
;   (vals (list-of scheme-value?))
;   (env environment?)))
