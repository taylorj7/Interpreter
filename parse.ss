; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(define get-const-lambda-args
  (lambda (ilos)
    (cond
     [(pair? (cdr ilos)) (cons (car ilos) (get-const-lambda-args (cdr ilos)))]
     [else (list (car ilos))])))

(define get-var-lambda-arg
  (lambda (ilos)
    (cond
     [(pair? (cdr ilos)) (get-var-lambda-arg (cdr ilos))]
     [else (cdr ilos)])))

(define remove-refs
  (lambda (losor)
    (cond
     [(null? losor) '()]
     [(ref? (car losor)) (cons (cadar losor) (remove-refs (cdr losor)))]
     [else (cons (car losor) (remove-refs (cdr losor)))])))

(define parse-exp
  (lambda (expr)
    (cond
     [(symbol? expr) (var-exp expr)]
     [(number? expr) (lit-exp expr)]
     [(vector? expr) (lit-exp expr)]
     [(boolean? expr) (lit-exp expr)]
     [(string? expr) (lit-exp expr)]
     [(not (pair? expr)) (eopl:error 'parse-exp "Incorrect syntax: ~s" expr)]
     [(eqv? (car expr) 'lambda)
      (cond
       [(null? (cdr expr)) (eopl:error 'parse-exp "lambda-expr without arguments or body: ~s" expr)]
       [(null? (cddr expr)) (eopl:error 'parse-exp "lambda-expr without body: ~s" expr)]
       [(list? (cadr expr)) (if ((list-of symbol-or-ref?) (cadr expr))
				(lambda-const-args-exp (remove-refs (cadr expr)) (map ref? (cadr expr)) (map parse-exp (cddr expr)))
				(eopl:error 'parse-exp "lambda-expr variables of incorrect type: ~s" expr))]
       [(pair? (cadr expr)) (if ((improper-list-of symbol-or-ref?) (cadr expr))
				(lambda-const-var-args-exp (remove-refs (get-const-lambda-args (cadr expr))) (map ref? (get-const-lambda-args (cadr expr))) (get-var-lambda-arg (cadr expr)) (map parse-exp (cddr expr)))
				(eopl:error 'parse-exp "lambda-expr variables of incorrect type: ~s" expr))]
       [(symbol? (cadr expr)) (lambda-var-args-exp (cadr expr) (map parse-exp (cddr expr)))])]
     [(eqv? (car expr) 'if)
      (cond
       [(null? (cdr expr)) (eopl:error 'parse-exp "if-statement without condition or statements: ~s" expr)]
       [(null? (cddr expr)) (eopl:error 'parse-exp "if-statement without statements: ~s" expr)]
       [(null? (cdddr expr)) (if-true-exp (parse-exp (cadr expr)) (parse-exp (caddr expr)))]
       [else (if-exp (parse-exp (cadr expr)) (parse-exp (caddr expr)) (parse-exp (caddr (cdr expr))))])]
     [(eqv? (car expr) 'let)
	  (cond
	   [(null? (cdr expr)) (eopl:error 'parse-exp "let-expr without variable list or body: ~s" expr)]
	   [(null? (cddr expr)) (eopl:error 'parse-exp "let-expr without body: ~s" expr)]
	   [(not (or (symbol? (cadr expr)) (let-expression-args-list? (cadr expr)))) 
	    (eopl:error 'parse-exp "let-expr variable list malformed: ~s" expr)]
	   [(symbol? (cadr expr))
	    (cond
	     [(null? (cdddr expr)) (eopl:error 'parse-exp "named let-expr without body: ~s" expr)]
	     [(not (let-expression-args-list? (caddr expr)))
		   (eopl:error 'parse-exp "named let-expr variable without body: ~s" expr)]
	     [else (named-let-exp (cadr expr)
				  (remove-refs (map car (caddr expr)))
				  (map ref? (map car (caddr expr)))
				  (map (lambda (arg-val) (parse-exp (cadr arg-val))) (caddr expr))
				  (map parse-exp (cdddr expr)))])]
	   [else (let-exp (remove-refs (map car (cadr expr)))
			  (map ref? (map car (cadr expr)))
			  (map (lambda (arg-val) (parse-exp (cadr arg-val))) (cadr expr))
			  (map parse-exp (cddr expr)))])]
     [(or (eqv? (car expr) 'let*) (eqv? (car expr) 'letrec))
       (cond
	[(null? (cdr expr)) (eopl:error 'parse-exp "let-expr without variable list or body: ~s" expr)]
	[(null? (cddr expr)) (eopl:error 'parse-exp "let-expr without body: ~s" expr)]
	[(not (let-expression-args-list? (cadr expr)))
	 (eopl:error 'parse-exp "let* or letrec variable list malformed: ~s" expr)]
	[else (if (eqv? (car expr) 'let*)
		  (let*-exp (remove-refs (map car (cadr expr)))
			    (map ref? (map car (cadr expr)))
			    (map (lambda (arg-val) (parse-exp (cadr arg-val))) (cadr expr))
			    (map parse-exp (cddr expr)))
		  (letrec-exp (remove-refs (map car (cadr expr)))
			      (map ref? (map car (cadr expr)))
			      (map (lambda (arg-val) (parse-exp (cadr arg-val))) (cadr expr))
			      (map parse-exp (cddr expr))))])]
     [(eqv? (car expr) 'set!)
      (cond
       [(null? (cdr expr)) (eopl:error 'parse-exp "set!-expr without variable or new binding: ~s" expr)]
       [(null? (cddr expr)) (eopl:error 'parse-exp "set!-expr without new binding: ~s" expr)]
       [(not (null? (cdddr expr))) (eopl:error 'parse-exp "set!-expr with too many arguments: ~s" expr)]
       [(not (symbol? (cadr expr))) (eopl:error 'parse-exp "set!-expr malformed variable: ~s" expr)]
       [else (set!-exp (cadr expr) (parse-exp (caddr expr)))])]
     [(eqv? (car expr) 'begin) (begin-exp (map parse-exp (cdr expr)))]
     [(eqv? (car expr) 'cond)
      (cond
       [(null? (cdr expr)) (eopl:error 'parse-exp "invalid syntax (~s)" expr)]
       [else (let cond-loop ([conds-exprs (cdr expr)] [rev-conds '()] [rev-thens '()])
	       (cond
		[(null? conds-exprs) (cond-exp (reverse rev-conds) (reverse rev-thens))]
		[(eqv? (caar conds-exprs) 'else) (if (not (null? (cdr conds-exprs)))
						     (eopl:error 'parse-exp "misplaced aux keyword else: ~s" expr)
						     (cond-else-exp (reverse rev-conds) (reverse rev-thens) (map parse-exp (cdar conds-exprs))))]
		[else (cond-loop (cdr conds-exprs) (cons (parse-exp (caar conds-exprs)) rev-conds) (cons (map parse-exp (cdar conds-exprs)) rev-thens))]))])]
     [(eqv? (car expr) 'and) (and-exp (map parse-exp (cdr expr)))]
     [(eqv? (car expr) 'or) (or-exp (map parse-exp (cdr expr)))]
     [(eqv? (car expr) 'case)
      (cond
       [(or (null? (cdr expr)) (null? (cddr expr))) (eopl:error 'parse-exp "invalid syntax (~s)" expr)]
       [else (let case-loop ([keys-exprs (cddr expr)] [rev-keys '()] [rev-exprs '()])
	       (cond
		[(null? keys-exprs) (case-exp (parse-exp (cadr expr)) (reverse rev-keys) (reverse rev-exprs))]
		[(eqv? (caar keys-exprs) 'else) (if (not (null? (cdr keys-exprs)))
						    (eopl:error 'parse-exp "misplaced aux keyword else: ~s" expr)
						    (case-else-exp (parse-exp (cadr expr)) (reverse rev-keys) (reverse rev-exprs) (map parse-exp (cdar keys-exprs))))]
		[else (case-loop (cdr keys-exprs) (cons (map lit-exp (caar keys-exprs)) rev-keys) (cons (map parse-exp (cdar keys-exprs)) rev-exprs))]))])]
     [(eqv? (car expr) 'while)
		(cond
			[(or (null? (cdr expr)) (null? (cddr expr))) (eopl:error 'parse-exp "invalid syntax ~s" expr)]
			[else (while-exp (parse-exp (cadr expr)) (map parse-exp (cddr expr)))])]
     [(eqv? (car expr) 'define)
      (cond
       [(or (null? (cdr expr)) (null? (cddr expr)) (not (null? (cdddr expr)))) (eopl:error 'parse-exp "invalid syntax ~s" expr)]
       [(not (symbol? (cadr expr))) (eopl:error 'parse-exp "define-exp malformed name: ~s" expr)]
       [else (define-exp (cadr expr) (parse-exp (caddr expr)))])]
     [(eq? (car expr) 'quote) (if (or (null? (cdr expr)) (not (null? (cddr expr))))
				  (eopl:error 'parse-exp "invalid quote syntax: ~s" expr)
				  (lit-exp (cadr expr)))]
     [else (app-exp (parse-exp (car expr)) (map parse-exp (cdr expr)))])))

(define reconstruct-refs
  (lambda (var isref)
    (if isref
	(list 'ref var)
	var)))

(define unparse-exp
  (lambda (expr)
    (case (car expr)
      [(var-exp)
       (cadr expr)]
      [(lit-exp)
       (cadr expr)]
      [(lambda-const-args-exp)
       (cons 'lambda (cons (map reconstruct-refs (cadr expr) (caddr expr)) (map unparse-exp (caddr (cdr expr)))))]
      [(lambda-const-var-args-exp)
	(cons 'lambda (cons (append (map reconstruct-refs (cadr expr) (caddr expr)) (caddr (cdr expr))) (map unparse-exp (caddr (cddr expr)))))]
      [(lambda-var-args-exp)
	(cons 'lambda (cons (cadr expr) (map unparse-exp (caddr expr))))]
      [(if-exp)
	(list 'if (unparse-exp (cadr expr)) (unparse-exp (caddr expr)) (unparse-exp (caddr (cdr expr))))]
      [(if-true-exp)
        (list 'if (unparse-exp (cadr expr)) (unparse-exp (caddr expr)))]
      [(let-exp)
	(cons 'let (cons (map (lambda (vars ref exp) (list (reconstruct-refs vars ref) (unparse-exp exp))) (cadr expr) (caddr expr) (caddr (cdr expr))) (map unparse-exp (caddr (cddr expr)))))]
      [(named-let-exp)
	(cons 'let (cons (cadr expr) (cons (map (lambda (vars ref exp) (list (reconstruct-refs vars ref) (unparse-exp exp))) (caddr expr) (caddr (cdr expr)) (caddr (cddr expr))) (map unparse-exp (caddr (cddr expr))))))]
      [(let*-exp)
	(cons 'let* (cons (map (lambda (vars ref exp) (list (reconstruct-refs vars ref) (unparse-exp exp))) (cadr expr) (caddr expr) (caddr (cdr expr))) (map unparse-exp (caddr (cddr expr)))))]
      [(letrec-exp)
	(cons 'letrec (cons (map (lambda (vars ref exp) (list (reconstruct-refs vars ref) (unparse-exp exp))) (cadr expr) (caddr expr) (caddr (cdr expr))) (map unparse-exp (caddr (cddr expr)))))]
      [(set!-exp)
	(cons 'set! (cons (cadr expr) (list (unparse-exp (caddr expr)))))]
      [(app-exp)
	(cons (unparse-exp (cadr-expr)) (map unparse-exp (caddr expr)))]
      [(begin-exp)
	(cons 'begin (map unparse-exp (cadr expr)))]
      [(cond-exp)
	(cons 'cond (map (lambda (condition if-thens) (cons (unparse-exp condition) (map unparse-exp if-thens))) (cadr expr) (caddr expr)))]
      [(cond-else-exp)
        (cons 'cond (append (map (lambda (condition if-thens) (cons (unparse-exp condition) (map unparse-exp if-thens))) (cadr expr) (caddr expr)) (list (cons 'else (map unparse-exp (caddr (cdr expr)))))))]
      [(and-exp)
	(cons 'and (map unparse-exp (cadr expr)))]
      [(or-exp)
	(cons 'or (map unparse-exp (cadr expr)))]
      [(case-exp)
	(cons 'case (cons (unparse-exp (cadr expr)) (map (lambda (keys exprs) (append (list (map unparse-exp keys)) (map unparse-exp exprs))) (caddr expr) (caddr (cdr expr)))))]
      [(case-else-exp)
	(cons 'case (cons (unparse-exp (cadr expr)) (append (map (lambda (keys exprs) (append (list (map unparse-exp keys)) (map unparse-exp exprs))) (caddr expr) (caddr (cdr expr))) (list (cons 'else (map unparse-exp (caddr (cddr expr))))))))]
      [(while-exp)
	(cons 'while (cons (unparse-exp (cadr expr)) (append (map unparse-exp (caddr expr)))))]
      [(define-exp)
	(cons 'define (cons (cadr expr) (list (unparse-exp (caddr expr)))))]
      [else (eopl:error 'huh "What")])))

(define syntax-expand
  (lambda (exp)
    (case (car exp)
      [(var-exp) exp]
      [(lit-exp) exp]
      [(lambda-const-args-exp)
       (lambda-const-args-exp (cadr exp) (caddr exp) (map syntax-expand (caddr (cdr exp))))]
      [(lambda-const-var-args-exp)
       (lambda-const-var-args-exp (cadr exp) (caddr exp) (caddr (cdr exp)) (map syntax-expand (caddr (cddr exp))))]
      [(lambda-var-args-exp)
       (lambda-var-args-exp (cadr exp) (map syntax-expand (caddr expr)))]
      [(if-exp)
       (if-exp (syntax-expand (cadr exp))
	       (syntax-expand (caddr exp))
	       (syntax-expand (caddr (cdr exp))))]
      [(if-true-exp)
       (if-true-exp (syntax-expand (cadr exp))
		    (syntax-expand (caddr exp)))]
      [(let-exp)
       (app-exp (lambda-const-args-exp (cadr exp) (caddr exp) (map syntax-expand (caddr (cddr exp)))) (map syntax-expand (caddr (cdr exp))))]
      [(named-let-exp)
       (syntax-expand (named-let-exp->letrec-exp (cadr exp) (caddr exp) (caddr (cdr exp)) (caddr (cddr exp)) (caddr (cdddr exp))))]
      [(let*-exp)
       (syntax-expand (let*-let-exp (cadr exp) (caddr exp) (map syntax-expand (caddr (cdr exp))) (map syntax-expand (caddr (cddr exp)))))]
      [(letrec-exp)
       (syntax-expand (letrec-exp->let-set-exp (cadr exp) (caddr exp) (caddr (cdr exp)) (caddr (cddr exp))))]
      [(set!-exp)
       (set!-exp (cadr exp) (syntax-expand (caddr exp)))]
      [(app-exp)
       (app-exp (syntax-expand (cadr exp)) (map syntax-expand (caddr exp)))]
      [(begin-exp)
       (app-exp (lambda-const-args-exp '() '() (map syntax-expand (cadr exp))) '())]
      [(cond-exp)
       (syntax-expand (cond-exp->if-exps (cadr exp) (caddr exp)))]
      [(cond-else-exp)
       (syntax-expand (cond-else-exp->if-exps (cadr exp) (caddr exp) (caddr (cdr exp))))]
      [(and-exp)
       (syntax-expand (and-exp->if-exps (cadr exp)))]
      [(or-exp)
       (syntax-expand (or-exp->if-exps (cadr exp)))]
      [(case-exp)
       (syntax-expand (case-exp->cond-exp (cadr exp) (caddr exp) (caddr (cdr exp))))]
      [(case-else-exp)
       (syntax-expand (case-else-exp->cond-else-exp (cadr exp) (caddr exp) (caddr (cdr exp)) (caddr (cddr exp))))]
      [(while-exp)
       (syntax-expand (named-let-exp 'loop '() '() '() (list (if-true-exp (cadr exp) (begin-exp (append (caddr exp) (list (parse-exp '(loop)))))))))]
      [(define-exp)
       (define-exp (cadr exp) (syntax-expand (caddr exp)))]
      [else (eopl:error 'huh "What")])))

(define let*-let-exp
  (lambda (vars refs exps bodies)
    (cond
     [(null? vars) (eopl:error 'What? "How did I get here?")]
     [(null? (cdr vars)) (let-exp vars refs exps bodies)]
     [else (let-exp
	    (list (car vars))
	    (list (car refs))
	    (list (car exps))
	    (list (let*-let-exp (cdr vars) (cdr refs) (cdr exps) bodies)))])))

(define cond-exp->if-exps
  (lambda (conditions if-thenss)
    (cond
     [(null? conditions) (eopl:error 'Huh? "This shouldn't be happening...")]
     [(null? (cdr conditions)) (if-true-exp (car conditions) (begin-exp (car if-thenss)))]
     [else (if-exp (car conditions) (begin-exp (car if-thenss)) (cond-exp->if-exps (cdr conditions) (cdr if-thenss)))])))

(define cond-else-exp->if-exps
  (lambda (conditions if-thenss cond-elses)
    (cond
     [(null? conditions) (if (null? cond-elses)
			     (eopl:error 'Seriously? "Really, how are you doing this? ~s" cond-elses)
			     (app-exp (lambda-const-args-exp '() '() cond-elses) '()))]
     [(null? (cdr conditions)) (if-exp (car conditions) (begin-exp (car if-thenss)) (begin-exp cond-elses))]
     [else (if-exp (car conditions) (begin-exp (car if-thenss)) (cond-else-exp->if-exps (cdr conditions) (cdr if-thenss) cond-elses))])))

(define and-exp->if-exps
  (lambda (bools)
    (cond
     [(null? bools) (lit-exp #t)]
     [(null? (cdr bools)) (car bools)]
     [else (if-exp (app-exp (var-exp 'not) (list (car bools)))
		   (lit-exp #f)
		   (and-exp->if-exps (cdr bools)))])))

(define or-exp->if-exps
  (lambda (bools)
    (if (null? bools)
	(lit-exp #f)
	(app-exp (lambda-const-args-exp (list 'temp)
					(list #f)
					(list (if-exp (var-exp 'temp)
						      (var-exp 'temp)
						      (or-exp->if-exps (cdr bools)))))
		 (list (car bools))))))


(define case-exp->cond-exp
  (lambda (id keyss exprss)
    (cond-exp (map (lambda (keys) (or-exp (map (lambda (key) (app-exp (var-exp 'eqv?) (list id key))) keys))) keyss) exprss)))

(define case-else-exp->cond-else-exp
  (lambda (id keyss exprss case-elses)
    (cond-else-exp (map (lambda (keys) (or-exp (map (lambda (key) (app-exp (var-exp 'eqv?) (list id key))) keys))) keyss) exprss case-elses)))

(define letrec-exp->let-set-exp
  (lambda (vars refs exps bodies)
    (let-exp vars
	     refs
	     (map (lambda (exp) (lit-exp #f)) vars)
	     (append (map (lambda (vars exp) (set!-exp vars exp)) vars exps) bodies))))

(define named-let-exp->letrec-exp
  (lambda (name vars refs exps bodies)
    (app-exp (letrec-exp (list name)
			 (list #f)
			 (list (lambda-const-args-exp vars refs
						bodies))
			 (list (var-exp name)))
	     exps)))
