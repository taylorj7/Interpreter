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
       [(list? (cadr expr)) (if ((list-of symbol?) (cadr expr))
				(lambda-const-args-exp (cadr expr) (map parse-exp (cddr expr)))
				(eopl:error 'parse-exp "lambda-expr variables of incorrect type: ~s" expr))]
       [(pair? (cadr expr)) (if ((improper-list-of symbol?) (cadr expr))
				(lambda-const-var-args-exp (get-const-lambda-args (cadr expr)) (get-var-lambda-arg (cadr expr)) (map parse-exp (cddr expr)))
				(eopl:error 'parse-exp "lambda-expr variables of incorrect type: ~s" expr))]
       [(symbol? (cadr expr)) (lambda-var-args-exp (cadr expr) (map parse-exp (cddr expr)))])]
     [(eqv? (car expr) 'if)
      (cond
       [(null? (cdr expr)) (eopl:error 'parse-exp "if-statement without condition or statements: ~s" expr)]
       [(null? (cddr expr)) (eopl:error 'parse-exp "if-statement without statements: ~s" expr)]
       [(null? (cdddr expr)) (if-true-exp (parse-exp (cadr expr)) (parse-exp (caddr expr)))]
       [else (if-exp (parse-exp (cadr expr)) (parse-exp (caddr expr)) (parse-exp (cadddr expr)))])]
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
				  (map car (caddr expr))
				  (map (lambda (arg-val) (parse-exp (cadr arg-val))) (caddr expr))
				  (map parse-exp (cdddr expr)))])]
	   [else (let-exp (map car (cadr expr))
			  (map (lambda (arg-val) (parse-exp (cadr arg-val))) (cadr expr))
			  (map parse-exp (cddr expr)))])]
     [(or (eqv? (car expr) 'let*) (eqv? (car expr) 'letrec))
       (cond
	[(null? (cdr expr)) (eopl:error 'parse-exp "let-expr without variable list or body: ~s" expr)]
	[(null? (cddr expr)) (eopl:error 'parse-exp "let-expr without body: ~s" expr)]
	[(not (let-expression-args-list? (cadr expr)))
	 (eopl:error 'parse-exp "let* or letrec variable list malformed: ~s" expr)]
	[else (if (eqv? (car expr) 'let*)
		  (let*-exp (map car (cadr expr))
			    (map (lambda (arg-val) (parse-exp (cadr arg-val))) (cadr expr))
			    (map parse-exp (cddr expr)))
		  (letrec-exp (map (lambda (arg-val)
				     (list (car arg-val) (parse-exp (cadr arg-val))))
				   (cadr expr)) (map parse-exp (cddr expr))))])]
     [(eqv? (car expr) 'set!)
      (cond
       [(null? (cdr expr)) (eopl:error 'parse-exp "set!-expr without variable or new binding: ~s" expr)]
       [(null? (cddr expr)) (eopl:error 'parse-exp "set!-expr without new binding: ~s" expr)]
       [(not (null? (cdddr expr))) (eopl:error 'parse-exp "set!-expr with too many arguments: ~s" expr)]
       [(not (symbol? expr)) (eopl:error 'parse-exp "set!-expr malformed variable: ~s" expr)]
       [else (set!-expr (cadr expr) (cadr expr))])]
     [(pair? expr)
      (cond
       [(eq? (car expr) 'quote) (if (or (null? (cdr expr)) (not (null? (cddr expr))))
				    (eopl:error 'parse-exp "invalid quote syntax: ~s" expr)
				    (lit-exp (cadr expr)))]
       [else (app-exp (parse-exp (car expr)) (map parse-exp (cdr expr)))])])))
(define syntax-expand
	(lambda (exp)
		[cases expression exp
			[var-exp (id) (var-exp id)]
			[lit-exp (id) (lit-exp id)]
			[lambda-const-args-exp (id body) (lambda-const-args-exp id (map syntax-expand body))]
			[lambda-const-var-args-exp (const var body) (lambda-const-var-args-exp const var (map syntax-expand body))]
			[lambda-var-args-exp (id body) (lambda-var-args-exp id (map syntax-expand body))]
			[if-exp (condition ifthen ifelse) (if-exp (syntax-expand condition) (syntax-expand ifthen) (syntax-expand ifelse))]
			[if-true-exp (condition ifthen) (if-exp (syntax-expand condition) (syntax-expand ifthen))]
			[let-exp (vars exps body) (app-exp (lambda-const-args-exp vars (map syntax-expand body)) (map syntax-expand exps))]
			[named-let-exp (name vars exps body) (named-let-exp name vars (map syntax-expand exps) (map syntax-expand body))]
			[let*-exp (vars exps body) (let*-let-exp vars exps (syntax-expand body))]
			[letrec-exp (vars body) (letrec-exp vars (map syntax-expand body))]
			[set!-exp (var val) (set!-exp var (syntax-expand val))]
			[app-exp (operator operand) (app-exp (syntax-expand operator) (map syntax-expand operand))]]))
			
(define let*-let-exp
	(lambda (vars body)
		[cond	((eq? '() vars) body)
				(else (app-exp (lambda-const-args-exp (car vars) (let*-let-exp (cdr vars) (cdr exps) body)) (car exps)))]))










