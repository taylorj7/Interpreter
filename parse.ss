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
		  (letrec-exp (map car (cadr expr))
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
	 [(pair? expr)
      (cond
       [(eq? (car expr) 'quote) (if (or (null? (cdr expr)) (not (null? (cddr expr))))
				    (eopl:error 'parse-exp "invalid quote syntax: ~s" expr)
				    (lit-exp (cadr expr)))]
       [else (app-exp (parse-exp (car expr)) (map parse-exp (cdr expr)))])])))

(define unparse-exp
  (lambda (expr)
    (cases expression expr
      [var-exp (var)
	var]
      [lit-exp (lit)
	lit]
      [lambda-const-args-exp (list-of-args list-of-body)
	(cons 'lambda (cons list-of-args (map unparse-exp list-of-body)))]
      [lambda-const-var-args-exp (const-id var-id list-of-body)
	(cons 'lambda (cons (append const-id var-id) (map unparse-exp list-of-body)))]
      [lambda-var-args-exp (id list-of-body)
	(cons 'lambda (cons id (map unparse-exp list-of-body)))]
      [if-exp (condition if-then if-else)
	(list 'if (unparse-exp condition) (unparse-exp if-then) (unparse-exp if-else))]
      [if-true-exp (condition if-then)
        (list 'if (unparse-exp condition) (unparse-exp if-then))]
      [let-exp (vars exps bodies)
	(cons 'let (cons (map (lambda (var exp) (list var (unparse-exp exp))) vars exps) (map unparse-exp bodies)))]
      [named-let-exp (name vars exps bodies)
	(cons 'let (cons name (cons (map (lambda (var exp) (list var (unparse-exp exp))) vars exps) (map unparse-exp bodies))))]
      [let*-exp (vars exps bodies)
	(cons 'let* (cons (map (lambda (var exp) (list var (unparse-exp exp))) vars exps) (map unparse-exp bodies)))]
      [letrec-exp (vars exps bodies)
	(cons 'letrec (cons (map (lambda (var exp) (list var (unparse-exp exp))) vars exps) (map unparse-exp bodies)))]
      [set!-exp (var val)
	(cons 'set! (cons var (list (unparse-exp val))))]
      [app-exp (operator operands)
	(cons (unparse-exp operator) (map unparse-exp operands))]
      [begin-exp (bodies)
	(cons 'begin (map unparse-exp bodies))]
      [cond-exp (conditions if-thenss)
	(cons 'cond (map (lambda (condition if-thens) (cons (unparse-exp condition) (map unparse-exp if-thens))) conditions if-thenss))]
      [cond-else-exp (conditions if-thenss cond-elses)
        (cons 'cond (append (map (lambda (condition if-thens) (cons (unparse-exp condition) (map unparse-exp if-thens))) conditions if-thenss) (list (cons 'else (map unparse-exp cond-elses)))))]
      [and-exp (bools)
	(cons 'and (map unparse-exp bools))]
      [or-exp (bools)
	(cons 'or (map unparse-exp bools))]
      [case-exp (id keyss exprss)
	(cons 'case (cons (unparse-exp id) (map (lambda (keys exprs) (append (list (map unparse-exp keys)) (map unparse-exp exprs))) keyss exprss)))]
      [case-else-exp (id keyss exprss case-elses)
	(cons 'case (cons (unparse-exp id) (append (map (lambda (keys exprs) (append (list (map unparse-exp keys)) (map unparse-exp exprs))) keyss exprss) (list (cons 'else (map unparse-exp case-elses))))))]
	  [while-exp (condit bodies)
	(cons 'while (cons (unparse-exp condit) (append (map unparse-exp bodies))))])))

(define syntax-expand
	(lambda (exp)
		(cases expression exp
			[var-exp (id) (var-exp id)]
			[lit-exp (id) (lit-exp id)]
			[lambda-const-args-exp (id body) (lambda-const-args-exp id (map syntax-expand body))]
			[lambda-const-var-args-exp (const var body) (lambda-const-var-args-exp const var (map syntax-expand body))]
			[lambda-var-args-exp (id body) (lambda-var-args-exp id (map syntax-expand body))]
			[if-exp (condition ifthen ifelse) (if-exp (syntax-expand condition) (syntax-expand ifthen) (syntax-expand ifelse))]
			[if-true-exp (condition ifthen) (if-true-exp (syntax-expand condition) (syntax-expand ifthen))]
			[let-exp (vars exps body) (app-exp (lambda-const-args-exp vars (map syntax-expand body)) (map syntax-expand exps))]
			[named-let-exp (name vars exps body) (syntax-expand (named-let-exp->letrec-exp name vars exps body))]
			[let*-exp (vars exps body) (syntax-expand (let*-let-exp vars (map syntax-expand exps) (map syntax-expand body)))]
			[letrec-exp (vars exps body) (syntax-expand (letrec-exp->let-set-exp vars exps body))]
			[set!-exp (var val) (set!-exp var (syntax-expand val))]
			[app-exp (operator operand) (app-exp (syntax-expand operator) (map syntax-expand operand))]
			[begin-exp (bodies) (app-exp (lambda-const-args-exp '() (map syntax-expand bodies)) '())]
			[cond-exp (conditions if-thenss) (syntax-expand (cond-exp->if-exps conditions if-thenss))]
			[cond-else-exp (conditions if-thenss cond-elses) (syntax-expand (cond-else-exp->if-exps conditions if-thenss cond-elses))]
			[and-exp (bools) (syntax-expand (and-exp->if-exps bools))]
			[or-exp (bools) (syntax-expand (or-exp->if-exps bools))]
			[case-exp (id keyss exprss) (syntax-expand (case-exp->cond-exp id keyss exprss))]
			[case-else-exp (id keyss exprss case-elses) (syntax-expand (case-else-exp->cond-else-exp id keyss exprss case-elses))]
			[while-exp (id bodies) (while-exp (syntax-expand id) (map syntax-expand bodies))])))

(define let*-let-exp
  (lambda (vars exps bodies)
    (cond
     [(null? vars) (eopl:error 'What? "How did I get here?")]
     [(null? (cdr vars)) (let-exp vars exps bodies)]
     [else (let-exp
	    (list (car vars))
	    (list (car exps))
	    (list (let*-let-exp (cdr vars) (cdr exps) bodies)))])))

(define cond-exp->if-exps
  (lambda (conditions if-thenss)
    (cond
     [(null? conditions) (eopl:error 'Huh? "This shouldn't be happening...")]
     [(null? (cdr conditions)) (if-true-exp (car conditions) (begin-exp (car if-thenss)))]
     [else (if-exp (car conditions) (begin-exp (car if-thenss)) (cond-exp->if-exps (cdr conditions) (cdr if-thenss)))])))

(define cond-else-exp->if-exps
  (lambda (conditions if-thenss cond-elses)
    (cond
     [(null? conditions) (if (null? cond-elses) (eopl:error 'Seriously? "Really, how are you doing this? ~s" cond-elses) (app-exp (lambda-const-args-exp '() cond-elses) '()))]
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
    (cond
     [(null? bools) (lit-exp #f)]
     [(null? (cdr bools)) (car bools)]
     [else (if-exp (app-exp (var-exp 'not) (list (app-exp (var-exp 'not) (list (car bools)))))
		   (car bools)
		   (or-exp->if-exps (cdr bools)))])))

(define case-exp->cond-exp
  (lambda (id keyss exprss)
    (cond-exp (map (lambda (keys) (or-exp (map (lambda (key) (app-exp (var-exp 'eqv?) (list id key))) keys))) keyss) exprss)))

(define case-else-exp->cond-else-exp
  (lambda (id keyss exprss case-elses)
    (cond-else-exp (map (lambda (keys) (or-exp (map (lambda (key) (app-exp (var-exp 'eqv?) (list id key))) keys))) keyss) exprss case-elses)))

(define letrec-exp->let-set-exp
  (lambda (vars exps bodies)
    (let-exp vars
	     (map (lambda (exp) (lit-exp #f)) vars)
	     (append (map (lambda (var exp) (set!-exp var exp)) vars exps) bodies))))

(define named-let-exp->letrec-exp
  (lambda (name vars exps bodies)
    (app-exp (letrec-exp (list name)
			 (list (lambda-const-args-exp vars
						bodies))
			 (list (var-exp name)))
	     exps)))
