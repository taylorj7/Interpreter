
(define syntax-expand
	(lambda (exp)
		[cases expression (exp)
			[var-exp (id) (var-exp id)]
			[lit-exp (id) (lit-exp id)]
			[lambda-const-args-exp (id body) (lambda-const-args-exp id (map syntax-expand body))]
			[lambda-const-var-args-exp (const var body) (lambda-const-var-args-exp const var (map syntax-expand body))]
			[lambda-var-args-exp (id body) (lambda-var-args-exp id (map syntax-expand body))]
			[if-exp (condition ifthen ifelse) (if-exp (syntax-expand condition) (syntax-expand ifthen) (syntax-expand ifelse))]
			[if-true-exp (condition ifthen) (if-exp (syntax-expand condition) (syntax-expand ifthen))]
			[let-exp (vars exps body) (app-exp (lambda-const-args-exp vars (map syntax-expand body)) (map syntax-expand exps))]
			[let*-exp (vars body) (let*-let-exp vars body)]

(define let*-let-exp
	(lambda (vars body)
		[cond	((eq? '() vars) body)
				(else (app-exp (lambda-const-args-exp (caar vars) (let*-let-exp (cdr vars))) (cadr vars)))]))
(define-datatype expression expression? 
  [named-let-exp
   (name symbol?)
   (vars (list-of symbol?))
   (exps (list-of expression?))
   (body (list-of expression?))]
  [let*-exp
   (vars let-expression-args-list?)
   (body (list-of expression?))]
  [letrec-exp
   (vars let-expression-args-list?)
   (body (list-of expression?))]
  [set!-exp
   (var symbol?)
   (val expression?)]
  [app-exp
   (operator expression?)
   (operands (list-of expression?))])