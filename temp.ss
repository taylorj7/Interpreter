(define apply-prim-proc
  (lambda (prim-proc args)
    (case prim-proc
      [(+) (apply + args)]
      [(-) (apply - args)]
      [(*) (apply * args)]
      [(/) (apply / args)]
      [(add1) (cond
	       [(or (null? args) (not (null? (cdr args)))) (eopl:error 'add1 "incorrect argument count in call (~s ~s)" prim-proc args)]
	       [else (+ (1st args) 1)])]
      [(sub1) (cond
	       [(or (null? args) (not (null? (cdr args)))) (eopl:error 'sub1 "incorrect argument count in call (~s ~s)" prim-proc args)]
	       [else (- (1st args) 1)])]
      [(zero?) (cond
	       [(or (null? args) (not (null? (cdr args)))) (eopl:error 'zero? "incorrect argument count in call (~s ~s)" prim-proc args)]
	       [else (zero? (1st args))])]
      [(not) (cond
	       [(or (null? args) (not (null? (cdr args)))) (eopl:error 'not "incorrect argument count in call (~s ~s)" prim-proc args)]
	       [else (not (1st args))])]
      [(=) (cond
	    [(null? args) (eopl:error '= "incorrect argument count in call (~s)" prim-proc)]
	    [else (apply = args)])]
      [(<) (cond
	    [(null? args) (eopl:error '< "incorrect argument count in call (~s)" prim-proc)]
	    [else (apply < args)])]
      [(<=) (cond
	    [(null? args) (eopl:error '<= "incorrect argument count in call (~s)" prim-proc)]
	    [else (apply <= args)])]
      [(>) (cond
	    [(null? args) (eopl:error '> "incorrect argument count in call (~s)" prim-proc)]
	    [else (apply > args)])]
      [(>=) (cond
	    [(null? args) (eopl:error '>= "incorrect argument count in call (~s)" prim-proc)]
	    [else (apply >= args)])]
      [(cons) (cond
	       [(or (null? args) (null? (cdr args)) (not (null? (cddr args)))) (eopl:error 'cons "incorrect argument count in call (~s ~s)" prim-proc args)]
	       [else (cons (1st args) (2nd args))])]
      [(car) (cond
	      [(or (null? args) (not (null? (cdr args)))) (eopl:error 'car "incorrect argument count in call (~s ~s)" prim-proc args)]
	      [else (car (1st args))])]
      [(cdr) (cond
	      [(or (null? args) (not (null? (cdr args)))) (eopl:error 'cdr "incorrect argument count in call (~s ~s)" prim-proc args)]
	      [else (cdr (1st args))])]
	  [(caar) (cond
	      [(or (null? args) (not (null? (cdr args)))) (eopl:error 'caar "incorrect argument count in call (~s ~s)" prim-proc args)]
	      [else (caar (1st args))])]
      [(cdar) (cond
	      [(or (null? args) (not (null? (cdr args)))) (eopl:error 'cdar "incorrect argument count in call (~s ~s)" prim-proc args)]
	      [else (cdar (1st args))])]
	  [(cadr) (cond
	      [(or (null? args) (not (null? (cdr args)))) (eopl:error 'cadr "incorrect argument count in call (~s ~s)" prim-proc args)]
	      [else (cadr (1st args))])]
      [(cddr) (cond
	      [(or (null? args) (not (null? (cdr args)))) (eopl:error 'cddr "incorrect argument count in call (~s ~s)" prim-proc args)]
	      [else (cddr (1st args))])]
	  [(caaar) (cond
	      [(or (null? args) (not (null? (cdr args)))) (eopl:error 'caaar "incorrect argument count in call (~s ~s)" prim-proc args)]
	      [else (caaar (1st args))])]
      [(caadr) (cond
	      [(or (null? args) (not (null? (cdr args)))) (eopl:error 'caadr "incorrect argument count in call (~s ~s)" prim-proc args)]
	      [else (caadr (1st args))])]
	  [(cadar) (cond
	      [(or (null? args) (not (null? (cdr args)))) (eopl:error 'cadar "incorrect argument count in call (~s ~s)" prim-proc args)]
	      [else (cadar (1st args))])]
      [(caddr) (cond
	      [(or (null? args) (not (null? (cdr args)))) (eopl:error 'caddr "incorrect argument count in call (~s ~s)" prim-proc args)]
	      [else (cdaar (1st args))])]
	  [(cdaar) (cond
	      [(or (null? args) (not (null? (cdr args)))) (eopl:error 'cdaar "incorrect argument count in call (~s ~s)" prim-proc args)]
	      [else (caddr (1st args))])]
      [(cdadr) (cond
	      [(or (null? args) (not (null? (cdr args)))) (eopl:error 'cdadr "incorrect argument count in call (~s ~s)" prim-proc args)]
	      [else (cdadr (1st args))])]
	  [(cddar) (cond
	      [(or (null? args) (not (null? (cdr args)))) (eopl:error 'cddar "incorrect argument count in call (~s ~s)" prim-proc args)]
	      [else (cddar (1st args))])]
      [(cdddr) (cond
	      [(or (null? args) (not (null? (cdr args)))) (eopl:error 'cdddr "incorrect argument count in call (~s ~s)" prim-proc args)]
	      [else (cdddr (1st args))])]
	  [(list) (apply list args)]
	  [(assq) (cond
		  [(or (null? args) (null? (cadr args)) (not (null? (cddr args)))) (eopl:error 'assq "incorrect argument count in call (~s ~s)" prim-proc args)]
		  [else (assq (car args) (cadr args))])]
	  [(null?) (cond
		  [(or (null? args) (not (null? cdr args))) (eopl:error 'null? "incorrect argument count in call (~s ~s)" prim-proc args)]
		  [else (null? (car args))])]
	  [(eq?) (cond
		  [(or (null? args) (null? (cadr args)) (not (null? cddr args))) (eopl:error 'eq? "incorrect argument count in call (~s ~s)" prim-proc args)]
		  [else (eq? (car args) (cadr args))])]
	  [(equal?) (cond
		  [(or (null? args) (null? (cadr args)) (not (null? cddr args))) (eopl:error 'equal? "incorrect argument count in call (~s ~s)" prim-proc args)]
		  [else (equal? (car args) (cadr args))])]
	  [(atom?) (cond
		  [(or (null? args) (not (null? cdr args))) (eopl:error 'atom? "incorrect argument count in call (~s ~s)" prim-proc args)]
		  [else (atom? (car args))])]
	  [(length) (cond
		  [(or (null? args) (not (null? cdr args))) (eopl:error 'length "incorrect argument count in call (~s ~s)" prim-proc args)]
		  [else (length (car args))])]
	  [(list->vector) (cond
		  [(or (null? args) (not (null? cdr args))) (eopl:error 'list->vector "incorrect argument count in call (~s ~s)" prim-proc args)]
		  [else (list->vector (car args))])]
	  [(list?) (cond
		  [(or (null? args) (not (null? cdr args))) (eopl:error 'list? "incorrect argument count in call (~s ~s)" prim-proc args)]
		  [else (list? (car args))])]
	  [(pair?) (cond
		  [(or (null? args) (not (null? cdr args))) (eopl:error 'pair? "incorrect argument count in call (~s ~s)" prim-proc args)]
		  [else (pair? (car args))])]
	  [(procedure?) (cond
		  [(or (null? args) (not (null? cdr args))) (eopl:error 'procedure? "incorrect argument count in call (~s ~s)" prim-proc args)]
		  [else (procedure? (car args))])]
	  [(vector->list) (cond
		  [(or (null? args) (not (null? cdr args))) (eopl:error 'vector->list "incorrect argument count in call (~s ~s)" prim-proc args)]
		  [else (vector->list (car args))])]
	  [(vector) (apply vector args)]
	  [(make-vector) (cond
		  [(or (null? args) (not (null? cdr args))) (eopl:error 'make-vector "incorrect argument count in call (~s ~s)" prim-proc args)]
		  [else (make-vector (car args))])]
	  [(newline) (cond
		  [(not (null? args)) (eopl:error 'newline "incorrect argument count in call (~s ~s)" prim-proc args)]
		  [else (newline)])]
	  [(display) (cond
		  [(not (null? args)) (eopl:error 'display "incorrect argument count in call (~s ~s)" prim-proc args)]
		  [else (display)])]
	  [(vector?) (cond
		  [(or (null? args) (not (null? cdr args))) (eopl:error 'vector? "incorrect argument count in call (~s ~s)" prim-proc args)]
		  [else (vector? (car args))])]
	  [(symbol?) (cond
		  [(or (null? args) (not (null? cdr args))) (eopl:error 'number? "incorrect argument count in call (~s ~s)" prim-proc args)]
		  [else (symbol? (car args))])]
	  [(number?) (cond
		  [(or (null? args) (not (null? cdr args))) (eopl:error 'symbol? "incorrect argument count in call (~s ~s)" prim-proc args)]
		  [else (number? (car args))])]
	  [(vector-ref) (cond
		  [(or (null? args) (not (null? cddr args)) (null? (cdr args))) (eopl:error 'vector-ref "incorrect argument count in call (~s ~s)" prim-proc args)]
		  [else (vector-ref (car args) (cadr args))])]
	  [(set-car!) (cond
		  [(or (null? args) (not (null? cddr args)) (null? (cdr args))) (eopl:error 'vector-ref "incorrect argument count in call (~s ~s)" prim-proc args)]
		  [else (vector-ref (car args) (cadr args))])]
	  [(set-cdr!) (cond
		  [(or (null? args) (not (null? cddr args)) (null? (cdr args))) (eopl:error 'vector-ref "incorrect argument count in call (~s ~s)" prim-proc args)]
		  [else (vector-ref (car args) (cadr args))])]
	  [(vector-set!) (cond
		  [(or (null? args) (not (null? cddr args)) (null? (cdr args))) (eopl:error 'vector-ref "incorrect argument count in call (~s ~s)" prim-proc args)]
		  [else (vector-ref (car args) (cadr args))])]
      [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-op)])))
(define *prim-proc-names* '(vector-ref set-car! set-cdr! vector-set!))












