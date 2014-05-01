(define get-x
	(lambda (ls len)
		[cond	((eq? 0 len) (list ls))
				(else (append (list (car ls)) (get-x (cdr ls) (- len 1))))]))