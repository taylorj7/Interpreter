; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3

(define empty-env
  (lambda ()
    '()))
;    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (cons (cons syms vals) env)))
;    (extended-env-record syms vals env)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (xsym) (eqv? sym xsym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
     ((null? ls) #f)
     ((pred (car ls)) 0)
     (else (let ((list-index-r (list-index pred (cdr ls))))
	     (if (number? list-index-r)
		 (+ 1 list-index-r)
		 #f))))))

(define set-ref!
  (lambda (ref new-val)
    (vector-set! (car ref) (cdr ref) new-val)))

(define apply-env
  (lambda (env sym succeed fail)
    (apply-env-ref env sym (lambda (v) (succeed (deref v))) fail)))

(define deref
  (lambda (ref)
    (vector-ref (car ref) (cdr ref))))

(define apply-env-ref
  (lambda (env sym succeed fail) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
    (if (null? env) ; Empty environment
	(fail)
	(let ([pos (list-find-position sym (caar env))])
	  (if (number? pos)
	      (succeed (cons (cdar env) pos))
	      (apply-env-ref (cdr env) sym succeed fail))))))
;    (cases environment env
;      (empty-env-record ()
;        (fail))
;      (extended-env-record (syms vals env)
;	(let ((pos (list-find-position sym syms)))
;      	  (if (number? pos)
;	      (succeed (list-ref vals pos))
;	      (apply-env-ref env sym succeed fail)))))))
