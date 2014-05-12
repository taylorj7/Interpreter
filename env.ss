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

(define vector-add-left
  (lambda (vec new-item)
    (let ([new-vec (make-vector (add1 (vector-length vec)))])
      (vector-set! new-vec 0 new-item)
      (let loop ([i 0])
	(if (< i (vector-length vec))
	    (begin
	      (vector-set! new-vec (add1 i) (vector-ref vec i))
	      (loop (add1 i)))
	    new-vec)))))
