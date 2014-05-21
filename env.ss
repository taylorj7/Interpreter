; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3

(define empty-env
  (lambda ()
    '()))
;    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (if (equal? env global-env)
	(cons (cons syms vals) (empty-env))
	(cons (cons syms vals) env))))
;    (cons (cons syms vals) env)))
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
  (lambda (ref new-val k)
    (apply-k k (vector-set! (car ref) (cdr ref) new-val))))

(define apply-env ;For future note mainly just a wrapper
  (lambda (env sym succeed fail)
    (apply-env-ref env sym (deref-continuation succeed) fail)))

(define deref ;Wrapped by apply-k
  (lambda (ref k)
    (apply-k k (vector-ref (car ref) (cdr ref)))))

(define apply-env-ref
  (lambda (env sym succeed fail) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
    (if (null? env) ; Empty environment
	(apply-k fail (void)) ;This is garbage. no really. garbage. Now it is slightly less smelly garbage. BUT IT IS STILL GARBAGE.
	(let ([pos (list-find-position sym (caar env))])
	  (if (number? pos)
	      (apply-k succeed (cons (cdar env) pos))
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
