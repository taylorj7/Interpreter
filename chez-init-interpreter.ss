;; save the nicer chez behavior
(define chez-printf printf)
(define chez-pretty-print pretty-print)


;; use the nicer chez behavior for these
(define sllgen:pretty-print chez-pretty-print)
(define eopl:pretty-print chez-pretty-print)
(define define-datatype:pretty-print chez-pretty-print)


;;I do not want to get into the debugger:
(define eopl:error-stop (lambda () '()))

(define eopl:error errorf)

;;; ------------------------------
;;; general helpers

(define always?
  (lambda (x) #t))

(define list-of
  (lambda (pred . l)
    (let ((all-preds (cons pred l)))
      (lambda (obj)
        (let loop ((obj obj) (preds '()))
          (or 
            ;; if list is empty, preds should be, too
            (and (null? obj) (null? preds))
            (if (null? preds)
                ;; if preds is empty, but list isn't, then recycle
                (loop obj all-preds)
                ;; otherwise check and element and recur.
                (and (pair? obj)
                     ((car preds) (car obj))
                     (loop (cdr obj) (cdr preds))))))))))
										 
										 
