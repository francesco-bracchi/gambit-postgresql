(##namespace ("postgresql/exception#"))
(##include "~~lib/gambit#.scm")

(define-structure backend-exception
  (fields read-only:)
  (query read-only: init: #f))

(define (and-cdr e)
  (and e (cdr e)))

(define-macro (define-field-getter key)
  (let ((exception (gensym 'exception))
	(symbol-append (lambda ss (string->symbol (apply string-append (map symbol->string ss))))))
    `(define (,(symbol-append 'backend-exception- key) ,exception)
       (and-cdr (assq ',key (backend-exception-fields ,exception))))))

(define-macro (define-field-getters . keys)
  `(begin ,@(map (lambda (key) `(define-field-getter ,key)) keys)))

(define-field-getters severity sql-state message)
