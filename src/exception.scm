(##namespace ("postgresql/exception#"))
(##include "~~lib/gambit#.scm")

(define-structure postgresql-exception
  extender: define-postgresql-exception
  (connection read-only: unprintable:))

(define-postgresql-exception unsupported-authentication-method-exception 
  (code read-only: unprintable:)
  (name read-only: unprintable:))

(define-postgresql-exception backend-exception
  (fields read-only:))

(define-macro (define-value-getter name)
  (let ((ex (gensym 'ex))
	(vals (gensym 'vals)))
    `(define (,(symbol-append 'backend-exception- name) ,ex)
       (let ((,vals (backend-exception-fields ,ex)))
	 (cond
	  ((assq ,(list 'quote name) ,vals) => cdr)
	  (else #f))))))

;; (define-macro (define-value-getters . names)
;;   `(begin ,@(map (lambda (n) `(define-value-getter ,n))
;; 		 names)))
	
;; (define-value-getters 
;;   severity 
;;   sql-state 
;;   message
;;   file
;;   line 
;;   routine)
  
