(##namespace ("postgresql/exception#"))
(##include "~~lib/gambit#.scm")

(include "exception#.scm")

(declare (standard-bindings)
	 (extended-bindings)
	 (fixnum)
	 (block))

(define-macro (define-standard-getters . as)
  (let ((symbol-append (lambda as (string->symbol (apply string-append (map symbol->string as))))))
    `(begin 
       ,@(map (lambda (a) 
		(let ((ex (gensym 'ex)))
		  `(define (,(symbol-append 'backend-exception- a) ,ex)
		     (backend-exception-get-field ,ex (quote ,a)))))
	      as))))
  
(define (backend-exception-get-field ex key)
  (cond
   ((assq (backend-exception-fields ex) key) => cdr)
   (else #f)))

(define-standard-getters 
  severity
  sql-state
  message
  file
  line
  routine)

(define unsupported-authentication-method-connection 
  postgresql-exception-connection)

(define backend-exception-connectin
  postgresql-exception-connection)
