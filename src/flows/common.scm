(##namespace ("postgresql/flows/common#"))
(##include "~~/lib/gambit#.scm")

(include "../connection#.scm")
(include "../exception#.scm")
(include "../messages/io#.scm")
(include "../messages/frontend#.scm")
(include "../messages/backend#.scm")

(define (default-handler args)
  (pp `(error ,args)))

(define (resume value #!optional (connection (current-connection)))
  (let((consumer (connection-consumer connection)))
    (thread-send consumer (cons 'value value))))

(define (resume-and-raise ex #!optional (connection (current-connection)))
  (pp (type-exception-arguments ex))
  (let((consumer (connection-consumer connection)))
    (thread-send consumer (cons 'exception ex))))

(define current-connection-parameters (make-parameter (make-table)))

(define (make-handler-table #!optional (default raise-error))
  (make-table init: default)) 

(define (handler-table-set! table type handler)
  (table-set! table type  handler))

(define (handler-table-ref table type)
  (table-ref table type))

