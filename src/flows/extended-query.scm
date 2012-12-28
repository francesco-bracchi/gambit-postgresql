(##namespace ("postgresql/flows/simple-query#"))
(##include "~~/lib/gambit#.scm")

(include "../connection#.scm")
(include "../exception#.scm")
(include "../messages/io#.scm")
(include "../messages/frontend#.scm")
(include "../messages/backend#.scm")
(include "common#.scm")

(define (ignore-message . args)
  (handle-next-message))

(define extended-query-handler (make-handler-table ignore-message))

(define-handler-definer extended-query-handler)

(include "asynchronous.scm")

(define-handler (error-response pairs)
  (resume-and-raise (make-backend-exception pairs)))


(define (extended-query query #!optional 
			(name "")
			(parameter-types '()) 
			(connection (current-connection)))
  
  (connection-handler-table-set! connection extended-query-handler)
  (send-message-and-wait (parse name query parameter-types) connection))

