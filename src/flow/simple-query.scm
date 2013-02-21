(##namespace ("postgresql/flow/simple-query#"))
(##include "~~/lib/gambit#.scm")

(include "../utils/queue#.scm")
(include "../connection#.scm")
(include "../exception#.scm")
(include "../messages/io#.scm")
(include "../messages/frontend#.scm")
(include "../messages/backend#.scm")
(include "handler#.scm")
(include "notifications#.scm")
(include "cleanup#.scm")

(define current-description (make-parameter '()))

(define current-function (make-parameter (lambda (x) x)))

(define current-status (make-parameter #f))

(define (apply-description descr row) row)

(define-handler-table simple-query-table
  ((ready-for-query status)
   (connection-status-set! (current-connection) status)
   (current-status))

  ((error-response fields) 
   (raise (make-backend-exception (current-connection) fields)))
  
  ((command-complete tag)
   (handle-next-message))
  
  ((row-description fields description)
   (pp `(DESC ,description))
   (current-description description)
   (handle-next-message))
  
  ((data-row columns row)
   (let ((function (current-function))
	 (status (current-status)))
     (current-status (apply function status row))
     (handle-next-message)))

  ((empty-query-response)
   (handle-next-message))

  ;; TODO move this in a separate file
  ((notification-response pid channel payload)
   (let ((queue (connection-notifications (current-connection)))
	 (notification (make-notification pid channel payload)))
     (push! notification queue)
     (handle-next-message))))

(define (simple-query sql-string #!key
		      (function (lambda x #t))
		      (initial-value #f)
		      (connection (current-connection)))
  (cleanup connection)
  (connection-status-set! connection 'simple-query)
  (parameterize
      ((current-connection connection)
       (current-input-port (connection-port connection))
       (current-output-port (connection-port connection))
       (current-description '())
       (current-function function)
       (current-status initial-value)
       (current-handler-table simple-query-table))
    (send-message (query sql-string))
    (handle-next-message)))

(define (simple-query-generator sql-string #!optional (connection (current-connection)))
  (letrec ((state (lambda (return0)
		    (simple-query sql-string
				  initial-value: return0
				  function: (lambda (return . rest) 
					      (call/cc (lambda (state0) 
							 (set! state state0) 
							 (return rest))))
				  connection: connection)
		    #!eof)))
    (lambda () (call/cc state))))
