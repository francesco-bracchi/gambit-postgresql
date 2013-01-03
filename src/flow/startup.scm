(##namespace ("postgresql/flow/startup#"))
(##include "~~/lib/gambit#.scm")

(include "../connection#.scm")
(include "../exception#.scm")
(include "../messages/io#.scm")
(include "../messages/frontend#.scm")
(include "../messages/backend#.scm")
(include "handler#.scm")

(define-macro (request-ok) 0)

(define-macro (request-clear-password) 3)

;; TODO: fill up this function
(define (code->name code)
  'unknown)

(define-handler-table startup-table

  ((authentication code)
   (cond
    ((= code (request-ok)) (handle-next-message))
    ((= code (request-clear-password) ) 
     (let ((pwd (connection-password (current-connection))))
       (send-message (password pwd))
       (handle-next-message)))
    (else (raise (make-unsupported-authentication-method-exception
		  (current-connection)
		  code
		  (code->name code))))))

  ((backend-key pid secret)
   (connection-pid-set! (current-connection) pid)
   (connection-secret-set! (current-connection) secret)
   (handle-next-message))
  
  ((error-response fields) 
   (raise (make-backend-exception (current-connection) fields)))
  
  ;; ((notice-response ..) ...)

  ;; ((notification-response ..) ..)
 
  ((backend-key-data pid secret)
   (let ((connection (current-connection)))
     (connection-pid-set! connection pid)
     (connection-secret-set! connection secret)
     (handle-next-message)))

  ((parameter-status key value) 
   (connection-parameter-set! key value)
   (handle-next-message))
  
  ((ready-for-query status) 
   (current-connection)))

(define (startup-flow connection)
  (parameterize
   ((current-connection connection)
    (current-input-port (connection-port connection))
    (current-output-port (connection-port connection))
    (current-handler-table startup-table))
   (let ((database (connection-database connection))
	 (username (connection-username connection)))
     (send-message (startup database username))
     (handle-next-message))))
