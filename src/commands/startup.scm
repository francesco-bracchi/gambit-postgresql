(##namespace 
 ("postgresql/commands/startup#"
  connection-init!
  ))

(##namespace ("postgresql/commands/startup#"))
(##include "~~/lib/gambit#.scm")

(include "../utils/queue#.scm")
(include "../connection#.scm")
(include "../exception#.scm")
(include "../notification#.scm")
(include "../messages/frontend#.scm")
(include "../messages/backend#.scm")

(declare (standard-bindings)
	 (extended-bindings)
	 (fixnum)
	 (block))

(define request-ok 0)

(define request-clear-password 3)

(define request-kerberos 2)

(define request-md5-password 5)

(define request-scm-credential 6)

(define request-gss 7)

(define request-sspi 9)

(define (code->name code)
  (cond
   ((eq? code request-clear-password) 'clear-password)
   ((eq? code request-kerberos) 'kerberos)
   ((eq? code request-md5-password) 'md5)
   ((eq? code request-scm-credential) 'scm)
   ((eq? code request-gss) 'gss)   
   ((eq? code request-sspi) 'sspi)
   (else 'unknown)))

(define (connection-init! connection)
  (let ((database (connection-database connection))
	(username (connection-username connection)))
    (call-with-connection-port 
     connection
     (lambda () 
       (send-message (startup database username))
       (handle-startup-response)))))

(define (raise-unsupported-method code)
  (raise (make-unsupported-authentication-method-exception
	  (current-connection)
	  code
	  (code->name code))))

(define (handle-startup-response)
  (recv-message
   ((authentication code)
    (cond
     ((= code request-ok) 
      (handle-startup-response))
     ((= code request-clear-password) 
      (send-message (password (connection-password (current-connection))))
      (handle-startup-response))
     (else 
      (raise-unsupported-method code))))
   
   ((backend-key pid secret)
    (connection-pid-set! (current-connection) pid)
    (connection-secret-set! (current-connection) secret)
    (handle-startup-response))

   ((error-response fields)
    (raise (make-backend-exception (current-connection) fields)))

   ((notification-response pid channel payload)
    (connection-notification-push! (current-connection) (make-notification pid channel payload))
    (handle-startup-response))

   ((backend-key-data pid secret)
    (connection-pid-set! (current-connection) pid)
    (connection-secret-set! (current-connection) secret)
    (handle-startup-response))
   
   ((parameter-status key value)
    (connection-parameter-set! key value)
    (handle-startup-response))
   
   ((ready-for-query status)
    (connection-status-set! (current-connection) status)
    (current-connection))))
