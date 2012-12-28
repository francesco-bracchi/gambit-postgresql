(##namespace ("postgresql/flows/startup#"))
(##include "~~/lib/gambit#.scm")

(include "../connection#.scm")
(include "../exception#.scm")
(include "../messages/io#.scm")
(include "../messages/frontend#.scm")
(include "../messages/backend#.scm")
(include "common#.scm")
(include "idle#.scm")

(define (ignore-message . args)
  (handle-next-message))

(define startup-handler (make-handler-table ignore-message))

(define-handler-definer startup-handler)

(define-macro (request-ok) 0)

(define-macro (request-clear-password) 3)

(define (authentication-failed-detail code)
  (string-append 
   "the server require an authentication protocol not supported by the client"
   "this library (postgresql) only supports clear password authentication. "))

(define current-backend-key-data (make-parameter #f))

(define-handler (authentication code)
  (cond
   ((= code (request-ok))
    (handle-next-message))
   ((= code (request-clear-password))
    (send-message (password (connection-password (current-connection))))
    (handle-next-message))
   (else
    (resume-and-raise
     (make-backend-exception
      `((message . "authentication not supported")
	(code . code)
	(detail . ,(authentication-failed-detail code))))))))

(define-handler (ready-for-query status)
  (resume (current-connection))  
  (connection-handler-table-set! (current-connection) idle-handler)
  #;(current-table-handler idle-handler)
  (handle-next-message))

(define-handler (error-response pairs)
  (resume-and-raise (make-backend-exception pairs)))

(define-handler (backend-key-data pid secret)
  (connection-pid-set! (current-connection) pid)
  (connection-secret-set! (current-connection) secret)
  (handle-next-message))

(include "asynchronous.scm")

(define (startup #!optional (connection (current-connection)))
  (let ((database (connection-database connection))
	(username (connection-username connection)))
    (connection-handler-table-set! connection startup-handler)
    (send-message-and-wait (startup database username) connection)))

