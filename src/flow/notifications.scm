(##namespace ("postgresql/flow/notifications#"))
(##include "~~lib/gambit#.scm")

(include "../utils/queue#.scm")
(include "../connection#.scm")
(include "../exception#.scm")
(include "../messages/io#.scm")
(include "../messages/frontend#.scm")
(include "../messages/backend#.scm")
(include "handler#.scm")
(include "cleanup#.scm")

;; TODO: other asynchronous messages like notice-response

(define-structure notification
  (pid read-only: unprintable:)
  (channel read-only:)
  (payload read-only:))

(define-handler-table notifications-table
  ((notification-response pid channel payload) 
   (make-notification pid channel payload))

  ((error-response fields) 
   (raise (make-backend-exception (current-connection) fields)))
  
  ((parameter-status key value)
   (connection-parameter-set! key value)
   (handle-next-message)))

(define (wait-for-notifications #!optional (connection (current-connection)))
  (parameterize 
      ((current-connection connection)
       (current-input-port (connection-port connection))
       (current-output-port (connection-port connection))
       (current-handler-table notifications-table))
    (handle-next-message)))

(define (recv-notification #!optional (connection (current-connection)))
  (cleanup connection)
  (let ((queue (connection-notifications connection)))
    (if (empty? queue) 
	(wait-for-notifications connection)
	(pop! queue))))
