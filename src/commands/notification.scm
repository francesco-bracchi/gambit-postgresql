(##namespace ("postgresql/commands/notification#"))

(##include "~~lib/gambit#.scm")

(include "../utils/queue#.scm")
(include "../utils/type-readers#.scm")
(include "../connection#.scm")
(include "../exception#.scm")
(include "../notification#.scm")
(include "../messages/frontend#.scm")
(include "../messages/backend#.scm")

(define (connection-notification #!optional (connection (current-connection)))
  (let ((queue (connection-notifications connection)))
    (if (empty? queue)
	(wait-for-notifications connection)
	(pop! queue))))

(define (wait-for-notifications connection)
  (call-with-connection-port
   connection
   handle-notification-message))

(define (handle-notification-message)
  (recv-message
   ((notification-response pid channel payload)
    (pp `(notification response))
    (make-notification pid channel payload))
   
   ((error-response fields)
    (pp `(error response))
    (raise (make-backend-exception (current-connection) fields)))

   ((parameter-status key value)
    (pp `(parameter status))
    (connection-parameter-set! key value)
    (handle-notification-message))))
