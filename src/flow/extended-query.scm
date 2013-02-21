(##namespace ("postgresql/flow/extended-query#"))
(##include "~~lib/gambit#.scm")

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

(define current-portal (make-parameter ""))

(define current-maximum-result-rows (make-parameter 0))

(define current-function (make-parameter (lambda x #t)))

(define current-status (make-parameter #f))

(define (from-u8vector vect desc)
  (let ((oid (field-descriptor-type desc)))
    (call-with-input-u8vector vect (get-reader (oid->type oid)))))

(define-structure statement 
  (id read-only:)
  (connection read-only: unprintable:))

(define-structure portal
  (id read-only:)
  (connection read-only: unprintable:))

(define-handler-table extended-query-table
  ((parse-complete)
   (handle-next-message))

  ((bind-complete)
   (handle-next-message))

  ((portal-suspended)
   (send-message (execute (portal-id (current-portal)) (current-maximum-result-rows)))
   (handle-next-message))

  ((error-response fields) 
   (raise (make-backend-exception (current-connection) fields)))
  
  ((command-complete tag)
   (connection-status-set! (current-connection) 'idle)
   (current-status))

  ((empty-query-response)
   (connection-status-set! (current-connection) 'idle)
   (current-status))

  ((ready-for-query status)
   (connection-status-set! (current-connection) 'idle)
   (current-status))
  
  ((notification-response pid channel payload)
   (let ((queue (connection-notifications (current-connection)))
	 (notification (make-notification pid channel payload)))
     (push! notification queue)
     (handle-next-message)))

  ((row-description fields description)
   (current-description description)
   (handle-next-message))
  
  ((data-row columns row)
   (let ((function (current-function))
	 (status (current-status))
	 (description (current-description)))
     (current-status (apply function status (map from-u8vector row description)))
     (handle-next-message))))
  

(define (prepare sql-string #!key
		 (id (symbol->string (gensym 'p)))
		 (types '()) 
		 (connection (current-connection)))
  (parameterize ((current-connection connection)
		 (current-input-port (connection-port connection))
		 (current-output-port (connection-port connection)))
    (send-message (parse id sql-string types))
    (make-statement id connection)))

(define (postgresql/flow/extended-query#bind statement
					     arguments
					     #!optional
					     (id (symbol->string (gensym 'b)))
					     (types '()))
  (let ((connection (statement-connection statement)))
    (parameterize ((current-connection connection)
		   (current-output-port (connection-port connection))
		   (current-input-port (connection-port connection)))
      (send-message (bind id (statement-id statement) arguments '()))
      (make-portal id connection))))

(define (to-portal subject connection arguments)
  (cond
   ((portal? subject) subject)
   ((statement? subject) (postgresql/flow/extended-query#bind subject arguments))
   ((string? subject) (to-portal (prepare subject connection: connection) connection arguments))
   (else (error "can't execute it" subject))))

(define (postgresql/flow/extended-query#execute maybe-portal
						#!key
						(arguments '())
						(initial-value #f)
						(function (lambda x #t))
						(maximum-result-rows 0)
						(connection (current-connection)))
  (let* ((portal (to-portal maybe-portal connection arguments))
	 (connection (portal-connection portal)))
    (cleanup connection)
    (parameterize
	((current-connection connection)
	 (current-input-port (connection-port connection))
	 (current-output-port (connection-port connection))
	 (current-description '())
	 (current-portal portal)
	 (current-function function)
	 (current-status initial-value)
	 (current-maximum-result-rows maximum-result-rows)
	 (current-handler-table extended-query-table))
      (send-message (describe 'portal (portal-id portal)))
      (send-message (execute (portal-id portal) maximum-result-rows))
      (send-message (flush))
      (handle-next-message))))

(define (postgresql/flow/extended-query#execute-generator portal #!optional (arguments '()) (connection (current-connection)))
  (letrec ((state (lambda (return0)
		    (postgresql/flow/extended-query#execute portal
							    initial-value: return0
							    arguments: arguments
							    function: (lambda (return . rest) 
									(call/cc (lambda (state0) 
										   (set! state state0) 
										   (return rest))))
							    connection: connection)
		    #!eof)))
    (lambda () (call/cc state))))

