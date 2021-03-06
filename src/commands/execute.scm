(##namespace ("postgresql/commands/execute#"))

(##include "~~lib/gambit#.scm")

(include "execute#.scm")

(include "../utils/queue#.scm")
(include "../utils/type-readers#.scm")
(include "../connection#.scm")
(include "../exception#.scm")
(include "../notification#.scm")
(include "../messages/frontend#.scm")
(include "../messages/backend#.scm")

(declare (standard-bindings)
	 (extended-bindings)
	 (fixnum)
	 (block))

  
(define (connection-prepare sql-string
			    #!key
			    (id (symbol->string (gensym 'p)))
			    (types '())
			    (connection (current-connection)))
  (call-with-connection-port
   connection
   (lambda () 
     (send-message (parse id sql-string types))
     (make-statement id connection))))

(define (connection-bind statement arguments 
			 #!key 
			 (id (symbol->string (gensym 'b)))
			 (types (map (lambda (x) 0) arguments)))
  (call-with-connection-port
   (statement-connection statement)
   (lambda ()
     (send-message (bind id (statement-id statement) arguments types))
     (make-portal id (current-connection)))))

(define (to-portal thing arguments connection)
  (cond
   ((portal? thing) thing)
   ((statement? thing) (connection-bind thing arguments))
   ((string? thing) (to-portal (connection-prepare thing connection: connection) arguments connection))
   (else (error "can't make a portal out of " thing))))

(define (connection-execute thing
			    #!key
			    (arguments '())
			    (initial-value '())
			    (reducer (lambda (lst . vals) (cons vals lst)))
			    (maximum-result-rows 0)
			    (connection
			     (cond
			      ((statement? thing) (statement-connection thing))
			      ((portal? thing) (portal-connection thing))
			      (else (current-connection)))))
  (let ((portal (to-portal thing arguments connection)))
    (call-with-connection-port
     (portal-connection portal)
     (lambda ()
       (send-message (describe 'portal (portal-id portal)))
       (send-message (execute (portal-id portal) maximum-result-rows))
       (send-message (flush))
       (handle-execute-result portal initial-value reducer maximum-result-rows)))))

(define (from-u8vector vect desc) 
  (let* ((oid (field-descriptor-type desc))
	 (type (connection-oid->name oid)))
    (u8vector->data type vect)))

(define (handle-execute-result portal initial-value reducer maximum-result-rows)
  (let handle-next-message ((description #f)
			    (value initial-value))
    (recv-message     
     ((parse-complete)
      (handle-next-message description value))

     ((bind-complete)
      (handle-next-message description value))

     ((portal-suspended)
      (send-message (execute (portal-id portal) maximum-result-rows))
      (handle-next-message description value))
     
     ((notice-response alist)
      ((connection-notice-handler (current-connection)) (current-connection) alist)
      (handle-next-message description value))
     
     ((error-response fields)
      (raise (make-backend-exception (current-connection) fields)))
     
     ((command-complete tag)
      (connection-status-set! (current-connection) tag)
      value)
     
     ((empty-query-response)
      (connection-status-set! (current-connection) 'idle)
      value)
     
     ((no-data)
      (handle-next-message description value))

     ((ready-for-query status)
      (connection-status-set! (current-connection) status)
      value)

     ((notification-response pid channel payload)
      (connection-notification-push! (current-connection) (make-notification pid channel payload))
      (handle-next-message description value))
     
     ((row-description fields description)
      (handle-next-message description value))

     ((data-row column row)
      (handle-next-message description (apply reducer value (map from-u8vector row description)))))))


;; alias
(define postgresql/commands/execute#prepare connection-prepare)

(define postgresql/commands/execute#bind connection-bind)

(define postgresql/commands/execute#execute connection-execute)
