;; # Query 
;;
;; This is the *simple statement* API call for postgresql
;; 

(##namespace ("postgresql/commands/query#"))

(##include "~~lib/gambit#.scm")

(include "../utils/queue#.scm")
(include "../utils/type-readers#.scm")
(include "../connection#.scm")
(include "../exception#.scm")
(include "../notification#.scm")
(include "../messages/frontend#.scm")
(include "../messages/backend#.scm")

(define (connection-query sql
			  #!key
			  (initial-value #f)
			  (reducer (lambda x #t))
			  (connection (current-connection)))
  (call-with-connection-port
   connection
   (lambda ()
     (send-message (query sql))
     (send-message (flush))
     (handle-query-result initial-value reducer))))

(define (from-u8vector vect desc) 
  (let* ((oid (field-descriptor-type desc))
	 (type (connection-oid->name oid)))
    (u8vector->data type vect)))

(define (handle-query-result initial-value reducer)
  (let handle-next-message ((description #f)
			    (value initial-value))
    (recv-message
     ((notice-response alist)
      ((connection-notice-handler (current-connection)) (current-connection) alist)
      (handle-next-message description value))
     
     ((error-response fields)
      (raise (make-backend-exception (current-connection) fields)))
     
     ((command-complete tag)
      (connection-status-set! (current-connection) tag)
      (handle-next-message description value))
     
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


