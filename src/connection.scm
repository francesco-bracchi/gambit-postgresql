(##namespace ("postgresql/connection#"))
(##include "~~/lib/gambit#.scm")

(include "connection#.scm")
(include "utils/queue#.scm")
(include "messages/frontend#.scm")
(include "messages/backend#.scm")
(include "commands/execute#.scm")
(include "commands/startup#.scm")

(define current-connection (make-parameter #f))

(define (open-database-port server-address port-number)
  (open-tcp-client
   (list server-address: server-address
	 port-number: port-number)))

(define (open-connection 
		 #!key
		 (database "postgres")
		 (username "postgres")
		 (password "")
		 (server-address "localhost")
		 (port-number 5432)
		 (character-encoding 'UTF-8)
		 (notice-handler (lambda (conn values) 'ignore)))
  (let* ((port (open-database-port server-address port-number))
	 (connection (make-connection 
		      database
		      username
		      password 
		      server-address
		      port-number
		      character-encoding
		      port
		      (make-table)
		      (make-queue)
		      notice-handler)))
    (connection-init! connection)
    (connection-update-oid-table! connection)
    connection))

(define (close-connection #!optional (connection (current-connection)))
  (let ((port (connection-port connection)))
    (send-message (terminate) port)
    (close-port port)
    (connection-pid-set! connection -1)
    (connection-status-set! connection 'closed)))

(define (call-with-connection database-or-settings function)
  (if (string? database-or-settings)
      (call-with-connection (list database: database-or-settings))
      (let ((connection (apply open-connection database-or-settings)))
	(with-exception-catcher
	 (lambda (ex) 
	   (close-connection connection)
	   (raise ex))
	 (lambda () 
	   (let ((v (function connection)))
	     (close-connection connection)
	     v))))))

(define (with-connection database-or-settings function)
  (call-with-connection
   database-or-settings
   (lambda (connection)
     (parameterize
      ((current-connection connection))
      (function)))))

(define (connection-parameter key #!optional (connection (current-connection)))
  (table-ref (connection-parameters connection) key))

(define (connection-parameter-set! key value #!optional (connection (current-connection)))
  (table-set! (connection-parameters connection) key value))

(define (call-with-connection-port connection function)
  (let ((port (connection-port connection)))
    (parameterize
     ((current-connection connection)
      (current-input-port port)
      (current-output-port port))
     (function))))

(define (connection-notification-push! connection value)
  (connection-notifications-set! connection 
				 (push! value (connection-notifications connection))))


(define (connection-update-oid-table! #!optional (connection (current-connection)))
  (let* ((to-string (lambda (u8) (call-with-input-u8vector u8 (lambda (port) (read-line port #f)))))
	 (to-symbol (lambda (u8) (string->symbol (to-string u8))))
	 (to-int (lambda (u8) (string->number (to-string u8)))))
    (connection-oid-table-set!
     connection
     (list->table
      (connection-execute 
       "SELECT typname, oid FROM pg_type"
       connection: connection
       initial-value: '()
       reducer: (lambda (ass typname oid) (cons (cons (to-int oid) (to-symbol typname)) ass)))))))

(define (connection-oid->name oid #!optional (connection (current-connection)))
  (let ((table (connection-oid-table connection)))
    (and table (table-ref table oid #f))))
  

(define (with-transaction function #!optional (connection (current-connection)))
  (parameterize 
   ((current-connection connection))
   (with-exception-catcher
    (lambda (ex) 
      (connection-execute (string-append "ROLLBACK"))
      (raise ex))
    (lambda () 
      (connection-execute (string-append "BEGIN"))
      (function)
      (connection-execute (string-append "COMMIT TRANSACTION"))))))
