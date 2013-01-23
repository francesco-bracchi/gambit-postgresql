(##namespace ("postgresql/connection#"))
(##include "~~/lib/gambit#.scm")

(include "utils/queue#.scm")
(include "messages/frontend#.scm")
(include "messages/backend#.scm")
(include "flow/startup#.scm")

;; TODO: use macro accessor and create getters/setters that has as optional argument connection i.e. current-connection
(define-structure connection
  (database read-only:)
  (username read-only: unprintable:)
  (password read-only: unprintable:)
  (server-address read-only: unprintable:)
  (port-number read-only: unprintable:)
  (character-encoding read-only: unprintable:)
  (notification-handler unprintable:)
  (port read-only: unprintable:)
  (parameters unprintable:)
  (pid init: -1 unprintable:)
  (secret unprintable: init: 0)
  (reader unprintable: init: #f)
  (status init: 'startup)
  (notifications unprintable: read-only:) ;; TODO: this is a queue, use a better data structure
  ;; (handler-table unprintable: init: idle-handler)
  ;; (reader-port unprintable: init: #f)
  ;; (type-readers unprintable:)
  )

(define (read-messages connection server)
  (parameterize
   ((current-connection connection)
    (current-input-port (connection-port connection))
    (current-output-port (connection-port connection))
    (current-handler-table startup-table))
   (handle-next-message)))
  
(define (start-reader connection)
  (receive (client server) (open-vector-pipe)
	   (connection-data-port-set! connection client)
	   (thread-start! (make-thread (lambda () (read-messages connection server))))))

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
		 (notification-handler (lambda _ 'ignore)))
  (let* ((port (open-database-port server-address port-number))
	 (connection (make-connection database
				      username
				      password
				      server-address
				      port-number
				      character-encoding
				      notification-handler
				      port
				      (make-table)
				      (make-queue))))
    (startup-flow connection)
    ;; (start-reader connection)
    connection))

(define (close-connection #!optional (connection (current-connection)))
  (let ((port (connection-port connection)))
    (send-message (terminate) port)
    (close-port port)
    (connection-pid-set! connection -1)))

(define (call-with-connection database-or-settings function)
  (if (string? database-or-settings)
      (call-with-connection (list database: database-or-settings))
      (let ((connection (apply open-connection database-or-settings)))
	(with-exception-catcher
	 (lambda (ex) 
	   (close-connection connection)
	   (raise ex))
	 (lambda () 
	   (function connection)
	   (close-connection connection))))))

(define (with-connection database-or-settings function)
  (call-with-connection
   database-or-settings
   (lambda (connection)
     (parameterize
      ((current-connection connection))
      (function)))))
  
;; (define (sql-eval q #!optional (connection (current-connection)))
;;   (connection-mode-set! connection 'simple-query)
;;   (send-message (query q) (connection-port connection))
;;   (read (connection-communication-port connection)))

(define (connection-parameter key #!optional (connection (current-connection)))
  (table-ref (connection-parameters connection) key))

(define (connection-parameter-set! key value #!optional (connection (current-connection)))
  (table-set! (connection-parameters connection) key value))

