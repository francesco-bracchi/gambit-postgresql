(##namespace ("postgresql/connection#"))
(##include "~~/lib/gambit#.scm")

(include "messages/frontend#.scm")
(include "messages/backend#.scm")
(include "flows/common#.scm")
(include "flows/startup#.scm")
(include "flows/idle#.scm")
(include "flows/simple-query#.scm")
(include "flows/close#.scm")
(include "connection#.scm")

;; TODO: use macro accessor and create getters/setters that has as optional argument connection i.e. current-connection
(define-structure connection
  (database read-only:)
  (username read-only: unprintable:)
  (password read-only: unprintable:)
  (server-address read-only: unprintable:)
  (pid init: -1)
  (secret unprintable: init: 0)
  (port-number read-only: unprintable:)
  (character-encoding read-only: unprintable:)
  (handler-table unprintable:)
  (notification-handler unprintable:)
  (reader unprintable: init: #f)
  (port read-only: unprintable:)
  (consumer unprintable:)
  (parameters unprintable:)
  (type-readers unprintable:))
  
(define current-connection (make-parameter #f))

(define current-description-table (make-parameter #f))

(define current-port-table (make-parameter #f))

(define (message-handler tag)
  (table-ref (connection-handler-table (current-connection)) tag))

(define (make-reader #!optional (connection (current-connection)))
  (thread-start!
   (make-thread
    (lambda () 
      (with-exception-catcher 
       (lambda (ex) 'ignore)
       (lambda () 
      (parameterize
       ((current-connection connection)
	(current-reader-port (connection-port connection))
	(current-writer-port (connection-port connection))
	(current-message-handler message-handler)
	(current-description-table (make-table))
	(current-port-table (make-table))
	)
       (with-exception-catcher
	resume-and-raise
	handle-next-message))))))))

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
				      idle-handler
				      notification-handler
				      port
				      (current-thread)
				      (make-table) ; parameters
				      (make-table) ; type-readers 
				      ))
	 (reader (make-reader connection)))
    (connection-reader-set! connection reader)
    (connection-consumer-set! connection (current-thread))
    (startup connection)))

(define (close-connection #!optional (connection (current-connection)))
  (close connection))

(define (call-with-connection database-or-settings function)
  (let ((connection (cond
		     ((string? database-or-settings)
		      (open-connection database: database-or-settings))
		     (else (apply open-connection database-or-settings)))))
    (with-exception-catcher
     (lambda (ex) 
       (close-connection connection)
       (raise ex))
     (lambda () 
       (let ((value (function connection)))
	 (close-connection connection)
	 value)))))

(define (with-connection database-or-settings function)
  (call-with-connection 
   database-or-settings 
   (lambda (connection)
     (parameterize 
      ((current-connection connection))
      (function)))))

(define (open-simple-query #!key
			   query
			   (connection (current-connection)))
  (simple-query query connection))


(define current-result-port (make-parameter #f))

(define (call-with-simple-query query-or-settings function)
  (function (cond
	     ((string? query-or-settings) 
	      (open-simple-query query: query-or-settings))
	     (else
	      (apply open-simple-query query-or-settings)))))

(define (with-simple-query query-or-settings function)
  (call-with-simple-query
   query-or-settings
   (lambda (port)
     (parameterize
      ((current-result-port port))
      (function)))))

(define (append-reverse rs ss)
  (if (null? rs) ss
      (append-reverse (cdr rs) (cons (car rs) ss))))

(define (init-and-settings settings)
  (let remove ((ss settings) (rs '()))
    (cond
     ((null? ss) (values #f (reverse rs)))
     ((eq? (car ss) init:) (values (cadr ss) (append-reverse rs (cddr ss))))
     (else (remove (cdr ss) (cons (car ss) rs))))))
  
(define (reduce-query query-or-settings function)
  (if (string? query-or-settings)
      (reduce-query (list query: query-or-settings) function)
      (receive (init settings) (init-and-settings query-or-settings)
	       (call-with-simple-query
		settings 
		(lambda (port)
		  (let reduce ((init (or init (read port)))
			       (row (read port)))
		    (if (eof-object? row) 
			init
			(reduce (apply function (cons init row)) (read port)))))))))

(define *default* (list 'default))

(define (connection-parameter name #!optional (default *default*) (connection (current-connection)))
  (if (eq? default *default*)
      (table-ref (connection-parameters connection) name)
      (table-ref (connection-parameters connection) name default)))

;; (define (map-query query-or-settings function)
;;   (reverse 
;;    (reduce-query (append query-or-settings (list init: '()))
;; 		 (lambda (rlist . args) (cons (apply function args) rlist)))))


(define (with-transaction fn #!optional (connection (current-connection)))
  (with-exception-catcher
   (lambda (ex)
     ;; (if (not (backend-exception? ex) ... ?))
     (open-simple-query 
      query: "ROLLBACK TRANSACTION" 
      connection: connection)
     (raise ex))
   (lambda () 
     (open-simple-query
      query: "BEGIN TRANSACTION" 
      connection: connection)
     (let ((result (fn)))
       (open-simple-query query: "COMMIT TRANSACTION" 
			  connection: connection)
       result))))
     
