(##namespace "postgresql/connection#")
(##include "~~/lib/gambit#.scm")

(include "messages/io#.scm")
(include "messages/frontend#.scm")
(include "messages/backend#.scm")

(define-structure connection
  constructor: _make-connection
  database
  username
  password
  hostname
  port-number
  character-encoding
  port
  thread
  )

(define-structure result
  connection
  status
  tuple-description
  tuples
  notices)

(define (result-names result)
  (map field-descriptor-name 
       (result-tuple-description result)))

(define-type backend-exception
  fields
  (query init: #f))

(define current-connection (make-parameter #f))

(define (make-connection database username
			 #!key
			 (password "")
			 (hostname "localhost")
			 (port-number 5432)
			 (character-encoding 'UTF-8))
  (let* ((port 
	  (open-tcp-client
	   (list server-address: server-address
		 port-number: port-number)))
	 (thread
	  (thread-start!
	   (make-thread
	    (connection-new database
			    username
			    password
			    server-address
			    port-number
			    character-encoding)))))
    (_make-connection
     database
     username
     password
     hostname
     port-number
     character-encoding
     port
     thread)))
			 
