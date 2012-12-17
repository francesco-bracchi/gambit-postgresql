(##namespace ("postgresql/connect#"))
(##include "~~lib/gambit#.scm")

(define current-connection (make-parameter #f))

(define current-reader-table (make-parameter #f))

(define (connect #!key 
		 (database-name "default")
		 (user "root")
		 (password "")
		 (hostname "localhost")
		 (port-number 5432)
		 (char-encoding 'UTF-8)
		 (reader-table (current-reader-table)))
  (let*((port (open-tcp-client (list server-address: hostname
				     port-number: port-number)))
	(parameterize
	 ((current-input-port port)
	  (current-output-port port)
	  (current-connection connection)
	  (current-handler-table startup-handler-table)
	 (startup-connection)
	 (if (not (current-reader-table)) (init-current-reader-table connection))
	 (current-handler-table default-handler-table)
	 
	
