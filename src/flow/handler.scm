(##namespace ("postgresql/flow/handler#"))
(##include "~~lib/gambit#.scm")
(include "../messages/messages#.scm")
(include "../messages/backend#.scm")

(define current-handler-table (make-parameter (make-table)))

(define (handle-message message #!optional (table (current-handler-table)))
  (let* ((name (message-name message))
	 (handler (table-ref table name)))
    (apply handler (message-values message))))

(define (handle-next-message #!optional 
			     (table (current-handler-table))
			     (port (current-input-port)))
  (handle-message (read-message port) table))
