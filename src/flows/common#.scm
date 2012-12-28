(##namespace 
 ("postgresql/flows/common#"
  resume
  default-handler
  current-connection-parameters
  send-message-and-wait
  resume-and-raise
  make-handler-table
  handler-table-ref
  handler-table-set!
  current-description
  current-rowset
  ))

(define-macro (send-message-and-wait msg #!optional (_connection '(current-connection)))
  (let ((connection (gensym 'connection))
	(port (gensym 'port))
	(message (gensym 'message))
	(type (gensym 'type))
	(value (gensym 'value)))
    `(let* ((,connection ,_connection)
	    (,port (connection-port ,connection)))
       (connection-consumer-set! ,connection (current-thread))
       (send-message ,msg (connection-port ,connection))
       (let* ((,message (thread-receive))
	      (,type (car ,message))
	      (,value (cdr ,message)))
	 (case ,type
	   ((exception) (raise ,value))
	   ((value) ,value)
	   (else (error "unknwon message type from reader")))))))

(define-macro (define-handler-definer name)
  (let* ((qname (list 'quote name))
	 (key (gensym 'key))
	 (rest (gensym 'rest)))
    `(define-macro (define-handler ,key . ,rest)
       (if (pair? ,key)
	   `(define-handler ,(car ,key) (lambda ,(cdr ,key) #;(pp (list ,@(cdr ,key))) ,@,rest))
	   `(handler-table-set! ,,qname ,,key ,(car ,rest))))))

