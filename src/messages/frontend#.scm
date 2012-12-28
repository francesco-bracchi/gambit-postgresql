(namespace 
 ("postgresql/messages/frontend#"

  current-writer-port
  send-message-raw
  message-writer
  
  ;; message types
  bind
  close
  copy-data
  copy-done
  copy-fail
  describe
  execute
  flush 
  function-call
  parse
  password
  query-message
  sync
  terminate
  send-startup-message
  
  ;; macros
  send-message
  message->u8vector))

;; (message->u8vector (bind a b c)

(define-macro (message->u8vector message)
  (let ((port (gensym 'port))
	(type (car message))
	(args (cdr message)))
    `(call-with-output-u8vector
      (u8vector)
      (lambda (,port) 
	(parameterize 
	 ((current-output-port ,port))
	 ((message-writer ,type) ,@args))))))

(define-macro (send-message message #!optional (port `(current-writer-port)))
  (let ((type (car message))
	(args (cdr message)))
    (if (eq? type 'startup)
	`(send-startup-message ,@(append args (list port)))
	`(send-message-raw ,type (message->u8vector ,message) ,port))))

