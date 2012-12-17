(namespace 
 ("postgresql/messages/frontend#"
  send-message

  send-message-raw
  message-writer
))



(define-macro (send-message formals #!optional (port `(current-output-port)))
  (let ((type (car formals))
	(args (cdr formals)))
    `(send-message-raw ,type ((message-writer ,type) ,@args) ,port)))
