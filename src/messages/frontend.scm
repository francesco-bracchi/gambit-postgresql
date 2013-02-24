(##namespace ("postgresql/messages/frontend#"))
(##include "~~lib/gambit#.scm")

(include "io#.scm")
(include "messages#.scm")

(define-tag-set frontend 
  (startup            #\s)
  (bind               #\B)
  (close              #\C)
  (copy-data          #\d)
  (copy-done          #\b)
  (copy-fail          #\f)
  (describe           #\D)
  (execute            #\E)
  (flush              #\H)
  (function-call      #\F)
  (parse              #\P)
  (password           #\p)
  (query              #\Q)
  (sync               #\S)
  (terminate          #\X))

(define *message-writers* (make-vector (- frontend/hi frontend/lo -1)))

(define (message->u8vector message)
  (call-with-output-u8vector
   (u8vector)
   (lambda (port)
     (parameterize 
      ((current-output-port port))
      (apply (message-writer message) (message-values message))))))

(define (message-writer message)
  (vector-ref *message-writers* (- (message-code message) frontend/lo)))

(define (send-code-and-body code body #!optional (port (current-output-port)))
  (write-u8 code port)
  (send-int32 (+ 4 (u8vector-length body)) port)
  (send-bytes body port)
  (force-output port))

(define (send-message message #!optional (port (current-output-port)))
  ;;(pp `(SENDING (,(message-name message) ,@(message-values message))))
  (parameterize
   ((current-output-port port))
   (if (eq? (message-name message) 'startup)
       (apply send-startup-message (message-values message))
       (send-code-and-body (message-code message) (message->u8vector message)))
   (force-output port)))

(define-macro (define-message name)
  (let ((values (gensym 'values)))
    `(define (,name . ,values) 
       (make-message ',name (frontend/name->code ',name) ,values))))

(define-macro (define-message-writer head . body)
  (let ((name (car head))
	(args (cdr head)))
    `(begin (define-message ,name)
	    (vector-set! *message-writers*
			 (- (frontend/name->code ',name) frontend/lo)
			 (lambda ,args ,@body)))))

(define-message startup)

(define-macro (protocol-version) 196608)

(define (send-startup-message database user #!optional (port (current-output-port)))
  (let ((len (+ 25 (string-length database) (string-length user))))
    (send-int32 len port)
    (send-int32 (protocol-version) port)
    (send-string "user" port)
    (send-string user port)
    (send-string "database" port)
    (send-string database port)
    (send-char #\nul port)
    (force-output port)))
		     
(include "message-writers.scm")

