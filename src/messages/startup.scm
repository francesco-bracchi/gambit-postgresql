(##namespace ("postgresql/startup#"))
(##include "~~lib/gambit#.scm")

(include "io#.scm")
(include "frontend#.scm")
(include "backend#.scm")

(define protocol-version 196608)

(define request-ok 0)

(define request-clear-password 3)

(define (send-startup-message database user #!optional (port (current-output-port)))
  (let ((len (+ 25 (string-length name) (string-length user))))
    (send-int32 len port)
    (send-int32 ProtocolVersion port)
    (send-string "user" port)
    (send-string user port)
    (send-string "database" port)
    (send-string database port)
    (send-char #\nul port)
    (force-output port)))

(define (startup-handler connection)
  (let ((handler (make-message-handler)))
    (



  
