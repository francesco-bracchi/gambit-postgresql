(##namespace ("postgresql/startup#"))

(##include "~~lib/gambit#.scm")

(define ProtocolVersion 196608)

(define RequestOk 0)

(define RequestClearPassword 3)

(define (send-startup-message database user #!optional (port (current-output-port)))
  (let ((len (+ 25 (string-length name) (string-length user))))
    (send-int32 len)
    (send-int32 ProtocolVersion)
    (send-string "user")
    (send-string user)
    (send-string "database")
    (send-string database)
    (send-char #\nul)
    (force-output)))

(define (startup-handler connection)
  (let ((handler (make-message-handler)))
    (



  
