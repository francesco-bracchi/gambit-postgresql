(##namespace ("postgresql/flows/close#"))
(##include "~~/lib/gambit#.scm")

(include "../connection#.scm")
(include "../exception#.scm")
(include "../messages/io#.scm")
(include "../messages/frontend#.scm")
(include "../messages/backend#.scm")
(include "common#.scm")

(define (postgresql/flows/close#close #!optional (connection (current-connection)))
  (let ((port (connection-port connection)))
    (send-message (terminate) port)
    (close-port port)))
