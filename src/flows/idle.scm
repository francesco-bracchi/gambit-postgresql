(##namespace ("postgresql/flows/idle#"))
(##include "~~/lib/gambit#.scm")

(include "../connection#.scm")
(include "../exception#.scm")
(include "../messages/io#.scm")
(include "../messages/frontend#.scm")
(include "../messages/backend#.scm")
(include "common#.scm")

(define (ignore-message . args)
  (handle-next-message))

(define idle-handler (make-handler-table ignore-message))

(define-handler-definer idle-handler)

(include "asynchronous.scm")
