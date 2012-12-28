(##namespace ("postgresql/flows/copy#"))
(##include "~~/lib/gambit#.scm")

(include "../connection#.scm")
(include "../exception#.scm")
(include "../messages/io#.scm")
(include "../messages/frontend#.scm")
(include "../messages/backend#.scm")
(include "common#.scm")

(define (ignore-message . args)
  (handle-next-message))

(define copy-handler (make-handler-table ignore-message))

(define-handler-definer copy-handler)

(define current-description (make-parameter #f))
(define current-rowset (make-parameter #f))

(include "asynchronous.scm")

(define-handler (error-response pairs)
  (resume-and-raise (make-backend-exception pairs)))
