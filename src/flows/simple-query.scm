(##namespace ("postgresql/flows/simple-query#"))
(##include "~~/lib/gambit#.scm")

(include "../connection#.scm")
(include "../exception#.scm")
(include "../messages/io#.scm")
(include "../messages/frontend#.scm")
(include "../messages/backend#.scm")
(include "common#.scm")
(include "idle#.scm")

(define (ignore-message . args)
  (handle-next-message))

(define simple-query-handler (make-handler-table ignore-message))

(define empty-port 
  (let ((port (open-vector (vector))))
    (close-port port)
    port))

(define-handler-definer simple-query-handler)

(include "asynchronous.scm")

(define-handler (command-complete tag)
  (handle-next-message))

;; (define-handler (copy-in-response binary? columns types)
;;   (pp `(COPY IN (binary? ,binary?) ,(map cons columns type)))
;;   (handle-next-message))

;; (define-handler (copy-out-response binary? columns types)
;;   (pp `(COPY OUT (binary? ,binary?) ,(map cons columns type)))
;;   (handle-next-message))

(define-handler (row-description fields description)
  (pp `(ROW DESCRIPTION ,fields ,description))
  (receive (client server) (open-vector-pipe)
	   (table-set! (current-description-table) "simple-query" description)
	   (table-set! (current-port-table) "simple-query" server)
	   (resume client))
  (handle-next-message))

(define-handler (data-row columns row)
  (pp `(DATA ROW ,columns ,row))
  (let* ((connection (current-connection))
	 (port (table-ref (current-port-table) "simple-query"))
	 (description (table-ref (current-description-table) "simple-query")))
    ;; TODO get types out from descriptions
    (write row port))
  (handle-next-message))

(define-handler (empty-query-response)
  (pp `(EMPTY QUERY RESPONSE))
  (handle-next-message))

(define-handler (ready-for-query status)
  (pp `(READY FOR QUERY ,status))
  (let ((port (table-ref (current-port-table) "simple-query" #f)))
    (if port
	(begin (close-port port)
	       (table-set! (current-port-table) "simple-query"))
	(resume empty-port)))
  
  (connection-handler-table-set! (current-connection) idle-handler)
  (handle-next-message))

(define (simple-query string #!optional (connection (current-connection)))
  (connection-handler-table-set! connection simple-query-handler)
  (send-message-and-wait (query-message string) connection))
