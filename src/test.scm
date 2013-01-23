(load "utils/queue")
(load "connection.scm")
(load "exception.scm")
(load "messages/io")
(load "messages/frontend")
(load "messages/backend")
(load "flow/handler")
(load "flow/notifications")
(load "flow/startup")
(load "flow/cleanup")
(load "flow/simple-query")
(load "flow/extended-query")

;(include "messages/frontend#.scm")
(include "messages/backend#.scm")
(include "connection#.scm")
(include "exception#.scm")


(include "flow/notifications#.scm")
(include "flow/simple-query#.scm")
(include "flow/extended-query#.scm")

(define (repeat n fn)
  (if (<= n 1) (fn)
      (begin (fn)
	     (repeat (- n 1) fn))))

(define (test)
  (with-exception-catcher
   (lambda (ex)
     (pp ex)
     (cond
      ((unsupported-authentication-method-exception? ex)
       (pp (postgresql-exception-connection ex)))
      (else (raise ex))))
   (lambda () 
     (with-connection 
      (list database: "notapipe"
	    username: "nap_user"
	    password: "LKZ3WC")
      (lambda ()
	(simple-query "SELECT typname, oid FROM pg_type"
		      function: (lambda (status type oid) (pp `(NAME ,type ,oid)) status))
	(pp (simple-query "LISTEN papa"))
	(pp `(WAITING FOR NOTIFICATION ON CHANNEL papa))
	(pp (recv-notification)))))))

(define (test1)
  (with-connection 
   (list database: "notapipe"
	 username: "nap_user"
	 password: "LKZ3WC")
   (lambda ()
     (pp (current-connection))
     (let ((next (simple-query-generator "SELECT typname, oid FROM pg_type")))
       (do ((j (next) (next)))
	   ((eof-object? j) 'HAHA)
	 (pp `(J ,j)))))))


(define (test2)
  (with-connection 
   (list database: "notapipe"
	 username: "nap_user"
	 password: "LKZ3WC")
   (lambda ()
     (pp 'ciao)
     (pp (execute "SELECT typname, oid FROM pg_type"
		  function: (lambda (state typename oid) (cons (list 'HAHA typename oid) state)) )))))

;;(time (test))
;;(time (test1))
(time (test2))
