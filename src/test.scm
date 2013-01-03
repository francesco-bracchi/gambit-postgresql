(load "connection.scm")
(load "exception.scm")
(load "messages/io")
(load "messages/frontend")
(load "messages/backend")
(load "flow/handler")
(load "flow/startup")

;(include "messages/frontend#.scm")
(include "messages/backend#.scm")
(include "connection#.scm")
(include "exception#.scm")

(define (repeat n fn)
  (if (<= n 1) (fn)
      (begin (fn)
	     (repeat (- n 1) fn))))

;; (time (repeat 2 (lambda () 
;; 		  (open-connection database: "notapipe" 
;; 				   username: "nap_user" 
;; 				   password: "LKZ3WC"))))

(define (test)
  (with-exception-catcher
   (lambda (ex)
     (pp ex)
     (cond
      ((unsupported-authentication-method-exception? ex)
       (pp (postgresql-exception-connection ex)))))
   (lambda () 
     (with-connection 
      (list database: "notapipe"
	    username: "nap_user"
	    password: "LKZ3WC")
      (lambda ()
	(pp (current-connection))
	(close-port (connection-port (current-connection))))))))

(time (repeat 100 test))
