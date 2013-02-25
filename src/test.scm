(load "statprof")
(load "utils/queue")
(load "utils/type-readers")
(load "connection.scm")
(load "exception.scm")
(load "messages/io")
(load "messages/frontend")
(load "messages/backend")
(load "commands/startup")
(load "commands/execute")

(include "connection#.scm")
(include "exception#.scm")
(include "commands/execute#.scm")

(define (repeat n fn)
  (if (<= n 1) (fn)
      (begin (fn)
	     (repeat (- n 1) fn))))

(define (test)
  (with-connection 
   (list database: "notapipe"
	 username: "nap_user"
	 password: "LKZ3WC")
   (lambda () 
     (connection-execute 
      "SELECT * from accounts"
      initial-value: 0
      function: (lambda (count . whatever) 
		  (+ count 1)))
     (pp (table->list (connection-oid-table (current-connection)))))))


(define (test-notif
(test)
