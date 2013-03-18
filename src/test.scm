(load "utils/queue")
(load "utils/type-readers")
(load "connection.scm")
(load "exception.scm")
(load "messages/io")
(load "messages/frontend")
(load "messages/backend")
(load "commands/startup")
(load "commands/execute")
(load "commands/notification")

(include "connection#.scm")
(include "exception#.scm")
(include "commands/execute#.scm")
(include "commands/notification#.scm")

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

(define (receive-notification) 
  (with-connection
   (list database: "notapipe"
	 username: "nap_user"
	 password: "LKZ3WC")
   (lambda () 
     (pp `preparing-to-receive)
     (connection-execute "LISTEN papa")
     (pp (connection-notification))
     (pp 'received))))

(define (send-notification)
  (with-connection
   (list database: "notapipe"
	 username: "nap_user"
	 password: "LKZ3WC")
   (lambda () (connection-execute "NOTIFY papa"))))

(define (test-notification)
  (thread-start! (make-thread receive-notification))
  (thread-sleep! 3)
  (pp 'sending...)
  (send-notification)
  (pp 'sent)
  (thread-sleep! 10)
  (pp 'done))

(define (repl) 
  (with-connection
   (list database: "notapipe"
	 username: "nap_user"
	 password: "LKZ3WC")
   run))

(define (run)
  (display `(,(connection-database (current-connection)) =>))
  (let ((line (read-line)))
    (if (eof-object? line) 'DONE
	(with-exception-catcher
	 (lambda (ex) (pp ex) (run))
	 (lambda () 
	   (connection-execute 
	    line
	    function: (lambda (state . whatever)  (pp whatever) #f))
	   (pp `(<= ,(connection-status (current-connection))))
	   (run))))))

(repl)
