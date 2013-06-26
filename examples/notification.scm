(include "params.scm")
(include "~~postgresql/postgresql#.scm")

(define (with/conn fn) 
  (with-connection
   (list database: (database) 
	 username: (username)
	 password: (password)
	 port-number: (port-number)
	 server-address: (server-address))
   fn))

(define *channel* "postgresqlgambitch")

(define *payload* "'ping'")

(define (wait)
  (connection-query (string-append "LISTEN " *channel*))
  (pp `(received ,(connection-notification)))
  (exit))

(define (send)
  (connection-query (string-append "NOTIFY " *channel* "," *payload*)))

(define (main)
  (thread-start!
   (make-thread 
    (lambda () 
      (with-exception-catcher
       pp
       (lambda () 
	 (pp `(a message will be sent in 5 seconds))
	 (thread-sleep! 5)
	 (with/conn send))))))
  (with-exception-catcher
   pp
   (lambda () (with/conn wait))))

(main)
