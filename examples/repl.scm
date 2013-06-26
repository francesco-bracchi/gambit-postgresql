;; # REPL

;; Just a simple repl tool like psql, but simpler

(include "~~postgresql/postgresql#.scm")
(include "params.scm")

(define (prompt)
  `(,(connection-database (current-connection)) =>))

(define (run)
  (display (prompt))
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

(define (repl) 
  (with-connection
   (list database: (database) 
	 username: (username)
	 password: (password)
	 port-number: (port-number)
	 server-address: (server-address))
   run))

(define help
  `("gsi ~~postgresql/postgresql repl <options>"
    "options can be: "
    "-d <database> "
    "-u <username> "
    "-x <password> "
    "-p <port number> "
    "-s <database>"))

(define (println s)
  (display s)
  (newline))

(define (main)
  (with-exception-catcher
   (lambda (ex)
     (pp (wrong-number-of-arguments-exception-procedure ex))
     (pp ex)
     (for-each println help))
   repl))

(main)
