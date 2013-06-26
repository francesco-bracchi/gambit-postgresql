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
	   (execute 
	    line
	    reducer: (lambda (state . whatever)  (pp whatever) #f))
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
  `("gsi ~~postgresql/postgresql repl"))

(define (println s)
  (display s)
  (newline))

(define (main)
  (with-exception-catcher
   (lambda (ex)
     (pp ex)
     (for-each println help))
   repl))

(main)
