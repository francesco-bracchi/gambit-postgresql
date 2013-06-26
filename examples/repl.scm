(include "~~postgresql/postgresql#.scm")

(define (get-parameter name)
  (cond
   ((member name (command-line)) => cadr)
   (else #f)))

(define (database) 
  (or (get-parameter "-d") "postgres"))

(define (username)
  (or (get-parameter "-u") "postgres"))

(define (password)
  (or (get-parameter "-x") ""))

(define (server-address)
  (or (get-parameter "-s") "localhost"))

(define (port-number)
  (or (get-parameter "-p") 5432))

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
	 server-address: (server-address)
   run))

(define help
  `("gsi ~~postgresql/postgresql repl -d <database> -u <username> -x <password> -p <port number> -s <database>"))

(define (main)
  (with-exception-catcher
   (lambda (ex)
     (pp ex)
     (pp help))
   repl))
