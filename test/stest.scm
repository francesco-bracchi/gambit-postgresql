  
(define-structure suite
  name
  (description unprintable:)
  (handler unprintable:)
  (state unprintable:))


(define my-suite (with-meta 
		  (description: "ciao")
		  (whatever: dsadsad))
  
  
(suite () 

(define-macro (suite formals . body)
  `(let ((,self (lambda ,formals ,@body)))
     



   
(define my-suite
  (make-suite setup: (lambda () (connect! foo bar))
	      teardown: (lambda () (disconnect!))
	      hanlder: (lambda () 
		    
