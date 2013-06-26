
(define (test-notification)
  (with-connection
   (list database: "notapipe"
	 username: "nap_user"
	 password: "LKZ3WC")
   (lambda () 
     (connection-query "LISTEN papa")
     (thread-sleep! 10)
     (pp (connection-notification)))))
