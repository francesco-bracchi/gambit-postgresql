
(define database (make-parameter "postgres"))

(define username (make-parameter "postgres"))

(define password (make-parameter ""))

(define server-address (make-parameter "localhost"))

(define port-number (make-parameter 5432))

(define (read-parameter name)
  (display name)
  (display "(default ")
  (display (eval `(,name)))
  (display "): ")
  (let ((val (read-line)))
    (cond
     ((equal? val "") 
      (set! val (eval `(,name))))
     ((eq? name 'port-number)
      (set! val (string->number val))))
    (eval `(,name ,val))
    val))
  
(for-each read-parameter '(database username password server-address port-number))

