;; TODO describe each tag as part of meta information provided for maintainance 

(##namespace ("postgresql/messages/backend#"))
(##include "~~lib/gambit#.scm")
(include "io#.scm")
(include "messages#.scm")
(include "backend#.scm")

(define-tag-set backend
  (authentication          #\R)
  (backend-key-data        #\K)
  (bind-complete           #\2)
  (close-complete          #\3)
  (command-complete        #\C)
  (copy-data               #\d)
  (copy-done               #\c)
  (copy-fail               #\f)
  (copy-in-response        #\G)
  (copy-out-response       #\H)
  (copy-both-response      #\W)
  (data-row                #\D)
  (empty-query-response    #\I)
  (error-response          #\E)
  (function-call-response  #\V)
  (no-data                 #\n)
  (notice-response         #\N)
  (notification-response   #\A)
  (parameter-description   #\t)
  (parameter-status        #\S)
  (parse-complete          #\1)
  (portal-suspended        #\s)
  (ready-for-query         #\Z)
  (row-description         #\T))

(define (field-descriptor-binary? fd)
  (not (field-descriptor-text? fd)))

(define (buffer->data buffer reader) 
  (call-with-input-u8vector 
   buffer
   (lambda (data-port)
     (parameterize 
      ((current-input-port data-port))
      (reader (u8vector-length buffer))))))

;; TODO: rename to recv-message
(define (read-message #!optional (port (current-input-port)))
  (let* ((code (read-u8 port))
	 (length (recv-int32 port))
	 (buffer (recv-bytes (- length 4) port))
	 (reader (vector-ref *message-readers* (- code backend/lo)))
	 (data (buffer->data buffer reader)))
    ;(pp `(RECEIVING (,(backend/code->name code) ,@data)))
    (make-message (backend/code->name code) code data)))

(define (repeat n fn)
  (let repeat ((j 0) (rs '()))
    (if (>= j n)
	(reverse rs)
	(repeat (+ j 1) (cons (fn) rs)))))

(define (recv-data-pair #!optional (port (current-input-port)))
  (let ((len (recv-int32 port)))
    (if (= len -1) #\nul
	(recv-bytes len port))))

(define (int->bool j) (= j 0))   

(define (recv-error-assoc #!optional (port (current-input-port)))
  (let recv ((pairs '()))
    (let ((c (read-u8 port)))
      (if (or (eq? c #!eof) (= c 0))
	  (reverse pairs)
	  (let* ((key (field-type c))
		 (value (recv-string port))
		 (pair (cons key value)))
	    (recv (cons pair pairs)))))))

(define (map->function alist)
  (let* ((keys (map car alist))
	 (values (map cdr alist))
	 (lo (apply min keys))
	 (hi (+ 1 (apply max keys)))
	 (vector (make-vector (- hi lo))))
    (map (lambda (key value) (vector-set! vector (- key lo) value)) keys values)
    (lambda (key) (vector-ref vector (- key lo)))))

(define field-type 
  (map->function 
   (map (lambda (p) (cons (char->integer (car p)) (cdr p)))
	'((#\S . severity)
	  (#\C . sql-state)
	  (#\M . message)
	  (#\D . detail)
	  (#\H . hint)
	  (#\P . position)
	  (#\p . internal-position)
	  (#\q . internal-query)
	  (#\W . where)
	  (#\F . file)
	  (#\L . line)
	  (#\R . routine)))))

;; (define (field-type c)
;;   ;; put this in a vector reference
;;   (cond
;;    ((= c (char-code #\S)) 'severity)
;;    ((= c (char-code #\C)) 'sql-state)
;;    ((= c (char-code #\M)) 'message)
;;    ((= c (char-code #\D)) 'detail)
;;    ((= c (char-code #\H)) 'hint)
;;    ((= c (char-code #\P)) 'position)
;;    ((= c (char-code #\p)) 'internal-position)
;;    ((= c (char-code #\q)) 'internal-query)
;;    ((= c (char-code #\W)) 'where)
;;    ((= c (char-code #\F)) 'file)
;;    ((= c (char-code #\L)) 'line)
;;    ((= c (char-code #\R)) 'routine)
;;    (else (string->symbol (string-append "unknown-" (string c))))))

(define (recv-description #!optional (port (current-input-port)))
  (let* ((name (recv-string port))
	 (table (recv-int32 port))
	 (column (recv-int16 port))
	 (type-id (recv-int32 port))
	 (size (recv-int16 port))
	 (modifier (recv-int32 port))
	 (mode (recv-int16 port)))
    (make-field-descriptor name 
			   (if (= table 0) #f table)
			   (if (= column 0) #f column)
			   type-id
			   size 
			   modifier
			   (= mode 0))))

(define *message-readers* (make-vector (- backend/hi backend/lo -1)))

(define-macro (define-message-reader header . body)
  (let* ((name (car header))
	 (formals (cdr header)))
    `(vector-set! *message-readers*  
		  (- (backend/name->code ',name) backend/lo)
		  (lambda ,formals ,@body))))

(include "message-readers.scm")
