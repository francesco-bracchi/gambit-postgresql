;; TODO describe each tag as part of meta information provided for maintainance 

(##namespace ("postgresql/messages/backend#"))
(##include "~~lib/gambit#.scm")

(include "io#.scm")
;(include "messages#.scm")
(include "backend#.scm")

(declare (standard-bindings)
	 (extended-bindings)
	 (fixnum)
	 (block))

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

(define *message-readers* (make-vector (- backend/hi backend/lo -1)))

;; TODO: rename to recv-message
(define (read-message #!optional (port (current-input-port)))
  (let* ((code (read-u8 port))
	 (length (recv-int32 port))
	 (buffer (recv-bytes (- length 4) port))
	 (reader (vector-ref *message-readers* (- code backend/lo)))
	 (data (buffer->data buffer reader)))
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

;; ## Message readers definition

(define-macro (define-message-reader header . body)
  (let* ((name (car header))
	 (formals (cdr header)))
    `(vector-set! *message-readers*  
		  (- (backend/name->code ',name) backend/lo)
		  (lambda ,formals ,@body))))

(define-message-reader (authentication length)
  (let ((code (recv-int32)))
    (list code)))

(define-message-reader (backend-key-data length)
  (let* ((pid (recv-int32))
	 (secret (recv-int32)))
    (list pid secret)))

(define-message-reader (bind-complete length)
  '())

(define-message-reader (close-complete length)
  '())

(define-message-reader (command-complete length)
  (let* ((command-tag (recv-string)))
    (list command-tag)))

(define-message-reader (copy-data length)
  (let* ((data (recv-bytes length)))
    (list data)))

(define-message-reader (copy-done length)
  '())

(define-message-reader (copy-fail length)
  (let* ((message (recv-string)))
    (list message)))

(define-message-reader (copy-in-response length)
  (let* ((type (recv-int8))
	 (columns (recv-int16))
	 (column-types (repeat columns recv-int16)))
    (list (int->bool type)
	     columns
	     (map int->bool column-types))))

(define-message-reader (copy-out-response length)
  (let* ((type (recv-int8))
	 (columns (recv-int16))
	 (column-types (repeat columns recv-int16)))
    (list (int->bool type)
	  columns
	  (map int->bool column-types))))

(define-message-reader (copy-both-response length)
  (let* ((type (recv-int8))
	 (columns (recv-int16))
	 (column-types (repeat columns recv-int16)))
    (list (int->bool type)
	  columns
	  (map int->bool column-types))))

(define-message-reader (data-row length)
  (let* ((columns (recv-int16))
	 (row (repeat columns recv-data-pair)))
    (list columns row)))

(define-message-reader (empty-query-response length)
  '())

(define-message-reader (error-response length)
  (let* ((pairs (recv-error-assoc)))
    (list pairs)))

(define-message-reader (function-call-response length)
  (let* ((data-length (recv-int16)))
    (if (= data-length -1)
	`(#!void)
	(let ((data (recv-bytes data-length)))
	  (list data)))))

(define-message-reader (no-data length)
  '())

(define-message-reader (notice-response length)
  (let* ((pairs (recv-error-assoc)))
    (list pairs)))

(define-message-reader (notification-response length)
  (let* ((pid (recv-int32))
	 (channel (recv-string))
	 (payload (recv-string)))
    (list pid channel (if (eof-object? payload) "" payload))))

(define-message-reader (parameter-description)
  (let* ((parameters (recv-int16))
	 (oids (repeat parameters recv-int32)))
    (list parameters oids)))

(define-message-reader (parameter-status length)
  (let* ((name (recv-string))
	 (value (recv-string)))
    (list name value)))

(define-message-reader (parse-complete length)
  '())

(define-message-reader (portal-suspended length)
  '())

(define-macro (char-code c) (char->integer c))

(define-message-reader (ready-for-query length)
  (let* ((code (read-u8))
	 (transaction-status 
	  (cond
	   ((= code (char-code #\I)) 'idle)
	   ((= code (char-code #\T)) 'transaction)
	   ((= code (char-code #\E)) 'failed-transaction)
	   (else 'unknown))))
    (list transaction-status)))

(define-message-reader (row-description length)
  (let* ((fields (recv-int16))
	 (descriptions (repeat fields recv-description)))
    (list fields descriptions)))
