(##namespace ("postgresql/messages/backend#"))
(##include "~~lib/gambit#.scm")
(include "io#.scm")

(define-structure field-descriptor
  name
  table
  index
  type
  type-size
  modifier
  text?)

(define (field-descriptor-binary? fd)
  (not (field-descriptor-text? fd)))
	     
;; this file contains raw backend handlers,
;; for each message type (defined by the first character received
;; there is an associated handler.

(define (really-recv-message handler #!optional (port (current-input-port)))
  (let* ((type (read-u8 port))
	 (length (recv-int32 port))
	 (data (recv-bytes (- length 5) port)))
    (handler type data port)))

(define-macro (lo) (char->integer #\0))

(define-macro (hi) (+ 1 (char->integer #\z)))

(define *message-readers* (make-vector (- (hi) (lo))))

(define-macro (define-message-reader header . body)
  (let* ((key (car header))
	 (formals (cdr header)))
    `(vector-set! *message-readers*  
		  (- (char->integer ,key) (lo))
		  (lambda ,formals ,@body))))

(define-macro (message-reader type)
  `(vector-ref *message-readers* (- ,type (lo))))
  
(define (raise-error . args) (error "handler not specified"))

(define (make-handler-table)
  (make-vector (- (hi) (lo)) raise-error))

(define (handler-table-set! table type handler)
  (vector-set! table type handler))

(define (handler-table-ref table type)
  (vector-ref table type))

(define current-handler-table (make-parameter (make-handler-table)))

(define (recv-message #!optional
		      (table (current-handler-table))
		      (port (current-input-port)))
  (really-recv-message 
   (lambda (type data)
     (call-with-input-u8vector 
      data
      (lambda (port) 
	(parameterize ((current-input-port port))
		      ((message-reader type) (u8vector-length data) (handler-table-ref table type))))))
   port))

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
      (if (= c 0) pairs
	  (let* ((key (field-type c) )
		 (value (recv-string port))
		 (pair (cons key value)))
	    (recv (cons pair pairs)))))))

(define (field-type c)
  ;; put this in a vector reference
  (cond
   ((char=? c #\S) 'severity)
   ((char=? c #\C) 'sql-state)
   ((char=? c #\M) 'message)
   ((char=? c #\D) 'detail)
   ((char=? c #\H) 'hint)
   ((char=? c #\P) 'position)
   ((char=? c #\p) 'internal-position)
   ((char=? c #\q) 'internal-query)
   ((char=? c #\W) 'where)
   ((char=? c #\F) 'file)
   ((char=? c #\L) 'line)
   ((char=? c #\R) 'routine)
   (else (string->symbol (string-append "unknown-" (string c))))))

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


(define authentication          #\R)
(define backend-key-data        #\K)
(define bind-complete           #\2)
(define close-complete          #\3)
(define command-complete        #\C)
(define copy-data               #\d)
(define copy-done               #\c)
(define copy-fail               #\f)
(define copy-in-response        #\G)
(define copy-out-response       #\H)
(define copy-both-response      #\W)
(define data-row                #\D)
(define empty-query-response    #\I)
(define error-response          #\E)
(define function-call-response  #\V)
(define no-data                 #\n)
(define notice-response         #\N)
(define notification-response   #\A)
(define parameter-description   #\t)
(define parameter-status        #\S)
(define parse-complete          #\1)
(define portal-suspended        #\s)
(define ready-for-query         #\Z)
(define row-description         #\T)

(define-message-reader (authentication length handler)
  (handler (recv-int32)))

(define-message-reader (backend-key-data length handler)
  (let* ((pid (recv-int32))
	 (secret (recv-int32)))
    (handler pid secret)))

(define-message-reader (bind-complete length handler)
  (handler))

(define-message-reader (close-complete length handler)
  (handler))

(define-message-reader (command-complete length handler)
  (let* ((command-tag (recv-string)))
    (handler command-tag)))

(define-message-reader (copy-data length handler)
  (let* ((data (recv-bytes length)))
    (handler data)))

(define-message-reader (copy-done length handler)
  (handler))

(define-message-reader (copy-fail length handler)
  (let* ((message (recv-string)))
    (handler message)))

(define-message-reader (copy-in-response length handler)
  (let* ((type (recv-int8))
	 (columns (recv-int16))
	 (column-types (repeat columns recv-int16)))
    (handler (int->bool type)
	     columns
	     (map int->bool column-types))))

(define-message-reader (copy-out-response length handler)
  (let* ((type (recv-int8))
	 (columns (recv-int16))
	 (column-types (repeat columns recv-int16)))
    (handler (int->bool type)
	     columns
	     (map int->bool column-types))))

(define-message-reader (copy-both-response length handler)
  (let* ((type (recv-int8))
	 (columns (recv-int16))
	 (column-types (repeat columns recv-int16)))
    (handler (int->bool type)
	     columns
	     (map int->bool column-types))))

(define-message-reader (data-row length handler)
  (let* ((columns (recv-int16))
	 (row (repeat columns recv-data-pair)))
    (handler columns row)))

(define-message-reader (empty-query-response length handler)
  (handler))

(define-message-reader (error-response length handler)
  (let* ((pairs (recv-error-assoc)))
    (handler pairs)))

(define-message-reader (function-call-response length handler)
  (let* ((data-length (recv-int16)))
    (if (= data-length -1)
	(handler #!void)
	(let ((data (recv-bytes data-length)))
	  (handler data)))))

(define-message-reader (no-data length handler)
  (handler))

(define-message-reader (notice-response length handler)
  (let* ((pairs (recv-error-assoc)))
    (handler pairs)))

(define-message-reader (notification-response length handler)
  (let* ((pid (recv-int32))
	 (channel (recv-string))
	 (payload (recv-string)))
    (handler pid channel payload)))

(define-message-reader (parameter-description handler)
  (let* ((parameters (recv-int16))
	 (oids (repeat parameters recv-int32)))
    (handler parameters oids)))

(define-message-reader (parameter-status length handler)
  (let* ((name (recv-string))
	 (value (recv-string)))
    (handler name value)))

(define-message-reader (parse-complete length handler)
  (handler))

(define-message-reader (portal-suspended length handler)
  (handler))

(define-message-reader (ready-for-query length handler)
  (let* ((transaction-status (read-u8)))
    (handler transaction-status)))

(define-message-reader (row-description length handler)
  (let* ((fields (recv-int16))
	 (descriptions (repeat fields recv-description)))
    (handler fields descriptions)))
