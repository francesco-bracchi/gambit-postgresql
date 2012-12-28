;; TODO describe each tag as part of meta information provided for maintainance 

(##namespace ("postgresql/messages/backend#"))
(##include "~~lib/gambit#.scm")
(include "io#.scm")

(define-macro (char-code c) (char->integer c))

(define-macro (lo) (char->integer #\0))

(define-macro (hi) (+ 1 (char->integer #\z)))

(define *-tags-* '())

(define-macro (define-tag name char)
  `(begin 
     (define ,name (char-code ,char))
     (set! *-tags-* (cons ,name *-tags-*))))

(define-tag authentication          #\R)
(define-tag backend-key-data        #\K)
(define-tag bind-complete           #\2)
(define-tag close-complete          #\3)
(define-tag command-complete        #\C)
(define-tag copy-data               #\d)
(define-tag copy-done               #\c)
(define-tag copy-fail               #\f)
(define-tag copy-in-response        #\G)
(define-tag copy-out-response       #\H)
(define-tag copy-both-response      #\W)
(define-tag data-row                #\D)
(define-tag empty-query-response    #\I)
(define-tag error-response          #\E)
(define-tag function-call-response  #\V)
(define-tag no-data                 #\n)
(define-tag notice-response         #\N)
(define-tag notification-response   #\A)
(define-tag parameter-description   #\t)
(define-tag parameter-status        #\S)
(define-tag parse-complete          #\1)
(define-tag portal-suspended        #\s)
(define-tag ready-for-query         #\Z)
(define-tag row-description         #\T)

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

(define (really-handle-next-message handler #!optional (port (current-input-port)))
  (let* ((type (read-u8 port))
	 (length (recv-int32 port))
	 (data (recv-bytes (- length 4) port)))
    (handler type data)))

(define *message-readers* (make-vector (- (hi) (lo))))

(define-macro (define-message-reader header . body)
  (let* ((key (car header))
	 (formals (cdr header)))
    `(vector-set! *message-readers*  
		  (- ,key (lo))
		  (lambda ,formals ,@body))))

(define-macro (message-reader type)
  `(vector-ref *message-readers* (- ,type (lo))))

(define current-message-handler (make-parameter (lambda (type) (error "empty message handler"))))

(define current-reader-port (make-parameter (current-input-port)))

(define (handle-next-message #!optional
			     (port (current-reader-port))
			     (message-handler (current-message-handler)))
  (really-handle-next-message 
   (lambda (type data)
     (call-with-input-u8vector 
      data
      (lambda (data-port) 
	(parameterize ((current-input-port data-port))
		      ((message-reader type) (u8vector-length data) (message-handler type))))))
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

(define-message-reader (authentication length handler)
  (handler (recv-int 4)))

(define-message-reader (backend-key-data length handler)
  (let* ((pid (recv-int 4))
	 (secret (recv-int 4)))
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
  (let* ((code (read-u8))
	 (transaction-status 
	  (cond
	   ((= code (char-code #\I)) 'idle)
	   ((= code (char-code #\T)) 'transaction)
	   ((= code (char-code #\E)) 'failed-transaction)
	   (else 'unknown))))
    (handler transaction-status)))

(define-message-reader (row-description length handler)
  (let* ((fields (recv-int16))
	 (descriptions (repeat fields recv-description)))
    (handler fields descriptions)))
