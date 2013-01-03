;; included from backend.scm

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
    (list pid channel payload)))

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
