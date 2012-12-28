(##namespace ("postgresql/messages/frontend#"))
(##include "~~lib/gambit#.scm")

(include "io#.scm")

(define-macro (lo) (char->integer #\0))

(define-macro (hi) (+ 1 (char->integer #\z)))


(define-macro (define-tag name char)
  `(define ,name ,(char->integer char)))

(define-tag bind                    #\B)
(define-tag close                   #\C)
(define-tag copy-data               #\d)
(define-tag copy-done               #\b)
(define-tag copy-fail               #\f)
(define-tag describe                #\D)
(define-tag execute                 #\E)
(define-tag flush                   #\H)
(define-tag function-call           #\F)
(define-tag parse                   #\P)
(define-tag password                #\p)
(define-tag query-message           #\Q)
(define-tag sync                    #\S)
(define-tag terminate               #\X)

(define current-writer-port (make-parameter (current-output-port)))

(define (send-message-raw type body #!optional (port (current-output-port)))
  (write-u8 type port)
  (send-int32 (+ 4 (u8vector-length body)) port)
  (send-bytes body port)
  (force-output port))

(define *message-writers* (make-vector (- (hi) (lo)) (lambda _ (raise "undefined writer"))))

(define (message-writer type)
  (vector-ref *message-writers* (- type (lo))))

(define-macro (define-message-writer header . body)
  (let* ((key (car header))
	 (formals (cdr header)))
    `(begin
       (vector-set! *message-writers* 
		    (- ,key (lo))
		    (lambda ,formals ,@(if (null? body) '(u8vector) body))))))

;; (send-message (bind a b c))
;; (send-message (bind a b c) port)

;; SPECIAL CASE TREAT SEPARATELY (define cancel-request         #\?)

(define (same? list #!optional (test? eq?))
  (let same ((c (car list)) (cs (cdr list)))
    (cond
     ((null? cs) c)
     ((test? c (car cs)) (same c (cdr cs)))
     (else #f))))
	    
(define (send-binding-formats formats)
  (cond
   ((null? formats) (send-int 2 0))
   ((same? formats =) (send-int 2 (car formats)))
   (else (send-int 2 (length formats))
	 (for-each (lambda (format) (send-int 2 format)) formats))))

(define (send-result-formats formats)
  (cond
   ((null? formats) (send-int 2 0))
   ((and (= 1 (length formats)) (text-format? (car formats)))
    (send-int 2 0))
   ((same? formats =) (send-int 2 (car formats)))
   (else (send-int 2 (length formats))
	 (for-each (lambda (format) (send-int 2 format)) formats))))

(define (send-binding-values values #!optional (port (current-output-port)))
  (for-each send-binding-value values port))

(define (send-binding-value value port)
  (cond
   ((null? value) (send-int 4 -1 port))
   ((u8vector? value) 
    (send-int 4 (u8vector-length value) port)
    (send-bytes value port))
   (else (error "wrong value, must be u8vector or null"))))

(define (send-type type #!optional (port (current-output-port)))
  (send-char (cond
	      ((eq? type 'statement) #\s)
	      ((eq? type 'portal) #\p)
	      (else (error "unknown close type" type)))
	     port))

(define-message-writer (bind portal source bindings results)
    (send-string portal)
    (send-string source)
    (send-binding-formats (map bindings-format bindings))
    (send-int 2 (length bindings))
    (send-binding-values (map bindings-value bindings))
    (send-result-formats (map bindings-format results)))

(define-message-writer (close type name)
  (send-type type)
  (send-string name))

(define-message-writer (copy-data data)
  (send-bytes data))

(define-message-writer (copy-done)
  'done)

(define-message-writer (copy-fail reason)
  (send-string reason))

(define-message-writer (describe type portal-or-statement)
  (send-type type)
  (send-string portal-or-statement))

(define-message-writer (execute portal-or-statement max)
  (send-string portal-or-statement)
  (send-int 4 max))

(define-message-writer (flush)
  'flush)

(define-message-writer (function-call fid arguments result)
  (send-int 4 fid)
  (send-bindings-formats (map bindings-format arguments))
  (send-int 2 (length arguments))
  (send-bindings-values (map bindings-value arguments))
  (send-int (bindings-format result)))

(define-message-writer (parse name query parameter-types)
  (send-string name)
  (send-string query)
  (for-each send-parameter-type parameter-types))

(define-message-writer (password pwd)
  (send-string pwd))

(define-message-writer (query-message q)
  (send-string q))

(define-message-writer (sync)
  'sync)

(define-message-writer (terminate)
  'terminate)

(define-macro (protocol-version) 196608)

(define (send-startup-message database user #!optional (port (current-output-port)))
  (let ((len (+ 25 (string-length database) (string-length user))))
    (send-int32 len port)
    (send-int32 (protocol-version) port)
    (send-string "user" port)
    (send-string user port)
    (send-string "database" port)
    (send-string database port)
    (send-char #\nul port)
    (force-output port)))

