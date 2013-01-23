;; this file is included in frontend.scm

;; (define (binding-format value) 0)

;; (define (binding-value value) value)

;; (define (send-binding-formats bs) 
;;   (for-each send-int32 bs))

;; (define (send-result-formats bs) 
;;   (for-each send-int32 bs)) 

;; (define (send-binding-values vs)
;;   'TBD)

;todo raise a postgresql exception
(define (send-type t) 
  (cond
   ((eq? t 'portal) (send-char #\p))
   ((eq? t 'statement) (send-char #\s))
   (else (error "unkown type"))))


(define (format arg) 
  (cond
   ((eq? (u8vector? arg) 1))
   (else 0)))

(define (send-object arg)
  (cond
   ((eq? arg '()) (send-int32 -1))
   ((binary? (format arg))
    (send-int32 (u8vector-length arg))
    (write-subu8vector arg 0 (u8vector-length arg)))
   (else 
    ;; todo: use character encoding specified by the connection
    (send-object (call-with-output-u8vector
		  (u8vector)
		  (lambda (port) (write arg port)))))))

(define (all? t? ls) 
  (cond
   ((null? ls) #t)
   ((t? (car ls)) (all? t? (cdr ls)))
   (else #f)))

(define-message-writer (bind portal source arguments results)
  ;; TODO: use ONLY text format right now (this is something that goes to text level
  ;; better TODO: arguments is an alist of (u8vector . binary/text) where binary/text is 1 or 0.
  ;; results is a list of binary/text 
  ;; (bind "portal123" "statement456" '((#u8(10 20 30 40 ...) . 1) ...) '(0 1 1 0 1))
  ;; let arguments be u8vector (it's flow level that should handle this)
  (let*((len (length arguments))
	(formats (map format arguments))
	(text? (all? (lambda (x) (= x 0)) formats))
	(bin? (all? (lambda (x) (= x 1)) formats))
	(format-len (cond (text? 0) (bin? 1) (else len)))
	(format-codes (cond (text? '()) (bin? '()) (else formats)))

	(rlen (length results))
	(rtext? (all? (lambda (x) (= x 0)) results))
	(rbin? (all? (lambda (x) (= x 1)) results))
	(rformat-len (cond (rtext? 0) (rbin? 1) (else rlen)))
	(rformat-codes (cond (rtext? '()) (rbin? '()) (else results))))
    
    (send-string portal)
    (send-string source)

    ;; send input format info
    (send-int16 format-len)
    (for-each send-int16 format-codes)
    
    ;; send input
    (send-int16 len)
    (for-each send-object arguments)
    
    ;; send output format
    (send-int16 rformat-len)
    (for-each send-int16 rformat-codes)))

  #;(send-int16 (length bindings))
  #;(send-binding-values (map binding-value bindings))
  #;(send-result-formats (map binding-format results))


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
  (send-int16 (length parameter-types))
  (for-each send-int32 parameter-types))

(define-message-writer (password pwd)
  (send-string pwd))

(define-message-writer (query q)
  (send-string q))

(define-message-writer (sync)
  'sync)

(define-message-writer (terminate)
  'terminate)

