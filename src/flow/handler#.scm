(##namespace
 ("postgresql/flow/handler#"
  current-handler-table
  handle-message
  handle-next-message

  define-handler-table
  define-handler
  ))

;; TODO: add default handler 
;; TODO: add extensions for handlers

(define-macro (define-handler head . body)
  (let* ((name (car head))
	 (formals (cdr head)))
    `(table-set! (current-handler-table) ',name (lambda ,formals ,@body))))

(define-macro (define-handler-table name . handlers)
  (let((n (gensym 'name))
       (default `(lambda args (error "unhandled message"))))
    `(begin
       (define ,name (make-table init: ,default))
       (parameterize
	((current-handler-table ,name))
	(begin ,@(map (lambda (handler) `(define-handler ,@handler))
		      handlers))))))


