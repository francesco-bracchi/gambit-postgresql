
(define-handler (notification-response pid channel payload)
  ((connection-notification-handler (current-connection)) pid channel payload)
  (handle-next-message))

(define-handler (notice-response pairs)
  (pp `(NOTICE ,pairs))
  (handle-next-message))

(define-handler (parameter-status key value)
  (table-set! (connection-parameters (current-connection))
	      key
	      value)
  (handle-next-message))

(define-handler (copy-in-response text? columns types)
  (pp `(COPY IN (binary? ,binary?) ,(map cons columns type)))  
  ;; save the old handler
  (connection-handler-table-set! connection copy-handler)
  (handle-next-message))

(define-handler (copy-out-response text? columns types)
  ;; save the old handler
  (pp `(COPY OUT (binary? ,binary?) ,(map cons columns type)))
  (handle-next-message))

