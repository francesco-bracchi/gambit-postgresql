;; this file is included in frontend.scm

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

(define-message-writer (query q)
  (send-string q))

(define-message-writer (sync)
  'sync)

(define-message-writer (terminate)
  'terminate)

