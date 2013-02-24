(##namespace 
 ("postgresql/utils/type-readers#"
  current-reader-table
  current-writer-table
  
  set-reader! 
  set-writer!

  u8vector->data
  data->u8vector

  define-reader 
  define-writer))

(define-macro (define-reader type . body)
  `(set-reader! ',type (lambda () ,@body)))

(define-macro (define-writer type . body)
  `(set-writer! ',type (lambda () ,@body)))



