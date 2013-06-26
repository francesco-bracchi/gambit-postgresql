(##namespace 
 ("postgresql/exception#" 
  make-postgresql-exception
  postgresql-exception?

  make-unsupported-authentication-method-exception
  unsupported-authentication-method-connection  
  unsupported-authentication-method-exception?
  unsupported-authentication-method-exception-connection
  unsupported-authentication-method-exception-code
  unsupported-authentication-method-exception-name

  make-backend-exception
  backend-exception?
  backend-exception-connection
  backend-exception-fields
  backend-exception-severity
  backend-exception-sql-state
  backend-exception-message
  backend-exception-file
  backend-exception-line
  backend-exception-routine))
  
(define-structure postgresql-exception
  id: 8ec24425-3847-47e3-8d37-7cff1abdab73
  extender: define-postgresql-exception
  (connection read-only: unprintable:))

(define-postgresql-exception unsupported-authentication-method-exception 
  id: e6ee8a60-7f22-46af-bdf1-74154b592dc1
  (code read-only: unprintable:)
  (name read-only: unprintable:))

(define-postgresql-exception backend-exception
  id: 8ae94359-4849-4ba2-bf75-3f19c86f3fbb
  (fields read-only:))
