(##namespace 
 ("postgresql/exception#" 
  make-postgresql-exception
  postgresql-exception?

  make-unsupported-authentication-method-exception 
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
  
  
  
  
