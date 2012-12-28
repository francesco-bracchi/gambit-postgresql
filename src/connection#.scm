(##namespace
 ("postgresql/connection#"

  current-connection
  open-connection
  call-with-connection
  with-connection

  current-result-port
  open-simple-query
  call-with-simple-query
  with-simple-query
  reduce-query

  make-connection
  connection?
  connection-database 
  connection-username
  connection-password
  connection-server-address
  connection-port-number
  connection-character-encoding
  connection-notice-handler
  connection-reader connection-reader-set!
  connection-port
  connection-consumer connection-consumer-set!
  connection-handler-table connection-handler-table-set!
  current-description-table
  current-port-table
  connection-parameters connection-parameters-set!
  connection-type-readers connection-type-readers-set!
  connection-pid connection-pid-set!
  connection-secret connection-secret-set!
  connection-constructors-table connection-constructors-table-set!

  connection-parameter
  
  with-transaction
  ))

		    
