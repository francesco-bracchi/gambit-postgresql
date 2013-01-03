(##namespace
 ("postgresql/connection#"
  open-connection
  connection-database
  connection-username
  connection-password
  connection-server-address
  connection-port-number
  connection-character-encoding
  connection-notification-handler connection-notification-handler-set!
  connection-parameters connection-parameter connection-parameter-set!
  connection-pid connection-pid-set!
  connection-secret connection-secret-set!
  connection-reader
  connection-handler-table connection-handler-table-set! 
  connection-port

  current-connection
  open-connection
  close-connection
  call-with-connection
  with-connection
  ))
