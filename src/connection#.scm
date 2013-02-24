(##namespace
 ("postgresql/connection#"
  open-connection
  connection-database
  connection-username
  connection-password
  connection-server-address
  connection-port-number
  connection-character-encoding
  connection-parameters connection-parameter connection-parameter-set!
  connection-notifications connection-notifications-set!
  connection-notice-handler connection-notice-handler-set!
  connection-pid connection-pid-set!
  connection-secret connection-secret-set!
  connection-status connection-status-set!
  connection-notification-push!
  connection-oid-table connection-oid-table-set!
  connection-oid->name

  current-connection
  open-connection
  close-connection
  call-with-connection
  with-connection
  with-transaction

  call-with-connection-port))

(define-structure connection
  id: c8bb6384-aa47-4041-b51d-6798dbe814ba
  (database read-only:)
  (username read-only: unprintable:)
  (password read-only: unprintable:)
  (server-address read-only: unprintable:)
  (port-number read-only: unprintable:)
  (character-encoding read-only: unprintable:)
  (port read-only: unprintable:)
  (parameters unprintable:)
  (notifications unprintable:)
  (notice-handler unprintable:) 
  (pid unprintable: init: -1)
  (secret unprintable: init: 0)
  (status init: 'uninitialized)
  (oid-table init: #f)
  )
