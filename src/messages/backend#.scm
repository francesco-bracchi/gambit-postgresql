(##namespace 
 ("postgresql/messages/backend#"

  make-field-descriptor
  field-descriptor?
  field-descriptor-name
  field-descriptor-table
  field-descriptor-index
  field-descriptor-type
  field-descriptor-type-size
  field-descriptor-modifier
  field-descriptor-text?
  field-descriptor-binary?
  
  ;; message types
  authentication
  backend-key-data
  bind-complete
  close-complete
  command-complete
  copy-data
  copy-done
  copy-fail
  copy-in-response
  copy-out-response
  copy-both-response
  data-row
  empty-query-response
  error-response
  function-call-response
  no-data
  notice-response
  notification-response
  parameter-description
  parameter-status
  parse-complete
  portal-suspended
  ready-for-query
  row-description

  handle-next-message

  current-message-handler
  current-reader-port
  *-tags-*
  ))

