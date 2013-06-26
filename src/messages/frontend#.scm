(namespace 
 ("postgresql/messages/frontend#"

  *message-writers*
  frontend/name->code
  frontend/lo

  send-message
  
  ;; message constructors
  startup
  bind
  close
  copy-data
  copy-done
  copy-fail
  describe
  execute
  flush
  function-call
  parse
  password
  query
  sync
  terminate
  ))

;; (send-message (startup "database" "user") <port>)
