(##namespace 
 ("postgresql/commands/execute#"
  connection-prepare
  connection-bind
  connection-execute
  
  ;; aliases
  prepare
  bind
  execute
  ))

(define-structure statement
  id: d0fa9f52-2799-4351-9695-4152f4fcc50e
  (id read-only:)
  (connection read-only: unprintable:))

(define-structure portal
  id: fab9386e-a924-455d-b3b7-181afc2e3bcd
  (id read-only:)
  (connection read-only: unprintable:))
