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

  make-handler-table
  handler-table-ref
  handler-table-set!

  current-handler-table
  
  recv-message))
