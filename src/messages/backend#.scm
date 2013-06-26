(##namespace 
 ("postgresql/messages/backend#"

  field-descriptor?
  field-descriptor-name
  field-descriptor-table
  field-descriptor-index
  field-descriptor-type
  field-descriptor-modifier
  field-descriptor-text?
  field-descriptor-binary?
  
  read-message

  backend/name->code
  backend/code->name
  backend/lo
  backend/hi
  *message-readers*
  ))

(##include "messages#.scm")

(define-structure field-descriptor
  id: 7cc35c5d-9a37-4c93-88a7-c2fb3a02aa5c
  (name read-only:)
  (table read-only: unprintable:)
  (index read-only: unprintable:)
  (type read-only:)
  (type-size read-only: unprintable:)
  (modifier read-only: unprintable:)
  (text? read-only: unprintable:))
 
(define-macro (recv-message . actions)
  `(case-message (read-message) ,@actions))
  
(define-macro (case-message message . actions)
  (let ((m (gensym 'message))
	(c (gensym 'code)))
    `(let* ((,m ,message)
	    (,c (message-code ,m)))
       (pp (list 'RECEIVE ,m))
       (cond
	,@(map (lambda (action)
		 (let* ((head (car action))
			(key (car head))
			(params (cdr head))
			(body (cdr action)))
		   `((eq? ,c (backend/name->code ',key))
		     (apply (lambda ,params ,@body) (message-values ,m)))))
		actions)))))
