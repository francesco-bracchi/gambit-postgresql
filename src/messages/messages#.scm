(##namespace 
 ("postgresql/messages/messages#"
  define-tags

  make-message
  message?
  message-name
  message-code
  message-values))

(define-macro (define-tags . pairs)
  (let* ((names (map car pairs))
	 (chars (map cadr pairs))
	 (codes (map char->integer chars))
	 (name (gensym 'name))
	 (code (gensym 'code))
	 (table (gensym 'table)))
    `(begin
       (define-macro (lo) ,(apply min codes))
       (define-macro (hi) ,(+ 1 (apply max codes)))
       (define code->name 
	 (let ((,table (make-vector (- (hi) (lo)))))
	   (begin ,@(map (lambda (name code) `(vector-set! ,table (- ,code (lo)) ',name)) names codes))
	   (lambda (,code) (vector-ref ,table (- ,code (lo))))))
       
       (define-macro (name->code ,name)
	 (case ,name 
	   ,@(append (map (lambda (name code) `((,name) ,code))
			  names codes)
		     `((else (error "unknown name" ,name)))))))))

(define-structure message
  id: 6f6dcebf-47e4-40f1-9e8b-39cb922b3f42
  (name read-only:)
  (code read-only: unprintable:)
  (values read-only:))
