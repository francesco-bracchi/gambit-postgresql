(##namespace 
 ("postgresql/messages/messages#"
  make-message
  message?
  message-name
  message-code
  message-values
  message-charcode

  define-tag-set))
							   
(define-structure message
  id: 6f6dcebf-47e4-40f1-9e8b-39cb922b3f42
  (name read-only:)
  (code read-only: unprintable:)
  (values read-only:))

;; this is a simple macro that put again character in the game
(define-macro (message-charcode m)
  `(char->integer (message-code ,m)))

;; this macro define a tag set, mainly defining 2 functions:
;; 
;; + `<set>/code->name` maps integer codes to names
;; 
;; + `<set>/name->code` does the opposite of `<set>/code->name`
(define-macro (define-tag-set name . pairs)
  (let* ((names (map car pairs))
	 (chars (map cadr pairs))
	 (codes (map char->integer chars))
	 (lo (apply min codes))
	 (hi (apply max codes))
	 (symbol-append
	  (lambda los (string->symbol 
		  (apply string-append
			 (map symbol->string los)))))
	 (v (gensym 'v))
	 (c (gensym 'c)))
    `(begin
       (define ,(symbol-append name '/code->name)
	 (let ((,v (make-vector ,(- hi lo -1))))
	   (begin ,@(map (lambda (code name) `(vector-set! ,v ,(- code lo) ',name)) codes names))
	   (lambda (,c) (vector-ref ,v (- ,c ,lo)))))

       (define ,(symbol-append  name '/name->code)
	 (let ((,v (list->table
		    ',(map cons names codes))))
	   (lambda (,c) (table-ref ,v ,c #f))))

       (define ,(symbol-append name '/lo) ,lo)
       (define ,(symbol-append name '/hi) ,hi))))

  
