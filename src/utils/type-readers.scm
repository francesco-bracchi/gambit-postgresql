(##namespace ("postgresql/utils/type-readers#"))

(##include "~~lib/gambit#.scm")
	      
(define current-reader-table (make-parameter (make-table)))

(define current-writer-table (make-parameter (make-table)))

(define (get-reader type #!optional (table (current-reader-table)))
  (table-ref table type #f))
	      
(define (get-writer type #!optional (table (current-writer-table)))
  (table-ref table type #f))

(define (set-reader! type reader #!optional (table (current-reader-table)))
  (table-set! table type reader))

(define (set-writer! type writer #!optional (table (current-writer-table)))
  (table-set! table type writer))

(define (type-of data)
  (cond
   ((string? data) 'text)
   ((char? data) 'char)
   ((number? data) 'number)
   ((u8vector? data) 'byte-vector)
   ((vector? data) 'vector)
   (else #f)))

(define (u8vector->data type vector #!optional
			(table (current-reader-table)))
  (let ((reader (get-reader type table)))
    (if reader
	(with-input-from-u8vector vector reader)
	vector)))

(define (data->u8vector data #!optional
			(type (type-of data))
			(table (current-writer-table)))
  (let ((writer (get-writer type table)))
    (if writer 
	(with-output-to-u8vector (u8vector) writer)
	data)))


(define (read-text) (read-line (current-input-port) #f))

(define (read-number) (string->number (read-text)))

(define (write-text txt)
  (display txt))

(set-reader! 'text read-text)
(set-reader! 'int2 read-number)
(set-reader! 'int4 read-number)
(set-reader! 'char read-char)

(set-writer! 'text write-text)
