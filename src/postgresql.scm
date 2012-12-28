;; @author Francesco Bracchi
;; @email frbracch@gmail.com

;; postgresql protocol version 3.
;; inspired by pgscsh-0.4
;; lacks 1) CopyData message handlers, (mh... needed)
;;       2) various cyphered authenticaton protocols (ssh tunneling?)
;;       3) Prepared statements (could be used language level commands
;;          PREPARE and EXECUTE).
;;       4) function call (could be used SELECT function (a1, ...) statement
;;       5) correct management of asyncronous messages
;;       6) many others...

;;; PG.scm a socket level interface implementation protocol

(##namespace ("postgresql#"))
(##include "~~/lib/gambit#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (block)
         (mostly-fixnum)
         ;;(not safe)
         )

;;; CONSTANTS

(define ProtocolVersion 196608)

(define RequestOk 0)

(define RequestClearPassword 3)

;;; STRUCTURES
(define-structure connection
  database
  username
  password
  host
  port
  encoding
  socket)

(define-structure result
  connection
  status
  tuple-description
  tuples
  notices)

(define (result-names r)
  (map field-descriptor-name
       (result-tuple-description r)))
  

(define-structure field-descriptor
  name
  table
  index
  type
  type-size
  modifier
  binary?)

(define-type backend-exception
  fields
  (query init: #f))


;;; POSTGRES TYPES 
(define (send-int n i p)
  (declare (fixnum = <= - + * quotient remainder bitwise-and arithmetic-shift)
           (not safe))
  (if (= n 1) (write-u8 i p)
      (begin
        ; (send-int (- n 1) (quotient i 256) p)
        ;; (write-u8 (remainder i 256) p)
        (send-int (- n 1) (arithmetic-shift i -8) p)
        (write-u8 (bitwise-and i 255) p)
        )))

(define (send-netint n i p)
  (declare (fixnum = <= - + * quotient remainder bitwise-and arithmetic-shift)
           (not safe))
  (if (= n 1) (write-u8 i p)
      (begin
        ;; (write-u8 (remainder i 256) p)
        ;; (send-netint (- n 1) (quotient i 256) p)
        (write-u8 (bitwise-and i 255) p)
        (send-netint (- n 1) (arithmetic-shift i -8) p)
        )))

(define (send-string s p)
  (write-substring s 0 (string-length s) p)
  (print port: p #\nul))

(define (send-bytes buf p)
  (write-subu8vector buf 0 (u8vector-length buf) p))

(define (send-char c p)
  (write-u8 (char->integer c) p))

(define (recv-int n p)
  (let recv ((n n)
             (v 0))
    (declare (fixnum <= - + * arithmetic-shift bitwise-and)
             (not safe))
    (if (<= n 0) v
        (recv (- n 1)
              ;; (+ (read-u8 p) (* 256 v))
              (bitwise-ior (arithmetic-shift v 8) (read-u8 p))
              ))))

(define (recv-netint n p)
  (let recv ((n n)
             (v 0)
             (c 1))
    (declare (fixnum <= >= - + *)
             (not safe))
    (if (<= n 0) v
        (recv (- n 1)
              (+ (* c (read-u8 p)) v)
              (arithmetic-shift c 8)
              ;; (* c 256)
              ))))

(define (recv-string p)
  (let recv ((st '()))
    (let(
         (c (integer->char (read-u8 p))))
      (declare (fixnum char=?))
      (if (char=? c #\nul)
          (list->string (reverse st))
          (recv (cons c st))))))

(define (recv-bytes n p)
  (let(
       (res (make-u8vector n)))
    (read-subu8vector res 0 n p)
    res))

(define (recv-char p)
  (integer->char (read-u8 p)))

;;; POSTGRES MESSAGES
(define (send-password w con)
  (let((p (connection-socket con))
       (len (+ 5 (string-length w))))
  (send-char #\p p)
  (send-int 4 len p)
  (send-string w p)
  (force-output p)))

(define (send-query sql con)
  (let((p (connection-socket con))
       (len (+ 5 (string-length sql))))
    (send-char #\Q p)
    (send-int 4 len p)
    (force-output p)
    (send-string sql p)
    (force-output p)))

(define (send-term con)
  (let((p (connection-socket con)))
    (send-char #\X p)
    (send-int 4 4 p)))

(define (send-startup-packet con)
  (let*((p (connection-socket con))
        (dbs (connection-database con))
        (usr (connection-username con))
        (len (+ 25 (string-length dbs) (string-length usr))))
    (send-int 4 len p)
    (send-int 4 ProtocolVersion p)
    (send-string "user" p)
    (send-string usr p)
    (send-string "database" p)
    (send-string dbs p)
    (send-char #\nul p)
    (force-output p)))

(define (recv-startup-response con)
  (let(
       (p (connection-socket con)))
    (let recv ()
      (let ((c (recv-char p)))
        (cond
         ((char=? c #\R)
          (let*(
                (ml (recv-int 4 p))
                (cd (recv-int 4 p)))
            (cond
             ((= cd RequestOk)
              (recv))
             ((= cd RequestClearPassword)
              (send-password (connection-password con) con)
              (recv))
             (else
              (send-term con)
              (error "Authentication not supported" c)))))
         
         ((char=? c #\Z)
          (let*(
                (ml (recv-int 4 p))
                (kk (read-u8 p)))
            kk))
         
	 ((char=? c #\E)
	  (let*(
		(ml (recv-int 4 p))
		(ms (recv-record p)))
	    (close-port p)
	    (connection-socket-set! con #f)
	    (raise
	     (make-backend-exception ms))))

	 ((char=? c #\S)
	  (let*(
		(ml (recv-int 4 p))
		(pn (recv-string p))
		(vl (recv-string p)))
	    (recv)))

	 ((char=? c #\A)
	  (let*(
		(ml (recv-int 4 p))
		(id (recv-int 4 p))
		(ms (recv-string p))
		(ad (recv-string p)))
	    (recv)))

	 ((char=? c #\K)
	  (let*(
		(ml (recv-int 4 p))
		(id (recv-int 4 p))
		(sk (recv-int 4 p)))
	    (recv)))
         (else
          (send-term con)
          (connection-socket-set! con #f)
          (error "Unknown message format" c)))))))


(define (take n r p)
  (let take ((n n) (a '()))
    (if (= n 0) (reverse a)
        (take (- n 1) (cons (r p) a)))))

(define (recv-record p)
  (let recv ((a '()))
    (let(
         (c (recv-char p)))
      (if (char=? c #\nul) (reverse a)
          (let*(
                (msg (recv-string p)))
            (recv (cons (cons (field-type c) msg) a)))))))

(define (field-type c)
  ;; put this in a vector reference
  (cond
   ((char=? c #\S) "Severity")
   ((char=? c #\C) "SQLState")
   ((char=? c #\M) "Message")
   ((char=? c #\D) "Detail")
   ((char=? c #\H) "Hint")
   ((char=? c #\P) "Position")
   ((char=? c #\p) "Internal Position")
   ((char=? c #\q) "Internal Query")
   ((char=? c #\W) "Where")
   ((char=? c #\F) "File")
   ((char=? c #\L) "Line")
   ((char=? c #\R) "Routine")
   (else (string-append "Unknown field '" (string c) "'"))))
    
(define (recv-field-description p)
  (let*(
        (nm (recv-string p))
        (tb (recv-int 4 p))
        (cl (recv-int 2 p))
        (ty (recv-int 4 p))
        (ts (recv-int 2 p))
        (md (recv-int 4 p))
        (tx (recv-int 2 p)))
    (make-field-descriptor
     nm tb cl ty ts md (not (zero? tx)))))

(define *-null-element-* (- (expt 2 32) 1))

(define (recv-tuple-element p)
  (let(
       (l (recv-int 4 p)))
    (if (= l *-null-element-*) '()
        (recv-bytes l p))))

  
(define (recv-result con)
  (let(
       (p (connection-socket con)))
    (let recv ((td '())
               (ts '())
               (ns '())
               (rs '()))
      (let(
           (c (recv-char p)))
        (cond
         ((char=? c #\Z)
          (let*(
                (ml (recv-int 4 p))
                (kk (read-u8 p)))
            rs))
         
         ((char=? c #\C)
          (let*(
                (ml (recv-int 4 p))
                (st (recv-string p)))
            (recv '() '() '()
                  (make-result con st td (reverse ts) (reverse ns)))))
         
         ((char=? c #\T)
          (let*(
                (ml (recv-int 4 p))
                (tl (recv-int 2 p))
                (td1 (take tl recv-field-description p)))
            (recv td1 ts ns rs)))

         ((char=? c #\D)
          (let*(
                (ml (recv-int 4 p))
                (tl (recv-int 2 p))
                (t  (take tl recv-tuple-element p)))
            (recv td (cons (pg-read (connection-encoding con) td t) ts) ns rs)))
         
         ((char=? c #\I)
          (let*(
                (l (recv-int 4 p)))
            (recv td ts ns #f)))
         
         ((char=? c #\N)
          (let*(
                (ml (recv-int 4 p))
                (ms (recv-record p)))
            (recv td ts (cons ms ns) rs)))
         
         ((char=? c #\E)
          (let*(
                (ml (recv-int 4 p))
                (ms (recv-record p)))
            (close-port p)
            (connection-socket-set! con #f)
            (raise
             (make-backend-exception ms))))

         ((char=? c #\S)
          (let*(
                (ml (recv-int 4 p))
                (pn (recv-string p))
                (vl (recv-string p)))
            (recv td ts ns rs)))

         ((char=? c #\A)
          (let*(
                (ml (recv-int 4 p))
                (id (recv-int 4 p))
                (ms (recv-string p))
                (ad (recv-string p)))
            (recv td ts ns rs)))

         ((char=? c #\K)
          (let*(
                (ml (recv-int 4 p))
                (id (recv-int 4 p))
                (sk (recv-int 4 p)))
            (recv td ts ns rs)))
         (else
          (send-term con)
          (error "Unknown message from Backend: " c)))))))

;;; READER MANTAINANCE

(define *-init-readers-* #f)

(define *-readers-* (make-table init: (lambda (p) (read-line p))))

(define (pg-read enc td t)
  (cond
   ((null? t)
    '())
   
   ((null? (car t))
    (cons '() (pg-read enc (cdr td) (cdr t))))

   (else
    (let*(
          (d (car td))
          (e (car t))
          (r (table-ref
              *-readers-*
               (field-descriptor-type d))))
      (cons
       (call-with-input-u8vector
        (list char-encoding: enc
              init: e)
        r)
       (pg-read enc (cdr td) (cdr t)))))))


(define (set-readers! ls #!optional (con (current-connection)))
  (for-each
   (lambda (t)
     (let (
           (c (assoc (car t) ls)))
       (if c
           (let(
                (id (cadr t)))
             (table-set!
              *-readers-*
              (or (and (number? id) id)
                  (and (string? id) (string->number id)))
              (cadr c))))))
   (result-tuples
    (execute "SELECT typname, oid FROM pg_type;" con))))

(define (set-reader! t p c)
  (set-readers! `((,t ,p)) c))

(define (pg-read-bool p)
  (let(
       (c (read-char p)))
    (cond
     ((char=? c #\t) #t)
     ((char=? c #\f) #f)
     (else
      (error "cannot read data")))))

(define (pg-read-number p)
  (read p))

(define (pg-read-bytea p)
  (let read-bytea ((cs '()))
    (let(
           (c (read-u8 p)))
      (cond
       ((eof-object? c)
        (list->u8vector (reverse cs)))
       ((= c 92) ;; backslash
        (read-bytea (cons (recv-octet p) cs)))
       (else
        (read-bytea (cons c cs)))))))
  
(define (recv-octet p)
  (let(
       (a (- (read-u8 p) 48)))
    (cond
     ((= a 44) 92)
     (else
      (let*(
            (b (- (read-u8 p) 48))
            (c (- (read-u8 p) 48)))
        (+ (* a 64)
           (* b 8)
           c))))))

(define (nonprintable? c)
  (or (and (>= c 0) (<= c 31))
      (and (>= c 127) (<= c 255))))

(define (u8vector->bytea u)
  (call-with-output-string
   ""
   (lambda (p)
     (let for ((j 0))
       (if (< j (u8vector-length u))
           (let(
                (c (u8vector-ref u j)))
             (cond
              ((= c 0) (print port: p "\\\\000")) ;; tirare via questa che è un nonprintable
              ((= c 39) (print port: p "\\\\047"))
              ((= c 92) (print port: p "\\\\134"))
              ((nonprintable? c) (print-escape c p))
              (else (print port: p (integer->char c))))
             (for (+ j 1))))))))

(define (print-escape c #!optional (p (current-output-port)))
  (let*(
        (a (quotient c 64))
        (ar (remainder c 64))
        (b (quotient ar 8))
        (c (remainder ar 8)))
  (print port: p `("\\\\" ,a ,b ,c))))
   
(define (pg-read-string p)
  (let(
       (s (read-line p #f)))
    (cond
     ((eof-object? s) "")
     (else s))))
               
(define (really-init-readers #!optional (con (current-connection)))
  (set-readers!
   `(
     ("bool" ,pg-read-bool)
     ("boolean" ,pg-read-bool)
     ("char" ,pg-read-string)
     ("varchar" ,pg-read-string)
     ("text" ,pg-read-string)
     ("int2" ,pg-read-number)
     ("int4" ,pg-read-number)
     ("int8" ,pg-read-number)
     ("numeric" ,pg-read-number)
     ("oid" ,pg-read-number)
     ("float4" ,pg-read-number)
     ("float8" ,pg-read-number)
     ("money" ,pg-read-number)
     ("bytea" ,pg-read-bytea))
   con)
  (set! *-init-readers-* #t))

;;; MAIN INTERFACE

(define current-connection (make-parameter #f))

(define (connect dbn usr #!optional (pwd "") (hst "localhost") (prt 5432) (enc 'UTF-8))
  (let(
       (con (make-connection
             dbn
             usr
             pwd
             hst
             prt
             enc
             (open-tcp-client
              (list server-address: hst
                    port-number: prt)))))
    (send-startup-packet con)
    (recv-startup-response con)
    (if (not *-init-readers-*) (really-init-readers con))
    (make-will con (lambda (k)
                     (with-exception-catcher
                      (lambda (ex) 'ignore)
                      (lambda ()(disconnect k)))))
    con))

(define (connect! dbn usr #!optional (pwd "") (hst "localhost") (prt 5432))
  (current-connection
   (connect dbn usr pwd hst prt)))

(define (disconnect #!optional (con (current-connection)))
  (send-term con)
  (close-port (connection-socket con))
  (connection-socket-set! con #f))

(define (reconnect #!optional (con (current-connection)))
  (let(
       (p (open-tcp-client
               (list server-address: (connection-host con)
                     port-number: (connection-port con)))))
    (connection-socket-set! con p)
    (with-exception-catcher
     (lambda (ex)
       (connection-socket-set! con #f)
       (raise ex))
     (lambda () 
       (send-startup-packet con)
       (recv-startup-response con)))))

(define (execute w #!optional (con (current-connection)))
  (let(
       (ww
        (call-with-output-string
         ""
         (lambda (p)
           (print port: p w)))))
    (with-exception-catcher
     (lambda (ex)
       (let(
            (soc (connection-socket con)))
         (if soc
             (begin
             (connection-socket-set! con #f)
             (with-exception-catcher
              (lambda (ex) 'ignore)
              (lambda () (close-port soc)))))
         (if (backend-exception? ex) (backend-exception-query-set! ex ww))
         (raise ex)))
     (lambda ()
       (send-query ww con)
       (recv-result con)))))
   
(define (result-alists r)
  (let(
       (n (result-names r)))
    (map (lambda (t) (map cons n t))
         (result-tuples r))))

;; better?
(define (with-transaction th)
  (with-exception-catcher
   (lambda (ex)
     (if (backend-exception? ex) (raise ex)
         (begin
           (execute "ROLLBACK WORK")
           (raise ex))))
   (lambda ()
     (execute "BEGIN WORK")
     (th) 
     (execute "COMMIT WORK"))))

(define (escape enc s)
  (call-with-output-string
   ""
   (lambda (out)
     (let*(
           (buf (call-with-output-u8vector
                 (list char-encoding: enc)
                 (lambda (p)
                   (print port: p s))))
           (len (u8vector-length buf)))
       (let for ((j 0))
         (if (>= j len) 'ok
             (let*(
                   (c (u8vector-ref buf j))
                   (k (integer->char c)))
               (cond

                ((char=? k #\\)
                 (display "\\\\" out)
                 (for (+ j 1)))

                ((char=? k #\')
                 (display "\\'" out)
                 (for (+ j 1)))

                ((char=? k #\")
                 (display "\\\"" out)
                 (for (+ j 1)))

                ((char=? k #\newline)
                 (display "\\n" out)
                 (for (+ j 1)))
                
                ((char=? k #\backspace)
                 (display "\\b" out)
                 (for (+ j 1)))
                
                ((char=? k #\linefeed)
                 (display "\\f" out)
                 (for (+ j 1)))
                
                ((char=? k #\return)
                 (display "\\r" out)
                 (for (+ j 1)))
                
                ((char=? k #\tab)
                 (display "\\t" out)
                 (for (+ j 1)))
                 
                ((or (> c 128) (< c 20))
                 (display "\\" out)
                 (display (number->string c 8) out)
                 (for (+ j 1)))
                
                (else
                 (display k out)
                 (for (+ j 1)))))))))))


(define (c f #!optional (con (current-connection)))
  (cond
   ((null? f) "NULL")
   ((eq? f #t) "TRUE")
   ((eq? f #f) "FALSE")
   
   ((or (char? f) (string? f))
    `("E'" ,(escape (if con (connection-encoding con) 'UTF-8) f) #\'))

   ((number? f)
    f)
   
   ((u8vector? f)
    `("E'" ,(u8vector->bytea f) #\'))
   (else
    f)))
