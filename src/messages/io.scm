(##namespace ("postgreql/messages/io#"))
(##include "~~lib/gambit#.scm")

(define (send-int byte-length value #!optional (port (current-output-port)))
  (declare (fixnum = <= - + * quotient remainder bitwise-and arithmetic-shift)
           (not safe))
  (if (= byte-length 1) (write-u8 value port)
      (begin
        (send-int (- byte-length 1) (arithmetic-shift value -8) port)
        (write-u8 (bitwise-and value 255) port))))

(define (send-int8 value)
  (send-int 1 value))

(define (send-int16 value)
  (send-int 1 value))

(define (send-int32 value)
  (send-int 4 value))

(define (send-netint byte-length value #!optional (port (current-output-port)))
  (declare (fixnum = <= - + * quotient remainder bitwise-and arithmetic-shift)
           (not safe))
  (if (= byte-length 1) (write-u8 value port)
      (begin
        (write-u8 (bitwise-and value 255) port)
        (send-netint (- byte-length 1) (arithmetic-shift value -8) port))))

(define (send-string string #!optional (port (current-output-port)))
  (write-substring string 0 (string-length string) port)
  (print port: port #\nul))

(define (send-bytes u8vect #!optional (port (current-output-port)))
  (write-subu8vector u8vect 0 (u8vector-length u8vect) port))

(define (send-char char #!optional (port (current-output-port)))
  (write-u8 (char->integer char) port))

(define (recv-int byte-length #!optional (port (current-input-port)))
  (let recv ((byte-length byte-length)
             (value 0))
    (declare (fixnum <= - + * arithmetic-shift bitwise-and)
             (not safe))
    (if (<= byte-length 0) value
        (recv (- byte-length 1)
              (bitwise-ior (arithmetic-shift value 8) (read-u8 port))))))

(define (recv-int8 #!optional (port (current-output-port)))
  (recv-int 1 port))

(define (recv-int16 #!optional (port (current-output-port)))
  (recv-int 2 port))

(define (recv-int32 #!optional (port (current-output-port)))
  (recv-int 4 port))

(define (recv-netint byte-length #!optional (port (current-input-port)))
  (let recv ((byte-length byte-length)
             (value 0)
             (factor 1))
    (declare (fixnum <= >= - + *)
             (not safe))
    (if (<= byte-length 0) value
        (recv (- byte-length 1)
              (+ (* factor (read-u8 port)) value)
              (arithmetic-shift factor 8)))))

(define (recv-string #!optional (port (current-input-port)))
  (read-line port #\nul))
    
(define (recv-bytes length #!optional (port (current-input-port)))
  (let ((result (make-u8vector length)))
    (read-subu8vector result 0 length port)
    result))

(define (recv-char  #!optional (port (current-input-port)))
  (integer->char (read-u8 port)))
