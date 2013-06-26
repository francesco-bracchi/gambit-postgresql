;; quick ephemeral queue implementation

(##namespace ("postgresql/utils/queue#"))
(##include "~~lib/gambit#.scm")

(declare (standard-bindings)
	 (extended-bindings)
	 (fixnum)
	 (block)
	 (safe))
      
(define (make-queue)
  (cons '() '()))

(define-macro (queue-head q)
  `(car ,q))

(define-macro (queue-head-set! q e)
  `(set-car! ,q ,e))

(define-macro (queue-tail-set! q e)
  `(set-cdr! ,q ,e))

(define-macro (queue-tail q)
  `(cdr ,q))

(define (empty? q)
  (null? (queue-head q)))

(define (push! e q)
  (let ((head (queue-head q))
	(tail (queue-tail q))
	(tail1 (list e)))
    (if (null? head)
	(queue-head-set! q tail1)
	(set-cdr! tail tail1))
    (queue-tail-set! q tail1)
    q))

(define (pop! q)
  (let ((head (queue-head q))
	#;(tail (queue-tail q)))
    (if (null? head) (error "empty queue")
	(begin (queue-head-set! q (cdr head))
	       (car head)))))

(define (queue->list q)
  (queue-head q))

;; (define x (make-queue))

;; (pp `(= x ,(queue->list x)))

;; (pp `(x is empty? ,(empty? x)))

;; (push! 'foo x)
;; (push! 'bar x)

;; (pp `(= x ,(queue->list x)))

;; (pp `(x is empty? ,(empty? x)))

;; (pp `(pop: ,(pop! x)))

;; (pp `(= x ,(queue->list x)))

;; (pp `(pop: ,(pop! x)))

;; (pp `(= x ,(queue->list x)))

;; (pp `(x is empty? ,(empty? x)))




