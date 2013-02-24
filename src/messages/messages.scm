(##namespace ("postgresql/messages/messages#"))

(##include "~~lib/gambit#.scm")
(include "messages#.scm")

(define (message-charcode message)
  (integer->char (message-code message)))
