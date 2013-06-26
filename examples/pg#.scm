(include "params.scm")

(define (gsql#exit)
  (close-connection)
  (##exit))

(##include "~~lib/gambit#.scm")
(##namespace ("gsql#" exit))


(include "~~postgresql/postgresql#.scm")

(current-connection
 (open-connection
  database: (database)
  username: (username)
  password: (password)
  port-number: (port-number)
  server-address: (server-address)))


