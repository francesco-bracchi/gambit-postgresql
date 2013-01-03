# messages

## frontend
The file frontend#.scm and frontend.scm provides a macro, send-message
that sends a message to the current-output-port (or a port specified as
an extra parameter 

### example
For example sending startup message 

    (send-message (startup <database-name> <username>))

for all available messages see 
[[http://www.postgresql.org/docs/9.2/static/protocol-message-formats.html]]
(the ones marked with (F)ontend)
