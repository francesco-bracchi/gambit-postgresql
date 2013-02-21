# messages
this directory contains posrgresql messaging handling,
providing 2 functions: recv-message, and send-message used to communicate 
with postresql server.
THIS IS AN INTERNAL ABSTRACTION, IF YOU ARE NOT GOING TO CHANGE THE LIBRARY 
BEHAVIOR DO NOT TOUCH THIS DIRECTORY. NOT MEANT TO BE USED BY FINAL USER.

## messages
a message is a record type.
it has a name (a symbol) a code (a number) and a values i.e. 
the values carried by the message.

## frontend
provides a function send-message that sends a message on a port.
the argument is a message. a message can be build using a message constructor:
    
    ;; message constructors
    startup
    bind
    close
    copy-data
    copy-done
    copy-fail
    describe
    execute
    flush
    function-call
    parse
    password
    query
    sync
    terminate

for the meaning of each message refer to postgresql documentation.

## backend
Provides the recv-message, port is an optional argument

    (read-message #!optional (port (current-input-port)))

this function returns a message record type.
for their meanings check postgresql documentation.

for all available messages see 
[[http://www.postgresql.org/docs/9.2/static/protocol-message-formats.html]]
