(##namespace ("postgresql/notification#" 
	      make-notification
	      notification?
	      notification-pid
	      notification-channel
	      notification-payload))

(define-structure notification
  id: dccbba68-dad9-43af-b9a3-764ae9272e81
  (pid read-only: unprintable:)
  (channel read-only:)
  (payload read-only:))
