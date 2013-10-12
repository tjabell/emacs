(defun insert-current-time ()
	"Inser the current time"
	(interactive "*")
	(insert (current-time-string)))
(format-time-string "%l.%M %p" (current-time))




