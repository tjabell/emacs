(defcustom insert-time-format "%X"
	"Format for \\[insert-time] (c.f. 'format-time-string').")

(defcustom insert-date-format "%x"
	"Format for \\[insert-date] (c.f. 'format-time-string').")

(defun insert-current-time ()
	"Inser the current time"
	(interactive "*")
	(insert (current-time-string)))


(defun insert-time ()
	"Insert the current time according to insert-time-format."
	(interactive "*")
	(insert (format-time-string insert-time-format
															(current-time))))

(defun insert-date ()
	"Insert the current date according to insert-date-format."
	(interactive "*")
	(insert (format-time-string insert-date-format
															(current-time))))

(defun update-writestamps ()
	"Find writestamps and replace them with the current time"
	(save-excursion
		(save-restriction
			(save-match-data
				(widen)
				(goto-char (point-min))
				(while (search-forward "WRITESTAMP((" nil t)
					(let ((start (point)))
								(search-forward "))")
								(delete-region start (- (point) 2))
								(goto-char start)
								(insert-date))))))
	nil)
