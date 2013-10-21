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

(defcustom writestamp-format "%X"
	"Format for writestamp (c.f. 'format-time-string').")

(defcustom writestamp-prefix "WRITESTAMP(("
	"Unique string identifying start of writestamp.")

(defcustom writestamp-suffix "))"
	"Unique string identifying end of writestamp.")

(defun update-writestamps ()
	"Find writestamps and replace them with the current time"
	(save-excursion
		(save-restriction
			(save-match-data
				(widen)
				(goto-char (point-min))
				(let ((regexp (concat "^"														 
															(regexp-quote writestamp-prefix)
															"\\(.*\\)"
															(regexp-quote writestamp-suffix)
															"$")))
					(while (re-search-forward regexp nil t)
						(replace-match (format-time-string writestamp-format
																							 (current-time))
													 t t nil 1))))))
	nil)

(defcustom modifystamp-format "%X"
	"Format for writestamp (c.f. 'format-time-string').")

(defcustom modifystamp-prefix "MODIFYSTAMP(("
	"Unique string identifying start of writestamp.")

(defcustom modifystamp-suffix "))"
	"Unique string identifying end of writestamp.")

(defvar last-change-time nil
	"Time of last buffer modification.")

(make-variable-buffer-local 'last-change-time)

(add-hook 'after-change-functions 'remember-change-time nil t)

(add-hook 'local-write-file-hooks
					'(lambda ()
						 (if last-change-time
								  (update-modifystamps last-change-time))))

(defun remember-change-time (&rest unused)
	"Store the current time in 'last-change-time'."
	(setq last-change-time (current-time)))

(defun update-modifystamps (time)
	"Find writestamps and replace them with the current time"
	(save-excursion
		(save-restriction
			(save-match-data
				(widen)
				(goto-char (point-min))
				(let ((regexp (concat "^"														 
															(regexp-quote modifystamp-prefix)
															"\\(.*\\)"
															(regexp-quote modifystamp-suffix)
															"$")))
					(while (re-search-forward regexp nil t)
						(replace-match (format-time-string modifystamp-format
																							 time)
													 t t nil 1))))))
	(setq last-change-time nil)
	nil)

(defun maybe-update-modifystamps ()
	"Call 'update-modifystamps' if the buffer has been modified."
	(if last-change-time
			(update-modifystamps)))

(add-hook 'local-write-file-hooks 'maybe-update-modifystamps nil t)

MODIFYSTAMP((20:48:51))
