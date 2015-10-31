(defun habrams/insert-line-before (times)
  (interactive "p")
  (save-excursion
    (move-beginning-of-line 1)
    (newline times)))
