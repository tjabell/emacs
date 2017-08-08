(defun insert-current-date () (interactive)
       (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))

(defun price-amazon (n)
  (interactive "nEnter amount per hour: ")
  (with-current-buffer (current-buffer)
    (insert "Hour - " (number-to-string n) " Month - " (number-to-string  (* n 730)) " Year - " (number-to-string (* n 730 12)))))
