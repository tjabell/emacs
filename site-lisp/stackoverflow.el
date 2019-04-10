;;; http://emacs.stackexchange.com/questions/12799/move-form-up-and-down-on-paredit-mode
(defun reverse-transpose-sexps (arg)
  (interactive "*p")
  (transpose-sexps (- arg))
  ;; when transpose-sexps can no longer transpose, it throws an error and code
  ;; below this line won't be executed. So, we don't have to worry about side
  ;; effects of backward-sexp and forward-sexp.
  (backward-sexp (1+ arg))
  (forward-sexp 1))

;;; https://stackoverflow.com/questions/22091936/emacs-how-to-capitalize-all-keywords-example-in-sql
(defun point-in-comment ()
  (let ((syn (syntax-ppss)))
    (and (nth 8 syn)
         (not (nth 3 syn)))))

(defun my-capitalize-all-sqlserver-keywords ()
  (interactive)
  (require 'sql)
  (save-excursion
    (dolist (keywords sql-mode-ms-font-lock-keywords) 
      (goto-char (point-min))
      (while (re-search-forward (car keywords) nil t)
        (unless (point-in-comment)
          (goto-char (match-beginning 0))
          (upcase-word 1))))))
