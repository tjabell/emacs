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

(defun my-capitalize-all-sqlserver-keywords (min max)
  (interactive "r")
  (require 'sql)
  (save-excursion
    (dolist (keywords sql-mode-ms-font-lock-keywords) 
      (goto-char min)
      (while (re-search-forward (car keywords) nil t)
        (unless (or (point-in-comment) (> (point) max))
          (goto-char (match-beginning 0))
          (upcase-word 1))))))


;;; https://emacs.stackexchange.com/questions/5371/how-to-change-emacs-windows-from-vertical-split-to-horizontal-split
(defun my-window-split-toggle ()
  "Toggle between horizontal and vertical split with two windows."
  (interactive)
  (if (> (length (window-list)) 2)
      (error "Can't toggle with more than 2 windows!")
    (let ((func (if (window-full-height-p)
                    #'split-window-vertically
                  #'split-window-horizontally)))
      (delete-other-windows)
      (funcall func)
      (save-selected-window
        (other-window 1)
        (switch-to-buffer (other-buffer))))))
