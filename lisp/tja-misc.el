;;;###autoload
(defun insert-current-date ()
  (interactive)
  (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))
(defalias 'icd 'insert-current-date)

;;;###autoload
(defun insert-signature-for-code ()
  (interactive)
  (insert (shell-command-to-string "echo -n $(date +%Y%m%d)TJA")))
(defalias 'isc 'insert-signature-for-code)

;;;###autoload
(defun first-char-printer ()
  "Used this to print out katakana characters for anki slide deck"
  (interactive)
  (let ((c (string-to-char (buffer-substring (line-beginning-position) (1+ (line-beginning-position))))))
    (prin1 (get-char-code-property  c 'name) (current-buffer))))

;;;###autoload
(defun ebpa/edebug-remove-all-instrumentation ()
  "Remove all edebug instrumentation by visiting each function
definition and running `eval-defun`."
  (interactive)
  (mapatoms
   (lambda (symbol)
     (when-let (pos (car-safe (get symbol 'edebug)))
       (with-current-buffer (marker-buffer pos)
         (goto-char (marker-position pos))
         (eval-defun nil))))))

;;;###autoload
(defun tja-newline-directly-below ()
  "1. move to end of the line.
   2. insert newline with index"
  (interactive)
  (let ((oldcol (current-column)))
    (end-of-line)
    (newline)
    (dotimes (i oldcol)
      (insert " "))))

;;; format a json string in a buffer
;;;###autoload
(defun tja-json-mode-and-format-buffer ()
  (interactive)
  (json-mode)
  (json-pretty-print-buffer))

(defalias 'jm 'tja-json-mode-and-format-buffer)

(require 'notifications)

;;;###autoload
(defun tja-remind-me-in (minutes body)
  (interactive "sMinutes:\nsBody:")
  (let ((minutes (concat minutes " min")))
    (run-at-time minutes nil 'notifications-notify :title "Emacs alert" :body body)))

(provide 'tja-misc)
