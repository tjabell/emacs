(defun insert-current-date () (interactive)
       (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))

(defun insert-signature-for-code () (interactive)
       (insert (shell-command-to-string "echo -n $(date +%Y%m%d)TJA")))

(defun price-amazon (n)
  (interactive "nEnter amount per hour: ")
  (with-current-buffer (current-buffer)
    (insert "Hour - " (number-to-string n) " Month - " (number-to-string  (* n 730)) " Year - " (number-to-string (* n 730 12)))))

(defun first-char-printer ()
  "Used this to print out katakana characters for anki slide deck"
  (interactive)
  (let ((c (string-to-char (buffer-substring (line-beginning-position) (1+ (line-beginning-position))))))
    (prin1 (get-char-code-property  c 'name) (current-buffer))))

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


(defun my/newline-directly-below ()
  "1. move to end of the line.
   2. insert newline with index"

  (interactive)
  (let ((oldcol (current-column)))
    (end-of-line)
    (newline)
    (dotimes (i oldcol)
      (insert " "))))

;;; format a json string in a buffer
(defun my:json-mode-and-format-buffer ()
  (interactive)
  (json-mode)
  (json-pretty-print-buffer))

(defalias 'jm 'my:json-mode-and-format-buffer)

(defalias 'icd 'insert-current-date)
(defalias 'ics 'insert-signature-for-code)

(require 'notifications)
(defun my:remind-me-in (minutes body)
       (interactive "sMinutes:\nsBody:")
       (let ((minutes (concat minutes " min")))
         (run-at-time minutes nil 'notifications-notify :title "Emacs alert" :body body)))
