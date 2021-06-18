;;; Some emacs clock functions
;;; https://emacs.stackexchange.com/questions/30280/how-to-conveniently-insert-a-clock-entry
(require 'org)
(require 'helm)

;; extend helm for org headings with the clock in action
(defun dfeich/helm-org-clock-in (marker)
  "Clock into the item at MARKER"
  (with-current-buffer (marker-buffer marker)
    (goto-char (marker-position marker))
    (org-clock-in)
    (org-clock-out)))

(eval-after-load 'helm-org
  '(nconc helm-org-headings-actions
          (list
           (cons "Clock into task" #'dfeich/helm-org-clock-in))))

(defun jx/insert-custom-clock-entry ()
  (interactive)
  (insert "CLOCK: ")
  (org-time-stamp-inactive)
  (insert "--")
  ;; Inserts the current time by default.
  (let ((current-prefix-arg '(4))) (call-interactively 'org-time-stamp-inactive))
  (org-ctrl-c-ctrl-c))

;; Shortcut
;; (define-key evil-normal-state-map (kbd "C-'") 'jx/insert-custom-clock-entry)
;; (define-key evil-insert-state-map (kbd "C-'") 'jx/insert-custom-clock-entry)
