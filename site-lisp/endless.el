;; Note you may want to swap the following two keybinds.
;; Emacs' default keymap has `kill-sexp' on `C-M-k'.
(global-set-key (kbd "M-k")
                #'endless/forward-kill-sexp-or-dir)

(global-set-key (kbd "C-M-k")
                #'endless/backward-kill-sexp-or-dir)

(defun endless/forward-kill-sexp-or-dir (&optional p)
  "Kill forward sexp or directory.
If inside a string or minibuffer, and if it looks like
we're typing a directory name, kill forward until the next
/. Otherwise, `kill-sexp'"
  (interactive "p")
  (if (< p 0)
      (endless/backward-kill-sexp-or-dir (- p))
    (let ((r (point)))
      (if (and (or (in-string-p)
                   (minibuffer-window-active-p
                    (selected-window)))
               (looking-at "[^[:blank:]\n\r]*[/\\\\]"))
          (progn (search-forward-regexp
                  "[/\\\\]" nil nil p)
                 (kill-region r (point)))
        (kill-sexp p)))))

(defun endless/backward-kill-sexp-or-dir (&optional p)
  "Kill backwards sexp or directory."
  (interactive "p")
  (if (< p 0)
      (endless/forward-kill-sexp-or-dir (- p))
    (let ((r (point))
          (l (save-excursion
               (point))))
      (if (and (or (in-string-p)
                   (minibuffer-window-active-p
                    (selected-window)))
               (looking-back "[/\\\\][^[:blank:]\n\r]*"))
          (progn (backward-char)
                 (search-backward-regexp
                  "[/\\\\]" (point-min) nil p)
                 (forward-char)
                 (kill-region (point) l))
        (kill-sexp (- p))))))
