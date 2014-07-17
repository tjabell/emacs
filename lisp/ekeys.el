(global-set-key (kbd "C-x o") 'other-window)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-1") 'delete-other-windows)
(global-set-key (kbd "C-X C-m") 'execute-extended-command)
(global-set-key (kbd "<f11>") 'revert-buffer)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-z") 'repeat)
(global-set-key (kbd "M-/") 'delete-trailing-whitespace)
(global-set-key (kbd "C-<") 'delete-backward-char)

;;; org-mode activate
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Super + uppercase letter signifies a buffer/file
(global-set-key (kbd "s-S")                       ;; scratch
                (lambda()(interactive)(switch-to-buffer "*scratch*")))
(global-set-key (kbd "s-E")                       ;; .emacs
                (lambda()(interactive)(find-file "~/.emacs")))
(global-set-key (kbd "s-H")                       ;; scratch
                (lambda()(interactive)(switch-to-buffer "*shell*")))

(when (require 'e2wm nil t)
	(progn
		(global-set-key (kbd "s-d") 'e2wm:dp-edbi)
		(global-set-key (kbd "s-c") 'e2wm:dp-code)))

(global-set-key (kbd "C-!") 'eshell-here)
(global-set-key (kbd "<f1>") 'shell)
(global-set-key (kbd "<f5>") 'compile)
(global-set-key (kbd "<f6>") 'gud-gdb)

(global-set-key [(mouse-5)] '(lambda () (interactive) (smooth-scroll 1)))
(global-set-key [(mouse-4)] '(lambda () (interactive) (smooth-scroll -1)))

;; Multiple-cursors
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
