(global-set-key (kbd "C-x o") 'open-line)
(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "C-1") 'delete-other-windows)
(global-set-key (kbd "C-X C-m") 'execute-extended-command)
(global-set-key (kbd "<f11>") 'revert-buffer)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

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


