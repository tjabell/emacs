(global-set-key (kbd "C-X C-m") 'execute-extended-command)
(global-set-key (kbd "<f11>") 'revert-buffer)

;; Super + uppercase letter signifies a buffer/file
(global-set-key (kbd "s-S")                       ;; scratch
                (lambda()(interactive)(switch-to-buffer "*scratch*")))
(global-set-key (kbd "s-E")                       ;; .emacs
                (lambda()(interactive)(find-file "~/.emacs")))
(global-set-key (kbd "s-H")                       ;; scratch
                (lambda()(interactive)(switch-to-buffer "*shell*")))

