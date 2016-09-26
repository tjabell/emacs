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
(global-set-key (kbd "C-c C-f") 'iy-go-to-char)

;;; org-mode activate
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;;; Super + uppercase letter signifies a buffer/file
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

;;; Function keys
(global-set-key (kbd "C-!") 'eshell-here)
(global-set-key (kbd "<f1>") 'shell)
(global-set-key (kbd "<f2>") '(lambda () (interactive) (switch-to-buffer "*scratch*")))
(global-set-key (kbd "<f5>") 'compile)
(global-set-key (kbd "<f6>") 'gud-gdb)

(global-set-key [(mouse-5)] '(lambda () (interactive) (smooth-scroll 1)))
(global-set-key [(mouse-4)] '(lambda () (interactive) (smooth-scroll -1)))

;ar; Multiple-cursors
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(global-set-key (kbd "C-<tab>") 'next-buffer)
(global-set-key (kbd "C-<iso-lefttab>") 'previous-buffer)

(global-set-key (kbd "C-c o") '(lambda () (interactive) (find-file "~/org/notes.org")))

(global-set-key (kbd "M-p") 'ace-window)
(global-set-key (kbd "C-M-z") 'zap-up-to-char)

(global-set-key (kbd "C-S-o") 'my/insert-line-before)

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x o") 'ace-window)

(global-set-key (kbd "s-w") 'aya-create)
(global-set-key (kbd "s-y") 'aya-expand)

(global-set-key (kbd "C-:") 'avy-goto-char)

;; ;;; Helm keys since they are no longer bound by default: https://github.com/emacs-helm/helm/issues/1346
;; (define-key global-map [remap find-file] 'helm-find-files)
;; (define-key global-map [remap occur] 'helm-occur)
;; (define-key global-map [remap list-buffers] 'helm-buffers-list)
;; (define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
;; (global-set-key (kbd "M-x") 'helm-M-x)

(when (and (require 'ivy nil t)
           (require 'counsel nil t)
           (require 'swiper nil t))
  (load-library "ivy-keys"))
