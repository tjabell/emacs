(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(setq initial-scratch-message "")

;; Backup settings				
(setq
 make-backup-files t
 backup-by-copying t
 backup-directory-alist '(("." . "~/.saves"))
 delete-old-versions t
 auto-save-mode nil
 auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(if (or (eq system-type 'windows-nt)
        (eq system-type 'cygwin))
    (progn
      (defun open-explorer ()
        (interactive)
        (shell-command "explorer ."))
      (global-set-key (kbd "<f12>") 'open-explorer)
      (set-face-attribute 'default nil :font "Consolas 8"))
  nil)

(set-register ?e '(file . "~/emacs"))
(set-register ?p '(file . "~/projects"))
(set-register ?s '(file . "~/software_learning"))
(set-register ?b '(file . "~/books_learning"))
(set-register ?h '(file . "~/"))

(if (or (eq system-type 'windows-nt)
        (eq system-type 'cygwin)
    (set-register ?s '(file . "c:\\source")))
    nil)

;(if (or (eq system-type 'gnu/linux))
;    (add-to-list 'default-frame-alist
;                 '(font . "Inconsolata 9")))

(add-to-list 'auto-mode-alist '("emacs.loadpackages" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("Gruntfile" . js-mode))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

(setq scheme-program-name "guile")

(defalias 'rr 'replace-regexp)
(defalias 'rs 'replace-string)
(defalias 'qrr 'query-replace-regexp)
(defalias 'wm 'whitespace-mode)
(defalias 'nxml 'nxml-mode)

(require 'server)
(unless (server-running-p)
  (message "Starting server...")
  (server-start))
