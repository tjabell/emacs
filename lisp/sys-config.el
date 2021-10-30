(if (fboundp 'tool-bar-mode)
    (progn
      (tool-bar-mode 0)
      (scroll-bar-mode 0))
  nil)

(menu-bar-mode 0)

;; Backup settings
(setq
 make-backup-files t
 backup-by-copying t
 backup-directory-alist '(("." . "~/.saves"))
 delete-old-versions t
 auto-save-mode nil
 auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;;To “turn off” the backup feature for remote files and stop TRAMP from saving to the backup directory, use this:
(add-to-list 'backup-directory-alist
             (cons tramp-file-name-regexp nil))

(if (or (eq system-type 'windows-nt)
        (eq system-type 'cygwin))
    (progn
      (defun open-explorer ()
        (interactive)
        (shell-command "explorer ."))
      (global-set-key (kbd "<f12>") 'open-explorer)
      (set-face-attribute 'default nil :font "Consolas 10"))
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

(add-to-list 'auto-mode-alist '("emacs.loadpackages" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("bashrc" . sh-mode))
(add-to-list 'auto-mode-alist '("bashbootstrap" . sh-mode))
(add-to-list 'auto-mode-alist '("diary" . diary-mode))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

(setq scheme-program-name "guile")

(defalias 'rr 'replace-regexp)
(defalias 'rs 'replace-string)
(defalias 'qrr 'query-replace-regexp)
(defalias 'wm 'whitespace-mode)
(defalias 'nxml 'nxml-mode)

;; [IMPORTANT] This function needs to take a (frame) arg
(defun my:on-new-frame (frame)
  (select-frame frame)
  (if (eq system-type 'windows-nt)
      (set-face-attribute 'default nil :family "Consolas" :height 100)
    (set-frame-font "DejaVu Sans Mono 10")))

;;; also see: http://stackoverflow.com/questions/18904529/after-emacs-deamon-i-can-not-see-new-theme-in-emacsclient-frame-it-works-fr
(if (daemonp)
    (add-hook 'after-make-frame-functions 'my:on-new-frame))

(require 'server)
(unless (server-running-p)
  (message "Starting server...")
  (server-start))

;;; Common Lisp
(add-to-list 'auto-mode-alist '("source-registry.conf" . lisp-mode))

(defun my:lisp-mode-init ()
  (paredit-mode))

(add-hook 'lisp-mode-hook
          #'my:lisp-mode-init)
