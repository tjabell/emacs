(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)

(setq initial-scratch-message "")

(if (or (eq system-type 'windows-nt)
				(eq system-type 'cygwin))
    (progn
      (defun open-explorer ()
				(interactive)
				(shell-command "explorer ."))
      (global-set-key (kbd "<f12>") 'open-explorer)
			(set-face-attribute 'default nil :font "Consolas 8"))
  nil)


(if (or (eq system-type 'gnu/linux))
		(add-to-list 'default-frame-alist
			     '(font . "Inconsolata 9")))

(add-to-list 'auto-mode-alist '(".emacs.loadpackages" . emacs-lisp-mode))

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
		(server-start))
