(tool-bar-mode 0)
(scroll-bar-mode 0)

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

(setq-default indent-tabs-mode t)
(setq-default tab-width 2)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

(setq scheme-program-name "guile")

(defalias 'rr 'replace-regexp)
(defalias 'qrr 'query-replace-regexp)
(defalias 'wm 'whitespace-mode)

;; configure packages
(when (require 'color-theme nil t)
  (color-theme-initialize)
  (color-theme-calm-forest))

(when (require 'auto-complete nil t)
	(progn
		(add-hook 'c-mode-common-hook (lambda ()
																	(auto-complete-mode 1)))
		(add-hook 'emacs-lisp-mode-hook (lambda ()
																	(auto-complete-mode 1)
																	(show-paren-mode)))

		(add-hook 'python-mode-hook (lambda ()
																	(auto-complete-mode 1)))))

(when (require 'paredit nil t)
	(add-hook 'emacs-lisp-mode-hook (lambda ()
																		(paredit-mode))))

(when (require 'powershell-mode nil t)
	(add-to-list 'auto-mode-alist '("\\.ps1" . powershell-mode)))

(when (and (require 'e2wm nil t)
					 (require 'edbi nil t))
	(load-library "e2wm-edbi"))

(when (require 'yasnippet nil t)
	(setq snippet-dirs 
				`(,(concat emacs-root "emacs/data/snippets/"))))

(when (require 'pymacs nil t)
	(setenv "PYMACS_PYTHON" "python2"))

