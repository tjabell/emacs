(add-hook 'scheme-mode-hook 'paredit-mode)

(add-hook 'hs-minor-mode-hook
					(lambda ()   (global-set-key (kbd "C-x C-o") 'hs-toggle-hiding)))

(add-hook 'python-mode-hook
					(lambda () 
						(hs-minor-mode 1)
						(fset 'hide-next
									"\C-e\C-x\C-o\C-n")))

;; configure packages
(when (require 'color-theme nil t)
  (color-theme-initialize)
  (color-theme-dark-blue2))

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
				`(,(concat emacs-root "emacs/data/snippets/")))
	(yas/global-mode))

(when (require 'pymacs nil t)
	(setenv "PYMACS_PYTHON" "python2")
	(pymacs-load "ropemacs" "rope-" t))

(when (require 'nose nil t)
	(defalias 'no 'nosetests-one)
	(defalias 'na 'nosetests-all)
	(defalias 'np 'nosetests-pdb-one)
	(defalias 'nm 'nosetests-module))

(when (require 'virtualenv nil t))

(when (require 'csharp-mode nil t)
	(add-hook 'csharp-mode-hook 'auto-revert-mode))
