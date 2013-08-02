(require 'cl)

(global-set-key (kbd "C-X C-m") 'execute-extended-command)
(global-set-key (kbd "<f11>") 'revert-buffer)

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
		(set-face-attribute 'default nil :font "courier 8") nil)

(add-to-list 'auto-mode-alist '(".emacs.loadpackages" . emacs-lisp-mode))
(add-hook 'scheme-mode-hook 'paredit-mode)

(setq scheme-program-name "guile")
(put 'upcase-region 'disabled nil)

(setq-default indent-tabs-mode t)
(setq-default tab-width 2)
(put 'narrow-to-page 'disabled nil)


; put any package initialization in this file
(add-hook 'after-init-hook 
					'(lambda ()    (load "~/emacs/.emacs.loadpackages")))
