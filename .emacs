(require 'cl)

(defvar emacs-root (if (or (eq system-type 'gnu/linux)
													 (eq system-type 'linux))
											 "/home/trevor/" "c:/users/trevor.abell/")
	"Home directory — the root emacs load-path.")

(cl-labels ((add-path (p)
	 (add-to-list 'load-path
			(concat emacs-root p))))
 (add-path "emacs/lisp") ;; all my personal elisp code
 (add-path "emacs/site-lisp")
 (add-path "emacs/site-lisp/dosbat"))

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/"))
      package-archive-contents nil)

; put any package initialization in this file
(add-hook 'after-init-hook 
					'(lambda ()    (load "~/emacs/emacs.loadpackages")))

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
