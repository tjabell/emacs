(require 'cl)

(defvar emacs-root (if (or (eq system-type 'gnu/linux)
													 (eq system-type 'linux))
											 "/home/trevor/" 		 "c:/users/trevor.abell/")
	"Home directory — the root emacs load-path.")

(cl-labels ((add-path (p)
	 (add-to-list 'load-path
			(concat emacs-root p))))
 (add-path "emacs/lisp") ;; all my personal elisp code
 (add-path "emacs/site-lisp")
 (add-path "emacs/site-lisp/dosbat")
 )

(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

; put any package initialization in this file
(add-hook 'after-init-hook 
					'(lambda ()    (load "~/emacs/.emacs.loadpackages")))
