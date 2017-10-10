(package-initialize)

(require 'cl)

(defvar emacs-root (if (or (eq system-type 'gnu/linux)
                           (eq system-type 'linux))
                       (format "/home/%s/" (user-login-name))
                     "c:/users/trevor.abell/")
    "Home directory — the root emacs load-path.")

(cl-labels ((add-path (p)
     (add-to-list 'load-path
                  (concat emacs-root p))))
 (add-path "emacs/use-package")
 (add-path "emacs/lisp") ;; all my personal elisp code
 (add-path "emacs/site-lisp") 
 (add-path "emacs/site-lisp/dosbat"))

(load-library "use-package")

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/"))
      package-archive-contents nil)

; put any package initialization in this file
(add-hook 'after-init-hook 
          '(lambda ()
             (load "~/emacs/emacs.loadpackages")))

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c5a044ba03d43a725bd79700087dea813abcb6beb6be08c7eb3303ed90782482" "1d9492749ca290d1702b2f331b99a2692cda19fb1e4aae4b9e75515027afbf3b" "3b819bba57a676edf6e4881bd38c777f96d1aa3b3b5bc21d8266fa5b0d0f1ebf" "3a727bdc09a7a141e58925258b6e873c65ccf393b2240c51553098ca93957723" default)))
 '(flymake-google-cpplint-command "/usr/bin/cpplint")
 '(org-agenda-files
   (quote
    ("~/org/atsol.org" "~/org/notes.org" "/home/trevor/projects/management/management.org")))
 '(send-mail-function (quote mailclient-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq visible-bell t)

;; Experimental
(setenv "NODE_NO_READLINE" "1")

(add-to-list 'auto-mode-alist '(".emacs.custom" . emacs-lisp-mode))
(let ((custom-file (concat emacs-root ".emacs.custom")))
  (if (file-exists-p custom-file)
      (load-library custom-file)))
