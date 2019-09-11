(require 'cl)
(require 'package)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives
                 (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))
    (add-to-list 'package-archives
                 (cons "melpa" (concat proto "://melpa.org/packages/")))))

(package-initialize)

(defvar emacs-root (if (or (eq system-type 'gnu/linux)
                           (eq system-type 'linux))
                       (format "/home/%s/" (user-login-name))
                     "c:/users/trevor.abell/")
  "Home directory — the root emacs load-path.")

(cl-labels ((add-path (p)
                      (add-to-list 'load-path
                                   (concat emacs-root p))))
  (add-path "emacs/lisp") ;; all my personal elisp code
  (add-path "emacs/site-lisp") 
  (add-path "emacs/site-lisp/dosbat"))


(defun install-use-package-if-not-installed ()
  (unless (package-installed-p 'use-package)
    (package-install 'use-package)))

(install-use-package-if-not-installed)
(require 'use-package)

                                        ; put any package initialization in this file
(add-hook 'after-init-hook
          '(lambda ()
             (load "~/emacs/emacs.loadpackages")))

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(setq visible-bell t)

;; Experimental
(setenv "NODE_NO_READLINE" "1")

(add-to-list 'auto-mode-alist '(".emacs.custom" . emacs-lisp-mode))
(let ((custom-file (concat emacs-root ".emacs.custom")))
  (if (file-exists-p custom-file)
      (load-library custom-file)))

;;; Keychain Setup
;;; pacman -S keychain
;;; add to bash profile "eval `keychain --eval id_rsa`"
;;; relies on keychain-environment package: (use-package keychain-environment :ensure t) - should be in pkg-config

                                        ; Add and uncomment below lines in .emacs
                                        ;(keychain-refresh-environment)

(setq js2-strict-missing-semi-warning nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c5a044ba03d43a725bd79700087dea813abcb6beb6be08c7eb3303ed90782482" "1d9492749ca290d1702b2f331b99a2692cda19fb1e4aae4b9e75515027afbf3b" "3b819bba57a676edf6e4881bd38c777f96d1aa3b3b5bc21d8266fa5b0d0f1ebf" "3a727bdc09a7a141e58925258b6e873c65ccf393b2240c51553098ca93957723" default)))
 '(split-height-threshold nil)
 '(flymake-google-cpplint-command "/usr/bin/cpplint")
 '(send-mail-function (quote mailclient-send-it)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
