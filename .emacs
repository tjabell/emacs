;;; Common Configuration, should be manually synced here for now
(require 'cl-lib)
(require 'package)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024 2)
            )

(setq package-user-dir (concat user-emacs-directory "elpa"))

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

(when (not (file-exists-p (concat user-emacs-directory "elpa/archives/gnu/archive-contents")))
    (package-refresh-contents))

(package-initialize)

(defvar emacs-root (if (or (eq system-type 'gnu/linux)
                           (eq system-type 'linux))
                       (format "/home/%s/" (user-login-name))
                     "c:/users/trevor.abell/")
  "Home directory — the root emacs load-path.")

(setq my:emacs-code-path (concat emacs-root "emacs/"))

(cl-labels ((add-path
	         (p)
             (add-to-list
	          'load-path
              (concat my:emacs-code-path p))))
  (add-path "lisp/") ;; all my personal elisp code
  (add-path "site-lisp/")
  (add-path "site-lisp/dosbat/"))


(defun install-use-package-if-not-installed ()
  (unless (package-installed-p 'use-package)
    (package-install 'use-package)))

(install-use-package-if-not-installed)
(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)

(defun my:init ()
  (load (concat my:emacs-code-path "emacs.loadpackages"))
  (keychain-refresh-environment))

(my:init)

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(setq visible-bell t)

(require 'org-tempo)
;; ********* Experimental/Local configuration can be placed here *********
