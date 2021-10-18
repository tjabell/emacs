;;; Common Configuration, should be manually synced here for now
(require 'cl-lib)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024 2))

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again.")))

;;; (package-initialize)
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
  (add-path "site-lisp/dosbat/")
  (add-path "org-transclusion/"))

;; Bootstrap use-package
(straight-use-package 'use-package)

(eval-when-compile
  (require 'use-package))

;;;;  Effectively replace use-package with straight-use-package
;;; https://github.com/raxod502/straight.el/blob/develop/README.md#integration-with-use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(defun my:init ()
  (load (concat my:emacs-code-path "emacs.loadpackages"))
  (keychain-refresh-environment))

(my:init)

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(setq visible-bell t)

(setq my:lsp-server "/usr/bin/omnisharp")

(require 'org-tempo)
;; ********* Experimental/Local configuration can be placed here *********
