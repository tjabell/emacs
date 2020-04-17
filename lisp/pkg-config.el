(require 'use-package)

;;   "List of packages that I like.")

(use-package paredit :ensure t)
(use-package iedit :ensure t)
(use-package avy :ensure t)
(use-package ace-window :ensure t)
(use-package multiple-cursors :ensure t)

(use-package skewer-mode :ensure t)
(use-package emmet-mode :ensure t)
(use-package web-mode :ensure t)
(use-package js-comint :ensure t)
(use-package csv-mode :ensure t)

(use-package geiser :ensure t)

(use-package tern :ensure t)
(use-package company-tern :ensure t)
(use-package js2-refactor :ensure t)
(use-package json-mode :ensure t)

(use-package auto-yasnippet :ensure t)
(use-package exec-path-from-shell :ensure t)
(use-package counsel-projectile :ensure t)
(use-package paradox :ensure t)
(use-package keychain-environment :ensure t)

(use-package persistent-scratch :ensure t)

(use-package cider :ensure t)
(use-package editorconfig
  :ensure t
  :config (editorconfig-mode 1))

(use-package prettier-js
  :ensure t)

(use-package impatient-mode
  :ensure t)

;;;;;;;;;;;;;;;;;;
;;; Org Mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((ruby . t)
   (emacs-lisp . t)
   (python . t)
   (perl . t)
   (haskell . t)
   (C . t)))

(use-package org
  :init (progn
          (setq org-default-notes-file (concat org-directory "/notes.org"))
          (setq org-todo-keywords
                '((sequence "TODO" "TEST" "DONE")))
          (setq org-capture-templates
                '(("j" "Journal" entry (file+datetree "~/org/journal.org")
                   "* %?\nEntered on %U\n %i\n %a")
                  ("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
                   "* TODO %?\n %i\n %a")))
          (setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
          (setq org-agenda-file-regexp "\\`[^.].*\\.org'\\|[0-9]+")
          (setq org-babel-default-header-args:C
                (cons '(:tangle . "yes")
                      (assq-delete-all :tangle org-babel-default-header-args))))
  :bind ("C-c c" . org-capture))
;;; End Org
;;;;;;;;;;;

(use-package uniquify
  :config
  (setq
   uniquify-buffer-name-style 'post-forward
   uniquify-separator ":"))

(use-package dired-x)

(use-package emmet-mode
  :ensure t
  :config
  (progn
    (add-hook 'web-mode-hook 'emmet-mode)
    (define-key web-mode-map (kbd "C-/") 'emmet-expand-line)))

(use-package js-comint
  :ensure t
  :config
   ;; From here: http://stackoverflow.com/questions/13862471/using-node-js-with-js-comint-in-emacs
   (setq inferior-js-mode-hook
        (lambda ()
          ;; We like nice colors
          (ansi-color-for-comint-mode-on)
          ;; Deal with some prompt nonsense
          (add-to-list
           'comint-preoutput-filter-functions
           (lambda (output)
             (replace-regexp-in-string "\033\\[[0-9]+[A-Z]" "" output))))))

(use-package sgml-mode
  :ensure t
  :config (progn
            (add-hook 'html-mode-hook 'emmet-mode)
            (define-key html-mode-map (kbd "C-/") 'emmet-expand-line)))


(use-package web-mode
  :ensure t
  :config (progn
              (add-to-list 'auto-mode-alist '("\\.cshtml?\\'" . web-mode))
              (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))))

;;;;;;;;;;;;;;;;;;
;;; Ivy/Counsel
(use-package counsel
  :ensure t
  :bind (("C-h" . counsel-projectile)
         ("<f1> f" . counsel-describe-function)
         ("<f1> v" . counsel-describe-variable)
         ("<f1> l" . counsel-find-library)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-ag)
         ("C-x l" . counsel-locate)))

(use-package ivy
  :ensure t
  :config (progn (setq ivy-height 50)
                 (setq ivy-use-virtual-buffers t)
                 (setq ivy-count-format "(%d/%d) "))
  :bind (("C-c C-r" . ivy-resume)
         ("<f6>" . ivy-resume)))

(define-key ivy-minibuffer-map (kbd "<left>") 'counsel-up-directory)
(define-key ivy-minibuffer-map (kbd "C-l") 'counsel-up-directory)
(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
;;; End Ivy
;;;;;;;;;;;;;;;;;;
(use-package projectile
  :ensure t
  :config (setq projectile-completion-system 'ivy))




(use-package swiper
  :ensure t
  :bind ("C-s" . swiper))

(use-package yasnippet
  :ensure t
  :config (add-to-list 'yas-snippet-dirs "~/emacs/data/snippets/"))

(use-package company
  :ensure t
  :config (progn
            (setq company-idle-delay 0)
            (setq company-minimum-prefix-length 1)))

(use-package nose
  :ensure t
  :config
  (progn
    (defalias 'no 'nosetests-one)
    (defalias 'na 'nosetests-all)
    (defalias 'np 'nosetests-pdb-one)
    (defalias 'nm 'nosetests-module)))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package ace-jump-mode
  :ensure t
  :bind ("C-." . ace-jump-mode)
  :config (progn
            (ace-jump-mode-enable-mark-sync)
            (define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)))


(use-package xterm-color
  :ensure t)


(add-hook 'eshell-mode-hook
            (lambda ()
              (setq xterm-color-preserve-properties t)))
(require 'eshell)
; TODO: Figure out why this is breaking
;(add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
;(setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))

;; (when (require 'helm nil t)
;;   (global-set-key (kbd "C-c h") 'helm-mini)
;;   (when (require 'projectile nil t)
;;     (require 'helm-projectile nil t)
;;     (global-set-key (kbd "C-c h") 'helm-projectile)

;;     (setq projectile-enable-caching t)))


(use-package neotree
  :ensure t
  :bind ("<f8>" . neotree-toggle))

(use-package magit
  :ensure t
  :bind ("<f10>" . magit-status))

(use-package visual-regexp :ensure t)

(use-package visual-regexp-steroids :ensure t)

(use-package key-chord
  :ensure t
  :config (progn
            (key-chord-define-global "qo" 'other-window)
            (key-chord-define-global "qp" 'other-window)
            (key-chord-define-global "qf" 'find-file)
            (key-chord-define-global "xf" 'find-file)
            (key-chord-define-global "qk" 'kill-this-buffer)
            (key-chord-define-global "xk" 'kill-this-buffer)
            (key-chord-define-global "qr" 'previous-buffer)
            (key-chord-define-global "qn" 'next-buffer)
            (key-chord-define-global "qs" 'save-buffer)
            (key-chord-define-global "xs" 'save-buffer)
            (key-chord-define-global "qh" 'shell)
            (key-chord-define-global "xh" 'shell)
            ;; Too easy to misstype
            ;; (key-chord-define-global "qe" 'eval-defun)
            ;(key-chord-define-global "qj" 'eval-print-last-sexp)
            (key-chord-define-global "qb" 'ivy-switch-buffer)
            (when (require 'xah-lee nil t)
              (key-chord-define-global "qr" 'xah-next-user-buffer)
              (key-chord-define-global "qn" 'xah-previous-user-buffer))))

;;; Ansi colors in compile buffer
;;; From: http://stackoverflow.com/questions/3072648/cucumbers-ansi-colors-messing-up-emacs-compilation-buffer
(when (require 'ansi-color nil t)
  (defun colorize-compilation-buffer ()
    (toggle-read-only)
    (ansi-color-apply-on-region (point-min) (point-max))
    (toggle-read-only)))

(when (require 'ebuild-mode nil t)
  (add-to-list 'auto-mode-alist '(".ebuild" . ebuild-mode)))

(add-to-list 'auto-mode-alist '("mutt" . mail-mode))

(when (and (require 'e2wm nil t)
           (require 'edbi nil t))
  (load-library "e2wm-edbi"))

(defun my:compilation-filter-init ()
  (colorize-compilation-buffer))

(defun my:emacs-lisp-mode-init ()
  (paredit-mode))

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(defun my:js2-mode-init ()
  (local-set-key (kbd "C-c C-e") 'js2-next-error)
  (local-set-key (kbd "C-x C-e") 'js-send-last-sexp)
  (local-set-key (kbd "C-M-x") 'js-send-last-sexp-and-go)
  (local-set-key (kbd "C-c b") 'js-send-buffer)
  (local-set-key (kbd "C-c C-b") 'js-send-buffer-and-go)
  (local-set-key (kbd "C-c l") 'js-load-file-and-go)
  (local-set-key (kbd "C-c M-j") 'run-js)
  (semantic-mode 0)
  (custom-set-variables
   '(js2-bounce-indent-p t)))

;; (defun my:ac-c-header-init ()
;;   (require 'auto-complete-c-headers)
;;   (add-to-list 'achead:include-directories '("/usr/include" "/usr/lib/include")))

(defun my:cpp-mode-init ()
  (unless (or (file-exists-p "makefile")
              (file-exists-p "Makefile"))
    (set (make-local-variable 'compile-command)
         (concat "make -k CXXFLAGS='-std=c++11' "
                 (file-name-sans-extension (or buffer-file-name "C-BUFFER")))))
  (semantic-mode))

(defun my:python-mode-init ()
  (hs-minor-mode 1)
  (setq python-indent-offset 4)
  (fset 'hide-next
        "\C-e\C-x\C-o\C-n")
  (elpy-mode))

(defun my:c-mode-init ()
  (google-make-newline-indent)
  (google-set-c-style))

(defun my:web-mode-init ()
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (yas-activate-extra-mode 'html-mode))

(defvar my-csharp-default-compiler nil)
(setq my-csharp-default-compiler "mono @@FILE@@")

(defun my-csharp-get-value-from-comments (marker-string line-limit)
  my-csharp-default-compiler)


(use-package omnisharp
  :ensure t
  :bind (:map omnisharp-mode-map
              ([remap xref-find-definitions] . omnisharp-go-to-definition)
              ([remap xref-find-references] . omnisharp-find-usages)
              ;; `xref-pop-marker-stack' works as expected.
              ))

(defun my:csharp-init ()
  (setq omnisharp-server-executable-path "/home/trevor/omnisharp/run")
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-omnisharp))
  (company-mode)
  (hs-minor-mode 1)
  (auto-revert-mode)
  (linum-mode)
  (c-set-style "c#")
  (flycheck-mode)
  (omnisharp-mode))

(defun my:term-mode-hook ()
  (setq yas-dont-activate t))

(defun my:sql-interactive-mode-init ()
  (toggle-truncate-lines t))

(add-hook 'compilation-filter-hook
          #'my:compilation-filter-init)

(add-hook 'csharp-mode-hook
          #'my:csharp-init)

(add-hook 'web-mode-hook
          #'my:web-mode-init)

(add-hook 'python-mode-hook
          #'my:python-mode-init)

(add-hook 'c-mode-common-hook
          #'my:c-mode-init)

(add-hook 'c++-mode-hook
          #'my:cpp-mode-init)

(add-hook 'haskell-mode-hook
          'turn-on-haskell-simple-indent)

(add-hook 'scheme-mode-hook
          'paredit-mode)

(add-hook 'hs-minor-mode-hook
          (lambda ()   (global-set-key (kbd "C-x C-o") 'hs-toggle-hiding)))

(add-hook 'js2-mode-hook
          'my:js2-mode-init)

(add-hook 'emacs-lisp-mode-hook
          'my:emacs-lisp-mode-init)

(add-hook 'term-mode-hook 'my:term-mode-hook)

(add-hook 'sql-interactive-mode-hook #'my:sql-interactive-mode-init)

;; Misc file types and their default modes
(add-to-list 'auto-mode-alist '("\\.editorconfig" . conf-mode))

(ivy-mode 1)
(show-paren-mode 1)
(auto-complete-mode 0)
(key-chord-mode 1)
(global-ede-mode 1)
(projectile-global-mode)
;(global-company-mode)
;(yas/global-mode 1)
;(helm-mode 1)

;;; Semantic
;(semantic-mode t)
;(global-semantic-idle-completions-mode t)
;(global-semantic-decoration-mode t)
;(global-semantic-highlight-func-mode t)
;(global-semantic-show-unmatched-syntax-mode 0)

;;; End Semantic
