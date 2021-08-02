(require 'use-package)
;;   "List of packages that I like.")

(use-package paredit )
(use-package iedit)
(use-package avy )
(use-package ace-window )
(use-package multiple-cursors )

(use-package skewer-mode )
(use-package emmet-mode)
(use-package js-comint )
(use-package csv-mode )

;(use-package geiser )

;(use-package tern )
(use-package js2-refactor )
(use-package json-mode )

(use-package auto-yasnippet )
(use-package exec-path-from-shell )
(use-package counsel-projectile )
(use-package paradox )
(use-package keychain-environment )

(use-package persistent-scratch )

;(use-package cider )
(use-package editorconfig
  
  :config (editorconfig-mode 1))

(use-package prettier-js)

(use-package impatient-mode)

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
          ;; (setq org-capture-templates
          ;;       '(("j" "Journal" entry (file+datetree "~/org/journal.org")
          ;;          "* %?\nEntered on %U\n %i\n %a")
          ;;         ("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
          ;;          "* TODO %?\n %i\n %a")
          ;;         ("p" "Project" entry (file+headline "~/org/projects.org" "Projects")
          ;;          "* TODO %?\n %i\n %a")))
          ;; (setq org-capture-templates
          ;;       org-roam-capture-templates)
          (setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
          (setq org-agenda-file-regexp "\\`[^.].*\\.org'\\|[0-9]+")
          (setq org-babel-default-header-args:C
                (cons '(:tangle . "yes")
                      (assq-delete-all :tangle org-babel-default-header-args)))
          (setq org-hide-emphasis-markers t)
          )
  :bind (("C-c c" . org-capture)
         ("C-c d" . org-roam-dailies-find-today)
         ("C-c C-x m" . org-meta-return)
         ("C-c C-x r" . org-metaright)
         ("C-c C-x l" . org-metaleft)
         ))

;;; End Org
;;;;;;;;;;;

;;;;;;;;;;;;
;;; Org Roam
;;;;;;;;;;;;
(setq org-roam-v2-ack t)

(global-set-key (kbd "C-c o") 'org-roam-node-find)
(define-key org-mode-map (kbd "C-c t") 'org-roam-tag-add)

(setq org-roam-directory "~/org-roam")
(unless (file-exists-p org-roam-directory)
  (make-directory org-roam-directory))
(add-hook 'after-init-hook 'org-roam-setup)

(use-package key-chord)
(key-chord-define org-mode-map "[[" #'my/insert-roam-link)

(defun my/insert-roam-link ()
  "Inserts an Org-roam link."
  (interactive)
  (insert "[[roam:]]")
  (backward-char 2))

(require 'org-roam-dailies)

(setq org-roam-dailies-directory "daily/")

;; (setq org-roam-dailies-capture-templates
;;       '(("d" "default" entry
;;          #'org-roam-capture--get-point
;;          "* %?"
;;          :file-name "daily/%<%Y-%m-%d>"
;;          :head "#+title: %<%Y-%m-%d>\n\n")))

;; (setq org-roam-dailies-capture-templates
;;       '(("l" "lab" entry
;;          #'org-roam-capture--get-point
;;          "* %?"
;;          :file-name "daily/%<%Y-%m-%d>"
;;          :head "#+title: %<%Y-%m-%d>\n"
;;          :olp ("Lab notes"))

;;         ("j" "journal" entry
;;          #'org-roam-capture--get-point
;;          "* %?"
;;          :file-name "daily/%<%Y-%m-%d>"
;;          :head "#+title: %<%Y-%m-%d>\n"
;;          :olp ("Journal"))))


;;; End org roam

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda ()
                             (org-bullets-mode 1)
                             (variable-pitch-mode 1))))


(use-package emmet-mode)

(use-package js-comint
  
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
  
  :config (progn
            (add-hook 'html-mode-hook 'emmet-mode)
            (define-key html-mode-map (kbd "C-/") 'emmet-expand-line)))


(use-package web-mode
  :hook ((web-mode . abbrev-mode)
         (web-mode . my:web-mode-init)
         (web-mode . emmet-mode))
  :config (progn
            (add-to-list 'auto-mode-alist '("\\.cshtml?\\'" . web-mode))
            (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
            (add-to-list 'auto-mode-alist '("\\.vtl?\\'" . web-mode))
            (define-key web-mode-map (kbd "C-/") 'emmet-expand-line)))

(defun my:web-mode-init ()
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (yas-activate-extra-mode 'html-mode))

;;;;;;;;;;;;;;;;;;
;;; Ivy/Counsel
(use-package counsel
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
  
  :config (setq projectile-completion-system 'ivy))




(use-package swiper
  :bind ("C-s" . swiper))

(use-package yasnippet
  :config (add-to-list 'yas-snippet-dirs "~/emacs/data/snippets/"))

(use-package company
  :hook (after-init . global-company-mode)
  :config (progn
            (setq company-idle-delay 0.0
                  company-minimum-prefix-length 1)))


(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package ace-jump-mode
  :bind ("C-." . ace-jump-mode)
  :config (progn
            (ace-jump-mode-enable-mark-sync)
            (define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)))

(use-package xterm-color)


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
  :bind ("<f8>" . neotree-toggle))

(use-package magit
  :bind ("<f10>" . magit-status))

(use-package visual-regexp )

(use-package visual-regexp-steroids )

(use-package key-chord
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
        "\C-e\C-x\C-o\C-n"))

(defun my:c-mode-init ()
  (google-make-newline-indent)
  (google-set-c-style))

(defvar my-csharp-default-compiler nil)
(setq my-csharp-default-compiler "mono @@FILE@@")

(defun my-csharp-get-value-from-comments (marker-string line-limit)
  my-csharp-default-compiler)


(use-package omnisharp  
  :bind (:map omnisharp-mode-map
              ([remap xref-find-definitions] . omnisharp-go-to-definition)
              ([remap xref-find-references] . omnisharp-find-usages)
              ;; `xref-pop-marker-stack' works as expected.
              ))

(defun my:csharp-init ()
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-omnisharp))
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

(add-hook 'python-mode-hook
          #'my:python-mode-init)

(add-hook 'c-mode-common-hook
          #'my:c-mode-init)

(add-hook 'c++-mode-hook
          #'my:cpp-mode-init)

;(add-hook 'haskell-mode-hook
;         'turn-on-haskell-simple-indent)

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

;;; Semantic
;(semantic-mode t)
;(global-semantic-idle-completions-mode t)
;(global-semantic-decoration-mode t)
;(global-semantic-highlight-func-mode t)
;(global-semantic-show-unmatched-syntax-mode 0)

;;; End Semantic
