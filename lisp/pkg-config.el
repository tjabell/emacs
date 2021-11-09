;;; -*- lexical-binding: t; -*-
(require 'use-package)

(use-package paredit)
(use-package iedit)
(use-package avy)
(use-package ace-window)
(use-package multiple-cursors)

(use-package skewer-mode)
(use-package emmet-mode)
(use-package js-comint)
(use-package csv-mode)

(use-package js2-refactor)
(use-package json-mode)

(use-package auto-yasnippet)
(use-package exec-path-from-shell)
(use-package paradox)
(use-package keychain-environment)

(use-package persistent-scratch)

;(use-package cider)
(use-package editorconfig
  :config (editorconfig-mode 1))

(use-package prettier-js)

(use-package impatient-mode)

(use-package string-inflection)

;;; Vertico, Consult, Marginalia, Savehist
;; Configuration below comes from
;; /home/trevor/org-roam/20211107090342-emacs_new_stack_vertico_consult_marginalia_savehist.org
(load-file "~/.emacs-min-new-stack.el")


(defun my/insert-roam-link ()
  "Inserts an Org-roam link."
  (interactive)
  (insert "[[roam:]]")
  (backward-char 2))

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

(use-package yasnippet
  :config (add-to-list 'yas-snippet-dirs "~/emacs/data/snippets/"))

(use-package company
  :hook (after-init . global-company-mode)
  :config (progn
            (setq company-idle-delay 0.0
                  company-minimum-prefix-length 1)))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

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


(use-package neotree)

(use-package magit
  :bind
  ("<f10>" . magit-status)
  ("C-x g" . magit-status))

(use-package visual-regexp )
(use-package visual-regexp-steroids )

(use-package key-chord
  :config (progn
            (key-chord-define-global "qo" 'other-window)
            (key-chord-define-global "qp" 'other-window)
            (key-chord-define-global "qf" 'find-file)
            ;; This might be too easy to mis-type, I forget why I commented q-j below
            (key-chord-define-global "qj" 'dired-jump)
            (key-chord-define-global "xf" 'find-file)
            (key-chord-define-global "qk" 'kill-this-buffer)
            (key-chord-define-global "xk" 'kill-this-buffer)
            (key-chord-define-global "qr" 'previous-buffer)
            (key-chord-define-global "qn" 'next-buffer)
            (key-chord-define-global "qs" 'save-buffer)
            (key-chord-define-global "xs" 'save-buffer)
            (key-chord-define-global "qh" 'vterm)
            (key-chord-define-global "xh" 'shell)
            ;; Too easy to misstype
            ;;  (key-chord-define-global "qe" 'eval-defun)
            ;;  (key-chord-define-global "qj" 'eval-print-last-sexp)
            (when (require 'xah-lee nil t)
              (key-chord-define-global "qr" 'xah-next-user-buffer)
              (key-chord-define-global "qn" 'xah-previous-user-buffer))
            (when (fboundp 'my/insert-roam-link)
              (key-chord-define org-mode-map "[[" #'my/insert-roam-link))))

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
  ;(google-make-newline-indent)
  ;(google-set-c-style)
  )

;; (defvar my-csharp-default-compiler nil)
;; (setq my-csharp-default-compiler "mono @@FILE@@")

;; (defun my-csharp-get-value-from-comments (marker-string line-limit)
;;   my-csharp-default-compiler)


(use-package omnisharp
  :bind (:map omnisharp-mode-map
              ([remap xref-find-definitions] . omnisharp-go-to-definition)
              ([remap xref-find-references] . omnisharp-find-usages)
              ;; `xref-pop-marker-stack' works as expected.
              ))

(defun my:term-mode-hook ()
  (setq yas-dont-activate t))

(defun my:sql-interactive-mode-init ()
  (toggle-truncate-lines t))

(add-hook 'compilation-filter-hook
          #'my:compilation-filter-init)


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
          (lambda () (global-set-key (kbd "C-x C-o") 'hs-toggle-hiding)))

(add-hook 'js2-mode-hook
          'my:js2-mode-init)

(add-hook 'emacs-lisp-mode-hook
          'my:emacs-lisp-mode-init)

(add-hook 'term-mode-hook 'my:term-mode-hook)

(add-hook 'sql-interactive-mode-hook #'my:sql-interactive-mode-init)

;; Misc file types and their default modes
(add-to-list 'auto-mode-alist '("\\.editorconfig" . conf-mode))


(show-paren-mode 1)
(auto-complete-mode 0)
(key-chord-mode 1)
(global-ede-mode 1)
(projectile-global-mode)

;;; pass
(use-package pass)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;;; Dont really get general yet
;; (use-package general
;;   :config
;;   (general-create-definer me/leader-keys
;;     ;:keymaps '(normal insert visual emacs)
;;     :prefix "C-M-]"
;;     :global-prefix "C-M-]"))

;; (me/leader-keys
;;  "t" '(:ignore t :which-key "toggles"))


(use-package hydra)
(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("+" text-scale-increase)
  ("-" text-scale-decrease)
  ("f" nil "finished" :exit t))

;;; Spacemacs
(use-package spacemacs-theme
  :defer t
  :init (load-theme 'spacemacs-dark t))
;;; End

;;; LSP
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :commands lsp)

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package flymake)

(use-package treemacs
  :bind ("<f8>" . treemacs))

(use-package lsp-treemacs
  :after lsp)

(use-package treemacs-projectile)

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred))

(use-package csharp-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-tree-sitter-mode))
  :hook ((csharp-mode . lsp-deferred)
         (csharp-tree-sitter-mode . lsp-deferred)))

;;; Tree sitter (experimental)
(use-package tree-sitter)
(use-package tree-sitter-langs)
(use-package tree-sitter-indent)

(use-package perspective
  :bind (("C-x k" . persp-kill-buffer*))
  :config (persp-mode))
