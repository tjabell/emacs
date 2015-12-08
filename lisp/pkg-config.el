(require 'google-c-style)
(require 'org-install)
(require 'ob-tangle)
(require 'org-journal)
(require 'uniquify)
(require 'dired-x)
(require 'flymake-google-cpplint)
(require 'paredit)
(require 'emmet-mode)
(require 'js-comint)
(require 'sgml-mode)
(require 'web-mode)
(require 'yasnippet)
(require 'company)
(require 'helm)
(require 'key-chord)
(require 'smart-mode-line)
(require 'ace-jump-mode)
(require 'ace-window)
(require 'company-c-headers)

;;; Org Mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((sh . t)
   (ruby . t)
   (emacs-lisp . t)
   (python . t)
   (perl . t)
   (haskell . t)
   (C . t)))

(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map (kbd "C-c c")  'org-capture)

(setq org-capture-templates
      '(("j" "Journal" entry (file+datetree "~/org/journal.org")
         "* %?\nEntered on %U\n %i\n %a")
        ("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
         "* TODO %?\n %i\n %a")))

(when (require 'org-journal nil t)
  (setq org-journal-dir (concat org-directory "/journal/")))

(setq org-agenda-file-regexp "\\`[^.].*\\.org'\\|[0-9]+")

(setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))

(setq org-babel-default-header-args:C
      (cons '(:tangle . "yes")
            (assq-delete-all :tangle org-babel-default-header-args)))
;; End Org mode

(when (require 'uniquify nil t)
  (setq
   uniquify-buffer-name-style 'post-forward
   uniquify-separator ":"))

(when (require 'dired-x nil t))

(when (require 'flymake-google-cpplint nil t)
;;; remember to install google-lint.py (pip install cpplint)
  (custom-set-variables
   '(flymake-google-cpplint-command "/usr/bin/cpplint")))



(when (require 'paredit nil t)
  (add-hook 'web-mode-hook 'emmet-mode))

(when (require 'emmet-mode)
  (define-key web-mode-map (kbd "C-/") 'emmet-expand-line))

(when (require 'js-comint nil t)
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

(when (require 'powershell-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.ps1" . powershell-mode)))

(when (require 'sgml-mode nil t)
  (add-hook 'html-mode-hook 'emmet-mode)
  (define-key html-mode-map (kbd "C-/") 'emmet-expand-line))

(when (require 'web-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.cshtml?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

(when (and (require 'e2wm nil t)
           (require 'edbi nil t))
  (load-library "e2wm-edbi"))

(when (require 'yasnippet nil t)
  (when (require 'dropdown-list nil t)
    (setq yas/prompt-functions '(yas/dropdown-prompt
                                 yas/ido-prompt)))
  (add-to-list 'yas-snippet-dirs
               "~/emacs/data/snippets/"))

;; (when (require 'pymacs nil t)
;;   (setenv "PYMACS_PYTHON" "python2")
;;   (pymacs-load "ropemacs" "rope-" t))

(when (require 'nose nil t)
  (defalias 'no 'nosetests-one)
  (defalias 'na 'nosetests-all)
  (defalias 'np 'nosetests-pdb-one)
  (defalias 'nm 'nosetests-module))

(when (require 'company)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3))

(when (require 'smart-mode-line nil t)
  (setq sml/theme 'respectful)
  (setq sml/no-confirm-load-theme t)
  (sml/setup))

(when (require 'ace-jump-mode nil t)
  (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
  (eval-after-load "ace-jump-mode"
    '(ace-jump-mode-enable-mark-sync))
  (define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)
  ;; ;;If you use viper mode : -- for viper mode maybe some day!
  ;; (define-key viper-vi-global-user-map (kbd "SPC") 'ace-jump-mode)
  ;; ;;If you use evil
  ;; (define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)
)

(when (require 'helm nil t)
  (global-set-key (kbd "C-c h") 'helm-mini)
  (when (require 'projectile nil t)
    (require 'helm-projectile nil t)
    (global-set-key (kbd "C-c h") 'helm-projectile)
    (setq projectile-enable-caching t)))

(when (require 'magit nil t)
  (global-set-key (kbd "<f10>") 'magit-status))

(when (require 'visual-regexp nil t)
  (when (require 'visual-regexp-steroids nil t)))

(when (require 'key-chord nil t)
  (key-chord-define-global "qo" 'other-window)
  (key-chord-define-global "qp" 'other-window)
  (key-chord-define-global "qf" 'find-file)
  (key-chord-define-global "xf" 'find-file)
  (key-chord-define-global "qk" 'kill-this-buffer)
  (key-chord-define-global "xk" 'kill-this-buffer)
  (key-chord-define-global "qr" 'previous-buffer)
  (key-chord-define-global "qn" 'next-buffer)
  (key-chord-define-global "qm" 'execute-extended-command)
  (key-chord-define-global "xm" 'execute-extended-command)
  (key-chord-define-global "qs" 'save-buffer)
  (key-chord-define-global "xs" 'save-buffer)
  (key-chord-define-global "qh" 'shell)
  (key-chord-define-global "xh" 'shell)
  ;; Too easy to misstype
  ;; (key-chord-define-global "qe" 'eval-defun)
  (key-chord-define-global "qj" 'eval-print-last-sexp)
  (key-chord-define-global "qb" 'previous-buffer)
  (when (require 'xah-lee nil t)
    (key-chord-define-global "qr" 'xah-next-user-buffer)
    (key-chord-define-global "qn" 'xah-previous-user-buffer)))

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

(defun my:compilation-filter-init ()
  (colorize-compilation-buffer))

(defun my:emacs-lisp-mode-init ()
  (paredit-mode))

(defun my:js2-mode-init ()
  (local-set-key "\C-x\C-e" 'js-send-last-sexp)
  (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
  (local-set-key "\C-cb" 'js-send-buffer)
  (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
  (local-set-key "\C-cl" 'js-load-file-and-go)
  (local-set-key (kbd "C-c C-e") 'js2-next-error)
  (semantic-mode t)
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
  (custom-set-variables
   '(js2-basic-offset 4)
   '(js2-bounce-indent-p t)))

(defun my:ac-c-header-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'achead:include-directories '("/usr/include" "/usr/lib/include")))

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

(defun my:csharp-init ()
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-omnisharp))
  (omnisharp-mode)
  (hs-minor-mode 1)
  (auto-revert-mode)
  (linum-mode)
  (c-set-style "c#")
  (if my-csharp-default-compiler
      (progn
        (fset 'orig-csharp-get-value-from-comments
              (symbol-function 'csharp-get-value-from-comments))
        (fset 'csharp-get-value-from-comments
              (symbol-function 'my-csharp-get-value-from-comments)))
    (flymake-mode)
    (flycheck-mode)))

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
(auto-complete-mode 0)
(key-chord-mode 1)
(global-ede-mode 1)
(projectile-global-mode)
(global-company-mode)
(yas/global-mode 1)
(helm-mode 1)

;;; Semantic
(semantic-mode t)
(global-semantic-idle-completions-mode t)
(global-semantic-decoration-mode t)
(global-semantic-highlight-func-mode t)
(global-semantic-show-unmatched-syntax-mode 0)
;;; End Semantic
