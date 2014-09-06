(global-ede-mode 1)
(load-theme 'zenburn t)

(require 'org-install)
(require 'ob-tangle)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((ruby . t)
   (emacs-lisp . t)
   (python . t)
   (perl . t)
   (haskell . t)))

(when (require 'uniquify nil t)
  (setq 
   uniquify-buffer-name-style 'post-forward
   uniquify-separator ":"))
(when (require 'dired-x nil t))

(add-hook 'scheme-mode-hook 'paredit-mode)

(add-hook 'hs-minor-mode-hook
          (lambda ()   (global-set-key (kbd "C-x C-o") 'hs-toggle-hiding)))

(add-hook 'python-mode-hook
          (lambda ()
            (hs-minor-mode 1)
            (fset 'hide-next
                  "\C-e\C-x\C-o\C-n")))

(add-hook 'c-mode-common-hook
          (lambda ()
            (unless (or (file-exists-p "makefile")
		(file-exists-p "Makefile"))
      (set (make-local-variable 'compile-command)
                   (concat "make -k CXXFLAGS='-std=c++11' "
                           (file-name-sans-extension (or buffer-file-name "C-BUFFER")))))))

(add-hook 'c-mode-common-hook
          (semantic-mode 1))

(defun my:add-semantic-to-autocomplate ()
  (add-to-list 'ac-sources 'ac-source-semantic))

(add-hook 'c-mode-common-hook
          'my:add-semantic-to-autocomplate)

(when (require 'flymake-google-cpplint nil t)
;;; remember to install google-lint.py (pip install cpplint)
  (progn
    (defun my:flymake-google-init ()
      (require 'flymake-google-cpplint)
      (custom-set-variables
       '(flymake-google-cpplint-command "/usr/bin/cpplint"))
      (flymake-google-cpplint-load))
    (add-hook 'c++-mode-hook 'my:flymake-google-init)
    (add-hook 'c-mode-hook 'my:flymake-google-init)))

(defun my:ac-c-header-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (add-to-list 'achead:include-directories '("/usr/include" "/usr/lib/include")))

(add-hook 'c++-mode-hook
          (lambda ()
            (semantic-mode)
            (my:ac-c-header-init)))

(when (require 'auto-complete nil t)
  (progn
    (require 'auto-complete-config)
    (ac-config-default)
    (add-hook 'c-mode-common-hook (lambda ()
                                  (auto-complete-mode 1)))
    (add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (auto-complete-mode 1)
                                  (show-paren-mode)))

    (add-hook 'python-mode-hook (lambda ()
                                  (auto-complete-mode 1)))))

(when (require 'paredit nil t)
  (add-hook 'emacs-lisp-mode-hook (lambda ()
                                    (paredit-mode))))

(when (require 'powershell-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.ps1" . powershell-mode)))

(when (and (require 'e2wm nil t)
           (require 'edbi nil t))
  (load-library "e2wm-edbi"))

(when (require 'yasnippet nil t)
  (add-to-list 'yas-snippet-dirs
               "~/emacs/data/snippets/"
               (yas/global-mode 1)))

(when (require 'pymacs nil t)
  (setenv "PYMACS_PYTHON" "python2")
  (pymacs-load "ropemacs" "rope-" t))

(when (require 'nose nil t)
  (defalias 'no 'nosetests-one)
  (defalias 'na 'nosetests-all)
  (defalias 'np 'nosetests-pdb-one)
  (defalias 'nm 'nosetests-module))

(when (require 'virtualenv nil t))

(when (require 'csharp-mode nil t)
  (add-hook 'csharp-mode-hook 'auto-revert-mode))

(when (require 'hidden-mode-line-mode nil t)
  (add-hook 'after-change-major-mode-hook 'hidden-mode-line-mode))

(when (require 'google-c-style nil t)
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-make-newline-indent))

(when (require 'smart-mode-line nil t)
  (setq sml/theme 'respectful)
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
  (helm-mode 1)
  (when (require 'projectile nil t)
    (require 'helm-projectile nil t)
    ;; (add-hook 'c-mode-common-hook 'projectile-on)
    ;; (add-hook 'python-mode-hook 'projectile-on)
    ;; (add-hook 'emacs-lisp-mode-hook 'projectile-on)
    (global-set-key (kbd "C-c h") 'helm-projectile)
    (setq projectile-enable-caching t)))

(when (require 'magit nil t)
  (global-set-key (kbd "<f10>") 'magit-status))

(when (require 'visual-regexp nil t)
  (when (require 'visual-regexp-steroids nil t)))

(when (require 'key-chord nil t)
  (key-chord-define-global "xo" 'other-window)
  (key-chord-define-global "qo" 'other-window)
  (key-chord-define-global "qf" 'find-file)
  (key-chord-define-global "xf" 'find-file)
  (key-chord-define-global "qr" 'previous-buffer)
  (key-chord-define-global "qn" 'next-buffer)
  (key-chord-define-global "qm" 'execute-extended-command)
  (key-chord-define-global "xm" 'execute-extended-command)
  (key-chord-define-global "qs" 'save-buffer)
  (key-chord-define-global "xs" 'save-buffer)
  (key-chord-define-global "qh" 'shell)
  (key-chord-define-global "xh" 'shell)
  (key-chord-define-global "qe" 'eval-defun)
  (key-chord-define-global "qj" 'eval-print-last-sexp)
  (key-chord-define-global "qb" 'previous-buffer)
  (key-chord-mode 1))


(when (require 'elpy nil t)
  (elpy-enable))

