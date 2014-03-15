(add-hook 'scheme-mode-hook 'paredit-mode)

(add-hook 'hs-minor-mode-hook
          (lambda ()   (global-set-key (kbd "C-x C-o") 'hs-toggle-hiding)))

(add-hook 'python-mode-hook
          (lambda () 
            (hs-minor-mode 1)
            (fset 'hide-next
                  "\C-e\C-x\C-o\C-n")))

(when (require 'flymake-google-cpplint nil t)
;;; remember to install google-lint.py
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
(add-to-list 'achead:include-directories '"/usr/lib/gcc/x86_64-pc-linux-gnu/4.7.3/include/g++-v4"))

(add-hook 'c++-mode-hook
          (lambda ()
            (semantic-mode)
            (my:ac-c-header-init)))

;; configure packages
(when (require 'color-theme nil t)
  (color-theme-initialize)
  (color-theme-dark-blue2))

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
  (setq snippet-dirs 
        `(,(concat emacs-root "emacs/data/snippets/")))
  (yas/global-mode 1))

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
  (add-hook 'c-mode-commonhook 'google-c-style)
  (add-hook 'c-mode-commonhook 'google-make-newline-indent))

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
  (helm-mode 1))
