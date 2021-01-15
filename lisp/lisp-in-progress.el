(require 'magit)
(require 'tempo)
(load-library "tempo-snippets")

(tempo-define-snippet
    "assembla-ticket-email"
  '((p "Space: " project) "+" p "@tickets.assembla.com"))

(tempo-define-snippet
    "velocity-null-check-with-default"
  '("#set( " (p "LVar name: " lv) " = "(p "Default: " dv) " )" &
    "#if($UtilMethods.isSet(" (p "RVar Name: " rv) "))" &
    > "#set( " (s  lv) " = " (s  rv) ")" &
    "#end"))

(define-abbrev web-mode-abbrev-table "vdefault"
  "" 'tempo-template-velocity-null-check-with-default)

(tempo-define-snippet
    "velocity-surround-with-text-default"
  '("#textDefault( \"" (p "Key name: " key) "\" , \"" (r "Default: " dv) "\" )" ))


(tempo-define-snippet
    "velocity-if-utilmethods-isset-directive"
  '("#if( $UtilMethods.isSet(" (p "var: " v) ") )" &
    > "#end"))
(define-abbrev web-mode-abbrev-table "vifutil"
  "" 'tempo-template-velocity-if-utilmethods-isset-directive)
(define-abbrev web-mode-abbrev-table "vifset"
  "" 'tempo-template-velocity-if-utilmethods-isset-directive)

(tempo-define-snippet
    "velocity-if-not-utilmethods-isset-directive"
  '("#if( !$UtilMethods.isSet(" (p "var: " v) ") )" &
    >"#end"))
(tempo-define-snippet
    "velocity-set-directive"
  '("#set( " (p "LVar: " lv) " = "(p "RVar: " dv) " )"))

(define-abbrev web-mode-abbrev-table "vset"
  "" 'tempo-template-velocity-set-directive)

(tempo-define-snippet
    "velocity-macro-responsive-background-image-set"
  '("data-bgset=\"#responsiveBackgroundImageSet(" (p "var:" v) ")"))

;;; use in vtl
;(local-set-key (kbd "C-c C-t t") 'tempo-template-velocity-surround-with-text-default)

(define-abbrev web-mode-abbrev-table "vifnotutil"
  "" 'tempo-template-velocity-if-not-utilmethods-isset-directive)
(define-abbrev web-mode-abbrev-table "vifnotset"
  "" 'tempo-template-velocity-if-not-utilmethods-isset-directive)



(defun my-magit-commit-all (message)
  (interactive "sCommit Message: ")
  (magit-call-git "commit" "-a" "-m" message)
  (magit-refresh)  )

(defun my-magit-commit-all-and-push (message)
  (interactive "sCommit Message: ")
  (magit-call-git "commit" "-a" "-m" message)
  (magit-call-git "push")
  (magit-refresh))


(global-set-key (kbd "C-c C-g A") 'my-magit-commit-all-and-push)

(global-set-key (kbd "<f7>") 'pop-global-mark)
