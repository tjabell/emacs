;;; TEMPO SNIPPETS
(require 'tempo)
(require 'string-inflection)

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

(tempo-define-snippet
    "esa-cr-ticket"
  '("** CR: BWSRAS-" (p "ticket number: " tn) &
    >"https://extendedstay.atlassian.net/browse/BWSRAS-" (s tn)))

(tempo-define-snippet
    "esa-ticket"
  '("** TODO BWSRAS-" (p "ticket number: " tn) " - " (p "description: " d) &
    >"https://extendedstay.atlassian.net/browse/BWSRAS-" (s tn) &
    >"feature/BWSRAS-" (s tn) "-" (string-inflection-kebab-case-function (replace-regexp-in-string "[[:space:].:/]+" "_" (tempo-lookup-named 'd)))))

(define-abbrev org-mode-abbrev-table "esacr"
  "" 'tempo-template-esa-cr-ticket)

(define-abbrev org-mode-abbrev-table "esat"
  "" 'tempo-template-esa-ticket)

(define-abbrev web-mode-abbrev-table "vifnotutil"
  "" 'tempo-template-velocity-if-not-utilmethods-isset-directive)
(define-abbrev web-mode-abbrev-table "vifnotset"
  "" 'tempo-template-velocity-if-not-utilmethods-isset-directive)

(global-set-key (kbd "<f7>") 'pop-global-mark)

;;; https://gist.github.com/kristianhellquist/3082383#gistcomment-2373734
(defun my:copy-current-line-position-to-clipboard ()
  "Copy current line in file to clipboard as '</path/to/file>:<line-number>'."
  (interactive)
  (let ((path-with-line-number
         (concat (buffer-file-name) ":" (number-to-string (line-number-at-pos)))))
    (kill-new path-with-line-number)
    (message (concat path-with-line-number " copied to clipboard"))))

(defun my:copy-relative-current-line-position-to-clipboard ()
  "Copy current line in file to clipboard as '</path/to/file>:<line-number>'."
  (interactive)
  (let ((path-with-line-number
         (concat (s-replace "/home/trevor/projects/extended_stay/src/frontend/" ""  (buffer-file-name)) ":" (number-to-string (line-number-at-pos)))))
    (kill-new path-with-line-number)
    (message (concat path-with-line-number " copied to clipboard"))))

(defun my:copy-project-current-line-position-to-clipboard ()
  "Copy current line in file to clipboard as '</path/to/file>:<line-number>'."
  (interactive)
  (cl-flet ((find-git-dir  ()
                        (file-truename
                         (locate-dominating-file (buffer-file-name (current-buffer)) ".git"))))
    (let* ((project-dir (find-git-dir))
          (path-with-line-number
           (concat "<proj>/" (s-replace project-dir ""  (buffer-file-name)) ":" (number-to-string (line-number-at-pos)))))
      (kill-new path-with-line-number)
      (message (concat path-with-line-number " copied to clipboard")))))

;;; ESA Functions to swap environments in URLs
(defun my-esa:replace-url-with-local ()
  (interactive)
  (let ((regex "http\[s\]*://.*?/")
        (replacement "http://localhost:8080/"))
    (while (re-search-forward regex nil t)
      (replace-match replacement))))

(defun my-esa:replace-url-with-dev ()
  (interactive)
  (let ((regex "http\[s\]*://.*?/")
        (replacement "http://***REMOVED***/"))
    (while (re-search-forward regex nil t)
      (replace-match replacement))))

(defun my-esa:vtl-copy-to-local ()
  (let* ((dest-suffix (substring
                       (buffer-file-name)
                       (length "/home/trevor/projects/extended_stay/src/frontend/src/"))))
    (message "copying %s %s"  buffer-file-name (concat "/mnt/local-dav/***REMOVED***/application/" dest-suffix))))

(defun my-esa:vtl-copy-to-dev ()
  (let* ((dest-suffix (substring
                       (buffer-file-name)
                       (length "/home/trevor/projects/extended_stay/src/frontend/src/"))))
    (message "copying: %s %s" buffer-file-name (concat "/mnt/dev-dav/***REMOVED***/application/" dest-suffix))
    (copy-file buffer-file-name (concat "/mnt/dev-dav/***REMOVED***/application/" dest-suffix) t)))


;;; Run in Vterm 


(defun my:insert-shrug ()
  (interactive)
  (insert "¯\_(ツ)_/¯"))
