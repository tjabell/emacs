;;; MAGIT EXTENSION FUNCTIONS
;;; Note: Converting to just use call process - shouldn't (require magit) anymore
(require 'magit)

(defun -get-autocommit-string (s)
  (format "[autocommit %s] %s" (shell-command-to-string "echo -n $(date +%Y-%m-%d)") s))

;;;###autoload
(defun tja-magit-commit-all (message)
  (interactive "sCommit Message: ")
  (magit-call-git "commit" "-a" "-m" message)
  (magit-refresh))

;;;###autoload
(defun tja-magit-commit-all-and-push (message)
  (interactive "sCommit Message: ")
  (magit-call-git "commit" "-a" "-m" message)
  (magit-call-git "push")
  (magit-refresh))

;; Only pushes project specific files (environment.org) and any todos
(defun tja-magit-commit-rebase-push-project-files (repo &rest additional-files)
  (tja--call-git-process-no-output repo "fetch" "--all")
  (tja--call-git-process-no-output repo "add" "./environment.org")
  (if additional-files
      (apply 'tja--call-git-process-no-output (append (list repo "add") additional-files)))
  (tja--call-git-process-no-output
   repo "commit" "-m"
   (-get-autocommit-string "todos, env"))
  (tja--call-git-process-no-output repo "rebase" "origin/master")
  (tja--call-git-process-no-output repo "push"))

(defun tja--call-git-process-no-output (repo &rest args)
  (apply 'call-process (append (list "git" nil nil nil "-C" repo) args)))

(defun tja-magit-commit-all-rebase-push (repo &optional master)
  (tja--call-git-process-no-output repo "fetch" "--all")
  (tja--call-git-process-no-output repo "add" "-A")
  (tja--call-git-process-no-output
   repo "commit" "-a" "-m"
   (-get-autocommit-string "dailies"))
  (tja--call-git-process-no-output repo "rebase" (or master "origin/master"))
  (tja--call-git-process-no-output repo "push"))

;;;###autoload
(defun tja-magit-commit-and-refresh-all-dailies ()
  (interactive)
  (let ((repo "/home/trevor/org-roam/"))
    (tja-magit-commit-all-rebase-push repo))
  (let ((repo "/home/trevor/emacs/"))
    (tja-magit-commit-all-rebase-push repo))
  (let ((repo "/home/trevor/projects/me"))
    (tja-magit-commit-all-rebase-push repo "origin/main")))

;;;###autoload
(defun tja-magit-commit-and-refresh-all-projects ()
  (interactive)
  (let ((repo "/home/trevor/projects/goddard/"))
    (tja-magit-commit-rebase-push-project-files repo "./todo_goddard.org" "./apis.org"))
  (let ((repo "/home/trevor/projects/parsus/"))
    (tja-magit-commit-rebase-push-project-files repo "./todo_parsus.org" "./meeting_updates.org"))
  (let ((repo "/home/trevor/projects/acdhh/"))
    (tja-magit-commit-rebase-push-project-files repo "./todo_acdhh-cdbms.org" "./todo_acdhh-www.org"))
  (let ((repo "/home/trevor/projects/extended_stay/"))
    (tja-magit-commit-rebase-push-project-files repo "./todo_esa.org"))
  (let ((repo "/home/trevor/projects/me/"))
    (tja-magit-commit-rebase-push-project-files repo "./todo_misc.org")))


(provide 'tja-magit)

(require 'vterm)

;;; https://www.reddit.com/r/emacs/comments/ft84xy/run_shell_command_in_new_vterm/
;;; I really don't get what this is doing 20211029TJA
(defun tja-vterm-run-in-vterm-kill (process event)
  "A process sentinel. Kills PROCESS's buffer if it is live."
  (let ((b (process-buffer process)))
    (and (buffer-live-p b)
         (kill-buffer b))))

;;;###autoload
(defun tja-vterm-run-in-vterm (command)
  "Execute string COMMAND in a new vterm.

  Interactively, prompt for COMMAND with the current buffer's file
  name supplied. When called from Dired, supply the name of the
  file at point.

  Like `async-shell-command`, but run in a vterm for full terminal features.

  The new vterm buffer is named in the form `*foo bar.baz*`, the
  command and its arguments in earmuffs.

  When the command terminates, the shell remains open, but when the
  shell exits, the buffer is killed."
  (interactive
   (list
    (let* ((f (cond (buffer-file-name)
                    ((eq major-mode 'dired-mode)
                     (dired-get-filename nil t))))
           (filename (concat " " (shell-quote-argument (and f (file-relative-name f))))))
      (read-shell-command "Terminal command: "
                          (cons filename 0)
                          (cons 'shell-command-history 1)
                          (list filename)))))
  (with-current-buffer (vterm (concat "*" command "*"))
    (set-process-sentinel vterm--process #'run-in-vterm-kill)
    (vterm-send-string command)
    (vterm-send-return)))

;;;###autoload
(defun tja-vterm-run-fbp-api ()
  (interactive)
  (open-or-start-vterm-buffer
   "*vterm* *FBP API*"
   "/home/trevor/projects/goddard/src/ipaas-franchiseeportal-api/"
   "./local-startup.sh"))

;;;###autoload
(defun tja-vterm-run-fbp-api-test ()
  (interactive)
  (with-current-buffer (vterm (concat "*vterm* *FBP API Tests*"))
    (vterm-send-string "cd /home/trevor/projects/goddard/src/ipaas-franchiseeportal-api/")
    (vterm-send-return)
    (vterm-send-string "./local-startup-tests.sh")
    (vterm-send-return)))

;;;###autoload
(defun tja-vterm-run-schools-api-test ()
  (interactive)
  (with-current-buffer (vterm (concat "*vterm* *FBP Schools API Tests*"))
    (vterm-send-string "cd /home/trevor/projects/goddard/src/ipaas-schools-api/")
    (vterm-send-return)
    (vterm-send-string "./local-startup-tests.sh")
    (vterm-send-return)))

;;;###autoload
(defun tja-vterm-run-faculty-api ()
  (interactive)
  (with-current-buffer (vterm (concat "*vterm* *FACULTY API*"))
    (vterm-send-string "cd /home/trevor/projects/goddard/src/ipaas-faculty-api/")
    (vterm-send-return)
    (vterm-send-string "./local-startup.sh")
    (vterm-send-return)))

;;;###autoload
(defun tja-vterm-run-tours-api ()
  (interactive)
  (with-current-buffer (vterm (concat "*vterm* *TOURS API*"))
    (vterm-send-string "cd /home/trevor/projects/goddard/src/ipaas-tours-api/")
    (vterm-send-return)
    (vterm-send-string "./local-startup.sh")
    (vterm-send-return)))

;;;###autoload
(defun tja-vterm-run-content-api ()
  (interactive)
  (with-current-buffer (vterm (concat "*vterm* *CONTENT API*"))
    (vterm-send-string "cd /home/trevor/projects/goddard/src/ipaas-content-api/")
    (vterm-send-return)
    (vterm-send-string "./local-startup.sh")
    (vterm-send-return)))

(defun open-or-start-vterm-buffer (buf folder startup-script)
    (if (buffer-live-p (get-buffer buf))
        (switch-to-buffer buf)
      (with-current-buffer (vterm (concat buf))
        (vterm-send-string (concat "cd " folder))
        (vterm-send-return)
        (vterm-send-string (concat ". " startup-script))
        (vterm-send-return))))

;;;###autoload
(defun tja-vterm-run-fbp-web ()
  (interactive)
  (open-or-start-vterm-buffer
   "*vterm* *FBP Web*"
   "/home/trevor/projects/goddard/src/FranchiseePortal-Website/"
   "./local-startup.sh"))

;;;###autoload
(defun tja-vterm-run-fbp-web-test ()
  (interactive)
  (with-current-buffer (vterm (concat "*vterm* *FBP Web Tests*"))
    (vterm-send-string "cd /home/trevor/projects/goddard/src/FranchiseePortal-Website/")
    (vterm-send-return)
    (vterm-send-string ". ./local-startup-test.sh")
    (vterm-send-return)))

;;;###autoload
(defun tja-vterm-log-franchiseportal-api ()
  (interactive)
  (with-current-buffer (vterm (concat "*vterm* *FBP WEB*"))
    (vterm-send-string "cd /home/trevor/")
    (vterm-send-return)
    (vterm-send-string "az webapp log tail --name ipaas-franchiseeportal-dev-useast-api --resource-group ipaas-dev-useast-rsg")
    (vterm-send-return)))

;;;###autoload
(defun tja-vterm-az-webapp-log (api-name environment)
  (interactive
   (list
    (completing-read "Api: "
                     '(("franchiseeportal")
                       ("content")
                       ("faculty")) nil t)
    (completing-read "Environment: "
                     '(("dev")
                       ("qa")) nil t)))
  (with-current-buffer (vterm (concat "* FBP API LOG Tail- " api-name " *"))
    (vterm-send-string "cd /home/trevor/")
    (vterm-send-return)
    (vterm-send-string (concat "az webapp log tail --name ipaas-" api-name "-" environment "-useast-api --resource-group ipaas-" environment "-useast-rsg"))
    (vterm-send-return)))

;;;###autoload
(defun tja--log-aem (env instance log)
  (let ((number (if (string-equal env "qa") "85656" "77402")))
    (with-current-buffer (vterm (concat "*vterm* *AEM LOG: " env "-"instance " ERROR *"))
      (vterm-send-string "cd /home/trevor/")
      (vterm-send-return)
      (vterm-send-string (concat  "aio cloudmanager:tail-logs " number " " instance " " log))
      (vterm-send-return))))

;;;###autoload
(defun tja-vterm-log-aem-author-dev-error ()
  (interactive)
  (tja--log-aem "dev" "author" "aemerror"))

;;;###autoload
(defun tja-vterm-log-aem-publish-dev-error ()
  (interactive)
  (tja--log-aem "dev" "publish" "aemerror"))

;;;###autoload
(defun tja-vterm-log-aem-author-qa-error ()
  (interactive)
  (tja--log-aem "qa" "author" "aemerror"))

;;;###autoload
(defun tja-vterm-log-aem-publish-qa-error ()
  (interactive)
  (tja--log-aem-dev "qa" "publish" "aemerror"))

;;;###autoload
(defun tja-vterm-esa-run-dotcms ()
  (interactive)
  (with-current-buffer (vterm (concat "*vterm* *DOTCMS*"))
    (vterm-send-string "cd /home/trevor/projects/extended_stay/src/frontend/")
    (vterm-send-return)
    (vterm-send-string ". ./local-startup.sh")
    (vterm-send-return)))

;;;###autoload
(defun tja-vterm-esa-run-esa-dotcms-node ()
  (interactive)
  (with-current-buffer (vterm (concat "*vterm* *DOTCMS - Frontend*"))
    (vterm-send-string "cd /home/trevor/projects/extended_stay/src/frontend/")
    (vterm-send-return)
    (vterm-send-string ". ./local-startup-node.sh")
    (vterm-send-return)))

(defun tja-vterm-mtsinai-run-prepc ()
  (interactive)
  (with-current-buffer (vterm (concat "*vterm* *PREPC*"))
    (vterm-send-string "cd /home/trevor/projects/mtsinai/src/parsus-internal.mountsinai-prepc")
    (vterm-send-return)
    (vterm-send-string ". ./local-startup.sh")
    (vterm-send-return)))
(provide 'tja-vterm)

;;;###autoload
(defun straight-open-repository-directory ()
"Open the Straight.el repository directory."
(interactive)
(let ((repository-dir (straight--repos-dir)))
  (when repository-dir
    (find-file repository-dir))))

;;;###autoload
(defun convert-spaces-to-underscores (start end)
  "Converts dashes to underscores in the region between START and END."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (while (search-forward " " end t)
      (replace-match "_" nil t))))

;;;###autoload
(defun convert-dashes-to-underscores (start end)
  "Converts dashes to underscores in the region between START and END."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (while (search-forward "-" end t)
      (replace-match "_" nil t))))


;;;###autoload
(defun copy-buffer-filename-to-kill-ring ()
  "Copy the filename of the current buffer to the kill ring."
  (interactive)
  (when buffer-file-name
    (kill-new buffer-file-name)
    (message "Filename copied to kill ring: %s" buffer-file-name)))

;;;###autoload
(defun insert-current-date ()
  (interactive)
  (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))
(defalias 'icd 'insert-current-date)

(defun insert-current-date2 ()
  (interactive)
  (insert (shell-command-to-string "echo -n $(date +%d-%b-%Y)")))
(defalias 'icd2 'insert-current-date2)
;;;###autoload
(defun insert-signature-for-code ()
  (interactive)
  (insert (shell-command-to-string "echo -n $(date +%Y%m%d)TJA")))
(defalias 'isc 'insert-signature-for-code)

;;;###autoload
(defun first-char-printer ()
  "Used this to print out katakana characters for anki slide deck"
  (interactive)
  (let ((c (string-to-char (buffer-substring (line-beginning-position) (1+ (line-beginning-position))))))
    (prin1 (get-char-code-property  c 'name) (current-buffer))))

;;;###autoload
(defun ebpa/edebug-remove-all-instrumentation ()
  "Remove all edebug instrumentation by visiting each function
definition and running `eval-defun`."
  (interactive)
  (mapatoms
   (lambda (symbol)
     (when-let (pos (car-safe (get symbol 'edebug)))
       (with-current-buffer (marker-buffer pos)
         (goto-char (marker-position pos))
         (eval-defun nil))))))

;;;###autoload
(defun tja-newline-directly-below ()
  "1. move to end of the line.
   2. insert newline with index"
  (interactive)
  (let ((oldcol (current-column)))
    (end-of-line)
    (newline)
    (dotimes (i oldcol)
      (insert " "))))

;;; format a json string in a buffer
;;;###autoload
(defun tja-json-mode-and-format-buffer ()
  (interactive)
  (json-mode)
  (json-pretty-print-buffer))

;; https://stackoverflow.com/questions/39861580/emacs-program-to-collapse-json-to-single-line
(defun json-to-single-line (beg end)
  "Collapse prettified json in region between BEG and END to a single line"
  (interactive "r")
  (if (use-region-p)
      (save-excursion
        (save-restriction
          (narrow-to-region beg end)
          (goto-char (point-min))
          (while (re-search-forward "\\s-+" nil t)
            (replace-match " "))))
    (print "This function operates on a region")))

(defun json-escape-for-common-lisp (beg end)
  "escapes json for use in a common lisp string"
  (interactive "r")
  (if (use-region-p)
      (save-excursion
        (save-restriction
          (narrow-to-region beg end)
          (goto-char (point-min))
          (while (re-search-forward "\"" nil t)
            (replace-match "\\\\\""))))
    (print "This function operates on a region")))

(defalias 'jm 'tja-json-mode-and-format-buffer)

(require 'notifications)

;;;###autoload
(defun tja-remind-me-in (minutes body)
  (interactive "sMinutes:\nsBody:")
  (let ((minutes (concat minutes " min")))
    (run-at-time minutes nil 'notifications-notify :title "Emacs alert" :body body)))

(provide 'tja-misc)

(defun tja-org-insert-image-from-wayland-clipboard ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (setq filename
        (concat
         (make-temp-name
          (concat (buffer-file-name)
                  "_"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (call-process "wl-paste" nil `(:file ,filename) nil "-t" "image/png")
  (insert (concat "[[" filename "]]"))
  (org-display-inline-images))

(provide 'tja-org)

(load-file "~/emacs/my-org-clockify-report.el")

(defun my:org-meta-move-to-top (&optional _arg)
  "Move the item at point up to the top of the org file just after the first header"
  (interactive "P")
  (cond
   ((run-hook-with-args-until-success 'org-metaup-hook))
   ((org-region-active-p)
    (let* ((a (save-excursion
                (goto-char (region-beginning))
                (line-beginning-position)))
           (b (save-excursion
                (goto-char (region-end))
                (if (bolp) (1- (point)) (line-end-position))))
           (c (save-excursion
                (goto-char a)
                (move-beginning-of-line 0)
                (point)))
           (d (save-excursion
                (goto-char a)
                (move-end-of-line 0)
                (point))))
      (transpose-regions a b c d)
      (goto-char c)))
   ((org-at-table-p) (org-call-with-arg 'org-table-move-row 'up))
   ((and (featurep 'org-inlinetask)
         (org-inlinetask-in-task-p))
    (org-drag-element-backward))
   ((org-at-heading-p) (call-interactively 'org-move-subtree-up))
   ((org-at-item-p) (call-interactively 'org-move-item-up))
   (t (org-drag-element-backward))))
  (defun my:org-move-item-to-top ()
    "Move the item at point up to the top of the org file just after the first header"
    (interactive)
    (unless (org-at-item-p) (error "Not at an item"))
    (let* ((col (current-column))
           (item (line-beginning-position))
           (struct (org-list-struct))
           (prevs (org-list-prevs-alist struct))
           (prev-item (org-list-get-prev-item (line-beginning-position) struct prevs)))
      (unless (or prev-item org-list-use-circular-motion)
        (user-error "Cannot move this item further up"))
      (if (not prev-item)
          (setq struct (org-list-send-item item 'end struct))
        (setq struct (org-list-swap-items prev-item item struct)))
      (org-list-write-struct struct (org-list-parents-alist struct))
      (org-move-to-column col)))

;;;###autoload
(defun tja-ocr-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (setq filename
        (concat
         (make-temp-name
          (concat (buffer-file-name)
                  "_"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (call-process "wl-paste" nil `(:file ,filename) nil "-t" "image/png")
  (call-process "tesseract" nil t nil filename "stdout"))

(provide 'tja-ocr)

(load-file "~/.azure-secrets.el")

(defun my:gsi/print-azure-ticket-title (ticket-number)
  (interactive "sTicket-number: ")
  (let* ((obj (my:gsi/get-azure-ticket ticket-number))
         (info (my:gsi/get-azure-ticket-title-and-id obj)))
    (insert (format "%s: %s" (car info) (cadr info)))))

(defun my:gsi/get-azure-ticket-title-and-id (obj)
  (let* ((props (aref (cdr (cadr obj)) 0))
         (id (cdar props))
         (title (cdr (assoc 'System.Title (assoc 'fields props)))))
    (list id title)))

(defun my:gsi/get-azure-ticket (ticket-number)
  (with-temp-buffer ; temp buffer to hold json data
    (let* ((username my/azure-un)
           (password my/azure-password)
           (api-version "7.0")
           (ticket-url (format "https://dev.azure.com/GoddardSystemsIT/_apis/wit/workitems?ids=%s&api-version=%s" ticket-number api-version))
           (url-request-extra-headers
            `(("Authorization" . ,(concat "Basic "
                                          (base64-encode-string
                                           (concat username ":" password) t))))))
      (url-insert-file-contents ticket-url))
    (json-read)))

(load-file "~/.azure-secrets.el")
(defun azure--session-call (path credentials)
   "Do a call to PATH with ARGS using current session. Does not check for session validity."
   (let ((azure-devops-v6-url "https://dev.azure.com/GoddardSystemsIT/_apis/wit/workitems?ids=12697&api-version=6.1-preview.3"))
     (apply #'request (concat azure-devops-v6-url path)
            :headers `(("Content-Type" . "application/json")
                       ("Authorization" . ,(format "Basic %s" credentials)))
            :sync t
            :parser 'json-read)))
 ;(azure--session-call "" (format "%s:%s" my/azure-un my/azure-password))

(defun point-in-comment ()
(let ((syn (syntax-ppss)))
  (and (nth 8 syn)
       (not (nth 3 syn)))))
(defun tja-sql-capitalize-all-sqlserver-keywords (min max)
  (interactive "r")
  (require 'sql)
  (save-excursion
    (dolist (keywords sql-mode-ms-font-lock-keywords)
      (goto-char min)
      (while (re-search-forward (car keywords) nil t)
        (unless (or (point-in-comment) (> (point) max))
          (goto-char (match-beginning 0))
          (upcase-word 1))))))

(defun arrayify (start end quote)
  "Turn strings on newlines into a QUOTEd, comma-separated one-liner."
  (interactive "r\nMQuote: ")
  (let ((insertion
         (mapconcat
          (lambda (x) (format "%s%s%s" quote x quote))
          (split-string (buffer-substring start end)) ", ")))
    (delete-region start end)
    (insert insertion)))

;;; js-beautify.el -- beautify some js code

(defgroup js-beautify nil
  "Use jsbeautify to beautify some js"
  :group 'editing)

(defcustom js-beautify-args "--keep-array-indentation"
  "Arguments to pass to jsbeautify script"
  :type '(string)
  :group 'js-beautify)

(defcustom js-beautify-path "/usr/bin/js-beautify"
  "Path to jsbeautifier node file"
  :type '(string)
  :group 'js-beautify)

(defun js-beautify ()
  "Beautify a region of javascript using the code from jsbeautify.org"
  (interactive)
  (let ((orig-point (point))
        (js-beautify-command (concat js-beautify-path
                                     " "
                                     js-beautify-args
                                     " "
                                     "-f -")))
    (unless (mark)
      (mark-defun))
    (shell-command-on-region (point)
                             (mark)
                             js-beautify-command
                             nil t)
    (goto-char orig-point)))

(provide 'js-beautify)
;;; js-beautify.el ends here

;;; https://gist.github.com/kristianhellquist/3082383#gistcomment-2373734
(defun tja-copy-current-line-position-to-clipboard ()
  "Copy current line in file to clipboard as '</path/to/file>:<line-number>'."
  (interactive)
  (let ((path-with-line-number
         (concat (buffer-file-name) ":" (number-to-string (line-number-at-pos)))))
    (kill-new path-with-line-number)
    (message (concat path-with-line-number " copied to clipboard"))))

(defun tja-esa-frontend-copy-relative-current-line-position-to-clipboard ()
  "Copy current line in file to clipboard as '</path/to/file>:<line-number>'."
  (interactive)
  (let ((path-with-line-number
         (concat (s-replace (expand-file-name (vc-root-dir)) ""  (buffer-file-name)) ":" (number-to-string (line-number-at-pos)))))
    (kill-new path-with-line-number)
    (message (concat path-with-line-number " copied to clipboard"))))

(defun tja-copy-project-current-line-position-to-clipboard ()
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
(defun my/esa:replace-url-with-local ()
  (interactive)
  (let ((regex "http\[s\]*://.*?/")
        (replacement "http://localhost:8080/"))
    (while (re-search-forward regex nil t)
      (replace-match replacement))))


;;; Run in Vterm
(defun my/insert-shrug ()
  (interactive)
  (insert "¯\_(ツ)_/¯"))

(defun openai/generate-amortization-calendar (principal rate years)
  (interactive "nPrincipal: \nnRate: \nnYears: ")
  "Generate an amortization calendar given the loan PRINCIPAL, annual interest RATE, and total YEARS of the loan."
  (let* ((monthly-rate (/ rate 1200.0))
         (total-months (* years 12))
         (payment (/ (* principal monthly-rate) (- 1 (expt (+ 1 monthly-rate) (- total-months))))))
    (insert (with-output-to-string
      (progn
        (princ (format "%-10s %-10s %-10s %-10s\n" "Month" "Payment" "Interest" "Principal"))
        (princ (make-string 50 ?-))
        (princ "\n")
        (cl-loop for month from 1 to total-months
                 for balance = principal then (- balance principal-paid)
                 for interest-paid = (* balance monthly-rate)
                 for principal-paid = (- payment interest-paid)
                 do (princ (format "%-10d %-10.2f %-10.2f %-10.2f\n" month payment interest-paid principal-paid))))))))

(load-file "/home/trevor/emacs/lisp/my-clockify.el")

(org-babel-load-file "~/projects/extended_stay/esa-elisp.org")

;; From chatgpt 2023-06-01
;;;###autoload
(defun my:escape-elisp-string (string)
  "Escapes special characters in the given STRING for reading as an Emacs Lisp string."
  (replace-regexp-in-string "[\"\\\\\a\b\f\n\r\t\v]"
                            (lambda (match)
                              (cond
                               ((string-equal match "\"") "\\\"")
                               ((string-equal match "\\") "\\\\")
                               ((string-equal match "\a") "\\a")
                               ((string-equal match "\b") "\\b")
                               ((string-equal match "\f") "\\f")
                               ((string-equal match "\n") "\\n")
                               ((string-equal match "\r") "\\r")
                               ((string-equal match "\t") "\\t")
                               ((string-equal match "\v") "\\v")))
                            string))

;; From chatgpt 2023-06-01
;;;###autoload
(defun my:escape-quotes (string)
  "Escapes quotes in the given STRING."
  (replace-regexp-in-string "\"" "\\\\\"" string))
