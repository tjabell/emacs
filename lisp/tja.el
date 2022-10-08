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
  (with-current-buffer (vterm (concat "*vterm* *FBP API*"))
    (vterm-send-string "cd /home/trevor/projects/goddard/src/ipaas-franchiseeportal-api/")
    (vterm-send-return)
    (vterm-send-string "./local_startup.sh")
    (vterm-send-return)))

(defun tja-vterm-run-faculty-api ()
  (interactive)
  (with-current-buffer (vterm (concat "*vterm* *FACULTY API*"))
    (vterm-send-string "cd /home/trevor/projects/goddard/src/ipaas-faculty-api/")
    (vterm-send-return)
    (vterm-send-string "./local-startup.sh")
    (vterm-send-return)))

  ;;;###autoload
(defun tja-vterm-run-fbp-web ()
  (interactive)
  (with-current-buffer (vterm (concat "*vterm* *FBP WEB*"))
    (vterm-send-string "cd /home/trevor/projects/goddard/src/FranchiseePortal-Website/")
    (vterm-send-return)
    (vterm-send-string "./local_startup.sh")
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
      (vterm-send-string "./local-startup.sh")
      (vterm-send-return)))

  ;;;###autoload
  (defun tja-vterm-esa-run-esa-dotcms-node ()
    (interactive)
    (with-current-buffer (vterm (concat "*vterm* *DOTCMS - Frontend*"))
      (vterm-send-string "cd /home/trevor/projects/extended_stay/src/frontend/")
      (vterm-send-return)
      (vterm-send-string "npm start")
      (vterm-send-return)))

  (provide 'tja-vterm)

;;;###autoload
(defun insert-current-date ()
  (interactive)
  (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))
(defalias 'icd 'insert-current-date)

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

(defalias 'jm 'tja-json-mode-and-format-buffer)

(require 'notifications)

;;;###autoload
(defun tja-remind-me-in (minutes body)
  (interactive "sMinutes:\nsBody:")
  (let ((minutes (concat minutes " min")))
    (run-at-time minutes nil 'notifications-notify :title "Emacs alert" :body body)))

(provide 'tja-misc)

(defun tja-org-screenshot ()
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

;;;###autoload
(defun tja-ocr-screenshot ()
  "Read a screenshot from the wl-paste clipboard and ocr the text into the current buffer at point"
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

(defun tja-insert-azure-title-text (ticket-number)
  (interactive "sTicket Number ")
  (insert (get-azure-title-text ticket-number)))

(defun tja-get-azure-title-text (ticket-number)
  (interactive)
  (with-current-buffer
      (get-buffer-create
       (let* ((username "parsus-ta@goddardsystems.com")
              (password "")
              (ticket-url (format "https://dev.azure.com/GoddardSystemsIT/_apis/wit/workitems?ids=%s&api-version=6.1-preview.3" ticket-number))
              (url-request-extra-headers
               `(("Authorization" . ,(concat "Basic "
                                             (base64-encode-string
                                              (concat username ":" password) t))))))
         (url-retrieve-synchronously ticket-url)))
    (goto-char (point-min))
    (re-search-forward "^$")
    (delete-region (point) (point-min))
    (let ((my-obj (json-parse-string (buffer-string))))
      (gethash "System.Title"
               (gethash "fields"
                        (aref
                         (gethash "value" my-obj)
                         0))))))

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
         (concat (s-replace "/home/trevor/projects/extended_stay/src/frontend/" ""  (buffer-file-name)) ":" (number-to-string (line-number-at-pos)))))
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
(defun my-esa:replace-url-with-local ()
  (interactive)
  (let ((regex "http\[s\]*://.*?/")
        (replacement "http://localhost:8080/"))
    (while (re-search-forward regex nil t)
      (replace-match replacement))))


;;; Run in Vterm 
(defun my:insert-shrug ()
  (interactive)
  (insert "¯\_(ツ)_/¯"))
