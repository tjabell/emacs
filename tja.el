;; [[file:tja.org::*EShell][EShell:1]]
(defun eshell/mkcd (dir)
  "Create DIR and change into it."
  (make-directory dir t)
  (eshell/cd dir))
;; EShell:1 ends here

;; [[file:tja.org::*Window Management][Window Management:1]]
(defun m/windows:save-window-configuration-to-file (filepath)
  "Save the current window configuration to the specified FILEPATH."
  (interactive "FSave window configuration to file: ")
  (with-temp-file filepath
    (prin1 (window-state-get (frame-root-window) t) (current-buffer)))
  (message "Window configuration saved to %s" filepath))

(defun m/windows:load-window-configuration-from-file (filepath)
  "Load the window configuration from the specified FILEPATH."
  (interactive "fLoad window configuration from file: ")
  (when (file-exists-p filepath)
    (with-temp-buffer
      (insert-file-contents filepath)
      (let ((config (read (current-buffer))))
        (window-state-put config (frame-root-window))))
    (message "Window configuration loaded from %s" filepath)))
;; Window Management:1 ends here

;; [[file:tja.org::*Read only mode][Read only mode:1]]
;; Define a minor mode for read-only navigation
(defvar read-only-navigation-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "f") 'forward-char)
    (define-key map (kbd "b") 'backward-char)
    (define-key map (kbd "e") 'end-of-line)
    (define-key map (kbd "a") 'beginning-of-line)
    (define-key map (kbd "v") 'scroll-up-command)
    map)
  "Keymap for `read-only-navigation-mode`.")

(define-minor-mode read-only-navigation-mode
  "Minor mode to use simple navigation keys in `read-only-mode`."
  :lighter " RO-NAV"
  :keymap read-only-navigation-mode-map)

;; Enable the minor mode when `read-only-mode` is active
(defun enable-read-only-navigation-mode ()
  "Enable `read-only-navigation-mode` when in `read-only-mode`."
  (if buffer-read-only
      (read-only-navigation-mode 1)
    (read-only-navigation-mode -1)))

(add-hook 'read-only-mode-hook 'enable-read-only-navigation-mode)
;; Read only mode:1 ends here

;; [[file:tja.org::*Mail][Mail:1]]
(global-unset-key (kbd "C-x m"))
;; Mail:1 ends here

;; [[file:tja.org::*Git][Git:1]]
(defun m/git:check-and-switch-git-branch (dir branch)
  "Check if the Git repository in DIR is on the specified BRANCH.
If not, try to switch to that branch. Return a status symbol:
- 'already-on-branch if already on the branch.
- 'switched-to-branch if switched successfully.
- 'branch-does-not-exist if the branch doesn't exist.
- 'not-a-git-repo if DIR is not a Git repository."
  (let ((default-directory dir))
    (condition-case nil
        (let* ((current-branch (string-trim
                                (shell-command-to-string "git rev-parse --abbrev-ref HEAD"))))
          (if (string-equal current-branch branch)
              (progn
                (message "Already on branch '%s'." branch)
                'already-on-branch)
            (if (zerop (call-process "git" nil nil nil "show-ref" "--verify" (format "refs/heads/%s" branch)))
                (progn
                  (shell-command (format "git checkout %s" branch))
                  (message "Switched to branch '%s'." branch)
                  'switched-to-branch)
              (progn
                (message "Warning: Branch '%s' does not exist in the repository at '%s'." branch dir)
                'branch-does-not-exist))))
      (error
       (message "Error: '%s' is not a Git repository." dir)
       'not-a-git-repo))))


(defun m/git:list-remote-branches-matching (dir ticket)
  (let* ((default-directory dir)
         (backend (vc-responsible-backend default-directory))
         (branches (vc-call-backend backend 'branches))
         (output (with-output-to-string
                   (with-current-buffer standard-output
                     (vc-git-command standard-output 0 nil "branch" "-r"))))
         (remote-branches  (split-string output "\n" t "[ \t]+"))
         (filtered-branches (cl-remove-if-not (lambda (b) (string-match-p (regexp-quote ticket) b)) remote-branches)))
    filtered-branches))

(defun m/git:checkout-branch (dir branch)
  (let* ((default-directory dir))
    (vc-git-command nil 0 nil "checkout" branch)))

(defun m/git:checkout-new-branch (dir new-branch origin-branch)
  (let* ((default-directory dir))
    (vc-git-command nil 0 nil "checkout" "-b" new-branch origin-branch)))

(defun m/git:merge-branch (dir branch)
  (let* ((default-directory dir))
    (vc-git-command nil 0 nil "merge" "--no-ff" branch)))

(defun m/git:push-branch (dir branch)
  (let* ((default-directory dir))
    (vc-git-command nil 0 nil "push" "origin" branch)))

(defun m/git:tag-branch (dir branch tag-name tag-desc)
  (let* ((default-directory dir))
    (vc-git-command nil 0 nil "tag" "-a" tag-name "-m" tag-desc branch)))

(defun m/git:make-date-version-number ()
  (format "v%s" (format-todays-date)))
;; Git:1 ends here

;; [[file:tja.org::*Markdown/Templating][Markdown/Templating:1]]

;; Helper function (same as before):
(defun m/gsi:azure-get-done-tickets-for-changelog ()
  (let* ((wql *WQL-FOR-DONE-TICKETS*)
         (response (m/gsi:get-azure-tickets wql 'format))) ; Assuming m/gsi:get-azure-tickets returns the raw JSON response
    (if (request-success-p response)
        (let ((data (json-read-from-string (request-response-data response))))
          (if (alist-get 'workItems data)
              data
            (error "No 'workItems' found in Azure API response")))
      (error "Azure DevOps API request failed: %s" (request-response-data response)))))

(defun m/gsi/emacs:changelog:insert-release ()
  "Insert a new release entry into the changelog file, fetching tickets from Azure DevOps."
  (interactive)
  (let* ((date (format-time-string "%Y-%m-%d" (org-read-date nil t nil "Select a date")))
         (version (read-string "Enter version string: "))
         (azure-tickets (m/gsi:azure-get-done-tickets-for-changelog))
         (tickets (if (and azure-tickets (alist-get 'workItems azure-tickets))
                      (mapcar (lambda (item) (number-to-string (alist-get 'id item))) (alist-get 'workItems azure-tickets))
                    (split-string (read-string "Enter ticket numbers (space-separated, or leave blank if fetched from Azure): ") " ")))
         (change-request (read-string "Enter Change Request (or leave blank): "))
         (release-plan (read-string "Enter Release Plan items (newline separated, or leave blank): "))
         (rollback-plan (read-string "Enter Rollback Plan items (newline separated, or leave blank): "))
         (release-markdown (concat
                            (format "## [%s] - %s\n" version date)
                            (format "Release Scheduled: 9:00PM ET\n")
                            (format "Release Completed: \n")
                            (if (string-empty-p change-request)
                                (format "Change Request: TBD\n\n")
                              (format "Change Request: [%s]()\n\n" change-request))
                            (format "### Tickets\n")
                            (mapconcat (lambda (ticket) (format "#%s\n" ticket)) tickets "") ; Add newline after each ticket
                            (if (string-empty-p release-plan)
                                (format "\n### Release Plan\n1. \n")
                              (format "\n### Release Plan\n%s" (mapconcat 'identity (split-string release-plan "\n") "\n")))
                            (if (string-empty-p rollback-plan)
                                (format "\n### Rollback Plan\n1. \n")
                              (format "\n### Rollback Plan\n%s" (mapconcat 'identity (split-string rollback-plan "\n") "\n"))))))


    (goto-char (point-min))             ; Go to beginning of buffer
    (search-forward "Canonical URL:") ; Find the canonical URL line (more robust anchor)
    (forward-line 3)         ; Skip three lines after "Canonical URL:"


    (insert release-markdown)        ; Insert the new release markdown

    ;; Optional: Reformat the buffer (e.g., using markdown-mode if available)
    (if (fboundp 'markdown-mode)
        (markdown-mode))))

;; (defun m/changelog:generate-release-markdown (date version items)
;;   "Generate a Markdown entry for a release.
;; DATE is a string formatted as 'yyyy-mm-dd'.
;; VERSION is the version string.
;; ITEMS is a list of item numbers, e.g., '(1 2 3 4)."
;;   (interactive
;;    (list
;;     (format-time-string "%Y-%m-%d" (org-read-date nil t nil "Select a date"))
;;     (read-string "Enter version string: ")
;;     (let ((input (read-string "Enter item numbers (space-separated): ")))
;;       (mapcar #'string-to-number (split-string input " ")))))
;;   (let ((header (format "## [%s] - %s\n" version date))
;;         (schedule "Release Scheduled: 9:00PM ET\n")
;;         (completed "Release Completed: \n")
;;         (change-request "Change Request: [TBD]()\n\n")
;;         (tickets-header "### Tickets\n")
;;         (ticket-list (mapconcat (lambda (item) (format "#%d" item)) items "\n"))
;;         (release-plan "\n### Release Plan\n1. \n")
;;         (rollback-plan "\n### Rollback Plan\n1. \n"))
;;     (with-current-buffer (generate-new-buffer "*Release Markdown*")
;;       (insert header schedule completed change-request tickets-header ticket-list release-plan rollback-plan)
;;       (markdown-mode)
;;       (pop-to-buffer (current-buffer)))))
;; Markdown/Templating:1 ends here

;; [[file:tja.org::*Magit][Magit:1]]
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
;; Magit:1 ends here

;; [[file:tja.org::*Compile][Compile:1]]
(setq special-display-buffer-names
      '("*compilation*"))

;; Stop compilation buffer from appearing in new window
(setq special-display-function
      (lambda (buffer &optional args)
        (get-buffer-window buffer 0)))

;;;###autoload
(defun tja-compile-leads-api-unit-tests ()
  (interactive)
  (compile "dotnet test /home/trevor/projects/goddard/src/ipaas-leads-api/Goddard.LeadsApi.UnitTests/Goddard.LeadsApi.UnitTests.csproj"))

(defun my:get-integration-test-command-with-filter (testcmd filter)
  (let* ((cmd (concat "dotnet test --logger='console;verbosity=detailed' " testcmd))
         (cmd (if (> (length filter) 0) (concat cmd " --filter \"" filter "\"") cmd)))
    cmd))

;;;###autoload
(defun my:compile-leads-api-integration-tests (filter)
  (interactive "sFilter: ")
  (let ((cmd (my:get-integration-test-command-with-filter
              "/home/trevor/projects/goddard/src/ipaas-leads-api/Goddard.LeadsApi.IntegrationTests/Goddard.LeadsApi.IntegrationTests.csproj"
              filter)))
    (compile cmd)))

;;;###autoload
(defun my:compile-recognitions-api-integration-tests (filter)
  (interactive "sFilter: ")
  (let ((cmd (my:get-integration-test-command-with-filter
              "/home/trevor/projects/goddard/src/ipaas-recognitions-api/GoddardRecognitions.IntegrationTests/GoddardRecognitions.IntegrationTests.csproj"
              filter)))
    (compile cmd)))

;;;###autoload
(defun m/gsi:compile-tours-api-unit-tests ()
  (interactive)
  (compile "dotnet test /home/trevor/projects/goddard/src/ipaas-tours-api/Goddard.ToursWebApi.UnitTests/Goddard.ToursWebApi.UnitTests.csproj"))

;;;###autoload
(defun m/gsi:compile-tours-api-integration-tests (filter)
  (interactive "sFilter: ")
  (let ((cmd (my:get-integration-test-command-with-filter "/home/trevor/projects/goddard/src/ipaas-tours-api/Goddard.ToursWebApi.IntegrationTests/Goddard.ToursWebApi.IntegrationTests.csproj" filter)))
    (compile cmd)))
;; Compile:1 ends here

;; [[file:tja.org::*Vterm][Vterm:1]]
(require 'vterm)

(defvar *CUSTOM-BRANCH* "custom/local")

(defun open-or-start-vterm-buffer (buf folder startup-script)
  (if (buffer-live-p (get-buffer buf))
      (switch-to-buffer buf)
    (with-current-buffer (vterm (concat buf))
      (vterm-send-string (concat "cd " folder))
      (vterm-send-return)
      (vterm-send-string startup-script)
      (vterm-send-return))))

;;; https://www.reddit.com/r/emacs/comments/ft84xy/run_shell_command_in_new_vterm/
;;; I really don't get what this is doing 20211029TJA
(defun m/gsi:vterm-run-in-vterm-kill (process event)
  "A process sentinel. Kills PROCESS's buffer if it is live."
  (let ((b (process-buffer process)))
    (and (buffer-live-p b)
         (kill-buffer b))))

;;;###autoload
(defun m/gsi:vterm-run-in-vterm (command)
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
(defun m/gsi:vterm-run-beancount-fava ()
  (interactive)
  (open-or-start-vterm-buffer
   "*vterm* *BEANCOUNT FAVA*"
   "/home/trevor/env/tools/"
   ". ./start-beancount-fava.sh"))

(defun m/gsi:vterm-run-beancount-import ()
  (interactive)
  (open-or-start-vterm-buffer
   "*vterm* *BEANCOUNT IMPORT*"
   "/home/trevor/env/tools/"
   ". ./start-beancount-import.sh"))

;;;###autoload
(defun m/gsi:vterm-connect-vpn-equinox ()
  (interactive)
  (open-or-start-vterm-buffer
   "*vterm* *EQUINOX VPN*"
   "/home/trevor/projects/equinox"
   "~/.secrets.sh && echo $EQUINOXPWD | sudo openconnect --no-dtls vpn.eqpmt.net -u eqpmt.net\\tabell -v"))

;;;###autoload
(defun m/gsi:vterm-connect-vpn-goddard ()
  (interactive)
  (open-or-start-vterm-buffer
   "*vterm* *GODDARD VPN*"
   "/home/trevor/projects/goddard"
   "~/.secrets.sh && echo $GODDARDPWD | sudo openconnect --no-dtls vpn.goddardsystems.com -u parsus-ta"))

;;;###autoload
(defun m/gsi:vterm-run-fbp-api ()
  (interactive)
  (let* ((project-dir "/home/trevor/projects/goddard/src/ipaas-franchiseeportal-api/")
         (custom-branch "custom/local-changes"))
    (m/git:check-and-switch-git-branch project-dir custom-branch)
    (open-or-start-vterm-buffer
     "*vterm* *FBP API*"
     "/home/trevor/projects/goddard/src/ipaas-franchiseeportal-api/"
     ". ./local-startup.sh")))

;;;###autoload
(defun m/gsi:vterm-stop-fbp-api ()
  (interactive)
  (m/vterm:stop "*vterm* *FBP API*"))

;;;###autoload
(defun m/gsi:vterm-run-fbp-api-test ()
  (interactive)
  (open-or-start-vterm-buffer "*vterm* *FBP API Tests*"
                              "/home/trevor/projects/goddard/src/ipaas-franchiseeportal-api/"
                              ". ./local-startup-tests.sh"))

;;;###autoload
(defun m/gsi:vterm-run-schools-api-test ()
  (interactive)
  (open-or-start-vterm-buffer "*vterm* *FBP Schools API Tests*"
                              "/home/trevor/projects/goddard/src/ipaas-schools-api/"
                              ". ./local-startup-tests.sh"))

;;;###autoload
(defun m/gsi:vterm-run-faculty-api ()
  (interactive)
  (open-or-start-vterm-buffer "*vterm* *FACULTY API*"
                              "/home/trevor/projects/goddard/src/ipaas-faculty-api/"
                              ". ./local-startup.sh"))

;;;###autoload
(defun m/gsi:vterm-run-schools-api ()
  (interactive)
  (open-or-start-vterm-buffer
   "*vterm* *SCHOOLS API*"
   "/home/trevor/projects/goddard/src/ipaas-schools-api/"
   ". ./local-startup.sh"))

;;;###autoload
(defun m/gsi:vterm-run-school-features-api ()
  (interactive)
  (open-or-start-vterm-buffer
   "*vterm* *SCHOOL FEATURES API*"
   "/home/trevor/projects/goddard/src/ipaas-schoolfeatures-api/"
   ". ./local-startup.sh"))

;;;###autoload
(defun m/gsi:vterm-run-authorization-api ()
  (interactive)
  (open-or-start-vterm-buffer "*vterm* *AUTHORIZATION API*"
                              "/home/trevor/projects/goddard/src/ipaas-authorization-api/"
                              ". ./local-startup.sh"))

;;;###autoload
    (defun m/gsi:vterm-run-mock-gsi-servers ()
      (interactive)
      (open-or-start-vterm-buffer
       "*vterm* *MOCK GSI SERVERS*"
       "/home/trevor/projects/goddard/src/mock-crm-server/"
       "./mock-gsi-servers"))

;;;###autoload
(defun m/gsi:vterm-run-tours-api ()
  (interactive)
  (let ((project-dir "/home/trevor/projects/goddard/src/ipaas-tours-api/")
        (branch *CUSTOM-BRANCH*))
    (m/git:check-and-switch-git-branch project-dir branch)
    (open-or-start-vterm-buffer
     "*vterm* *TOURS API*"
     project-dir
     ". ./local-startup.sh")))

;;;###autoload
(defun m/gsi:vterm-run-school-events-api ()
  (interactive)
  (let* ((project-dir "/home/trevor/projects/goddard/src/ipaas-schoolevents-api/")
           (custom-branch "custom/local-changes"))
      (m/git:check-and-switch-git-branch project-dir custom-branch)
      (open-or-start-vterm-buffer
       "*vterm* *SCHOOL EVENTS API*"
       project-dir
       ". ./local-startup.sh")))

(defun m/gsi:vterm-stop-tours-api ()
  (interactive)
  (m/vterm:stop "*vterm* *TOURS API*"))

;;;###autoload
(defun m/gsi:vterm-run-tours-api-test ()
  (interactive)
  (open-or-start-vterm-buffer
   "*vterm* *FBP TOURS API Tests*"
   "/home/trevor/projects/goddard/src/ipaas-tours-api/"
   ". ./local-startup-tests.sh"))

;;;###autoload
(defun m/gsi:vterm-run-leads-api ()
  (interactive)
  (let ((project-dir "/home/trevor/projects/goddard/src/ipaas-leads-api/")
        (branch *CUSTOM-BRANCH*))
    (m/git:check-and-switch-git-branch project-dir branch)
    (open-or-start-vterm-buffer
     "*vterm* *LEADS API*"
     project-dir
     ". ./local-startup.sh")))

;;;###autoload
(defun m/gsi:vterm-run-recognitions-api ()
  (interactive)
  (open-or-start-vterm-buffer
   "*vterm* *RECOGNITIONS API*"
   "/home/trevor/projects/goddard/src/ipaas-recognitions-api/"
   ". ./local-startup.sh"))

(defun m/vterm:stop (buffer)
  "Stop a vterm buffer by its name BUFFER."
  (when (get-buffer buffer)
    (with-current-buffer buffer
      (when (and (derived-mode-p 'vterm-mode)
                 (get-buffer-process buffer))
        (ignore-errors
          (vterm-send-string "\C-c")
          ;(sleep-for 1)  ;; Reduce sleep time if possible
          ;; Temporarily disable any query functions that might prevent this buffer from being stopped.
          ;; I.e. we really want to kill it
          (let ((kill-buffer-query-functions nil))
            (kill-buffer buffer))theme7-layout.component.html)))))

;; (defun tmp/stop-tours ()
;;   (let ((bufname "*vterm* *TOURS API*"))
;;     (with-current-buffer (get-buffer bufname)
;;       (let ((kill-buffer-query-functions nil))
;;         (kill-buffer bufname)))))

(defun m/gsi:vterm-stop-leads-api ()
  (interactive)
  (m/vterm:stop "*vterm* *LEADS API*"))

;;;###autoload
(defun m/gsi:vterm-run-leads-api-unit-test ()
  (interactive)
  (open-or-start-vterm-buffer
   "*vterm* *FBP LEADS API Tests*"
   "/home/trevor/projects/goddard/src/ipaas-leads-api/"
   ". ./local-startup-unit-tests.sh"))

;;;###autoload
(defun m/gsi:vterm-run-content-api ()
  (interactive)
  (open-or-start-vterm-buffer "*vterm* *CONTENT API*"
                              "/home/trevor/projects/goddard/src/ipaas-content-api/"
                              ". ./local-startup.sh"))

(require 'json-mode)

(defun curl-and-format-json (url buffer-name)
  "Fetch JSON data from the given URL using curl, place the result in a new buffer,
     set the buffer to json-mode, and format the buffer."
  (interactive "sEnter URL: ")
  (let ((json-buffer (generate-new-buffer buffer-name)))
    (with-current-buffer json-buffer
      (shell-command (concat "curl -sb -H 'Accept: application/json' '" url "'") t)
      (json-mode)
      (json-pretty-print-buffer))
    (pop-to-buffer json-buffer)))

(defun m/gsi:execute-content-api-call-qa-schools-randolph ()
  (interactive)
  (curl-and-format-json "https://ipaas-content-qa-useast-api.azurewebsites.net/api/v1/dcp/schools?crmId=09eaf707-0c18-db11-b2e1-0014221c4264" "*CONTENT-API-SCHOOLS-RANDOLPH*"))


;;;###autoload
(defun m/gsi:vterm-run-content-api-unit-test ()
  (interactive)
  (open-or-start-vterm-buffer
   "*vterm* *FBP CONTENT API Tests*"
   "/home/trevor/projects/goddard/src/ipaas-content-api/src/Goddard.ContentWebApiUnitTests"
   ". ./local-startup-unit-tests.sh"))

;;;###autoload
(defun m/gsi:vterm-run-fbp ()
  (interactive)
  (m/gsi:vterm-run-fbp-api)
  (m/gsi:vterm-run-fbp-web)
  ;; Schools api needed for login.  Other local apis can be run as required.
  (m/gsi:vterm-run-schools-api)
  (m/gsi:vterm-run-authorization-api)
  (m/gsi:vterm-run-mock-gsi-servers)
  (m/gsi:vterm-run-content-api))

;;;###autoload
(defun m/gsi:vterm-run-fbp-for-tours ()
  (interactive)
  (funcall-interactively #'m/gsi:vterm-run-fbp)
  (m/gsi:vterm-run-tours-api)
  (m/gsi:vterm-run-leads-api))

;;;###autoload
(defun m/gsi:vterm-stop-fbp ()
  (interactive)
  (m/gsi:vterm-stop-fbp-web)
  (m/gsi:vterm-stop-fbp-api)
  (m/gsi:vterm-stop-tours-api)
  (m/gsi:vterm-stop-leads-api)
  (m/vterm:stop "*vterm* *RECOGNITIONS API*")
  (m/vterm:stop "*vterm* *FACULTY API*")
  (m/vterm:stop "*vterm* *SCHOOLS API*")
  (m/vterm:stop "*vterm* *SCHOOL EVENTS API*")
  (m/vterm:stop "*vterm* *CONTENT API*")
  (m/vterm:stop "*vterm* *AUTHORIZATION API*")
  (m/vterm:stop "*vterm* *MOCK GSI SERVERS*"))

;;;###autoload
(defun m/gsi:vterm-run-fbp-web ()
  (interactive)
  (let* ((project-dir "/home/trevor/projects/goddard/src/FranchiseePortal-Website/")
         (custom-branch "custom/local-changes"))
    (m/git:check-and-switch-git-branch project-dir custom-branch)
    (open-or-start-vterm-buffer
     "*vterm* *FBP Web*"
     project-dir
     ". ./local-startup.sh")))

(defun m/gsi:vterm-stop-fbp-web ()
  (interactive)
  (m/vterm:stop "*vterm* *FBP Web*"))

;;;###autoload
(defun m/gsi:vterm-run-fbp-web-test ()
  (interactive)
  (open-or-start-vterm-buffer
   "*vterm* *FBP Web Tests*"
   "/home/trevor/projects/goddard/src/FranchiseePortal-Website/"
   ". ./local-startup-test.sh"))

;;;###autoload
(defun m/gsi:vterm-log-franchiseportal-api ()
  (interactive)
  (with-current-buffer (vterm (concat "*vterm* *FBP WEB*"))
    (vterm-send-string "cd /home/trevor/")
    (vterm-send-return)
    (vterm-send-string "az webapp log tail --name ipaas-franchiseeportal-dev-useast-api --resource-group ipaas-dev-useast-rsg")
    (vterm-send-return)))

;;;###autoload
(defun m/gsi:vterm-az-webapp-log (api-name environment)
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
(defun my:-log-aem (env instance log)
  (let ((number (if (string-equal env "qa") "85656" "77402")))
    (with-current-buffer (vterm (concat "*vterm* *AEM LOG: " env "-"instance " ERROR *"))
      (vterm-send-string "cd /home/trevor/")
      (vterm-send-return)
      (vterm-send-string (concat  "aio cloudmanager:tail-logs " number " " instance " " log))
      (vterm-send-return))))

;;;###autoload
(defun m/gsi:vterm-log-aem-author-dev-error ()
  (interactive)
  (my:-log-aem "dev" "author" "aemerror"))

;;;###autoload
(defun m/gsi:vterm-log-aem-publish-dev-error ()
  (interactive)
  (my:-log-aem "dev" "publish" "aemerror"))

;;;###autoload
(defun m/gsi:vterm-log-aem-author-qa-error ()
  (interactive)
  (my:-log-aem "qa" "author" "aemerror"))

;;;###autoload
(defun m/gsi:vterm-log-aem-publish-qa-error ()
  (interactive)
  (my:-log-aem-dev "qa" "publish" "aemerror"))

(defun m/mtsinai:vterm-mtsinai-run-prepc ()
  (interactive)
  (with-current-buffer (vterm (concat "*vterm* *PREPC*"))
    (vterm-send-string "cd /home/trevor/projects/mtsinai/src/parsus-internal.mountsinai-prepc")
    (vterm-send-return)
    (vterm-send-string ". ./local-startup.sh")
    (vterm-send-return)))

(defun m/net:get-wlan-ip-address ()
  "Retrieve the IP address of the wlan interface."
  (interactive)
  (let ((ip-output (shell-command-to-string "ip addr show wlan0 | grep 'inet ' | awk '{print $2}' | cut -d'/' -f1")))
    (string-trim ip-output)))

(defun m/wayvnc:check-or-start-wayvnc ()
  "Check if WayVNC is running, start it if not, and return its PID."
  (interactive)
  (let ((wayvnc-pid (shell-command-to-string "pgrep wayvnc"))
        (ip-to-bind (m/net:get-wlan-ip-address)))
    (if (string-empty-p wayvnc-pid)
        (progn
          (start-process "wayvnc" "*wayvnc*" "wayvnc"
                         "--output=HEADLESS-1"
                         "--max-fps=30"
                         ip-to-bind
                         "5900"
                         "-Ldebug")
          (message "WayVNC started on ip %s." ip-to-bind))
      (message "WayVNC is already running with PID: %s Probably on IP %s" (string-trim wayvnc-pid) ip-to-bind)
      (string-trim wayvnc-pid))))

(defun m/sway:check-or-create-headless-output ()
  "Check if a headless output 'HEADLESS-1' exists. If not, create it using swaymsg."
  (interactive)
  (let ((output-exists
         (shell-command-to-string "swaymsg -t get_outputs | grep 'HEADLESS-1'")))
    (if (string-empty-p (string-trim output-exists))
        (progn
          (shell-command
           "swaymsg create_output HEADLESS-1 resolution '2388x1668'")
          (m/sway:headless-set-ipad-resolution))
      (message "Headless output 'HEADLESS-1' already exists."))))

(defun m/sway:headless-set-ipad-resolution ()
  "Check if a headless output 'HEADLESS-1' exists. If so, set resolution to android pixel 7 pro."
  (interactive)
  (let ((output-exists
         (shell-command-to-string "swaymsg -t get_outputs | grep 'HEADLESS-1'")))
    (if (not (string-empty-p (string-trim output-exists)))
        (progn
          (shell-command
           "swaymsg output HEADLESS-1 resolution '2388x1668'")
          (message "Created headless output 'HEADLESS-1' with resolution 1668x2388."))
      (message "Headless output 'HEADLESS-1' already exists."))))

(defun m/sway:headless-set-android-resolution ()
  "Check if a headless output 'HEADLESS-1' exists. If so, set resolution to android pixel 7 pro."
  (interactive)
  (let ((output-exists
         (shell-command-to-string "swaymsg -t get_outputs | grep 'HEADLESS-1'")))
    (if (not (string-empty-p (string-trim output-exists)))
        (progn
          (shell-command
           "swaymsg output HEADLESS-1 resolution '892x412'")
          (message "Updated headless output 'HEADLESS-1' to resolution 892x412"))
      (message "Headless output 'HEADLESS-1' already exists."))))

(provide 'm/gsi:vterm)
;; Vterm:1 ends here

;; [[file:tja.org::*Misc functions][Misc functions:1]]
(defun m/replace-strings-in-region-with-random (start end)
  "Parse a list in the region between START and END.
Replace each string in the list with a random string of the same length."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (while (re-search-forward "\"\\([^\"]+\\)\"" end t)
      (let* ((original-string (match-string 1))
             (random-string (mapconcat (lambda (_) (char-to-string (+ ?a (random 26))))
                                       (make-list (length original-string) nil)
                                       "")))
        (replace-match (concat "\"" random-string "\"") t t)))))

;; Join Lines from: https://whatacold.io/blog/2023-06-12-emacs-join-lines/
;;;###autoload
(defvar m/join-lines--last-separator ","
  "Keep the last used separator for `w/join-lines', a comma by default.")

;;;###autoload
(defun m/join-lines (&optional specify-separator)
  "Join lines in the active region by a separator, by default the last used.
Specify the separator by typing C-u before executing this command.

Note: it depends on s.el."
  (interactive "P")
  (require 's)
  (unless (region-active-p)
    (error "select a region of lines first."))
  (let* ((separator (if (not specify-separator)
                        m/join-lines--last-separator
                      (read-string "Separator: ")))
         (text (buffer-substring-no-properties
               (region-beginning)
               (region-end)))
         (lines (split-string text "\n"))
         (result (s-join separator lines)))
    (delete-region (region-beginning) (region-end))
    (insert result)
    (setq w/join-lines--last-separator separator)))

;;;###autoload
(defun m/despacify (start end)
  (interactive "r")
  (unless (region-active-p)
    (error "select a region of lines first."))
  (replace-regexp-in-region "\\s-+" " " start end))

;;;###autoload
(defun m/straight-open-repository-directory ()
  "Open the Straight.el repository directory."
  (interactive)
  (let ((repository-dir (straight--repos-dir)))
    (when repository-dir
      (find-file repository-dir))))

;;;###autoload
(defun m/convert-spaces-to-underscores (start end)
  "Converts dashes to underscores in the region between START and END."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (while (search-forward " " end t)
      (replace-match "_" nil t))))

;;;###autoload
(defun m/convert-dashes-to-underscores (start end)
  "Converts dashes to underscores in the region between START and END."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (while (search-forward "-" end t)
      (replace-match "_" nil t))))


;;;###autoload
(defun m/copy-buffer-filename-to-kill-ring ()
  "Copy the filename of the current buffer to the kill ring."
  (interactive)
  (when buffer-file-name
    (kill-new buffer-file-name)
    (message "Filename copied to kill ring: %s" buffer-file-name)))

;;;###autoload
(defun m/insert-current-date ()
  (interactive)
  (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))
(defalias 'm/icd 'm/insert-current-date)

(defun m/insert-current-date2 ()
  (interactive)
  (insert (shell-command-to-string "echo -n $(date +%d-%b-%Y)")))
(defalias 'm/icd2 'm/insert-current-date2)

;;;###autoload
(defun m/insert-signature-for-code ()
  (interactive)
  (insert (shell-command-to-string "echo -n tja_$(date +%Y%m%d)")))
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
(defun m/json-to-single-line (beg end)
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

(defun m/json-escape-for-common-lisp (beg end)
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
(defun m/notify:remind-me-in (minutes body)
  (interactive "sMinutes:\nsBody:")
  (let ((minutes (concat minutes " min")))
    (run-at-time minutes nil 'notifications-notify :title "Emacs alert" :body body)))

(provide 'tja-misc)
;; Misc functions:1 ends here

;; [[file:tja.org::*Org screenshot][Org screenshot:1]]
(defun m/org:insert-org-ocr-image-from-wayland-clipboard ()
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
;; Org screenshot:1 ends here

;; [[file:tja.org::*Org Clock][Org Clock:1]]
(load-file "~/emacs/my-org-clockify-report.el")
;; Org Clock:1 ends here

;; [[file:tja.org::*Org workflow movement][Org workflow movement:1]]
(defun m/org:meta-move-to-top (&optional _arg)
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

(defun m/org:move-item-to-top ()
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

;;; wip
(defun m/org:generate-todo-report ()
  'wip)
;; Org workflow movement:1 ends here

;; [[file:tja.org::*OCR][OCR:1]]
;;;###autoload
(defun m/ocr:convert-clipboard-screenshot-to-text ()
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

(provide 'm/ocr)
;; OCR:1 ends here

;; [[file:tja.org::*Azure][Azure:1]]
;; set Azure UN/PW
(load-file "~/.azure-secrets.el")

(defun m/gsi:print-ticket-heading (ticket-number)
  (interactive "sTicket-number: ")
  (let* ((obj (m/gsi:get-azure-ticket ticket-number))
         (info (m/gsi:get-azure-ticket-title-and-id obj)))
    (insert (format "%s: %s" (car info) (cadr info)))))

(defun m/gsi:get-azure-ticket-title-and-id (obj)
  (let* ((props (aref (cdr (cadr obj)) 0))
         (id (cdar props))
         (title (cdr (assoc 'System.Title (assoc 'fields props)))))
    (list id title)))

(defun m/gsi:get-azure-ticket (ticket-number)
  (with-temp-buffer ; temp buffer to hold json data
    (let* ((username *MY-AZURE-UN*)
           (password *MY-AZURE-PW*)
           (api-version "7.0")
           (ticket-url (format "https://dev.azure.com/GoddardSystemsIT/_apis/wit/workitems?ids=%s&api-version=%s" ticket-number api-version))
           (url-request-extra-headers
            `(("Authorization" . ,(concat "Basic "
                                          (base64-encode-string
                                           (concat username ":" password) t))))))
      (url-insert-file-contents ticket-url))
    (json-read)))

(defun m/gsi:get-azure-tickets (wql display-fn)
  (let* ((username *MY-AZURE-UN*)
         (password *MY-AZURE-PW*)
         (api-version "7.1-preview.2")
         (api-url (format "https://dev.azure.com/GoddardSystemsIT/_apis/wit/wiql?api-version=%s" api-version))
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(concat "Basic "
                                        (base64-encode-string
                                         (concat username ":" password) t))))))
    (request
      api-url
      :type "POST"
      :sync t
      :data (json-encode `((query . ,wql)))
      :headers url-request-extra-headers
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (message (format "successfully retrieved from %s" api-url))
                  (funcall display-fn data)))
      :error (cl-function
              (lambda (&key symbol-status data error-thrown &allow-other-keys&rest _)
                (let ((buffer (get-buffer-create "*Example.org Response*")))
                  (with-current-buffer buffer
                    (erase-buffer)
                    (insert (format "Error:\n%s" error-thrown))
                    (display-buffer buffer))))))))


(defvar *WQL-FOR-DONE-TICKETS*
 "Select [System.Id], [System.Title], [System.State] From WorkItems
  Where ([System.WorkItemType] = 'User Story' OR [System.WorkItemType] = 'Bug')
  AND [System.TeamProject] = 'GTS Elevate and Ignite'
  AND [System.BoardColumnDone] = true
  AND [System.AssignedTo] = 'Parsus-TA@GoddardSystems.com'
  AND ([System.State] = 'UAT')
  AND [State] <> 'Removed'
  AND [State] <> 'Closed'
  order by [System.WorkItemType] desc, [Microsoft.VSTS.Common.Priority] asc, [System.CreatedDate] desc")

(defun m/gsi/emacs:azure-report-fbp-done-tickets ()
  (interactive)
  (cl-flet ((display-in-new-buffer (data)
              (let ((buffer (get-buffer-create "*Azure API Response*")))
                (with-current-buffer buffer
                  (erase-buffer)
                  (when (not data)
                    (insert "No tickets to release"))
                  (insert (json-encode data))
                  (json-pretty-print-buffer)
                  (json-mode)) ; Assuming you have json-mode installed for better readability
                (display-buffer buffer))))
    (let* ((wql *WQL-FOR-DONE-TICKETS*))
      (m/gsi:get-azure-tickets wql #'display-in-new-buffer))))

(defun m/gsi/emacs:azure-report-fbp-done-tickets-for-changelog ()
  (interactive)
  (cl-flet ((display-id-only-in-new-buffer (data)
              (let ((work-item-ids (mapcar (lambda (item)
                                             (format "#%d" (alist-get 'id item)))
                                           (alist-get 'workItems data)))
                    (buffer (get-buffer-create "*Azure API Response - Changelog*")))
                (with-current-buffer buffer
                  (erase-buffer)
                  (dolist (id work-item-ids)
                    (insert (format "%s\n" id)))
                  (display-buffer buffer)))))
    (let* ((wql *WQL-FOR-DONE-TICKETS*))
      (m/gsi:get-azure-tickets wql #'display-id-only-in-new-buffer))))
;; Azure:1 ends here

;; [[file:tja.org::*Azure Devops][Azure Devops:1]]
(load-file "~/.azure-secrets.el")
(defun azure--session-call (path credentials)
   "Do a call to PATH with ARGS using current session. Does not check for session validity."
   (let ((azure-devops-v6-url "https://dev.azure.com/GoddardSystemsIT/_apis/wit/workitems?ids=12697&api-version=6.1-preview.3"))
     (apply #'request (concat azure-devops-v6-url path)
            :headers `(("Content-Type" . "application/json")
                       ("Authorization" . ,(format "Basic %s" credentials)))
            :sync t
            :parser 'json-read)))
(require 'request)
(defun request-ex-response-success-p (response)
  (equal (request-response-status-code response) 200))

(defun azure-devops-list-projects (organization)
  "List all Azure DevOps projects in the specified organization."
  (let* ((url (format "https://dev.azure.com/%s/_apis/projects?api-version=7.0" organization))
         (auth  (base64-encode-string
                 (concat *MY-AZURE-UN* ":" *MY-AZURE-PW*) t))
         (headers `(("Authorization" . ,(format "Basic %s" auth))
                    ("Content-Type" . "application/json")))
         (response (request url :headers headers :sync t :parser 'json-read)))
    (if (request-ex-response-success-p response)
        (request-response-data response)
      (error "Azure DevOps API request failed: %s" (request-response-data response)))))

(defun azure-devops-pretty-print-projects ()
  "Pretty print Azure DevOps project names in a new buffer."
  (interactive)
  (let* ((org "GoddardSystemsIT")
         (projects-data (azure-devops-list-projects org))
         (projects (cdr (assoc "value" projects-data)))  ;; Extract the project list
         (buffer-name "*Azure DevOps Projects*")
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "Azure DevOps Projects:\n\n")
      (if projects
          (dolist (project projects)
            (insert (format "%s\n" (cdr (assoc "name" project)))))
        (insert "No projects found.\n"))
      (goto-char (point-min))  ; Move cursor to the beginning
      (display-buffer buffer-name))))  ; Display the buffer

;; Example usage:
;; (let ((organization "your_organization_name")
;;       (pat "your_pat"))
;;   (azure-devops-print-projects organization pat))

(defun )
;; Azure Devops:1 ends here

;; [[file:tja.org::*SQL][SQL:1]]
(defun m/sql:ef-to-sql ()
  "Convert Entity Framework debug output in the current buffer to an executable SQL statement.
  Example:
  Executed DbCommand (5ms) [Parameters=[@p0='2022-12-07T00:00:00.0000000' (DbType = Date), @p1='14' (Nullable = true)], CommandType='Text', CommandTimeout='30']"
  (interactive)
  (let ((params (make-hash-table :test 'equal))
        (sql-start "SET NOCOUNT ON;")
        (case-fold-search nil)
        (param-search-regexp "@\\(p[0-9]+\\)=\\('\\([^']*\\)'\\|NULL\\)\\(,\\| \\((DbType\\|(Nullable\\|(Size\\) = \\([^)]*\\))\\)"))
    ;; Parse the parameters from the debug output and store them in the hash table
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward param-search-regexp nil t)
        (puthash
         (substring-no-properties (match-string 1))
         (substring-no-properties (match-string 2))
         params)))

    ;; Find and process the SQL statement block
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward sql-start nil t)
        (let ((start (point)))
          (goto-char (point-max))
          (let ((sql (buffer-substring start (point))))
            ;; Replace the placeholders with actual parameter values
            (maphash
             (lambda (key value)
               (setq sql (replace-regexp-in-string (concat "@" key ",") (concat value ",") sql))
               (setq sql (replace-regexp-in-string (concat "@" key ")") (concat value ")") sql))
               (setq sql (replace-regexp-in-string (concat "@" key ";") (concat value ";") sql))
               (setq sql (replace-regexp-in-string (concat "@" key "
  ") (concat value "
  ") sql))
               )
             params)
            ;; Output the converted SQL
            (with-current-buffer (get-buffer-create "*EF-SQL*")
              (erase-buffer)
              (insert sql)
              (sql-mode)
              (display-buffer (current-buffer)))))))))

  (defun point-in-comment ()
    (let ((syn (syntax-ppss)))
      (and (nth 8 syn)
           (not (nth 3 syn)))))

  (defun m/sql:sql-capitalize-all-sqlserver-keywords (min max)
    (interactive "r")
    (require 'sql)
    (save-excursion
      (dolist (keywords sql-mode-ms-font-lock-keywords)
        (goto-char min)
        (while (re-search-forward (car keywords) nil t)
          (unless (or (point-in-comment) (> (point) max))
            (goto-char (match-beginning 0))
            (upcase-word 1))))))

(defvar m/sql:last-used-file nil
    "Stores the last used SQL file.")

(defvar m/sql:last-used-parameters nil
  "Stores the last used parameter values.")


;;; https://chatgpt.com/c/6ab254b1-9464-4509-a3a4-3313af1171e9
(defun m/sql:run-sqlcmd-with-connection (sql-file &optional additional-params)
  "Run sqlcmd with SQL-FILE as input, using a connection from `sql-connection-alist`.
  If ADDITIONAL-PARAMS is non-nil, it is added to the sqlcmd command."
  (interactive
   (let* ((default-file-path (if (and (buffer-file-name)
                                 (string-suffix-p ".sql" (buffer-file-name)))
                                 (buffer-file-name)
                          (if m/sql:last-used-file
                              m/sql:last-used-file)))
          (default-file (file-name-nondirectory default-file-path))
          (default-directory (when default-file-path
                               (expand-file-name (file-name-directory default-file-path))))
          (sql-file (read-file-name "SQL File: " default-directory nil t default-file))
          (sql-parameters (read-string "Additional sqlcmd parameters: " m/sql:last-used-parameters nil)))
     (list sql-file sql-parameters)))
  (let* ((connection-name (completing-read "Choose SQL connection: "
                                           (mapcar #'car sql-connection-alist)))
         (connection-info (cdr (assoc (intern connection-name) sql-connection-alist)))
         (user (cadr (assoc 'sql-user connection-info)))
         (password (cadr (assoc 'sql-password connection-info)))
         (server (cadr (assoc 'sql-server connection-info)))
         (database (cadr (assoc 'sql-database connection-info)))
         (integrated-auth (assoc 'sql-integrated-auth connection-info))
         (command (format "sqlcmd -S %s %s -d %s -i %s %s"
                          server
                          (if integrated-auth "-E" (format "-U %s -P %s" user password))
                          database
                          (replace-regexp-in-string " " "\\\\ " sql-file)
                          additional-params)))
    (when additional-params
      (setq m/sql:last-used-parameters additional-params))
    (when sql-file
      (setq  m/sql:last-used-file sql-file))

    (unless (and server database)
      (error "Server or Database information missing for the selected connection"))
    (when (y-or-n-p (format "Execute command: %s? " command))
      (message "Running: %s" command)
      (let ((output-buffer "*SQLCMD Output*"))
        (with-current-buffer (get-buffer-create output-buffer)
          (read-only-mode -1)
          (erase-buffer)
          (shell-command command output-buffer)
          (read-only-mode 1)
          (display-buffer output-buffer))))))
;; SQL:1 ends here

;; [[file:tja.org::*Arrayify][Arrayify:1]]
(defun arrayify (start end quote)
  "Turn strings on newlines into a QUOTEd, comma-separated one-liner."
  (interactive "r\nMQuote: ")
  (let ((insertion
         (mapconcat
          (lambda (x) (format "%s%s%s" quote x quote))
          (split-string (buffer-substring start end)) ", ")))
    (delete-region start end)
    (insert insertion)))
;; Arrayify:1 ends here

;; [[file:tja.org::*JS Beautify][JS Beautify:1]]
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
;; JS Beautify:1 ends here

;; [[file:tja.org::*Financial][Financial:1]]
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
;; Financial:1 ends here

;; [[file:tja.org::*Clockify][Clockify:1]]
(load-file "/home/trevor/.clockify-secrets.el")
(load-file "/home/trevor/emacs/lisp/my-clockify.el")
;; Clockify:1 ends here

;; [[file:tja.org::*Utility Functions][Utility Functions:1]]
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

;; Also used by org-clockify-report
(defun my:escape-quotes (string)
  "Escapes quotes in the given STRING."
  (replace-regexp-in-string "\"" "\\\\\"" string))

(defun my:escape-quotes-in-string (input)
  "Escape quotes in the given string INPUT."
  (replace-regexp-in-string "\"" "\\\"" input))

(defun my:escape-json-recursively (json-string)
  "Escape quotes in a JSON string, including nested JSON strings."
  (let ((json-escaped (my:escape-quotes-in-string json-string)))
    (with-temp-buffer
      (insert json-escaped)
      (goto-char (point-min))
      (while (re-search-forward "\\\\\"" nil t)
        (replace-match "\\\\\\\\\"" nil nil))
      (buffer-string))))


(defun my:buffer-to-elisp-string-recursive ()
  "Convert the entire buffer content to an elisp string with escaped quotes, handling nested JSON escaping.
  Note: tried this for json, easier to just parse the json I think"
  (interactive)
  (let* ((buffer-content (buffer-string))
         (escaped-content (my:escape-quotes-in-string)))
    (kill-new (concat "\"" escaped-content "\""))
    (message "Buffer content converted to elisp string with recursive JSON escaping and copied to clipboard.")))

                                        ;(global-set-key (kbd "C-c e") 'my:buffer-to-elisp-string-recursive)

;;   ;;;###autoload
;; (defun my:escape-quotes (string)
;;   "Escapes quotes in the given STRING."
;;   (replace-regexp-in-string "\"" "\\\\\"" string))

;; (defun my:buffer-to-elisp-string ()
;;   "Convert the entire buffer content to an elisp string with escaped quotes."
;;   (interactive)
;;   (let* ((buffer-content (buffer-string))
;;          (escaped-content (my:escape-quotes buffer-content)))
;;     (kill-new (concat "\"" escaped-content "\""))
;;     (message "Buffer content converted to elisp string and copied to clipboard.")))

;; (defun my:list-environment-variables ()
;;   "List all current environment variables."
;;   (interactive)
;;   (with-output-to-temp-buffer "*Environment Variables*"
;;     (dolist (env process-environment)
;;       (princ env)
;;       (princ "\n"))))
;; Utility Functions:1 ends here

;; [[file:tja.org::*My keymap][My keymap:1]]
;; A Ctl-c u keymap
;;   Ctl-c u g for GSI
;;   Ctl-c u e for ESA
;;   Ctl-c u m for Me
(defun m/gsi:insert-school-id ()
  (interactive)
  (insert "F0EDC50F-ED22-DE11-B821-0014221C4264"))
(defun m/esa:insert-dev-search-api ()
  (interactive)
  (insert "https://api.dev.bws.esa.com/search-results-widget-api/searchByGeo?lat=32.7766642&lng=-96.79698789999999&rooms=1&adults=1&child=0&code&rateType=ESH&checkIn=2023-10-04&checkOut=2023-10-11"))

(defvar my-keymap (make-sparse-keymap))
(global-set-key (kbd "C-c u") my-keymap)
(global-set-key (kbd "C-c u R") 'recompile)
(global-set-key (kbd "C-c u I") 'my:insert-signature-for-code)

;; GSI Keymap
;; uses leader keys for projects, t = tours, l = leads, then f5-7 run api, unit, integration tests
(defvar gsi-keymap (make-sparse-keymap))
(define-key gsi-keymap (kbd "e") 'm/gsi:insert-school-id)
(define-key gsi-keymap (kbd "r") 'tja-vterm-run-fbp)
(define-key my-keymap (kbd "g") gsi-keymap)

(defvar tours-keymap (make-sparse-keymap))
(define-key gsi-keymap (kbd "t") tours-keymap)
(define-key tours-keymap (kbd "<f5>") 'm/gsi:vterm-run-tours-api)
(define-key tours-keymap (kbd "<f6>") 'm/gsi:compile-tours-api-unit-tests)
(define-key tours-keymap (kbd "<f7>") 'm/gsi:compile-tours-api-integration-tests)

(defvar leads-keymap (make-sparse-keymap))
(define-key gsi-keymap (kbd "l") leads-keymap)
(define-key leads-keymap (kbd "<f5>") 'my:vterm-run-leads-api)
(define-key leads-keymap (kbd "<f6>") 'my:compile-leads-api-unit-tests)
(define-key leads-keymap (kbd "<f7>") 'my:compile-leads-api-integration-tests)

(defvar esa-keymap (make-sparse-keymap))
(define-key esa-keymap (kbd "a") 'm/esa:insert-dev-search-api)
(define-key esa-keymap (kbd "d") 'm/esa:vterm-run-dotcms)
(define-key esa-keymap (kbd "f") #'m/esa:vterm-run-dotcms-node)
(define-key my-keymap (kbd "e") esa-keymap)

(global-set-key (kbd "<f5>") '(lambda () (interactive) (find-file "~/projects/extended_stay/todo_esa.org")))
;; My keymap:1 ends here

;; [[file:tja.org::*Jiralib2][Jiralib2:1]]
(defun +jiralib2-extract-issue-id (issueKey)
  "Extracts the issue id from the issue key, e.g. ecomm-4952"
  (assoc 'id (jiralib2-get-issue issueKey)))

(defun +jiralib2-get-issue-id (issueKey)
  "Extracts the issue id from the issue key, e.g. ecomm-4952"
  (assoc 'id (jiralib2-get-issue issueKey)))

(defun +jiralib2-get-issue-detail (issue-or-key)
  (let ((issue)
        (id))
    (if (stringp issue-or-key)
        (setq issue (jiralib2-get-issue issue-or-key))
      (setq issue issue-or-key))

    (setq id (cdr (assoc 'id issue)))
    (jiralib2-session-call (concat "/rest/dev-status/1.0/issue/detail?issueId=" id "&applicationType=bitbucket&dataType=pullrequest"))))

(defun +jiralib2-get-branches (issue-detail)
  (caar (cdr (assoc 'detail *issue-detail*))))

(defun +jiralib2-extract-repository-names (issueKey)
  "Extracts repository names from the given DATA."
  (let* ((issueId (cdr (+jiralib2-extract-issue-id issueKey)))
         (issue (jiralib2-session-call (concat "/rest/dev-status/1.0/issue/detail?issueId=" issueId "&applicationType=bitbucket&dataType=pullrequest"))))
    (cl-destructuring-bind (_errors (_detail (_branches . ((_a . branches-list) . _)))) issue
      (let ((repos (mapcar (lambda (x) (cdr (assoc 'repositoryName x))) branches-list)))
        (delq nil (delete-dups repos))))))

(defun +jiralib2-repository-names (issueKey)
  "Extracts repository names from the given DATA."
  (interactive "sIssue Key: ")
  (let* ((repositoryList (+jiralib2-extract-repository-names issueKey)))
    (insert (format "%s" repositoryList))))
;; Jiralib2:1 ends here

;; [[file:tja.org::*Sly/Common Lisp][Sly/Common Lisp:1]]
;; From chatGPT Session https://chatgpt.com/c/90d883ce-9dea-40d5-9809-1486c4146305
(defun my:add-function-to-package (package-name function-name position)
  "Add FUNCTION-NAME to the export list of PACKAGE-NAME in packages.lisp at the given POSITION.
POSITION should be either 'start or 'end."
  (let ((package-file "packages.lisp"))
    (with-temp-buffer
      (insert-file-contents package-file)
      (goto-char (point-min))
      (if (re-search-forward (format "(defpackage %s" package-name) nil t)
          (if (re-search-forward "(:export" nil t)
              (let ((export-start (point)))
                (forward-sexp)
                (backward-char)
                (let ((export-end (point)))
                  (goto-char (if (eq position 'start) export-start export-end))
                  (if (eq position 'start)
                      (insert (format " :%s" function-name))
                    (insert (format " :%s" function-name))))
                (write-region (point-min) (point-max) package-file))
            (message "No export list found in package %s" package-name))
        (message "No package definition found for %s" package-name)))))

(defun my:add-current-function-to-package (package-name position)
  "Add the function at point to the export list of PACKAGE-NAME in packages.lisp at the given POSITION.
POSITION should be either 'start or 'end."
  (interactive "sPackage name: \nSPosition (start or end): ")
  (save-excursion
    (beginning-of-defun)
    (if (looking-at "(defun \\(\\_<[^ )]+\\_>\\)")
        (let ((function-name (match-string 1)))
          (add-function-to-package package-name function-name position))
      (message "No function at point"))))

(defun my:sly-eval-and-display (expression)
  "Evaluate the given EXPRESSION using sly-eval-async and display the result in a new buffer."
  (interactive "MExpression: ")
  (sly-eval-async
      `(cl:progn (cl:setf (cl:cdr (cl:assoc 'slynk:*string-elision-length* slynk:*slynk-pprint-bindings*)) 10000)
                 (slynk:eval-and-grab-output ,expression))
    (lambda (result)
      (let ((output-buffer (get-buffer-create "*Sly Eval Output*")))
        (with-current-buffer output-buffer
          (read-only-mode -1)
          (erase-buffer)
          (insert (cadr result))  ; (cadr result) contains the string output of the evaluation

          ;; Cleanup buffer from slynk metadata
          ;; Deletes the first quote and the last two lines with the lenght and the quote
          (goto-char (point-min))
          (delete-region (line-beginning-position) (line-beginning-position 2))
          (goto-char (point-max))
          (delete-region (line-beginning-position) (line-beginning-position 2))
          (forward-line -1)
          (delete-region (line-beginning-position) (line-beginning-position 2))
          ;; End cleanup

          ;; Temporarily set mode
          (sql-mode)
          (display-buffer output-buffer)))))
  (sly-eval-async
      `(cl:progn (cl:setf (cl:cdr (cl:assoc 'slynk:*string-elision-length* slynk:*slynk-pprint-bindings*)) 200))))
;; Sly/Common Lisp:1 ends here

;; [[file:tja.org::*Project specific functions][Project specific functions:1]]
(load "~/projects/extended_stay/esa-elisp.el")
(load "~/projects/extended_stay/esa-jira.el")
(load "~/projects/extended_stay/esa-merge-helper.el")
;; Project specific functions:1 ends here

;; [[file:tja.org::*Hideshow Extension][Hideshow Extension:1]]
(defun hs-cycle (&optional level)
  (interactive "p")
  (let (message-log-max
        (inhibit-message t))
    (if (= level 1)
        (pcase last-command
          ('hs-cycle
           (hs-hide-level 1)
           (setq this-command 'hs-cycle-children))
          ('hs-cycle-children
           ;; TODO: Fix this case. `hs-show-block' needs to be
           ;; called twice to open all folds of the parent
           ;; block.
           (save-excursion (hs-show-block))
           (hs-show-block)
           (setq this-command 'hs-cycle-subtree))
          ('hs-cycle-subtree
           (hs-hide-block))
          (_
           (if (not (hs-already-hidden-p))
               (hs-hide-block)
             (hs-hide-level 1)
             (setq this-command 'hs-cycle-children))))
      (hs-hide-level level)
      (setq this-command 'hs-hide-level))))

(defun hs-global-cycle ()
  (interactive)
  (pcase last-command
    ('hs-global-cycle
     (save-excursion (hs-show-all))
     (setq this-command 'hs-global-show))
    (_ (hs-hide-all))))

(use-package hideshow
  :bind (("C-S-<tab>" . hs-global-cycle)
         ("C-c <tab>" . hs-toggle-hiding)))
;; Hideshow Extension:1 ends here

;; [[file:tja.org::*My helper functions][My helper functions:1]]
(defun m/startup/display:enable-sway-and-wayvnc-headless-display ()
  (interactive)
  (m/sway:check-or-create-headless-output)
  (m/sway:headless-set-ipad-resolution)
  (m/wayvnc:check-or-start-wayvnc))
;; My helper functions:1 ends here

;; [[file:tja.org::*EXPERIMENTAL][EXPERIMENTAL:1]]
;;; https://gist.github.com/kristianhellquist/3082383#gistcomment-2373734
(defun m/file:copy-current-line-position-to-clipboard ()
  "Copy current line in file to clipboard as '</path/to/file>:<line-number>'."
  (interactive)
  (let ((path-with-line-number
         (concat (buffer-file-name) ":" (number-ring (line-number-at-pos)))))
    (kill-new path-with-line-number)
    (message (concat path-with-line-number " copied to clipboard"))))

(defun m/file:copy-relative-current-line-position-to-clipboard ()
  "Copy current line in file to clipboard as '</path/to/file>:<line-number>'."
  (interactive)
  (let ((path-with-line-number
         (concat (s-replace (expand-file-name (vc-root-dir)) ""  (buffer-file-name)) ":" (number-to-string (line-number-at-pos)))))
    (kill-new path-with-line-number)
    (message (concat path-with-line-number " copied to clipboard"))))

(defun m/file:copy-project-current-line-position-to-clipboard ()
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

(defun m/code:modify-paths-and-open-files (beginning end)
  "Modifies all the paths to be local linux paths then opens the files in buffer windows"
  (interactive "r")
  (call-interactively 'm/code:replace-windows-path-with-linux)
  (call-interactively 'm/code:open-local-files-from-stack-trace))

(defun m/code:open-local-files-from-stack-trace (beginning end)
  "Parse stack trace in current buffer and open local files at specified lines."
  (interactive "r")
  (save-excursion
    (goto-char beginning)
    (let ((buffer (current-buffer)))
      (while (re-search-forward "\\(/.+\\):\\(line \\([0-9]+\\)\\)" nil)
        (let ((file (match-string 1))
              (line (string-to-number (match-string 3))))

          (when (file-exists-p file)
            (save-excursion
              (find-file-other-window file)
              (goto-char (point-min))
              (forward-line (1- line))
              (recenter))))))))

(defun m/code:replace-windows-path-with-linux (beginning end)
  "Equinox:only - replace the windows c:\ paths with local linux path to project"
  (interactive "r")
  (save-excursion
    (goto-char beginning)
    (let ((buffer (current-buffer)))
      (while (re-search-forward "\\\\" nil t)
        (replace-match "/"))
      (goto-char beginning)
      ;; find the matching 
      (while (re-search-forward "C:/\\(.*?\\)/" nil t)
        (let ((project (downcase (match-string 1))))
          (replace-match (format "/home/trevor/projects/%s/src/" project)))))))

;;; ESA Functions to swap environments in URLs
(defun m:replace-url-with-local ()
  (interactive)
  (let ((regex "http\[s\]*://.*?/")
        (replacement "http://localhost:8080/"))
    (while (re-search-forward regex nil t)
      (replace-match replacement))))


;;; ¯\_(ツ)_/¯
(defun m:insert-shrug ()
  (interactive)
  (insert "¯\\_(ツ)_/¯"))
;; EXPERIMENTAL:1 ends here
