;;; MAGIT EXTENSION FUNCTIONS
;;; Note: Converting to just use call process - shouldn't (require magit) anymore
(require 'magit)

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
  (tja--call-git-process-no-output repo "commit" "-m" "[autocommit] todos, env")
  (tja--call-git-process-no-output repo "rebase" "origin/master")
  (tja--call-git-process-no-output repo "push"))

(defun tja--call-git-process-no-output (repo &rest args)
  (apply 'call-process (append (list "git" nil nil nil "-C" repo) args)))

(defun tja-magit-commit-all-rebase-push (repo &optional master)
  (tja--call-git-process-no-output repo "fetch" "--all")
  (tja--call-git-process-no-output repo "add" "-A")
  (tja--call-git-process-no-output repo "commit" "-a" "-m" "[autocommit] dailies")
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

;;;###autoload
(defun tja-magit-commit-and-refresh-all-projects-and-dailies ()
  (interactive)
  (tja-magit-commit-and-refresh-all-dailies)
  (tja-magit-commit-and-refresh-all-projects))

(provide 'tja-magit)
