;;; MAGIT EXTENSION FUNCTIONS
(require 'magit)

(defun my:magit-commit-all (message)
  (interactive "sCommit Message: ")
  (magit-call-git "commit" "-a" "-m" message)
  (magit-refresh))

(defun my:magit-commit-all-and-push (message)
  (interactive "sCommit Message: ")
  (magit-call-git "commit" "-a" "-m" message)
  (magit-call-git "push")
  (magit-refresh))

;; Only pushes project specific files (environment.org) and any todos
(defun my:magit-commit-rebase-push-project-files (repo &rest additional-files)
  (my:-call-git-process-no-output repo "fetch" "--all")
  (my:-call-git-process-no-output repo "add" "./environment.org")
  (if additional-files
      (apply 'my:-call-git-process-no-output (append (list repo "add") additional-files)))
  (my:-call-git-process-no-output repo "commit" "-m" "[autocommit] todos, env")
  (my:-call-git-process-no-output repo "rebase" "origin/master")
  (my:-call-git-process-no-output repo "push"))

(defun my:-call-git-process-no-output (repo &rest args)
  (apply 'call-process (append (list "git" nil nil nil "-C" repo) args)))

(defun my:magit-commit-all-rebase-push (repo)
  (my:-call-git-process-no-output repo "fetch" "--all")
  (my:-call-git-process-no-output repo "add" "-A")
  (my:-call-git-process-no-output repo "commit" "-a" "-m" "[autocommit] dailies")
  (my:-call-git-process-no-output repo "rebase" "origin/master")
  (my:-call-git-process-no-output repo "push"))

(defun my:magit-commit-and-refresh-all-dailies ()
  (interactive)
  (let ((repo "/home/trevor/org-roam/"))
    (my:magit-commit-all-rebase-push repo))
  (let ((repo "/home/trevor/emacs/"))
    (my:magit-commit-all-rebase-push repo))
  (let ((repo "/home/trevor/projects/me"))
    (my:magit-commit-all-rebase-push repo)))

(defun my:magit-commit-and-refresh-all-projects ()
  (interactive)
  (let ((repo "/home/trevor/projects/goddard/"))
    (my:magit-commit-rebase-push-project-files repo "./todo_goddard.org"))
  (let ((repo "/home/trevor/projects/parsus/"))
    (my:magit-commit-rebase-push-project-files repo "./todo_parsus.org" "./meeting_updates.org"))
  (let ((repo "/home/trevor/projects/acdhh/"))
    (my:magit-commit-rebase-push-project-files repo "./todo_acdhh-cdbms.org" "./todo_acdhh-www.org"))
  (let ((repo "/home/trevor/projects/extended_stay/"))
    (my:magit-commit-rebase-push-project-files repo "./todo_esa.org")))

(global-set-key (kbd "C-c C-g A") 'my-magit-commit-all-and-push)
