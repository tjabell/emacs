(require 'magit)

(tempo-define-snippet "assembla-ticket-email"
                       '((p "Space: " project) "+" p "@tickets.assembla.com"))


(defun my-magit-commit-all (message)
  (interactive "sCommit Message: ")
  (magit-call-git "commit" "-a" "-m" message)
  (magit-refresh)  )

(defun my-magit-commit-all-and-push (message)
  (interactive "sCommit Message: ")
  (magit-call-git "commit" "-a" "-m" message)
  (magit-call-git "push")
  (magit-refresh))
