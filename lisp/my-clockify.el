(require 'clockify)

(defun my:clockify-init ()
  (interactive)
;;; Need this to initialize clockify api I guess
  (setq my:clockify:userid  (clockify--user-info))

  (setq my:ws (clockify--workspaces))

  (setq my:ws:id (cdar (aref my:ws 0)) )

  (setq clockify--active-workspace-id my:ws:id)

  (setq my:esa-project-id "5ec8372146cf2748cd6a77c6"))

  (setq my:projects (clockify--projects))

  ;(setq my:gilbert:task-id "5e948870ea8094116e8c7188")
  ;(setq my:gilbert:project-id "5e948870ea8094116e8c7188")

  ;(setq my:karisma-pgs:project-id "5d702211a3fe2c1b233e9e30")

  ;(setq my:bonappetit-hosting-project-id "5ea87fcd7468d5567e641f34")

  ;(setq my:bonappetit-maintenance-project-id "5eacc1ec26b404092a8bf1aa")

  (setq my:parsus-training-id "5d702b48a3fe2c1b233ea83c")

  (setq my:acdhh-project-id "5d6fff262b576659a087efb4")

  (setq my:acdhh-public-website-task-id "5d6fffd2a3fe2c1b233e709b")

  (setq my:acdhh-cdbms-task-id "5d6fffd72b576659a087f096")

  ;(setq my:righter-project-id "5ec815cb46cf2748cd6a2023")

  ;(setq my:righter-bug-fix-task-id "5ec8161696f46724f636f516")
  ;(setq my:righter-startup-fix-task-id "5ec815f846cf2748cd6a20b8")
  ;(setq my:righter-enhancement-task-id "5ec8160996f46724f636f4f5")

  ;(setq my:equinox-project-id "5d6fe70bd162f830901a8df2")
  ;(setq my:equinox-gac-task-id "5d8e74c4a0b06a32d93904da")
  ;(setq my:equinox-rks-task-id "5d8e74bfad3d0067ca648e6b")
  ;(setq my:equinox-wpt-task-id "5d6fff98a3fe2c1b233e704d")

(defun my:clockify-print-projects ()
  (interactive)
  (unless (boundp my:projects)
    (setq my:projects (clockify--projects)))
  (with-current-buffer (get-buffer-create "*my-projects*")
    (seq-map (lambda (i) (insert (cl-prettyprint i))) my:projects)))

(defun my:clockify-print-tasks (project-id)
  (let ((tasks (clockify--tasks project-id)))
    (with-current-buffer (get-buffer-create (concat "*my-tasks-" project-id "*"))
      (seq-map (lambda (i) (cl-prettyprint i)) tasks))))
