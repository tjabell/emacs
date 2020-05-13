(defun my:clockify-init ()
;;; Need this to initialize clockify api I guess
  (setq my:clockify:userid  (clockify--user-info))

  (setq my:ws (clockify--workspaces))

  (setq my:ws:id (cdar (aref my:ws 0)) )

  (setq clockify--active-workspace-id my:ws:id)

  (setq my:projects (clockify--projects))
  ;(setq my:gilbert:task-id "5e948870ea8094116e8c7188")
  (setq my:gilbert:project-id "5e948870ea8094116e8c7188")

  (setq my:karisma-pgs:project-id "5d702211a3fe2c1b233e9e30")

  (setq my:bonappetit-hosting-project-id "5ea87fcd7468d5567e641f34")

  (setq my:bonappetit-maintenance-project-id "5eacc1ec26b404092a8bf1aa")

  (setq my:parsus-training-id "5d702b48a3fe2c1b233ea83c")

  (setq my:acdhh-project-id "5d6fff262b576659a087efb4")

  (setq my:acdhh-public-website-task-id "5d6fffd2a3fe2c1b233e709b")

  (setq my:acdhh-cdbms-task-id "5d6fffd72b576659a087f096"))
