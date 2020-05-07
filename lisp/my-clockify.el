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

  (setq my:parsus-training-id "5d702b48a3fe2c1b233ea83c"))
