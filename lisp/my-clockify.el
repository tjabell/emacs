(require 'json)
(require 'cl-lib)
(require 'request)
(require 'clockify)

(defun clockify--post (endpoint data)
  "Retrieve user information."
  (let ((response (request-response-data
		   (request endpoint
		     :type "POST"
		     :headers (clockify--generate-headers)
		     :sync t
		     :parser 'json-read
                     :data data
		     :error 'clockify--error-fn))))
    response))

(defun my:clockify/add-entry (st et desc project-id)
    (let* ((workspace-id "5d6de2a927f8c341bd8fc10d")
           (endpoint (concat (clockify--workspaces-endpoint) "/" workspace-id "/time-entries"))
           (data (json-encode-alist
                  `(
                    ("start" . ,st)
                    ("end". ,et)
                    ("billable" . "true")
                    ("description". ,desc)
                    ("projectId". ,project-id)
                    ))))
      (clockify--post endpoint data)))

(defun my:clockify-init ()
  (interactive)
;;; Need this to initialize clockify api I guess
  (setq my:clockify:userid  (clockify--user-info))

  ;; (setq my:ws (clockify--workspaces-endpoint))

  ;; (setq my:ws:id (cdar (aref my:ws 0)) )

  (setq clockify--active-workspace-id "5d6de2a927f8c341bd8fc10d")

  (setq my:esa-project-id "5ec8372146cf2748cd6a77c6")
  (setq my:projects (clockify--projects))
  "Clockify projects initiated")

(setq my:parsus-training-id "5d702b48a3fe2c1b233ea83c")

(setq my:acdhh-project-id "5d6fff262b576659a087efb4")

(setq my:acdhh-public-website-task-id "5d6fffd2a3fe2c1b233e709b")

(setq my:acdhh-cdbms-task-id "5d6fffd72b576659a087f096")

;;(setq my:equinox-project-id "5d6fe70bd162f830901a8df2")
;;(setq my:equinox-gac-task-id "5d8e74c4a0b06a32d93904da")
;;(setq my:equinox-rks-task-id "5d8e74bfad3d0067ca648e6b")
;;(setq my:equinox-wpt-task-id "5d6fff98a3fe2c1b233e704d")

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


;(my:clockify-init)
