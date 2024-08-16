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

(defun m/clockify:add-entry (st et desc project-id)
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

(defun m/clockify:init ()
  (interactive)
;;; Need this to initialize clockify api I guess
  (setq m/clockify:*clockify-user-id* (clockify--user-info))

  ;; (setq my:ws (clockify--workspaces-endpoint))

  ;; (setq my:ws:id (cdar (aref my:ws 0)) )

  (setq clockify--active-workspace-id "5d6de2a927f8c341bd8fc10d")

  (setq my:esa-project-id "5ec8372146cf2748cd6a77c6")
  (setq m/clockify:*projects* (clockify--projects))
  "Clockify projects initiated")

(defun m/clockify:print-projects ()
  (interactive)
  (unless (boundp 'm/clockify:*projects*)
    (setq m/clockify:*projects* (clockify--projects)))
  (with-current-buffer (get-buffer-create "*clockify-projects*")
    (erase-buffer)
    (let ((fields (mapcar (lambda (p) (list (assoc 'id p) (assoc 'name p) (assoc 'clientName p))) 
                          m/clockify:*projects*)))
      (dolist (field fields)
        (insert (format "id: %s, name: %s, clientName: %s\n"
                        (cdr (nth 0 field))
                        (cdr (nth 1 field))
                        (cdr (nth 2 field))))))))

(defun m/clockify:print-tasks (project-id)
  (let ((tasks (clockify--tasks project-id)))
    (with-current-buffer (get-buffer-create (concat "*my-tasks-" project-id "*"))
      (seq-map (lambda (i) (cl-prettyprint i)) tasks))))
