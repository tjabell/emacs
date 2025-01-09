(require 'json)
(require 'cl-lib)
(require 'request)
(require 'clockify)

(defvar clockify--init nil)
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

(defun clockify--init-api ()
  "Initialize clockify user info. Which sets the workspace and user id"
  (clockify--user-info)
  (setq clockify--init t))

(defun m/clockify:print-projects ()
  (interactive)
  (unless (not clockify--init)
    (clockify--init-api))
  (with-current-buffer (get-buffer-create "*clockify-projects*")
    (erase-buffer)
    (let ((fields (mapcar (lambda (p) (list (assoc 'id p) (assoc 'name p) (assoc 'clientName p))) 
                          (clockify--projects))))
      (dolist (field fields)
        (insert (format "id: %s, name: %s, clientName: %s\n"
                        (cdr (nth 0 field))
                        (cdr (nth 1 field))
                        (cdr (nth 2 field))))))))

(defun m/clockify:print-tasks (project-id)
  (let ((tasks (clockify--tasks project-id)))
    (with-current-buffer (get-buffer-create (concat "*my-tasks-" project-id "*"))
      (seq-map (lambda (i) (cl-prettyprint i)) tasks))))
