;;; https://www.assembla.com/wiki/show/breakoutdocs/Assembla_REST_API
(require 'xml)

(defvar assembla-base-url "https://www.assembla.com")

(defun assembla-tickets (space)
  (interactive "sTickets for space: ")
  (assembla-get-users space
                      (lambda (space)
                        (assembla-get-milestones space
                                                 (lambda (space)
                                                   (assembla-get-tickets space))))))

(defun assembla-url (&rest args)
  (mapconcat 'identity
         (cons assembla-base-url
           (mapcar (lambda (arg)
                 (if (numberp arg)
                 (number-to-string arg)
                   arg))
               args))
         "/"))

(defun assembla-retrieve (url callback &optional cbargs)
  (let ((url-request-extra-headers '(("Accept" . "application/xml"))))
    (url-retrieve url callback cbargs)))

(defun assembla-parse-response (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (xml-parse-region url-http-end-of-headers (point-max))))

(defvar assembla-users nil)
(defun assembla-get-users (space &optional callback)
  (lexical-let ((space space)
                (callback callback))
    (assembla-retrieve (assembla-url "spaces" space "users")
                       (lambda (status &optional cbargs)
                         (let* ((root (assembla-parse-response))
                                (users (xml-get-children (car root) 'user)))
                           (setq assembla-users nil)
                           (dolist (user users)
                             (let ((id (car (xml-node-children (car (xml-get-children user 'id)))))
                                   (login (car (xml-node-children (car (xml-get-children user 'login_name))))))
                               (push (cons id login) assembla-users))))
                         (and callback (funcall callback space))))))

(defvar assembla-milestones nil)
(defun assembla-get-milestones (space &optional callback)
  (lexical-let ((space space)
                (callback callback))
    (assembla-retrieve (assembla-url "spaces" space "milestones")
                       (lambda (status &optional cbargs)
                         (let* ((root (assembla-parse-response))
                                (milestones (xml-get-children (car root) 'milestone)))
                           (setq assembla-milestones nil)
                           (dolist (milestone milestones)
                             (let ((id (car (xml-node-children (car (xml-get-children milestone 'id)))))
                                   (title (car (xml-node-children (car (xml-get-children milestone 'title))))))
                               (push (cons id title) assembla-milestones))))
                         (and callback (funcall callback space))))))

;; curl -i -X GET -H "Accept: application/xml" http://username:password@www.assembla.com/spaces/SPACE/tickets
(defun assembla-get-tickets (space &optional report)
  (assembla-retrieve (assembla-url "spaces" space "tickets")
                     (lambda (status &optional cbargs)
                       (let* ((root (assembla-parse-response))
                              (tickets (xml-get-children (car root) 'ticket)))
                         (with-current-buffer (get-buffer-create "*ticket list*")
                           (erase-buffer)
                           (dolist (ticket tickets)
                             (insert (assembla-ticket-summary-string ticket))
                             (newline))
                           (goto-char (point-min))
                           (display-buffer (current-buffer)))))))

(defun assembla-ticket-summary-string (ticket)
  (let* ((number (car (xml-node-children (car (xml-get-children ticket 'number)))))
         (summary (car (xml-node-children (car (xml-get-children ticket 'summary)))))
         (assigned-to-id (car (xml-node-children (car (xml-get-children ticket 'assigned-to-id)))))
         (assigned-to (cdr (assoc assigned-to-id assembla-users)))
         (status (assembla-ticket-status ticket))
         (milestone (assembla-ticket-milestone ticket)))
     (format "(%s) %8s #%s %s -- %s"
             milestone
             (propertize status 'face (assembla-status-face status))
             number
             assigned-to
             summary)))

(defun assembla-status-face (status)
  (cdr (assoc status '(("New" . font-lock-type-face)
                       ("Accepted" . font-lock-constant-face)
                       ("Invalid" . font-lock-warning-face)
                       ("Fixed" . font-lock-doc-face)
                       ("Test" . font-lock-builtin-face)))))

(defun assembla-ticket-status (ticket)
  (let ((code (car (xml-node-children (car (xml-get-children ticket 'status))))))
    (cdr (assq (string-to-number code)
               '((0 . "New") (1 . "Accepted") (2 . "Invalid")
                 (3 . "Fixed") (4 . "Test"))))))
(defun assembla-ticket-milestone (ticket)
  (let ((id (car (xml-node-children (car (xml-get-children ticket 'milestone-id))))))
    (or (cdr (assoc id assembla-milestones)) "No Milestone")))
