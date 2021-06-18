(require 'cl-lib)

(defvar ESA-HOTELS)
(defvar CONTENT_TYPES)

(defun my-dotcms:setup-my-dotcms-token ()
    (request
      "http://localhost:8080/api/v1/authentication/api-token"
      :type "POST"
      :headers '(("Content-Type" . "application/json"))
      :data (json-encode '(("user" . "admin@dotcms.com")
                           ("password" . "admin")
                           ("expirationDays" .  1)
                           ("label" . "for testing")))
      :parser 'json-read
      :success (cl-function
                (lambda (&key response data &allow-other-keys)
                  (setq my:dotcms-token (alist-get 'token (car data)))
                  (message "successfully set key")))))

;;; Could not get this working 2021-04-13
(defun my-dotcms:add-to-bundle ()
  (request
      "http://localhost:8080/DotAjaxDirector/com.dotcms.publisher.ajax.RemotePublishAjaxAction/cmd/addToBundle"
      :type "POST"
      :headers '(("Content-Type" . "application/json")
                 ("Authorization:Bearer" . my:dotcms-token))
      :data (json-encode '(("assetIdentifier" . "15a0b415-4e11-47fe-ab8f-0e55a827820d")
                           ("bundleName" . "testing")))
      :parser 'json-read
      :success (cl-function
                (lambda (&key response data &allow-other-keys)
                  (setq my:dotcms-token (alist-get 'token (car data)))
                  (message "successfully added to bundle")))))

(defun my:print-json-result (data)
  (with-current-buffer (get-buffer-create "*dotcms-api-result*")
    (erase-buffer)
    (insert data)
    (json-mode)
    (json-pretty-print-buffer)
    (pop-to-buffer (current-buffer))))

(defun my-dotcms:print-list (l)
  (with-current-buffer (get-buffer-create "*dotcms-result*")
    (erase-buffer)
    (emacs-lisp-mode)    
    (mapcar #'cl-prettyprint l)
    (pop-to-buffer (current-buffer))))

(defmacro my-dotcms:set-var-from-api-call (url var)
  (if (not (boundp 'my:dotcms-token))
      (my-dotcms:setup-my-dotcms-token))
  `(request ,url
     :headers '(("Authorization:Bearer" . my:dotcms-token))
     :parser 'json-read
     :success (cl-function
               (lambda (&key response data &allow-other-keys)
                 (setq ,var data)))))

(defun my-dotcms:refresh-hotels ()
  (interactive)
  (my-dotcms:set-var-from-api-call "http://localhost:8080/api/content/query/+ContentType:Hotel/limit/0" ESA-HOTELS))

(defun my-dotcms:refresh-content-types ()
  (interactive)
  (my-dotcms:set-var-from-api-call "http://localhost:8080/api/v1/contenttype/" CONTENT_TYPES))

(defun my-dotcms:print-hotels-url-map ()
  (interactive)
  (unless (boundp 'ESA-HOTELS)
      (my-dotcms:refresh-hotels))
  (let ((hotels (cdar ESA-HOTELS))
        (url-map (mapcar (lambda (x) (assoc 'URL_MAP_FOR_CONTENT x)) hotels)))
    (my-dotcms:print-list url-map)))


(defun my-dotcms:hotel-by (siteId)
  (let* ((hotels (cdar ESA-HOTELS))
         (siteIdNum (if (numberp siteId) siteId (string-to-number siteId)))
         (hotel (cl-remove-if-not
                 (lambda (x)
                   (equalp (cdr (assoc 'siteId x)) siteIdNum))
                 hotels)))
    (if (equalp (length hotel) 0)
        '()
      (aref hotel 0))))

(defun my-dotcms:hotel-by (state)
  (interactive)
  (let ((hotels (cdar ESA-HOTELS)))
    (seq-filter
     (lambda (x)
       (s-contains-p
        (upcase (concat "/" state "/"))
        (upcase (cdr (assoc 'URL_MAP_FOR_CONTENT x)))))
     hotels)))

(defun my-dotcms:insert-hotel-url (s e)
  (interactive "r")
  (let* ((siteId (buffer-substring s e))
        (hotel (my-dotcms:hotel-by siteId))
        (url (cdr (assoc 'URL_MAP_FOR_CONTENT hotel))))
    (insert (concat " http://localhost:8080" url))))

(defun my-dotcms:insert-hotel (s e)
  (interactive "r")
  (let* ((siteId (buffer-substring s e))
        (hotel (my-dotcms:hotel-by siteId)))
    (cl-prettyprint hotel)))


