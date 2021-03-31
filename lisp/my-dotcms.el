(defun my:setup-my-dotcms-token ()
    (request
      "http://localhost:8080/api/v1/authentication/api-token"
      :type "POST"
      :headers '(("Content-Type" . "application/json"))
      :data (json-encode '(("user" . "admin@dotcms.com")
                           ("password" . "admin")
                           ("expirationDays" .  1)
                           ("label" . "for testing")))
      :parser 'json-read
      :success (cl-function (lambda (&key response data &allow-other-keys)
                              (setq my:dotcms-token (alist-get 'token (car data)))
                              (message my:dotcms-token)))))

(defun my:print-json-result (data)
  (with-current-buffer (get-buffer-create "*dotcms-api-result*")
                    (erase-buffer)
                    (insert data)
                    (json-mode)
                    (json-pretty-print-buffer)
                    (pop-to-buffer (current-buffer))))

;;; Need to get token setup first
(defun my-dotcms:get-content-types ()
  (request "http://localhost:8080/api/v1/contenttype/"
    :headers '(("Authorization:Bearer" . my:dotcms-token))
    :success (cl-function
              (lambda (&key response data &allow-other-keys)
                (my:print-json-result data)))))

(defun my-dotcms:get-hotels ()
  (request "http://localhost:8080/api/content/query/+ContentType:Hotel%20+Hotel.siteId:1550"
    :headers '(("Authorization:Bearer" . my:dotcms-token))
    :success (cl-function
              (lambda (&key response data &allow-other-keys)
                (my:print-json-result data)))))

(defun my-dotcms:get-hotels-url-map-content-by-siteid (siteId)
  (request (concat  "http://localhost:8080/api/content/query/+ContentType:Hotel%20+Hotel.siteId:" siteId)
    :headers '(("Authorization:Bearer" . my:dotcms-token))
    :parser 'json-read
    :success (cl-function
              (lambda (&key response data &allow-other-keys)
                (setq my:data data)
                (with-current-buffer (get-buffer-create "*dotcms-api-result-sexp*")
                    (erase-buffer)
                    (cl-prettyprint (cdr (assoc 'URL_MAP_FOR_CONTENT (aref (cdar data) 0))))
                    (lisp-mode)
                    (pop-to-buffer (current-buffer)))))))
