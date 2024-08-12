;; Thanks https://chatgpt.com/c/6ab254b1-9464-4509-a3a4-3313af1171e9
;; Usage example
;(let ((json-string "{\"name\":\"John\",\"age\":30,\"city\":\"New York\",\"hobbies\":[\"reading\",\"traveling\"]}"))
;  (obfuscate-json-string json-string))
(require 'json)

(defun m/json:random-string (length)
  "Generate a random string of given LENGTH."
  (let ((alphabet "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
        (result ""))
    (dotimes (_ length result)
      (setq result (concat result (string (aref alphabet (random (length alphabet)))))))))

(defun m/json:random-value (original-value)
"Generate a random value based on the type of ORIGINAL-VALUE."
  (cond
   ((stringp original-value) (random-string 10))
   ((numberp original-value) (random 10000))
   ((or (booleanp original-value) (equal :false original-value)) original-value)
   ((equal :null original-value) :null)
   (t (random-string 10))))  ; Default case for unexpected types

(defun m/json:obfuscate-json (json-data)
  "Recursively obfuscate values in JSON-DATA."
  (cond
   ((hash-table-p json-data)
    (maphash (lambda (key value)
               (puthash key (m/json:obfuscate-json value) json-data))
             json-data)
    json-data)
   ((vectorp json-data)
    (vconcat (mapcar #'m/json:obfuscate-json json-data)))
   (t (m/json:random-value json-data))))

(defun m/json:obfuscate-json-string (json-string)
  "Obfuscate all values in a JSON-STRING and return the resulting JSON string."
  (let ((json-data (json-parse-string json-string)))
    (json-serialize (m/json:obfuscate-json json-data))))

(defun m/json:obfuscate-json-buffer ()
  "Obfuscate all values in the current buffer containing JSON."
  (interactive)
  (let ((json-string (buffer-string)))
    (erase-buffer)
    (insert (m/json:obfuscate-json-string json-string))))

