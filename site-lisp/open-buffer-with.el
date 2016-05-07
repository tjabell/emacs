(defun open-buffer-with (txt)
  "create a new buffer, insert text"
  (pop-to-buffer (get-buffer-create (generate-new-buffer-name "temp")))
  (insert txt))
