(defun tja-org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (setq filename
        (concat
         (make-temp-name
          (concat (buffer-file-name)
                  "_"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (call-process "wl-paste" nil `(:file ,filename) nil "-t" "image/png")
  (insert (concat "[[" filename "]]"))
  (org-display-inline-images))

(provide 'tja-org)
