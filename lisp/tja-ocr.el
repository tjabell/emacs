;;;###autoload
(defun tja-ocr-screenshot ()
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
  (call-process "tesseract" nil nil nil ,filename "temp_ocr_output")
  (find-file "temp_ocr_output.txt"))

(provide 'tja-ocr)
