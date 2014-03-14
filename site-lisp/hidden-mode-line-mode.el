(defvar-local hidden-mode-line-mode nil)
(defvar-local hide-mode-line nil)

(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global nil
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  ;; Apparently force-mode-line-update is not always enough to
  ;; redisplay the mode-line
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled.  "
             "Use M-x hidden-mode-line-mode to make the mode-line appear."))))

(global-set-key (kbd "C-s-SPC") 'hidden-mode-line-mode)

;; Activate hidden-mode-line-mode
;;(hidden-mode-line-mode 1)

;; If you want to hide the mode-line in all new buffers
;; (add-hook 'after-change-major-mode-hook 'hidden-mode-line-mode)

;; Alternatively, you can paint your mode-line in White but then
;; you'll have to manually paint it in black again
;; (custom-set-faces
;;  '(mode-line-highlight ((t nil)))
;;  '(mode-line ((t (:foreground "white" :background "white"))))
;;  '(mode-line-inactive ((t (:background "white" :foreground "white")))))

;; Command to toggle the display of the mode-line as a header
(defvar-local header-line-format nil)

(defmacro with-face (str &rest properties)
    `(propertize ,str 'face (list ,@properties)))

(defun sl/make-header ()
	""
	(let* ((sl/full-header (abbreviate-file-name buffer-file-name))
				 (sl/header (file-name-directory sl/full-header))
				 (sl/drop-str "[...]"))
		(if (> (length sl/full-header)
					 (window-body-width))
				(if (> (length sl/header)
							 (window-body-width))
						(progn
							(concat (with-face sl/drop-str
																 :background "blue"
																 :weight 'bold
																 )
											(with-face (substring sl/header
																						(+ (- (length sl/header)
																									(window-body-width))
																							 (length sl/drop-str))
																						(length sl/header))
																 ;; :background "red"
																 :weight 'bold
																 )))
					(concat (with-face sl/header
														 ;; :background "red"
														 :foreground "#8fb28f"
														 :weight 'bold
														 )))
			(concat (with-face sl/header
												 ;; :background "green"
												 :foreground "black"
												 :weight 'bold
												 :foreground "#8fb28f"
												 )
							(with-face (file-name-nondirectory buffer-file-name)
												 :weight 'bold
												 ;; :background "red"
												 :foreground "red"
												 )
							(with-face " %l"
												 :weight 'bold)))))

(defun sl/display-header ()
    (setq header-line-format
          '("" ;; invocation-name
            (:eval (if (buffer-file-name)
                       (sl/make-header)
                     "%b")))))

;; (add-hook 'buffer-list-update-hook
;;           'sl/display-header)

(defun mode-line-in-header ()
  (interactive)
  (if (not header-line-format)
      (sl/display-header)
    (setq header-line-format nil))
  (set-window-buffer nil (current-buffer)))

(provide 'hidden-mode-line-mode)
