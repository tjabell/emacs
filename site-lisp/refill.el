(require 'limited)

(defvar refill-mode nil
	"Mode variable for refill minor mode.")

(make-variable-buffer-local 'refill-mode)

(defun refill-mode (&optional arg)
	"Refill minor mode."
	(interactive "P")
	(setq refill-mode
				(if (null arg)
						(not refill-mode)
					(> (prefix-numeric-value arg) 0)))
	(if refill-mode
			(add-hook 'after-change-functions 'refill nil t)
		(remove-hook 'after-change-functions 'refill t)))

(if (not (assq 'refill-mode minor-mode-alist))
		(setq minor-mode-alist
					(cons '(refill-mode " Refill")
								minor-mode-alist)))

(defun refill (start end len)
	"After a text change, refill the current paragraph."
	(let ((left (if (or (zerop len)
											(not (before-2nd-word-p start)))
									(limited-save-excursion
										(backward-paragraph 1))
								(limited-save-excursion
									(max (progn
												 (goto-char start)
												 (beginning-of-line 0)
												 (point))
											 (progn
												 (goto-char start)
												 (backward-paragraph 1)
												 (point)))))))
		(if (or (and (zerop len)
							(same-line-p start end)
							(short-line-p end))
						(and (eq (char-syntax (preceding-char))
										 ?\ )
								 (looking-at "\\s *$")))
				nil
			(limited-save-excursion 
				(fill-region left end nil nil t)))))

(defun short-line-p (end)
	(limited-save-excursion
		(end-of-line)
		(>= fill-column (point))))

(defun same-line-p (start end)
	(limited-save-excursion
		(goto-char start)
		(end-of-line)
		(<= end (point))))

(defun before-2nd-word-p (pos)
	"Does POS lie before the second word on the line?"
	(limited-save-excursion
		(goto-char pos)
		(beginning-of-line)
		(skip-syntax-forward (concat "^ "
																 (char-to-string
																	(char-syntax ?\n))))
		(skip-syntax-forward " ")
		(< pos (point))))

(provide 'refill-mode)
