(fset 'copy-last-test
   [?\C-r ?\} ?\C-f ?\C-  ?\C-r ?\[ ?\M-w ?\C-x ?\C-x ?\C-n ?\C-n ?\C-y])

(defalias 'clt 'copy-last-test)

(defmacro definteractive-string-func (functionname stringfunc)
  "Creates a function which operates on a region or returns a string"
  (let ((funsymbol (intern (concat "tabell-" functionname))))
    `(defun ,funsymbol (ξstring &optional ξfrom ξto)
       (interactive
        (if (use-region-p)
            (list nil (region-beginning) (region-end))
          (let ((bds (bounds-of-thing-at-point 'paragraph)) )
            (list nil (car bds) (cdr bds)) ) ) )

       (let (workOnStringP inputStr outputStr)
         (setq workOnStringP (if ξstring t nil))
         (setq inputStr (if workOnStringP ξstring (buffer-substring-no-properties ξfrom ξto)))
         (setq outputStr
               (funcall ,stringfunc inputStr))
         (if workOnStringP
             outputStr
           (save-excursion
             (delete-region ξfrom ξto)
             (goto-char ξfrom)
             (insert outputStr) )) )
         )))


(definteractive-string-func "make-mocks" (lambda (region-or-buffer)
                                           (let ((case-fold-search t))
                                             (replace-regexp-in-string  "private readonly \\(.*\\) \\(.*\\);" "\\2 = Mock<\\1>();"  region-or-buffer) )
                                           ))

(definteractive-string-func "make-mock-parameters" (lambda (region-or-buffer)
                                                     
                                                     (let ((case-fold-search t))
                                                       (replace-regexp-in-string  "[[:space:]]*\\(.*\\) =.*" "\\1,\n"  region-or-buffer) )
                                           ))

;; (defun tabell/make-mocks (ξstring &optional ξfrom ξto)
;;   "Transform from a declaration in the class to a set of mocks

;; When called interactively, work on current paragraph or text selection.

;; When called in lisp code, if ξstring is non-nil, returns a changed string.
;; If ξstring nil, change the text in the region between positions ξfrom ξto."
;;   (interactive
;;    (if (use-region-p)
;;        (list nil (region-beginning) (region-end))
;;      (let ((bds (bounds-of-thing-at-point 'paragraph)) )
;;        (list nil (car bds) (cdr bds)) ) ) )

;;   (let (workOnStringP inputStr outputStr)
;;     (setq workOnStringP (if ξstring t nil))
;;     (setq inputStr (if workOnStringP ξstring (buffer-substring-no-properties ξfrom ξto)))
;;     (setq outputStr
;;           (let ((case-fold-search t))
;;             (replace-regexp-in-string  "private readonly \\(.*\\) \\(.*\\);" "\\2 = Mock<\\1>();"  inputStr) )  )
;;     (if workOnStringP
;;         outputStr
;;       (save-excursion
;;         (delete-region ξfrom ξto)
;;         (goto-char ξfrom)
;;         (insert outputStr) )) ) )

;; (defun tabell/make-mocks-parameters (ξstring &optional ξfrom ξto)
;;   "Transform from a declaration in the class to a set of mocks

;; When called interactively, work on current paragraph or text selection.

;; When called in lisp code, if ξstring is non-nil, returns a changed string.
;; If ξstring nil, change the text in the region between positions ξfrom ξto."
;;   (interactive
;;    (if (use-region-p)
;;        (list nil (region-beginning) (region-end))
;;      (let ((bds (bounds-of-thing-at-point 'paragraph)) )
;;        (list nil (car bds) (cdr bds)) ) ) )

;;   (let (workOnStringP inputStr outputStr)
;;     (setq workOnStringP (if ξstring t nil))
;;     (setq inputStr (if workOnStringP ξstring (buffer-substring-no-properties ξfrom ξto)))
;;     (setq outputStr
;;           (let ((case-fold-search t))
;;             (replace-regexp-in-string  "[[:space:]]*\\(.*\\) =.*" "\\1,\n"  inputStr) )  )
;;     (if workOnStringP
;;         outputStr
;;       (save-excursion
;;         (delete-region ξfrom ξto)
;;         (goto-char ξfrom)
;;         (insert outputStr) )) ) )
