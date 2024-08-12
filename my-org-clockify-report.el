;; This is borrowed and modified from org-agenda-list and org-clock-get-clocktable in org-agenda.el
;; Then modified to print out an org clock report for the day
(defun m/org:org-clockify-report ()
  "Retrieves the arguments from the Agenda buffer and passes them through to the clockify table builder"
  (interactive)
  (setq args (get-text-property (min (1- (point-max)) (point)) 'org-last-args)
        start-day (nth 1 args)
        span (nth 2 args)
        today nil)
  (let* ((span (org-agenda-ndays-to-span (or span org-agenda-span)))
         (today (org-today))
         (sd (or start-day today))
         (ndays (org-agenda-span-to-ndays span sd))
         (start (if (or (null org-agenda-start-on-weekday)
                        (< ndays 7))
                    sd
                  (let* ((nt (calendar-day-of-week
                              (calendar-gregorian-from-absolute sd)))
                         (n1 org-agenda-start-on-weekday)
                         (d (- nt n1)))
                    (- sd (+ (if (< d 0) 7 0) d)))))
         (day-numbers (list start)))
    (dotimes (_ (1- ndays))
      (push (1+ (car day-numbers)) day-numbers))
    (setq day-numbers (nreverse day-numbers))
    (setq clocktable-start (car day-numbers)
          clocktable-end (1+ (or (org-last day-numbers) 0)))
    (let ((org-agenda-files (org-agenda-files nil 'ifmode))
          ;; the above line is to ensure the restricted range!
          (p (copy-sequence org-agenda-clockreport-parameter-plist))
          tbl)
      (setq p (org-plist-delete p :block))
      (setq p (plist-put p :tstart clocktable-start))
      (setq p (plist-put p :tend clocktable-end))
      (setq p (plist-put p :scope 'agenda))
      (setq p (plist-put p :link nil))
      (setq p (plist-put p :properties '("clockify-project-id")))
      (setq tbl (apply #'m/org:org-clockify-get-clockify-table p))
      (with-current-buffer (get-buffer-create "*clockify-fns*")
        (erase-buffer)
        (insert tbl)
        (emacs-lisp-mode)
        (switch-to-buffer-other-window "*clockify-fns*")
        ))))

(defun m/org:org-clockify-get-clockify-table (&rest props)
  "Takes props from the input agenda fn, passes them through to an org dblock"
  (setq props (plist-put props :name "clockify-table"))
  (unless (plist-member props :maxlevel)
    (setq props (plist-put props :maxlevel 2)))
  (unless (plist-member props :scope)
    (setq props (plist-put props :scope 'agenda)))
  (interactive)
  (with-temp-buffer
    (org-mode)
    (org-create-dblock props)
    (org-update-dblock)
    (forward-line 2)
    (buffer-substring (point) (progn
                                (re-search-forward "^[ \t]*#\\+END" nil t)
                                (line-beginning-position)))))

(defun org-dblock-write:clockify-table (params)
  "Writes the org dblock for a clockify table.  Gathers all of the org agenda files and converts them into a structured format to pass to the formatter function.  Which should write all of this out in a table."
  ;(setq params (org-combine-plists org-clocktable-defaults params))
  (catch 'exit
    (let* ((scope (plist-get params :scope))
           (base-buffer (org-base-buffer (current-buffer)))
           (files (pcase scope
                    (`agenda
                     (org-agenda-files t))
                    (`agenda-with-archives
                     (org-add-archive-files (org-agenda-files t)))
                    (`file-with-archives
                     (let ((base-file (buffer-file-name base-buffer)))
                       (and base-file
                            (org-add-archive-files (list base-file)))))
                    ((or `nil `file `subtree `tree
                         (and (pred symbolp)
                              (guard (string-match "\\`tree\\([0-9]+\\)\\'"
                                                   (symbol-name scope)))))
                     base-buffer)
                    ((pred functionp) (funcall scope))
                    ((pred consp) scope)
                    (_ (user-error "Unknown scope: %S" scope))))
           (block (plist-get params :block))
           (ts (plist-get params :tstart))
           (te (plist-get params :tend))
           (ws (plist-get params :wstart))
           (ms (plist-get params :mstart))
           (step (plist-get params :step))
           (hide-files (plist-get params :hidefiles))
           (formatter (or (plist-get params :formatter)
                          'my:org-clock/clockify-simple-fn-formatter))
           cc)
      ;; Check if we need to do steps
      (when block
        ;; Get the range text for the header
        (setq cc (org-clock-special-range block nil t ws ms)
              ts (car cc)
              te (nth 1 cc)))
      (when step
        ;; Write many tables, in steps
        (unless (or block (and ts te))
          (user-error "Clocktable `:step' can only be used with `:block' or `:tstart', `:tend'"))
        (org-clocktable-steps params)
        (throw 'exit nil))

      (org-agenda-prepare-buffers (if (consp files) files (list files)))

      (let ((origin (point))
            (tables
             (if (consp files)
                 (mapcar (lambda (file)
                           (with-current-buffer (find-buffer-visiting file)
                             (save-excursion
                               (save-restriction
                                 (org-clock-get-table-data file params)))))
                         files)
               ;; Get the right restriction for the scope.
               (save-restriction
                 (cond
                  ((not scope))	     ;use the restriction as it is now
                  ((eq scope 'file) (widen))
                  ((eq scope 'subtree) (org-narrow-to-subtree))
                  ((eq scope 'tree)
                   (while (org-up-heading-safe))
                   (org-narrow-to-subtree))
                  ((and (symbolp scope)
                        (string-match "\\`tree\\([0-9]+\\)\\'"
                                      (symbol-name scope)))
                   (let ((level (string-to-number
                                 (match-string 1 (symbol-name scope)))))
                     (catch 'exit
                       (while (org-up-heading-safe)
                         (looking-at org-outline-regexp)
                         (when (<= (org-reduced-level (funcall outline-level))
                                   level)
                           (throw 'exit nil))))
                     (org-narrow-to-subtree))))
                 (list (org-clock-get-table-data nil params)))))
            (multifile
             ;; Even though `file-with-archives' can consist of
             ;; multiple files, we consider this is one extended file
             ;; instead.
             (and (not hide-files)
                  (consp files)
                  (not (eq scope 'file-with-archives)))))

        (funcall formatter
                 origin
                 tables
                 (org-combine-plists params `(:multifile ,multifile)))))))

(defun format--decoded-time-to-clockify (time)
  (format-time-string "%FT%R:00.000-07:00" (encode-time time)))

(defun increment--dt-minutes (time minutes)
  (let ((delta (make-decoded-time :minute minutes)))
    (decoded-time-add time delta)))

(defun convert--org-time-thing-to-decoded-date (org-date)
  "Converts an org date things like 738644 to a decoded time (current timezone)"
  (let* ((date (calendar-gregorian-from-absolute org-date))
       (year (calendar-extract-year date))
       (month (calendar-extract-month date))
       (day (calendar-extract-day date))
       (time (make-decoded-time :second 0 :minute 0 :hour 0 :day day :month month :year year :dst nil :zone (car (current-time-zone)))))
  time))

(defun round--to-nearest-five-minutes (num)
  (* 5 (ceiling (/ num 5.0))))

(defun format--clockify-fn (start-time duration-minutes description project-id)
  (let* ((decoded-start-time (decode-time (org-read-date nil t start-time)))
         (end-time (format--decoded-time-to-clockify (increment--dt-minutes decoded-start-time duration-minutes))))
    (format "(m/clockify:add-entry \"%s\"  \"%s\" \"%s\" \"%s\")" start-time end-time (my:escape-quotes description) project-id)))

(defun my:org-clock/clockify-simple-fn-formatter (ipos tables params)
  (let* ((tstart (plist-get params :tstart))
         (gcal-tstart (calendar-gregorian-from-absolute tstart))
         (day-start (make-decoded-time  :second 0 :minute 0 :hour 06 :month (calendar-extract-month gcal-tstart) :day (calendar-extract-day gcal-tstart) :year (calendar-extract-year gcal-tstart) :dst t :zone (car (current-time-zone))))
         (current-time-block-start-time day-start)
         (lang (or (plist-get params :lang) "en"))
         (multifile (plist-get params :multifile))
         (block (plist-get params :block))
         (sort (plist-get params :sort))
         (header (plist-get params :header))
         (link (plist-get params :link))
         (maxlevel (or (plist-get params :maxlevel) 3))
         (emph (plist-get params :emphasize))
         (compact? (plist-get params :compact))
         (narrow (or (plist-get params :narrow) (and compact? '40!)))
         (filetitle (plist-get params :filetitle))
         (level? (and (not compact?) (plist-get params :level)))
         (timestamp (plist-get params :timestamp))
         (tags (plist-get params :tags))
         (properties (plist-get params :properties))
         (time-columns
          (if (or compact? (< maxlevel 2)) 1
            ;; Deepest headline level is a hard limit for the number
            ;; of time columns.
            (let ((levels
                   (cl-mapcan
                    (lambda (table)
                      (pcase table
                        (`(,_ ,(and (pred wholenump) (pred (/= 0))) ,entries)
                         (mapcar #'car entries))))
                    tables)))
              (min maxlevel
                   (or (plist-get params :tcolumns) 100)
                   (if (null levels) 1 (apply #'max levels))))))
         (indent (or compact? (plist-get params :indent)))
         (formula (plist-get params :formula))
         (case-fold-search t)
         (total-time (apply #'+ (mapcar #'cadr tables)))
         recalc narrow-cut-p)

    ;; Now we need to output this table stuff.
    (goto-char ipos)

    ;; Now iterate over the tables and insert the data but only if any
    ;; time has been collected.
    (insert-before-markers "\n") ; buffer gets formatted and loses the top line, which would be a time entry in this case
    (insert-before-markers ";; WARNING - GSI will only work for tours tasks right now, update the project id for maintenance tasks.  FBP Task Id is 5fd10f1e29e3892afebb82dc\n;; WARNING - ESA will only work with FBP Maint. ESA App id is 642d934d2a9edb25fb32d316\n(progn\n") ; buffer gets formatted and loses the top line, which would be a time entry in this case
    (when (and total-time (> total-time 0))
      (pcase-dolist (`(,file-name ,file-time ,entries) tables)
        (when (or (and file-time (> file-time 0))
                  (not (plist-get params :fileskip0)))
          ;; First the file time, if we have multiple files.
          (when multifile
            ;; Summarize the time collected from this file.
            )

          ;; Insert the clockify function data
          (when (> maxlevel 0)
            (setq project-id "no id found")
            (pcase-dolist (`(,level ,headline ,tgs ,ts ,time ,props) entries)
              (when (= level 1)
                (setq project-id (cdr (assoc "clockify-project-id" props))))
              (when (>= level 2)
                (insert-before-markers
                 (concat (format--clockify-fn (format--decoded-time-to-clockify current-time-block-start-time) (round--to-nearest-five-minutes time) headline project-id)) "\n"))
              (when (>= level 2)
                (setq current-time-block-start-time
                      (increment--dt-minutes current-time-block-start-time (round--to-nearest-five-minutes time)))))))))
    (delete-char -1)
    (insert-before-markers "\n)") ;; Closing paren for progn
    ;; Back to beginning, align the table, recalculate if necessary.
    (goto-char ipos)
    (when sort
      (save-excursion
        (org-table-goto-line 3)
        (org-table-goto-column (car sort))
        (org-table-sort-lines nil (cdr sort))))
    (when recalc (org-table-recalculate 'all))
    total-time))

(defun my:org-clock/clockify-fn-formatter (ipos tables params)
  "Takes all of the structured data from the Agenda view and files, and formats it into a table structure"
  (let* ((day-start (make-decoded-time  :second 0 :minute 0 :hour 06 :month 5 :day 6 :year 2023 :dst t :zone (car (current-time-zone))))
         (current-time-block-start-time day-start)
         (lang (or (plist-get params :lang) "en"))
         (multifile (plist-get params :multifile))
         (block (plist-get params :block))
         (sort (plist-get params :sort))
         (header (plist-get params :header))
         (link (plist-get params :link))
         (maxlevel (or (plist-get params :maxlevel) 3))
         (emph (plist-get params :emphasize))
         (compact? (plist-get params :compact))
         (narrow (or (plist-get params :narrow) (and compact? '40!)))
         (filetitle (plist-get params :filetitle))
         (level? (and (not compact?) (plist-get params :level)))
         (timestamp (plist-get params :timestamp))
         (tags (plist-get params :tags))
         (properties (plist-get params :properties))
         (time-columns
          (if (or compact? (< maxlevel 2)) 1
            ;; Deepest headline level is a hard limit for the number
            ;; of time columns.
            (let ((levels
                   (cl-mapcan
                    (lambda (table)
                      (pcase table
                        (`(,_ ,(and (pred wholenump) (pred (/= 0))) ,entries)
                         (mapcar #'car entries))))
                    tables)))
              (min maxlevel
                   (or (plist-get params :tcolumns) 100)
                   (if (null levels) 1 (apply #'max levels))))))
         (indent (or compact? (plist-get params :indent)))
         (formula (plist-get params :formula))
         (case-fold-search t)
         (total-time (apply #'+ (mapcar #'cadr tables)))
         recalc narrow-cut-p)

    (when (and narrow (integerp narrow) link)
      ;; We cannot have both integer narrow and link.
      (message "Using hard narrowing in clocktable to allow for links")
      (setq narrow (intern (format "%d!" narrow))))

    (pcase narrow
      ((or `nil (pred integerp)) nil)	;nothing to do
      ((and (pred symbolp)
            (guard (string-match-p "\\`[0-9]+!\\'" (symbol-name narrow))))
       (setq narrow-cut-p t)
       (setq narrow (string-to-number (symbol-name narrow))))
      (_ (user-error "Invalid value %s of :narrow property in clock table" narrow)))

    ;; Now we need to output this table stuff.
    (goto-char ipos)

    ;; Insert the text *before* the actual table.
    (insert-before-markers
     (or header
         ;; Format the standard header.
         (format "#+CAPTION: %s %s%s\n"
                 (org-clock--translate "Clock summary at" lang)
                 (format-time-string (org-time-stamp-format t t))
                 (if block
                     (let ((range-text
                            (nth 2 (org-clock-special-range
                                    block nil t
                                    (plist-get params :wstart)
                                    (plist-get params :mstart)))))
                       (format ", for %s." range-text))
                   ""))))

    ;; Insert the narrowing line
    (when (and narrow (integerp narrow) (not narrow-cut-p))
      (insert-before-markers
       "|"				;table line starter
       (if multifile "|" "")		;file column, maybe
       (if level? "|" "")		;level column, maybe
       (if timestamp "|" "")		;timestamp column, maybe
       (if tags "|" "")                 ;tags columns, maybe
       (if properties			;properties columns, maybe
           (make-string (length properties) ?|)
         "")
       (format "<%d>| |\n" narrow)))	;headline and time columns

    ;; Insert the table header line
    (insert-before-markers
     "|"				;table line starter
     (if multifile			;file column, maybe
         (concat (org-clock--translate "File" lang) "|")
       "")
     (if level?				;level column, maybe
         (concat (org-clock--translate "L" lang) "|")
       "")
     (if timestamp			;timestamp column, maybe
         (concat (org-clock--translate "Timestamp" lang) "|")
       "")
     (if tags "Tags |" "")              ;tags columns, maybe

     (if properties			;properties columns, maybe
         (concat (mapconcat #'identity properties "|") "|")
       "")
     (concat (org-clock--translate "Headline" lang)"|")
     "Clockify-Fn|"
     (concat (org-clock--translate "Time" lang) "|")
     (make-string (max 0 (1- time-columns)) ?|) ;other time columns
     (if (eq formula '%) "%|\n" "\n"))

    ;; Insert the total time in the table
    (insert-before-markers
     "|-\n"				;a hline
     "|"				;table line starter
     (if multifile (format "| %s " (org-clock--translate "ALL" lang)) "")
                                        ;file column, maybe
     "|" ; Clockify-fn column?
     (if level?    "|" "")		;level column, maybe
     (if timestamp "|" "")		;timestamp column, maybe
     (if tags      "|" "")		;timestamp column, maybe
     (make-string (length properties) ?|) ;properties columns, maybe
     (concat (format org-clock-total-time-cell-format
                     (org-clock--translate "Total time" lang))
             "| ")
     (format org-clock-total-time-cell-format
             (org-duration-from-minutes (or total-time 0))) ;time
     "|"
     (make-string (max 0 (1- time-columns)) ?|)
     (cond ((not (eq formula '%)) "")
           ((or (not total-time) (= total-time 0)) "0.0|")
           (t  "100.0|"))
     "\n")

    ;; Now iterate over the tables and insert the data but only if any
    ;; time has been collected.
    (when (and total-time (> total-time 0))
      (pcase-dolist (`(,file-name ,file-time ,entries) tables)
        (when (or (and file-time (> file-time 0))
                  (not (plist-get params :fileskip0)))
          (insert-before-markers "|-\n") ;hline at new file
          ;; First the file time, if we have multiple files.
          (when multifile
            ;; Summarize the time collected from this file.
            (insert-before-markers
             (format (concat "| %s %s | %s%s%s"
                             (format org-clock-file-time-cell-format
                                     (org-clock--translate "File time" lang))

                             ;; The file-time rollup value goes in the first time
                             ;; column (of which there is always at least one)...
                             " | *%s*|"
                             ;; ...and the remaining file time cols (if any) are blank.
                             (make-string (max 0 (1- time-columns)) ?|)

                             ;; Optionally show the percentage contribution of "this"
                             ;; file time to the total time.
                             (if (eq formula '%) " %s |" "")
                             "\n")

                     (if filetitle
                         (or (org-get-title file-name)
                             (file-name-nondirectory file-name))
                       (file-name-nondirectory file-name))
                     "| " ;clockify-fn
                     (if level?    "| " "") ;level column, maybe
                     (if timestamp "| " "") ;timestamp column, maybe
                     (if tags      "| " "") ;tags column, maybe
                     (if properties	    ;properties columns, maybe
                         (make-string (length properties) ?|)
                       "")
                     (org-duration-from-minutes file-time) ;time

                     (cond ((not (eq formula '%)) "")	   ;time percentage, maybe
                           ((or (not total-time) (= total-time 0)) "0.0")
                           (t
                            (format "%.1f" (* 100 (/ file-time (float total-time)))))))))

          ;; Get the list of node entries and iterate over it
          (when (> maxlevel 0)
            (pcase-dolist (`(,level ,headline ,tgs ,ts ,time ,props) entries)
              (when narrow-cut-p
                (setq headline
                      (if (and (string-match
                                (format "\\`%s\\'" org-link-bracket-re)
                                headline)
                               (match-end 2))
                          (format "[[%s][%s]]"
                                  (match-string 1 headline)
                                  (org-shorten-string (match-string 2 headline)
                                                      narrow))
                        (org-shorten-string headline narrow))))
              (cl-flet ((format-field (f) (format (cond ((not emph) "%s |")
                                                        ((= level 1) "*%s* |")
                                                        ((= level 2) "/%s/ |")
                                                        (t "%s |"))
                                                  f)))
                (setq short-headline (with-temp-buffer
                                       (insert (substring-no-properties headline))
                                       (search-backward "[")
                                       (buffer-substring (+ (point) 1) (- (buffer-end 1) 2))))
                (insert-before-markers
                 "|"		       ;start the table line
                 "|"

                 (concat "|" (format--clockify-fn (format--decoded-time-to-clockify current-time-block-start-time) (round--to-nearest-five-minutes time) short-headline))
                 (if multifile "|" "") ;free space for file name column?
                 (if level? (format "%d|" level) "") ;level, maybe
                 (if timestamp (concat ts "|") "")   ;timestamp, maybe
                 (if tags (concat (mapconcat #'identity tgs ", ") "|") "")   ;tags, maybe
                 (if properties		;properties columns, maybe
                     (concat (mapconcat (lambda (p) (or (cdr (assoc p props)) ""))
                                        properties
                                        "|")
                             "|")
                   "")
                 (if indent		;indentation
                     (org-clocktable-indent-string level)
                   "")
                 (format-field headline)
                 ;; Empty fields for higher levels.
                 (make-string (max 0 (1- (min time-columns level))) ?|)
                 (format-field (org-duration-from-minutes time))
                 (make-string (max 0 (- time-columns level)) ?|)
                 (if (eq formula '%)
                     (format "%.1f |" (* 100 (/ time (float total-time))))
                   "")
                 "\n")
                (when (= level 2)
                  (setq current-time-block-start-time
                        (increment--dt-minutes current-time-block-start-time (round--to-nearest-five-minutes time))))))))))
    (delete-char -1)
    (cond
     ;; Possibly rescue old formula?
     ((or (not formula) (eq formula '%))
      (let ((contents (org-string-nw-p (plist-get params :content))))
        (when (and contents (string-match "^\\([ \t]*#\\+tblfm:.*\\)" contents))
          (setq recalc t)
          (insert "\n" (match-string 1 contents))
          (beginning-of-line 0))))
     ;; Insert specified formula line.
     ((stringp formula)
      (insert "\n#+TBLFM: " formula)
      (setq recalc t))
     (t
      (user-error "Invalid :formula parameter in clocktable")))
    ;; Back to beginning, align the table, recalculate if necessary.
    (goto-char ipos)
    (skip-chars-forward "^|")
    (org-table-align)
    (when org-hide-emphasis-markers
      ;; We need to align a second time.
      (org-table-align))
    (when sort
      (save-excursion
        (org-table-goto-line 3)
        (org-table-goto-column (car sort))
        (org-table-sort-lines nil (cdr sort))))
    (when recalc (org-table-recalculate 'all))
    total-time))

;; Dont remember exactly what this was supposed to do.  Removing the interactive for now 01-Jun-2023
(defun m/org:org-clockify-clock-report (&optional arg)
  ;(interactive "P")
  (org-clock-remove-overlays)
  (when arg
    (org-find-dblock "clockify-table")
    (org-fold-show-entry))
  (pcase (org-in-clocktable-p)
    (`nil
     (org-create-dblock
      (org-combine-plists
       (list :scope (if (org-before-first-heading-p) 'file 'subtree))
       org-clock-clocktable-default-properties
       '(:name "clockify-table"))))
    (start (goto-char start)))
  (org-update-dblock))

(defun m/org-clock:get-clock-time ()
  (interactive)
  (let ((re (concat "[ \t]*" org-clock-string
                    " *[[<]\\([^]>]+\\)[]>]\\(-+[[<]\\([^]>]+\\)[]>]"
                    "\\([ \t]*=>.*\\)?\\)?"))
        ts te h m s neg)
    (cond
     ((not (looking-at re))
      nil)
     ((not (match-end 2))
      (when (and (equal (marker-buffer org-clock-marker) (current-buffer))
                 (> org-clock-marker (point))
                 (<= org-clock-marker (line-end-position)))
        ;; The clock is running here
        (setq org-clock-start-time
              (org-time-string-to-time (match-string 1)))
        (org-clock-update-mode-line)))
     (t
      ;; Prevent recursive call from `org-timestamp-change'.
      (cl-letf (((symbol-function 'org-clock-update-time-maybe) #'ignore))
        ;; Update timestamps.
        (save-excursion
          (goto-char (match-beginning 1)) ; opening timestamp
          (save-match-data (org-timestamp-change 0 'day)))
        ;; Refresh match data.
        (looking-at re)
        (save-excursion
          (goto-char (match-beginning 3)) ; closing timestamp
          (save-match-data (org-timestamp-change 0 'day))))
      ;; Refresh match data.
      (looking-at re)
      (end-of-line 1)
      (setq ts (match-string 1)
            te (match-string 3))
      (setq s (- (org-time-string-to-seconds te)
                 (org-time-string-to-seconds ts))
            neg (< s 0)
            s (abs s)
            h (floor (/ s 3600))
            s (- s (* 3600 h))
            m (floor (/ s 60))
            s (- s (* 60 s)))
      (message (concat (format-time-string "%Y-%m-%dT%H:%M:%S.000"
                                           (org-time-string-to-time ts))
                       " "
                       (format-time-string "%Y-%m-%dT%H:%M:%S.000"
                                           (org-time-string-to-time te))))
      t))))


(defun m/org-clock:make-clockify-string (total-time)
  (concat (format "(m/clockifyadd-entry \"2023-05-01\" \"06:55\" \"%d\" \"Test\")" (* 10.0 (fround (/ total-time 10.0))))))
