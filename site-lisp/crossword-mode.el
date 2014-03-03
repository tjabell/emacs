(require 'matrix)

(defun make-crossword (size)
	"Make a crossword grid with SIZE rows and columns."
	(if  (zerop (% size 2))
			(error "make-crossword: size must be odd"))
	(if (< size 3)
			(error "make-crossword: size must be 3 or greater"))
	(make-matrix size size nil))

(defsubst crossword-size (crossword)
	"Number of rows and columns in CROSSWORD"
	(matrix-rows crossword))

(defsubst crossword-ref (crossword row column)
	"Get the element of CROSSWORD at ROW and COLUMN"
	(matrix-ref crossword row column))

(defsubst crossword--set (crossword row column elt)
	"Internal function for setting crossword grid square"
	(matrix-set crossword row column elt))

(defun crossword-cousin-position (crossword row column)
	"Give the cousin position for CROSSWORD ROW and COLUMN."
	(let ((size (crossword-size crossword)))
		(cons (- size row 1) (- size column 1))))

(defun crossword-cousin-ref (crossword row column)
	"Get the cousin of CROSSWORD's ROW, COLUMN position."
	(let ((cousin-position (crossword-cousin-position crossword row column)))
		(crossword-ref crossword
									 (car cousin-position)
									 (cdr cousin-position))))

(defun crossword--cousin-set (crossword row column elt)
	"Internal function for setting the cousin of a cell."
	(let ((cousin-position (crossword-cousin-position crossword
																										row
																										column)))
		(crossword--set crossword
										(car cousin-position)
										(cdr cousin-position)
										elt)))

(defun crossword-store-letter  (crossword row column letter)
	"Given CROSSWORD, ROW, and COLUMN, put LETTER there."
	(crossword--set crossword row column letter)
	(if (numberp (crossword-cousin-ref crossword row column))
			nil
		(crossword--cousin-set crossword row column 'letter)))

(defun crossword-store-block (crossword row column)
	"Given CROSSWORD, ROW, and COLUMN, put a block there."
	(crossword--set crossword row column 'block)
	(crossword--cousin-set crossword row column 'block))

(defun crossword-clear-cell (crossword row column)
	"Erase the CROSSWORD cell at ROW, COLUMN."
	(let ((cousin-position (crossword-cousin-position crossword row column)))
		(if (and (not (equal cousin-position
												 (cons row column)))
						 (numberp (crossword-ref crossword
																		 (car cousin-position)
																		 (cdr cousin-position))))
				(crossword--set crossword row column 'letter)
			(crossword--set crossword row column nil)
			(crossword--set crossword
											(car cousin-position)
											(cdr cousin-position)
											nil))))

(defun crossword-one-letter-p (crossword row column)
	"Is CROSSWORD cell at ROW, COLUMN a one letter word"
	(and (not (eq (crossword-ref crossword row column)
								'block))
			 (or (and (crossword-block-p crossword (- row 1) column)
								(crossword-block-p crossword (+ row 1) column))
					 (and (crossword-block-p crossword row (- column 1))
								(crossword-block-p crossword row (+ column 1))))))

(defun crossword-block-p  (crossword row column)
	"Does CROSSWORD's ROW, COLUMN contain a block"
	(or (< row 0)
			(>= row (crossword-size crossword))
			(< column 0)
			(>= column (crossword-size crossword))
			(eq  (crossword-ref crossword row column) 'block)))


(defun crossword-insert-grid (crossword)
	"Insert CROSSWORD into the current buffer."
	(mapcar 'crossword-insert-row crossword))

(defun crossword-insert-row (row)
	"Insert ROW into the current buffer."
	(mapcar 'crossword-insert-cell row)
	(insert "\n"))

(defun crossword-insert-cell (cell)
	"Insert CELL into the current buffer."
	(insert (cond ((null cell) ".")
								((eq cell 'letter) "?")
								((eq cell 'block) "#")
								((numberp cell) cell)
								(t cell))
					" "))

(defun crossword-place-cursor (row column)
	"Move point to ROW, COLUMN"
	(goto-char (point-min))
	(forward-line row)
	(forward-char (* column 2)))

(defun crossword-cursor-coords ()
	"Compute (ROW . COLUMN) from cursor position."
	(cons (- (current-line) 1)
				(/ (current-column) 2)))

(defun current-line ()
	"Return line number containing point."
	(let ((result 1))
		(save-excursion
			(beginning-of-line)
			(while (not (bobp))
				(forward-line -1)
				(setq result (+ result 1)))
			result)))

(defun crossword-update-display (crossword)
	"Called after a change, keeps the display up to date."
	(let* ((coords (crossword-cursor-coords))
				 (cousin-coords (crossword-cousin-position crossword
																									 (car coords)
																									 (cdr coords))))
		(save-excursion
			(crossword-place-cursor (car coords)
															(cdr coords))
			(delete-char 2)
			(crossword-insert-cell (crossword-ref crossword
																						(car coords)
																						(cdr coords)))
			(crossword-place-cursor (car cousin-coords)
															(cdr cousin-coords))
			(delete-char 2)
			(crossword-insert-cell (crossword-ref crossword
																						(car cousin-coords)
																						(cdr cousin-coords))))))

;;; User Interface
(defun crossword-erase-command ()
	"Erase current crossword cell."
	(interactive)
	(let ((coords (crossword-cursor-coords)))
		(crossword-clear-cell crossword-grid
													(car coords)
													(cdr coords)))
	(crossword-update-display crossword-grid))

(defun crossword-block-command ()
	"Insert a block in current cell and cousin"
	(interactive)
	(let ((coords (crossword-cursor-coords)))
		(crossword-store-block crossword-grid
													 (car coords)
													 (cdr coords))
		(crossword-update-display crossword-grid)))

(defun crossword-self-insert ()
	"Selt-insert letter in current cell."
	(interactive)
	(let ((coords (crossword-cursor-coords)))
		(crossword-store-letter crossword-grid
														(car coords)
														(cdr coords)
														(aref (this-command-keys) 0)))
	(crossword-update-display crossword-grid))


;;; Navigation

(defun crossword-cursor-right (arg)
	"Move ARG cells to the right."
	(interactive "p")
	(let* ((coords (crossword-cursor-coords))
				 (new-column (+ arg (cdr coords))))
		(if (or (< new-column 0)
						(>= new-column (crossword-size crossword-grid)))
				(error "Out of bounds"))
		(crossword-place-cursor (car coords) new-column)))

(defun crossword-cursor-left (arg)
	"Move ARG cells to the left."
	(interactive "p")
	(crossword-cursor-right (- arg)))

(defun crossword-cursor-down (arg)
	"Move ARG cells down"
	(interactive "p")
	(let* ((coords (crossword-cursor-coords))
				 (new-row (+ arg (car coords))))
		(if (or (< new-row 0)
						(>= new-row (crossword-size crossword-grid)))
				(error "Out of bounds")
			(crossword-place-cursor new-row
															(cdr coords)))))


(defun crossword-cursor-up (arg)
	"Move ARG cells up"
	(interactive "p")
	(crossword-cursor-down (- arg)))


(defun crossword-beginning-of-row ()
	"Move to beginning of current row."
	(interacive)
	(let ((coords (crossword-cursor-coords)))
		(crossword-place-cursor (car coords) 0)))

(defun crossword-end-of-row ()
	"Move to end of current row."
	(interacive)
	(let ((coords (crossword-cursor-coords)))
		(crossword-place-cursor (car coords)
														(- (crossword-size crossword-grid)
															 1))))

(defun crossword-top-of-column ()
	"Move to top of current column."
	(interactive)
	(let ((coords (crossword-cursor-coords)))
		(crossword-place-cursor 0 (cdr coords))))

(defun crossword-bottom-of-column ()
	"Move to bottom of current column"
	(interactive)
	(let ((coords (crossword-cursor-coords)))
		(crossword-place-cursor (- (crossword-size crossword-grid)
															 1
															 (cdr coords)))))

(defun crossword-beginning-of-grid ()
	"Move to the beginning of the grid"
	(interactive)
	(crossword-place-cursor 0 0))

(defun crossword-end-of-grid ()
	"Move to the end of the grid"
	(interactive)
	(let ((size (crossword-size crossword-grid)))
		(crossword-place-cursor size size)))

(defun crossword-jump-to-cousin ()
	"Move to cousin of current cell."
	(interactive)
	(let* ((coords (crossword-cursor-coords))
				 (cousin (crossword-cousin-position crossword-grid
																						(car coords)
																						(cdr coords))))
		(crossword-place-cursor (car cousin)
														(cdr cousin))))

;;; Mode
(defun crossword (size)
	"Create a new buffer with an empty crossword grid."
	(interactive "nGrid size: ")
	(let* ((grid (make-crossword size))
				 (buffer (generate-new-buffer "*Crossword*")))
		(switch-to-buffer buffer)
		(crossword-insert-grid grid)
		(crossword-place-cursor 0 0)
		(crossword--mode-setup grid)))

(defun crossword-mode ()
	"Major mode for editing crossword puzzles.
Special Commands:
\\{crossword-mode-map}"
	(interactive)
	(crossword--mode-setup (crossword-parse-buffer))
)

(defun crossword--mode-setup (grid)
	"Auxiliary function to set up crossword mode"
	(kill-all-local-variables)
	(setq major-mode 'crossword-mode)
	(setq mode-name "Crossword")
	(use-local-map crossword-mode-map)
	(make-local-variable 'crossword-grid)
	(setq crossword-grid grid)
	(crossword-place-cursor 0 0)
	(run-hooks 'crossword-mode-hook))

;;; Keybindings
(defvar crossword-mode-map nil
	"Keymap for Crossword mode.")

(if crossword-mode-map
		nil
	(setq crossword-mode-map (make-keymap))
	(suppress-keymap crossword-mode-map)
	(let ((equivs
				 '((forward-char . crossword-cursor-right)
					 (backward-char . crossword-cursor-left)
					 (previous-line . crossword-cursor-up)
					 (next-line . crossword-cursor-down)
					 (beginning-of-line . crossword-beginning-of-row)
					 (end-of-line . crossword-end-of-row)
					 (beginning-of-buffer . crossword-beginning-of-grid)
					 (end-of-buffer . crossword-end-of-grid))))
		(while equivs
			(substitute-key-definition (car (car equivs))
																(cdr (car equivs))
																crossword-mode-map
																(current-global-map))
			(setq equivs (cdr equivs))))
	(let ((letters
				 '(?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M ?N ?O ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z 
							?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z )))
		(while letters
			(define-key crossword-mode-map
				(char-to-string (car letters))
				'crossword-self-insert)
			(setq letters (cdr letters))))
	(define-key crossword-mode-map " " 'crossword-erase-command)
	(define-key crossword-mode-map "#" 'crossword-block-command)
	(define-key crossword-mode-map "\C-ct" 'crossword-top-of-column)
	(define-key crossword-mode-map "\C-cb" 'crossword-bottom-of-column)
	(define-key crossword-mode-map "\C-c\C-c" 'crossword-jump-to-cousin))

(defvar crossword-mouse-location nil
	"Location of last mouse-down event")

(defun crossword-mouse-set-point (event)
	"Set point with the mouse."
	(interactive "@e")
	(mouse-set-point event)
	(let ((coords (crossword-cursor-coords)))
		(setq crossword-mouse-location coords)
		(crossword-place-cursor (car coords)
														(cdr coords))))

(defun crossword-mouse-block (event)
	"Set block with the mouse."
	(interactive "@e")
	(mouse-set-point event)
	(let ((coords (crossword-cursor-coords)))
		(if (equal coords crossword-mouse-location)
				(crossword-block-command))))

(defun crossword-mouse-erase (event)
	"Erase a cell with the mouse."
	(interactive "@e")
	(mouse-set-point event)
	(let ((coords (crossword-cursor-coords)))
		(if (equal coords crossword-mouse-location)
				(crossword-erase-command))))

(define-key crossword-mode-map [down-mouse-1] 'crossword-mouse-set-point)
(define-key crossword-mode-map [mouse-1] 'crossword-mouse-set-point)
(define-key crossword-mode-map [down-mouse-2] 'crossword-mouse-set-point)
(define-key crossword-mode-map [mouse-2] 'crossword-mouse-block)
(define-key crossword-mode-map [down-mouse-3] 'crossword-mouse-set-point)
(define-key crossword-mode-map [mouse-3] 'crossword-mouse-erase)


(provide 'crossword)
