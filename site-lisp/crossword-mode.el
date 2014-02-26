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
