;;; latex-pretty-symbols.el --- Display many latex symbols as their unicode counterparts

;; Copyright (C) 2011 Erik Parmann, PÃ¥l Drange
;;
;; Author: Erik Parmann <eparmann@gmail.com>
;;         PÃ¥l Drange
;; Created: 10. July 2011
;; Version: 1.0
;; Package-Version: 20150409.240
;; Keywords: convenience, display
;; Url: https://bitbucket.org/mortiferus/latex-pretty-symbols.el
;; Derived from  pretty-lambda.el (http://www.emacswiki.org/emacs/PrettyLambda ) by Drew Adams

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; License:
;; Licensed under the same terms as Emacs.

;;; Commentary:
;; Description: This library use font-locking to change the way Emacs displays
;;   various latex commands, like \Gamma, \Phi, etc.  It does not actually
;;   change the content of the file, only the way it is displayed.
;;
;; Quick start:
;;   add this file to load path, then (require 'latex-pretty-symbols)
;;

;;; TODO: The most pressing isue is a way to let not only single symbols be
;;   displayed, but also strings.  Then we can e.g display "âŸ¨âŸ¨CâŸ©âŸ©" instead of
;;   atldiamond.  Currently the 5 symbols gets placed on top of each other,
;;   resulting in a mighty mess.  This problem might be demomposed into two
;;   types: When the replaced string is bigger than the string replacing it
;;   (probably the easiest case), and the converse case.
;;
;;   Package it as a elpa/marmelade package.
;;   --A problem here is that marmelade destroys the unicode encoding. A
;;   possible fix for this is to change this code, so instead of containing the
;;   unicode characters directly, it can contain the code for each of them as an
;;   integer. This would probably be more portable/safe, but in some way less
;;   userfriendly, as one can not scan through the file to see which symbols it
;;   has, and to enter one one needs to find the code
;;
;;   Also it would be nice if it had some configuration possibilities. Eg the
;;   ability to add own abreviations through the customization interface, or
;;   toggle the display of math-curly-braces.
;;
;;   On a longer timeline, it would be nice if it could understand some basic
;;   newcommands, and automatically deduce the needed unicode (but this seems
;;   super hard).

(require 'cl-lib)
;;; Code:
(defun substitute-pattern-with-unicode-symbol (pattern symbol)
  "Add a font lock hook to replace the matched part of PATTERN with the Unicode
symbol SYMBOL.
Symbol can be the symbol directly, no lookup needed."
  (interactive)
  (font-lock-add-keywords
   nil
   `((,pattern
      (0 (progn
	   ;;Non-working section kind of able to compose multi-char strings:
	   ;; (compose-string-to-region (match-beginning 1) (match-end 1)
	   ;; 				  ,symbol
	   ;; 				  'decompose-region)
	   ;; (put-text-property (match-beginning 1) (match-end 1) 'display ,symbol)
	   (compose-region (match-beginning 1) (match-end 1)
	   		   ,symbol
	   		   'decompose-region)
	   nil))))))

;;The following code can be used to add strings, and not just single
;; characters. But not really working yet, as it does not remove the thing that
;; is below
;; (require 'cl)
;; (defun compose-string-to-region (start end string decomposingfunct)
;;   (loop for i from 0 to (- (length string) 1)
;; 	do (compose-region (+ i start) (+  start i 1) (substring string i (+ i 1) ) decomposingfunct)))


(defun substitute-patterns-with-unicode-symbol (patterns)
  "Mapping over PATTERNS, calling SUBSTITUTE-PATTERN-WITH-UNICODE for each of the patterns."
  (mapcar #'(lambda (x)
              (substitute-pattern-with-unicode-symbol (car x)
						      (cl-second x)))
          patterns))

(defun latex-escape-regex (str)
  "Gets a string, e.g. Alpha, returns the regexp matching the escaped
version of it in LaTeX code, with no chars in [a-z0-9A-Z] after it."
  (interactive "MString:")
  (concat "\\(\\\\" str "\\)[^a-z0-9A-Z]"))


(defun latex-escape-regexp-super-or-sub (str sup-or-sub backslash)
  "Gets a string, e.g. 1, a symbol 'sub or 'sup saying wether it
should be a superscript or subscript, and a boolean for
  backslash saying wether or not str should be backslashed (like
  \gamma). It returns the regexp matching the super/sub-scripted
  version of it in LaTeX code"
  ;; We can not use "(regexp-opt (list (concat "_" str) (concat "_{" str
  ;; "}")))", as it adds a "?:" after every group, which eveidently breaks the
  ;; font-locking engine in emacs. I assume the reason is like this: Normaly a
  ;; group (denoted with paranthesises) create a "backreference". This makes it
  ;; possible (I think) for the font-locking-engine no know where it actually
  ;; matched. The "?:" sais that we need to backreference. We have added one in
  ;; the innermost group, as we never need to know the location of the inner
  ;; matching, only the outermost. Adding "?:" where it can be done makes
  ;; matching more effecient, as the engine dont need to make a backreference
  (if backslash (setf str (concat "\\\\" str)))
  (cl-case sup-or-sub
    ('sub  (concat "\\(_\\(?:" str "\\|{" str "}\\)\\)"))
    ('sup  (concat "\\(\\^\\(?:" str "\\|{" str "}\\)\\)"))))

(defun latex-escape-regex-sub (str &optional backslash)
  "Gets a string, e.g. 1, returns the regexp matching the subscripted
version of it in LaTeX code."
  (interactive "MString:")
  (latex-escape-regexp-super-or-sub str 'sub backslash))

(defun latex-escape-regex-sup (str &optional backslash)
  "Gets a string, e.g. 1, returns the regexp matching the superscripted
version of it in LaTeX code."
  (interactive "MString:")
  (latex-escape-regexp-super-or-sub str 'sup backslash))

;;Goto http://www.fileformat.info/info/unicode/block/mathematical_operators/list.htm and copy the needed character
(defun latex-unicode-simplified ()
  "Adds a bunch of font-lock rules to display latex commands as
their unicode counterpart"
  (interactive)
  (substitute-patterns-with-unicode-symbol
   (list
    ;;These need to be on top, before the versions which are not subscriptet
    (list (latex-escape-regex-sub "beta" t)"áµ¦")
    (list (latex-escape-regex-sub "gamma" t)"áµ§")
    (list (latex-escape-regex-sub "rho" t)"áµ¨")
    (list (latex-escape-regex-sub "phi" t)"áµ©")
    (list (latex-escape-regex-sub "chi" t)"áµª")
    
    (list (latex-escape-regex "Alpha") "Î‘")
    (list (latex-escape-regex "Beta") "Î’")
    (list (latex-escape-regex "Gamma")"Î“")
    (list (latex-escape-regex "Delta")"Î”")
    (list (latex-escape-regex "Epsilon")"Î•")
    (list (latex-escape-regex "Zeta")"Î–")
    (list (latex-escape-regex "Eta")"Î—")
    (list (latex-escape-regex "Theta")"Î˜")
    (list (latex-escape-regex "Iota")"Î™")
    (list (latex-escape-regex "Kappa")"Îš")
    (list (latex-escape-regex "Lambda")"Î›")
    (list (latex-escape-regex "Mu")"Îœ")
    (list (latex-escape-regex "Nu")"Î")
    (list (latex-escape-regex "Xi")"Îž")
    (list (latex-escape-regex "Omicron")"ÎŸ")
    (list (latex-escape-regex "Pi")"Î ")
    (list (latex-escape-regex "Rho")"Î¡")
    (list (latex-escape-regex "Sigma")"Î£")
    (list (latex-escape-regex "Tau")"Î¤")
    (list (latex-escape-regex "Upsilon")"Î¥")
    (list (latex-escape-regex "Phi")"Î¦")
    (list (latex-escape-regex "Chi")"Î§")
    (list (latex-escape-regex "Psi")"Î¨")
    (list (latex-escape-regex "Omega")"Î©")
    (list (latex-escape-regex "alpha")"Î±")
    (list (latex-escape-regex "beta")"Î²")
    (list (latex-escape-regex "gamma")"Î³")
    (list (latex-escape-regex "delta")"Î´")
    (list (latex-escape-regex "epsilon")"Îµ")
    (list (latex-escape-regex "zeta")"Î¶")
    (list (latex-escape-regex "eta")"Î·")
    (list (latex-escape-regex "theta")"Î¸")
    (list (latex-escape-regex "iota")"Î¹")
    (list (latex-escape-regex "kappa")"Îº")
    (list (latex-escape-regex "lambda")"Î»")
    (list (latex-escape-regex "mu")"Î¼")
    (list (latex-escape-regex "nu")"Î½")
    (list (latex-escape-regex "xi")"Î¾")
    (list (latex-escape-regex "omicron")"Î¿")
    (list (latex-escape-regex "pi")"Ï€")
    (list (latex-escape-regex "rho")"Ï")
    (list (latex-escape-regex "sigma")"Ïƒ")
    (list (latex-escape-regex "tau")"Ï„")
    (list (latex-escape-regex "upsilon")"Ï…")
    (list (latex-escape-regex "phi") "Ï•")
    (list (latex-escape-regex "chi")"Ï‡")
    (list (latex-escape-regex "psi")"Ïˆ")
    (list (latex-escape-regex "omega")"Ï‰")

    ;; relations
    (list (latex-escape-regex "geq")"â‰¥")
    (list (latex-escape-regex "ge")"â‰¥")
    (list (latex-escape-regex "leq")"â‰¤")
    (list (latex-escape-regex "le")"â‰¤")
    (list (latex-escape-regex "neq")"â‰ ")

    ;; logical ops
    (list (latex-escape-regex "land")"âˆ§")
    (list (latex-escape-regex "lor")"âˆ¨")
    (list (latex-escape-regex "neg")"Â¬")
    (list (latex-escape-regex "rightarrow")"â†’")
    (list (latex-escape-regex "leftarrow")"â†")
    (list (latex-escape-regex "leftrightarrow")"â†”")
    (list (latex-escape-regex "Rightarrow")"â‡’")
    (list (latex-escape-regex "Leftarrow")"â‡")
    (list(latex-escape-regex "Leftrightarrow")"â‡”")
    (list (latex-escape-regex "forall")"âˆ€")
    (list (latex-escape-regex "exists")"âˆƒ")
    (list (latex-escape-regex "Diamond")"â‹„")
    (list (latex-escape-regex "Box")"â–¡")
    (list (latex-escape-regex "models")"âŠ§")
    (list (latex-escape-regex "bot")"âŠ¥")
    (list (latex-escape-regex "top")"âŠ¤")
    
    ;; infty before in
    (list (latex-escape-regex "infty")"âˆž")
    
    ;; set ops
    ;;Here I have chosen to have \} display as âŽ¬, to easy reading of set building opperations. Slightly uncertain
    (list "\\(\\\\}\\)" "âŽ¬")
    (list "\\(\\\\{\\)" "âŽ¨")
    
    (list (latex-escape-regex "mid")"|")
    (list (latex-escape-regex "in")"âˆŠ")
    (list (latex-escape-regex "notin")"âˆ‰")
    (list (latex-escape-regex "cup")"âˆª")
    (list (latex-escape-regex "cap")"âˆ©")
    (list (latex-escape-regex "setminus")"âˆ–")
    (list (latex-escape-regex "minus")"âˆ–")
    (list (latex-escape-regex "subseteq")"âŠ†")
    (list (latex-escape-regex "subset")"âŠ‚")
    (list (latex-escape-regex "emptyset")"âˆ…")
    (list (latex-escape-regex "ni")"âˆ‹")
    
    ;; generic math
    (list (latex-escape-regex "dots")"â€¦")
    
    ;;Superscript
    (list (latex-escape-regex-sup "0")"â°")
    (list (latex-escape-regex-sup "1")"Â¹")
    (list (latex-escape-regex-sup "2")"Â²")
    (list (latex-escape-regex-sup "3")"Â³")
    (list (latex-escape-regex-sup "4")"â´")
    (list (latex-escape-regex-sup "5")"âµ")
    (list (latex-escape-regex-sup "6")"â¶")
    (list (latex-escape-regex-sup "7")"â·")
    (list (latex-escape-regex-sup "8")"â¸")
    (list (latex-escape-regex-sup "9")"â¹")
    (list (latex-escape-regex-sup "-")"â»")
    (list (latex-escape-regex-sup "=")"â¼")
    (list (latex-escape-regex-sup "\\+")"âº")
    (list (latex-escape-regex-sup "a")"áµƒ")
    (list (latex-escape-regex-sup "b")"áµ‡")
    (list (latex-escape-regex-sup "c")"á¶œ")
    (list (latex-escape-regex-sup "d")"áµˆ")
    (list (latex-escape-regex-sup "e")"áµ‰")
    (list (latex-escape-regex-sup "f")"á¶ ")
    (list (latex-escape-regex-sup "g")"áµ")
    (list (latex-escape-regex-sup "h")"Ê°")
    (list (latex-escape-regex-sup "i")"â±")
    (list (latex-escape-regex-sup "j")"Ê²")
    (list (latex-escape-regex-sup "k")"áµ")
    (list (latex-escape-regex-sup "l")"Ë¡")
    (list (latex-escape-regex-sup "m")"áµ")
    (list (latex-escape-regex-sup "n")"â¿")
    (list (latex-escape-regex-sup "o")"áµ’")
    (list (latex-escape-regex-sup "p")"áµ–")
    (list (latex-escape-regex-sup "r")"Ê³")
    (list (latex-escape-regex-sup "s")"Ë¢")
    (list (latex-escape-regex-sup "t")"áµ—")
    (list (latex-escape-regex-sup "u")"áµ˜")
    (list (latex-escape-regex-sup "v")"áµ›")
    (list (latex-escape-regex-sup "w")"Ê·")
    (list (latex-escape-regex-sup "x")"Ë£")
    (list (latex-escape-regex-sup "y")"Ê¸")
    (list (latex-escape-regex-sup "z")"á¶»")
    
    (list (latex-escape-regex-sup "A")"á´¬")
    (list (latex-escape-regex-sup "B")"á´®")
    (list (latex-escape-regex-sup "D") "á´°")
    (list (latex-escape-regex-sup "E") "á´±")
    (list (latex-escape-regex-sup "G") "á´³")
    (list (latex-escape-regex-sup "H") "á´´")
    (list (latex-escape-regex-sup "I") "á´µ")
    (list (latex-escape-regex-sup "J") "á´¶")
    (list (latex-escape-regex-sup "K") "á´·")
    (list (latex-escape-regex-sup "L") "á´¸")
    (list (latex-escape-regex-sup "M") "á´¹")
    (list (latex-escape-regex-sup "N") "á´º")
    (list (latex-escape-regex-sup "O") "á´¼")
    (list (latex-escape-regex-sup "P") "á´¾")
    (list (latex-escape-regex-sup "R") "á´¿")
    (list (latex-escape-regex-sup "T") "áµ€")
    (list (latex-escape-regex-sup "U") "áµ")
    (list (latex-escape-regex-sup "V") "â±½")
    (list (latex-escape-regex-sup "W") "áµ‚")
    
    
    
    ;;Subscripts, unfortunately we lack important part of the subscriptet alphabet, most notably j and m
    (list (latex-escape-regex-sub "1")"â‚")
    (list (latex-escape-regex-sub "2")"â‚‚")
    (list (latex-escape-regex-sub "3")"â‚ƒ")
    (list (latex-escape-regex-sub "4")"â‚„")
    (list (latex-escape-regex-sub "5")"â‚…")
    (list (latex-escape-regex-sub "6")"â‚†")
    (list (latex-escape-regex-sub "7")"â‚‡")
    (list (latex-escape-regex-sub "8")"â‚ˆ")
    (list (latex-escape-regex-sub "9")"â‚‰")
    (list (latex-escape-regex-sub "x")"â‚“")
    (list (latex-escape-regex-sub "i")"áµ¢")
    (list (latex-escape-regex-sub "\\+")"â‚Š")
    (list (latex-escape-regex-sub "-")"â‚‹")
    (list (latex-escape-regex-sub "=")"â‚Œ")
    (list (latex-escape-regex-sub "a")"â‚")
    (list (latex-escape-regex-sub "e")"â‚‘")
    (list (latex-escape-regex-sub "o")"â‚’")
    (list (latex-escape-regex-sub "i")"áµ¢")
    (list (latex-escape-regex-sub "r")"áµ£")
    (list (latex-escape-regex-sub "u")"áµ¤")
    (list (latex-escape-regex-sub "v")"áµ¥")
    (list (latex-escape-regex-sub "x")"â‚“")
    
    
    ;; (list (latex-escape-regex "\\.\\.\\.") 'dots)
    (list (latex-escape-regex "langle")"âŸ¨")
    (list (latex-escape-regex "rangle")"âŸ©")
    (list (latex-escape-regex "mapsto")"â†¦")
    (list (latex-escape-regex "to")"â†’")
    (list (latex-escape-regex "times")"Ã—")
    (list (latex-escape-regex "equiv")"â‰¡")
    (list (latex-escape-regex "star")"â˜…")
    (list (latex-escape-regex "nabla")"âˆ‡")
    (list (latex-escape-regex "qed")"â–¡")
    (list (latex-escape-regex "lightning")"Ïž")
    
    ;;New: some of my own abreviations:
    
    ;;Go to
    ;; http://www.fileformat.info/info/unicode/block/letterlike_symbols/list.htm
    ;; to find some leters, or
    ;; http://www.fileformat.info/info/unicode/block/mathematical_alphanumeric_symbols/list.htm
    ;;  My mathcal like ones are from "MATHEMATICAL BOLD SCRIPT CAPITAL", an alternative block is Letterlike symbols:
    ;;http://en.wikipedia.org/wiki/Letterlike_Symbols
    
    (list (latex-escape-regex "impl") "â†’")
    (list (latex-escape-regex "iff") "â†”")
    (list (latex-escape-regex "M") "ð“œ")
    (list (latex-escape-regex "Mo") "ð“œ")
    (list (latex-escape-regex "Fr") "ð“•")
    (list (latex-escape-regex "gt") ">")
    (list (latex-escape-regex "lt") "<")
    (list (latex-escape-regex "from") ":")
    (list (latex-escape-regex "Pow") "ð’«")
					;"â„’"
    (list (latex-escape-regex "La") "ð“›")
    
    ;;Does not work, as it pushes them all into one character
    ;; (list (latex-escape-regex "atldiamond")"âŸ¨âŸ¨CâŸ©âŸ©")
    ;PÃ¥ls single letter abrevs:
    (list (latex-escape-regex "L") "ð“›")
    (list (latex-escape-regex "N") "ð“")
    (list (latex-escape-regex "E") "ð“”")
    (list (latex-escape-regex "C") "ð“’")
    (list (latex-escape-regex "D") "ð““")
    
    (list (latex-escape-regex "G") "ð“–")
    (list (latex-escape-regex "X") "ð“§")
    (list (latex-escape-regex "U") "ð“¤")
    (list (latex-escape-regex "Q") "ð“ ")
    
    
    ;;The following are not really working perfect
    ;; (list (latex-escape-regex "overline{R}") "RÌ„")
    ;; (list (latex-escape-regex "overline{X}") "XÌ„")
    ;; (list (latex-escape-regex "overline{G}") "GÌ„")
    
    
    
    ;;The following is some ugly fucks, as it has to match brackets! This makes
    ;;$\pair[A,B]$ into $âŸ¨A,BâŸ©$, but can not handle nesting of the pair command,
    ;;then it does not convert the last "]" as it should. One can make one
    ;;regexp matching level of stacking, but my mind blows after even 1
    ;;level. Regular expressions can not do general, arbitrary depth,
    ;;paranthesis matching, but maybe emacs's "regexps" are expressiable enough for
    ;;this?
    (list  "\\(\\\\pair\\[\\)" "âŸ¨")
    (list  "\\(?:\\\\pair\\[[^\]]*\\(]\\)\\)" "âŸ©")
    
    (list (latex-escape-regex "dagger") "â€ " )
    (list (latex-escape-regex "vDash") "âŠ¨" )
    (list (latex-escape-regex "bigvee") "â‹" )
    (list (latex-escape-regex "bigwedge") "â‹€" )
    (list (latex-escape-regex "biguplus") "â¨„" )
    (list (latex-escape-regex "bigcap") "â‹‚" )
    (list (latex-escape-regex "bigcup") "â‹ƒ" )
    (list (latex-escape-regex "ss") "ÃŸ")
    (list (latex-escape-regex "ae") "Ã¦")
    (list (latex-escape-regex "oe") "Å“")
    (list (latex-escape-regex "o") "Ã¸")
    (list (latex-escape-regex "AE") "Ã†")
    (list (latex-escape-regex "OE") "Å’")
    (list (latex-escape-regex "O") "Ã˜")
    (list (latex-escape-regex "aa") "Ã¥")
    (list (latex-escape-regex "AA") "Ã…")
    (list (latex-escape-regex "dag") "â€ ")
    (list (latex-escape-regex "ddag") "â€¡")
    (list (latex-escape-regex "S") "Â§")
    (list (latex-escape-regex "l") "Å‚")
    (list (latex-escape-regex "L") "Å")
    (list (latex-escape-regex "copyright") "Â©")
    (list (latex-escape-regex "epsilon") "Ïµ")
    (list (latex-escape-regex "varphi") "Ï†")
    (list (latex-escape-regex "vartheta") "Ï‘")
    (list (latex-escape-regex "varpi") "Ï–")
    (list (latex-escape-regex "varrho") "Ï±")
    (list (latex-escape-regex "varsigma") "Ï‚")
    (list (latex-escape-regex "aleph") "â„µ")
    (list (latex-escape-regex "hbar") "â„")
    (list (latex-escape-regex "ell") "â„“")
    (list (latex-escape-regex "wp") "â„˜")
    (list (latex-escape-regex "Re") "â„œ")
    (list (latex-escape-regex "Im") "â„‘")
    (list (latex-escape-regex "partial") "âˆ‚")
    (list (latex-escape-regex "surd") "âˆš")
    (list (latex-escape-regex "angle") "âˆ ")
    (list (latex-escape-regex "triangle") "â–³")
    (list (latex-escape-regex "flat") "â™­")
    (list (latex-escape-regex "natural") "â™®")
    (list (latex-escape-regex "sharp") "â™¯")
    (list (latex-escape-regex "clubsuit") "â™£")
    (list (latex-escape-regex "diamondsuit") "â™¢")
    (list (latex-escape-regex "heartsuit") "â™¡")
    (list (latex-escape-regex "spadesuit") "â™ ")
    (list (latex-escape-regex "coprod") "âˆ")
    (list (latex-escape-regex "int") "âˆ«")
    (list (latex-escape-regex "prod") "âˆ")
    (list (latex-escape-regex "sum") "âˆ‘")
    (list (latex-escape-regex "bigotimes") "â¨‚")
    (list (latex-escape-regex "bigoplus") "â¨")
    (list (latex-escape-regex "bigodot") "â¨€")
    (list (latex-escape-regex "oint") "âˆ®")
    (list (latex-escape-regex "bigsqcup") "â¨†")
    (list (latex-escape-regex "triangleleft") "â—")
    (list (latex-escape-regex "triangleright") "â–·")
    (list (latex-escape-regex "bigtriangleup") "â–³")
    (list (latex-escape-regex "bigtriangledown") "â–½")
    (list (latex-escape-regex "sqcap") "âŠ“")
    (list (latex-escape-regex "sqcup") "âŠ”")
    (list (latex-escape-regex "uplus") "âŠŽ")
    (list (latex-escape-regex "amalg") "â¨¿")
    (list (latex-escape-regex "bullet") "âˆ™")
    (list (latex-escape-regex "wr") "â‰€")
    (list (latex-escape-regex "div") "Ã·")
    (list (latex-escape-regex "odot") "âŠ™")
    (list (latex-escape-regex "oslash") "âŠ˜")
    (list (latex-escape-regex "otimes") "âŠ—")
    (list (latex-escape-regex "ominus") "âŠ–")
    (list (latex-escape-regex "oplus") "âŠ•")
    (list (latex-escape-regex "mp") "âˆ“")
    (list (latex-escape-regex "pm") "Â±")
    (list (latex-escape-regex "circ") "âˆ˜")
    (list (latex-escape-regex "circ") "â—‹")
    (list (latex-escape-regex "bigcirc") "â—¯")
    (list (latex-escape-regex "cdot") "â‹…")
    (list (latex-escape-regex "ast") "âˆ—")
    (list (latex-escape-regex "star") "â‹†")
    (list (latex-escape-regex "propto") "âˆ")
    (list (latex-escape-regex "sqsubseteq") "âŠ‘")
    (list (latex-escape-regex "sqsupseteq") "âŠ’")
    (list (latex-escape-regex "parallel") "âˆ¥")
    (list (latex-escape-regex "dashv") "âŠ£")
    (list (latex-escape-regex "vdash") "âŠ¢")
    (list (latex-escape-regex "nearrow") "â†—")
    (list (latex-escape-regex "searrow") "â†˜")
    (list (latex-escape-regex "nwarrow") "â†–")
    (list (latex-escape-regex "swarrow") "â†™")
    (list (latex-escape-regex "succ") "â‰»")
    (list (latex-escape-regex "prec") "â‰º")
    (list (latex-escape-regex "approx") "â‰ˆ")
    (list (latex-escape-regex "succeq") "â‰½")
    (list (latex-escape-regex "preceq") "â‰¼")
    (list (latex-escape-regex "supset") "âŠƒ")
    (list (latex-escape-regex "supseteq") "âŠ‡")
    (list (latex-escape-regex "in") "âˆˆ")
    (list (latex-escape-regex "gg") "â‰«")
    (list (latex-escape-regex "ll") "â‰ª")
    (list (latex-escape-regex "sim") "âˆ¼")
    (list (latex-escape-regex "simeq") "â‰ƒ")
    (list (latex-escape-regex "asymp") "â‰")
    (list (latex-escape-regex "smile") "âŒ£")
    (list (latex-escape-regex "frown") "âŒ¢")
    (list (latex-escape-regex "leftharpoonup") "â†¼")
    (list (latex-escape-regex "leftharpoondown") "â†½")
    (list (latex-escape-regex "rightharpoonup") "â‡€")
    (list (latex-escape-regex "rightharpoondown") "â‡")
    (list (latex-escape-regex "hookrightarrow") "â†ª")
    (list (latex-escape-regex "hookleftarrow") "â†©")
    (list (latex-escape-regex "bowtie") "â‹ˆ")
    (list (latex-escape-regex "models") "âŠ§")
    (list (latex-escape-regex "Longrightarrow") "âŸ¹")
    (list (latex-escape-regex "longrightarrow") "âŸ¶")
    (list (latex-escape-regex "longleftarrow") "âŸµ")
    (list (latex-escape-regex "Longleftarrow") "âŸ¸")
    (list (latex-escape-regex "longmapsto") "âŸ¼")
    (list (latex-escape-regex "longleftrightarrow") "âŸ·")
    (list (latex-escape-regex "Longleftrightarrow") "âŸº")
    (list (latex-escape-regex "cdots") "â‹¯")
    (list (latex-escape-regex "vdots") "â‹®")
    (list (latex-escape-regex "ddots") "â‹±")
    (list (latex-escape-regex "Vert") "âˆ¥")
    (list (latex-escape-regex "uparrow") "â†‘")
    (list (latex-escape-regex "downarrow") "â†“")
    (list (latex-escape-regex "updownarrow") "â†•")
    (list (latex-escape-regex "Uparrow") "â‡‘")
    (list (latex-escape-regex "Downarrow") "â‡“")
    (list (latex-escape-regex "Updownarrow") "â‡•")
    (list (latex-escape-regex "rceil") "âŒ‰")
    (list (latex-escape-regex "lceil") "âŒˆ")
    (list (latex-escape-regex "rfloor") "âŒ‹")
    (list (latex-escape-regex "lfloor") "âŒŠ")
    (list (latex-escape-regex "cong") "â‰…")
    (list (latex-escape-regex "rightleftharpoons") "â‡Œ")
    (list (latex-escape-regex "doteq") "â‰")
    )))

;;AUCTeX
(add-hook 'LaTeX-mode-hook 'latex-unicode-simplified)

;;latex-mode
(add-hook 'latex-mode-hook 'latex-unicode-simplified)
(provide 'latex-pretty-symbols)

;;; latex-pretty-symbols.el ends here
