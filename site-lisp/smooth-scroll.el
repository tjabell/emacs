;;; -------------------------------------------------------------- Smooth-Scroll
(defun smooth-scroll (increment)
 ;; scroll smoothly by intercepting the mouse wheel and 
 ;; turning its signal into a signal which
 ;; moves the window one line at a time, and waits for 
 ;; a period of time between each move
  (scroll-up increment) (sit-for 0.05)
  (scroll-up increment) (sit-for 0.02)
  (scroll-up increment) (sit-for 0.02)
  (scroll-up increment) (sit-for 0.05)
  (scroll-up increment) (sit-for 0.06)
  (scroll-up increment))

