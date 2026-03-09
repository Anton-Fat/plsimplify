(defun C:CONVSPLINETOPLINE ( / i ss)
  ;;; Convert selected splines to polylines

  (setvar "PLINECONVERTMODE" 0) ; linear segments only

  ;; select splines manually
  (setq ss (ssget '((0 . "SPLINE"))))
  (if (null ss)
    (progn
      (princ "\nNo splines selected.")
      (exit)
    )
  )

  (setq i -1)
  (setvar "CMDECHO" 0)

  ;; loop through selected splines
  (repeat (sslength ss)
    (setq i (1+ i))
    (command "_SPLINEEDIT" (ssname ss i) "_P" "1")
  )

  ;; update drawing
  (command "_REGENALL")

  (princ "\nSelected splines converted to polylines.")
  (princ)
)
;; ===================================================================
