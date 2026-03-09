(defun c:PLSIMPLIFY ( / ss i ent obj coords pts angLim radLim
                        p0 p1 p2 v1 v2 dot len1 len2 ang deviation
                        badIdx ans idx cosang arr newPts newCoords)

  (vl-load-com)

;; ===================================================================
(defun acos (x)
  (cond
    ((>= x 1.0) 0.0)
    ((<= x -1.0) pi)
    (T (atan (sqrt (abs (- 1.0 (* x x)))) x))
  )
)

;; vector length
(defun vlen (v)
  (sqrt (+ (* (car v) (car v)) (* (cadr v) (cadr v))))
)

;; dot product
(defun vdot (a b)
  (+ (* (car a) (car b)) (* (cadr a) (cadr b)))
)

;; vector subtraction
(defun vsub (a b)
  (mapcar '- a b)
)
;; ===================================================================

;; select polylines
(setq ss (ssget '((0 . "LWPOLYLINE"))))

(if (null ss)
  (progn
    (princ "\nNothing selected.")
    (exit)
  )
)

;; angle input (default 1)
(setq angLim (getreal "\nEnter minimum angle (degrees) <1>: "))
(if (null angLim) (setq angLim 1.0))

(setq radLim (* pi (/ angLim 180.0)))

;; delete confirmation (default Y)
(setq ans (getstring "\nDelete these vertices? [Y/N] <Y>: "))
(if (= ans "") (setq ans "Y"))

(setq i 0)

;; ===================================================================
;; loop through selection
(repeat (sslength ss)

  (setq ent (ssname ss i))
  (setq obj (vlax-ename->vla-object ent))

  (setq coords (vlax-get obj 'Coordinates))

  ;; convert coordinates to points
  (setq pts '())
  (setq idx 0)

  (while (< idx (length coords))
    (setq pts (cons (list (nth idx coords) (nth (+ idx 1) coords)) pts))
    (setq idx (+ idx 2))
  )

  (setq pts (reverse pts))

  ;; search vertices
  (setq badIdx '())
  (setq idx 1)

  (while (< idx (- (length pts) 1))

    (setq p0 (nth (- idx 1) pts))
    (setq p1 (nth idx pts))
    (setq p2 (nth (+ idx 1) pts))

    (setq v1 (vsub p0 p1))
    (setq v2 (vsub p2 p1))

    (setq len1 (vlen v1))
    (setq len2 (vlen v2))

    (if (and (> len1 0.0) (> len2 0.0))
      (progn
        (setq dot (vdot v1 v2))
        (setq cosang (/ dot (* len1 len2)))
        (setq cosang (min 1.0 (max -1.0 cosang)))

        (setq ang (acos cosang))
        (setq deviation (abs (- pi ang)))

        (if (< deviation radLim)
          (setq badIdx (cons idx badIdx))
        )
      )
    )

    (setq idx (1+ idx))
  )

  ;; delete vertices
  (if (and (> (length badIdx) 0) (= (strcase ans) "Y"))
    (progn

      (setq badIdx (vl-sort badIdx '<))

      (setq newPts '())
      (setq idx 0)

      (foreach p pts
        (if (not (member idx badIdx))
          (setq newPts (cons p newPts))
        )
        (setq idx (1+ idx))
      )

      (setq newPts (reverse newPts))

      ;; convert to coordinate array
      (setq newCoords '())
      (foreach p newPts
        (setq newCoords (append newCoords p))
      )

      ;; create SafeArray and write back
      (setq arr (vlax-make-safearray vlax-vbDouble (cons 0 (- (length newCoords) 1))))
      (vlax-safearray-fill arr newCoords)

      (vla-put-Coordinates obj arr)
    )
  )

  (setq i (1+ i))
)

(princ "\nFinished.")
(princ)

)
;; ===============================================================
