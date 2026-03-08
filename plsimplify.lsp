(defun c:PLSIMPLIFY ( / ent obj coords pts angLim radLim i
                        p0 p1 p2 v1 v2 dot len1 len2 ang deviation
                        badIdx ans idx cosang)

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

  ;; select polyline
  (setq ent (car (entsel "\nSelect LWPolyline: ")))

  (if (null ent)
    (progn
      (princ "\nNothing selected.")
      (exit)
    )
  )

  (setq obj (vlax-ename->vla-object ent))

  ;; check object type
  (if (/= (vla-get-objectname obj) "AcDbPolyline")
    (progn
      (princ "\nThe selected object is not an LWPolyline.")
      (exit)
    )
  )

  ;; angle input
  (setq angLim (getreal "\nEnter minimum angle (degrees): "))
  (if (null angLim) (exit))
  (setq radLim (* pi (/ angLim 180.0)))

  ;; get coordinates
  (setq coords (vlax-get obj 'Coordinates))

  ;; convert coordinates to point list
  (setq pts '())
  (setq i 0)
  (while (< i (length coords))
    (setq pts (cons (list (nth i coords) (nth (+ i 1) coords)) pts))
    (setq i (+ i 2))
  )
  (setq pts (reverse pts))

;; ===================================================================
  ;; search for vertices with small angles
  (setq badIdx '())
  (setq i 1)

  (while (< i (- (length pts) 1))

    (setq p0 (nth (- i 1) pts))
    (setq p1 (nth i pts))
    (setq p2 (nth (+ i 1) pts))

    (setq v1 (vsub p0 p1))
    (setq v2 (vsub p2 p1))

    (setq len1 (vlen v1))
    (setq len2 (vlen v2))

    (if (and (> len1 0.0) (> len2 0.0))
      (progn
        (setq dot (vdot v1 v2))
        (setq cosang (/ dot (* len1 len2)))
        ;; clamp value to avoid acos numerical errors
        (setq cosang (min 1.0 (max -1.0 cosang)))

        (setq ang (acos cosang))
        (setq deviation (abs (- pi ang)))

        (if (< deviation radLim)
          (setq badIdx (cons i badIdx))
        )
      )
    )

    (setq i (+ i 1))
  )

  (princ (strcat "\nVertices found: " (itoa (length badIdx))))

  ;; delete vertices if confirmed
  (if (> (length badIdx) 0)
    (progn
      (setq ans (getstring "\nDelete these vertices? (Y/N): "))
      (if (= (strcase ans) "Y")

        ;; rebuild coordinate list without bad vertices
        (progn
          (setq badIdx (vl-sort badIdx '<))
          ;; create new points list
          (setq newPts '())
          (setq i 0)
          (foreach p pts
            (if (not (member i badIdx))
              (setq newPts (cons p newPts))
            )
            (setq i (1+ i))
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

          (princ "\nVertices deleted.")

        )
      )
    )
  )

  (princ)
)
;; ===================================================================
