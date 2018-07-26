;; Matrix Determinant (Upper Triangular Form)  -  ElpanovEvgeniy
;; Args: m - nxn matrix

(defun detm (m / d)
  (cond
    ((null m) 1)
    ((and (zerop (caar m))
	  (setq	d (car (vl-member-if-not
			 (function (lambda (a) (zerop (car a))))
			 (cdr m)
		       )
		  )
	  )
     )
     (detm (cons (mapcar '+ (car m) d) (cdr m)))
    )
    ((zerop (caar m)) 0)
    ((*	(caar m)
	(detm
	  (mapcar
	    (function
	      (lambda (a / d)
		(setq d (/ (car a) (float (caar m))))
		(mapcar
		  (function
		    (lambda (b c) (- b (* c d)))
		  )
		  (cdr a)
		  (cdar m)
		)
	      )
	    )
	    (cdr m)
	  )
	)
     )
    )
  )
)

;; Matrix Determinant (Laplace Formula)  -  Lee Mac
;; Args: m - nxn matrix

(defun detm (m / i j)
  (setq	i -1
	j 0
  )
  (cond
    ((null (cdr m)) (caar m))
    ((null (cddr m))
     (- (* (caar m) (cadadr m)) (* (cadar m) (caadr m)))
    )
    ((apply '+
	    (mapcar
	      '(lambda (c)
		 (setq j (1+ j))
		 (* c
		    (setq i (- i))
		    (detm
		      (mapcar
			'(lambda (x / k)
			   (setq k 0)
			   (vl-remove-if
			     '(lambda (y) (= j (setq k (1+ k))))
			     x
			   )
			 )
			(cdr m)
		      )
		    )
		 )
	       )
	      (car m)
	    )
     )
    )
  )
)

;; Matrix Inverse  -  gile & Lee Mac
;; Uses Gauss-Jordan Elimination to return the inverse of a non-singular nxn matrix.
;; Args: m - nxn matrix

(defun invm (m / c f p r)

  (defun f (p m)
    (mapcar '(lambda (x)
	       (mapcar '(lambda (a b) (- a (* (car x) b))) (cdr x) p)
	     )
	    m
    )
  )
  (setq m (mapcar 'append m (imat (length m))))
  (while m
    (setq c (mapcar '(lambda (x) (abs (car x))) m))
    (repeat (vl-position (apply 'max c) c)
      (setq m (append (cdr m) (list (car m))))
    )
    (if	(equal 0.0 (caar m) 1e-14)
      (setq m nil
	    r nil
      )
      (setq p (mapcar '(lambda (x) (/ (float x) (caar m))) (cdar m))
	    m (f p (cdr m))
	    r (cons p (f p r))
      )
    )
  )
  (reverse r)
)

;; Identity Matrix  -  Lee Mac
;; Args: n - matrix dimension

(defun imat (n / i j l m)
  (repeat (setq i n)
    (repeat (setq j n)
      (setq l (cons (if	(= i j)
		      1.0
		      0.0
		    )
		    l
	      )
	    j (1- j)
      )
    )
    (setq m (cons l m)
	  l nil
	  i (1- i)
    )
  )
  m
)

;; Matrix Transpose  -  Doug Wilson
;; Args: m - nxn matrix

(defun trp (m)
  (apply 'mapcar (cons 'list m))
)

;; Matrix Trace  -  Lee Mac
;; Args: m - nxn matrix

(defun trc (m)
  (if m
    (+ (caar m) (trc (mapcar 'cdr (cdr m))))
    0
  )
)

;; Matrix x Matrix  -  Vladimir Nesterovsky
;; Args: m,n - nxn matrices

(defun mxm (m n)
  ((lambda (a) (mapcar '(lambda (r) (mxv a r)) m)) (trp n))
)

;; Matrix + Matrix  -  Lee Mac
;; Args: m,n - nxn matrices

(defun m+m (m n)
  (mapcar '(lambda (r s) (mapcar '+ r s)) m n)
)

;; Matrix x Scalar  -  Lee Mac
;; Args: m - nxn matrix, n - real scalar

(defun mxs (m s)
  (mapcar '(lambda (r) (mapcar '(lambda (n) (* n s)) r)) m)
)

;; Matrix x Vector  -  Vladimir Nesterovsky
;; Args: m - nxn matrix, v - vector in R^n

(defun mxv (m v)
  (mapcar '(lambda (r) (apply '+ (mapcar '* r v))) m)
)

;; Vector x Scalar  -  Lee Mac
;; Args: v - vector in R^n, s - real scalar

(defun vxs (v s)
  (mapcar '(lambda (n) (* n s)) v)
)

;; Vector Dot Product  -  Lee Mac
;; Args: u,v - vectors in R^n

(defun vxv (u v)
  (apply '+ (mapcar '* u v))
)

;; Vector Cross Product  -  Lee Mac
;; Args: u,v - vectors in R^3

(defun v^v (u v)
  (list
    (- (* (cadr u) (caddr v)) (* (cadr v) (caddr u)))
    (- (* (car v) (caddr u)) (* (car u) (caddr v)))
    (- (* (car u) (cadr v)) (* (car v) (cadr u)))
  )
)

;; Unit Vector  -  Lee Mac
;; Args: v - vector in R^2 or R^3

(defun vx1 (v)
  ((lambda (n)
     (if (equal 0.0 n 1e-10)
       nil
       (mapcar '/ v (list n n n))
     )
   )
    (distance '(0.0 0.0 0.0) v)
  )
)

;; Vector Norm (R^n)  -  Lee Mac
;; Args: v - vector in R^n

(defun |v| (v)
  (sqrt (apply '+ (mapcar '* v v)))
)

;; Unit Vector (R^n)  -  Lee Mac
;; Args: v - vector in R^n

(defun unit (v)
  ((lambda (n)
     (if (equal 0.0 n 1e-10)
       nil
       (vxs v (/ 1.0 n))
     )
   )
    (|v| v)
  )
)



(defun LM:SSBoundingBox	(ss / i l1 l2 ll ur)
  ;; © Lee Mac 2011
  (repeat (setq i (sslength ss))
    (vla-getboundingbox
      (vlax-ename->vla-object (ssname ss (setq i (1- i))))
      'll
      'ur
    )
    (setq l1 (cons (vlax-safearray->list ll) l1)
	  l2 (cons (vlax-safearray->list ur) l2)
    )
  )
  (mapcar '(lambda (a b) (apply 'mapcar (cons 'a b)))
	  (list min max)
	  (list l1 l2)
  )
)

(defun v+v (v1 v2 /)
  (mapcar '(lambda (r s) (+ r s)) v1 v2)
)
(defun c:exall (/ bSet)
  (setvar "qaflags" 1)
  (while (setq bSet (ssget "_X" '((0 . "INSERT"))))
    (command "_.explode" bSet "")
  )					; end while
  (repeat 3 (command "-purge" "all" "" "n"))
  (setvar "qaflags" 0)
  (princ)
)					; end of c:exall

(defun rep (n o str)
  (vl-list->string
    (mapcar '(lambda (x)
	       (if (= x (ascii o))
		 (ascii n)
		 x
	       )
	     )
	    (vl-string->list str)
    )
  )
)

					; Tests my ability to subdivide a block into squares
(defun c:applyBoxToDrawing (drawFunc /)

  (defun defaultDrawFunc (subBb i /)
    (setq
      pt1 (car subBb)
      pt2 (cadr subBb)
    )

					; Draw a box over the coordinates
    (command "._pline"
	     pt1
	     (list (car pt1) (cadr pt2))
	     (list (car pt1) (cadr pt2))
	     pt2
	     pt2
	     (list (car pt2) (cadr pt1))
	     (list (car pt2) (cadr pt1))
	     pt1
	     ""
    )
  )
  (if (not drawFunc)
    (setq drawFunc defaultDrawFunc)
  )

  (setq
    subSize	'(125 125)
					; Get the bounding box of the whole drawing
    bb		(LM:SSBoundingBox (ssget "A"))
					; Get the (width height)
    absoluteVec	(v+v (cadr bb) (vxs (car bb) -1))
					; Get how much I need to add to the x and y axis to make it divide perfectly into my subsize
					; xRemainder (rem (car absoluteVec) (car subSize))
					; yRemainer (rem (cadr absoluteVec) (cadr subSize))
					; Apply the vector to my bb so that I know it subdivides perfectly noweplox
					; perfectBb (m+m bb (list '(0 0) (list xRemainder yRemainder)))
					; How many rows we're iterating over
    rows	(fix (+ (/ (car absoluteVec) (car subSize)) 1))
					; How many columns we're iterating over per row
    columns	(fix (+ (/ (cadr absoluteVec) (cadr subSize)) 1))
					; What to increment the x value by in the loop
    xIncrement	(* rows (car subSize))
					; What to increment the y value by in the loop
    yIncrement	(* columns (car subSize))
					; Set our starting x to the last point's first number
    x		(car (car bb))
					; Set our starting y to the last point's second number
    y		(cadr (car bb))
    i		0
  )

					; Hold the snap
  (setq old_snap (getvar "snapmode"))
  (setq old_osnap (getvar "osmode"))
  (setvar "snapmode" 0)
  (setvar "osmode" 0)


  (while (< x (car (cadr bb)))
					; Things...
    (while (< y (cadr (cadr bb)))
					; Things...
      (drawFunc (list (list x y) (v+v subSize (list x y))) i)
					; And the increment
      (setq
	y (+ y (cadr subSize))
	i (+ i 1)
      )
    )
					; And the increment and reset the y
    (setq
      x	(+ x (car subSize))
      y	(cadr (car bb))
      i	(+ i 1)
    )
  )

					; Restore the snap
  (setvar "snapmode" old_snap)
  (setvar "osmode" old_osnap)
)

(defun trimSquare (subBb i /)
  (command "undo" "begin")
  (setq
    pt1	(car subBb)
    pt2	(cadr subBb)
  )

  (command "._pline"
	   pt1
	   (list (car pt1) (cadr pt2))
	   (list (car pt1) (cadr pt2))
	   pt2
	   pt2
	   (list (car pt2) (cadr pt1))
	   (list (car pt2) (cadr pt1))
	   pt1
	   ""
  )

  (setq boundingBoxBox (ssget "L"))
  (setq ss1 (ssget "A"))

  (command "._trim" boundingBoxBox "" ss1 "")
  (command "._erase" boundingBoxBox "")

					; New name, insertion point, entities, enter
  (setq
    filename (strcat (vl-filename-base (getvar "dwgname")))
    blkName  (strcat (rep "_" " " filename) "_part_" (itoa i))
  )

					; Will this bring the border box with it or not?
  (if (ssget "W" pt1 pt2)
    (progn
      (command-s "-purge" "B" blkName "N" "")
      (command-s "_-block" blkName pt1 (ssget "W" pt1 pt2) "")
      (setq writeDir ".")
      (setenv "DefaultFormatForSave" "1")
      (command-s ".wblock"
	       (strcat WriteDir blkName ".dxf")
	       16
	       blkName
	       ""
      )
    )
  )
  (command "undo" "end")
  (command "._UNDO" "1")
)

					; Generates a slightly smaller bounding box than the one given
					; Good for 
(defun c:testtrim (/)

  (c:applyBoxToDrawing trimSquare)
)

