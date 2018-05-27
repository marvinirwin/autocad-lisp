(defun c:BB (/ sel minpt maxpt old_snap old_osnap)

  (vl-load-com)
  (setq sel (vlax-ename->vla-object (car (entsel))))
  (setq old_snap (getvar "snapmode"))
  (setq old_osnap (getvar "osmode"))
  (setvar "snapmode" 0)
  (setvar "osmode" 0)

  (vla-getboundingbox sel 'minpt 'maxpt)
  (command "line"
	   (vlax-safearray->list minpt)
	   (vlax-safearray->list maxpt)
	   ""
  )
  (command "line"
	   (list (car (vlax-safearray->list minpt))
		 (cadr (vlax-safearray->list maxpt))
		 0
	   )
	   (list (car (vlax-safearray->list maxpt))
		 (cadr (vlax-safearray->list minpt))
		 0
	   )
	   ""
  )

  (setvar "snapmode" old_snap)
  (setvar "osmode" old_osnap)
  (princ)
)

(defun blockbb (block / l1 l2 ll ur)
  (vlax-for obj
	    (vla-item (vla-get-blocks
			(vla-get-activedocument (vlax-get-acad-object))
		      )
		      block
	    )
    (if	(vlax-method-applicable-p obj 'getboundingbox)
      (if
	(not
	  (vl-catch-all-error-p
	    (vl-catch-all-apply 'vla-getboundingbox (list obj 'll 'ur))
	  )
	)
	 (setq l1 (cons (vlax-safearray->list ll) l1)
	       l2 (cons (vlax-safearray->list ur) l2)
	 )
      )
    )
  )
  (if l1
    (list
      (apply 'mapcar (cons 'min l1))
      (apply 'mapcar (cons 'max l2))
    )
  )
)


(defun c:writeall (/ AllProfsList WriteDir i BlkName)

  (setq blockRefs (blocknames))


  (setq AllProfsList (blocknames))

  (setq
    WriteDir "C:\\Users\\Frodo\\wblock\\"
    ListLen  (length blockRefs)
    i	     0
  )

  (setenv "DefaultFormatForSave" "1")
  (while (< i ListLen)
    (progn
      (setq BlkName (strcat (blockname (nth i blockRefs))))
      (print (strcat BlkName "\n"))

      (command ".wblock"
	       (strcat WriteDir BlkName ".dxf")
	       16
	       BlkName
	       ""
      )
      (setq i (+ i 1))
    )
  )
  (princ)
)

(defun blockname (entref /) (cdr (assoc 2 (entget entref))))


(defun BlockToPng (blkname filename /)
  (vl-load-com)



)

(defun C:ExportAllAsPdf
			(/	    blockRefs  insertPt	  ss
			 BlkName    WriteDir   i	  blkBB
			 printPt1   printPt2
			)
  (setq	blockRefs (blocknames)
	insertPt  '(0 0 0)
  )

  (setq
    WriteDir "D:\\extrusion-pdfs\\"
    ListLen  (length blockRefs)
    i	     0
  )


  (while (< i ListLen)
    (progn
      (setq BlkName (strcat (blockname
			      (nth i blockRefs)
			    )
		    )
      )
      (exportOneAsPdf writeDir BlkName)
      (setq i (+ i 1))
    )
  )
)

(defun c:TestExport (/)
  (setq
    WriteDir "D:\\extrusion-pdfs\\"
  )

  (exportOneAsPdf writeDir "9JFCC1F")
)


(defun exportOneAsPdf (writeDir	  BlkName    /		blockRefs
		       insertPt	  ss	     BlkName	WriteDir
		       i	  blkBB	     printPt1	printPt2
		       mintpt	  maxpt
		      )
  (vl-load-com)


  (print (strcat BlkName "\n"))
  (command "erase" (ssget "A") "")
					; Insert the block at the insert point

  (command "_insert" BlkName insertPt "" "" "")
					; Get the bounding box of the block we just inserteed
  (vla-getboundingbox
    (vlax-ename->vla-object (ssname (ssget "A") 0))
    'minpt
    'maxpt
  )
  (setq
    minpt (vlax-safearray->list minpt)
    maxpt (vlax-safearray->list maxpt)
  )

					; Translate insertpoint -1, -1
  (setq printPt1 (list (- (car minpt) 1) (- (cadr minpt) 1) 0))
					; Translate top right bounding box +1 +1
  (setq	printPt2 (list (+ (car maxpt) 2)
		       (+ (cadr maxpt) 2)
		       0
		 )
  )

					; The plot command is huge
  (command-s "-plot"
	     "Yes"			;Detailed plot configuration? [Yes/No] <No>: Y
	     ""				;Enter a layout name or [?] <0_01a>:
	     "PublishToWeb PNG.pc3"	;Enter an output device name or [?] 
	     "User 1 (3000.00 x 3000.00 Pixels)"
					;Enter paper size or [?] 
	     ""				;Enter drawing orientation [Por.../Lan...] <Lan...>:
	     "No"			;Plot upside down? [Yes/No] <No>:
	     "Window"			;Enter plot area [D.../E.../L.../V.../W...] <Window>:
	     printPt1			;Enter lower left corner of window 
	     printPt2			;Enter upper right corner of window 
	     "1=0.003832"		;Enter plot scale or [Fit] 
	     (list 0 0)			;Enter plot offset (x,y) or [Center] plopt
	     "Yes"			;Plot with plot styles? [Yes/No] <Yes>:
	     ""				;Enter plot style table name or [?] <>:
	     "Yes"			;Plot with lineweights? [Yes/No] <Yes>:
	     "No"			;Scale lineweights with plot scale? [Yes/No] (

	     (strcat writeDir BlkName)
	     "Yes"			;Save changes to page setup [Yes/No]? <N> y
	     "Yes"			;Proceed with plot [Yes/No] <Y>:
  )
					; Delete everything
  (command "erase" (ssget "A") "")


)





					; AllProfs   	[] An arbitrary string array of a bunch of profile codes
					; bb 			[] Returns two three dimensional points of the bottom left and top right borders of an entity. 
					; insertShort.  [blockname, insertPoint] Abbreviation for calling the insert command.  Does not momentarily toggle snapping
					; blockname.	[entref] Abbreviation for getting the name of a block from its reference

					; !!! LAYOUT FUNCTIONS
					; blocknames	[] Returns an array of all blocknames in the document's block library

					; layout 		[row of block entities, start point] Lawith their names beneath them, 0,0 is the start point
					; spacerow 		[row of blocks] Returns the height of the tallest block, useful for placing rows on top of each other
					; writetext 	[origin, text] Creates text entities starting at the origin point
					; arrange 		[] Uses blocknames and 0,0,0 to lay out all the blocks in a square
					; blockbb 		[block name] Lee's Mac's function for getting the block inside block table and getting its bounding box without inserting it
					; splitFunc 	[l, n] ??? I don't know if I even use this
					; split 		[list of blocks, number of blocks per sublist] Splits the list into list of lists with n length, the last list might be smaller
					; !!! END LAYOUT FUNCTIONS

					; BB 			[] Prompts you to click an entity and then draws two lines in an ex through its bounding box
					; NestSel		[] Selects all blocks (which are present in the paperspace) which contain nested blocks
					; ContainsNested[] Checks the entities sub-entities for dxf code 0 AKA 'INSERT'
					; writeall		[] Uses blocknames to get a list to wblock to its target directory.  Right now it's "C:\\Users\\Frodo\\wblock\\"

(defun bb (ent / minpt maxpt)
  (vla-getboundingbox
    (vlax-ename->vla-object ent)
    'minpt
    'maxpt
  )
  (list	(vlax-safearray->list minpt)
	(vlax-safearray->list maxpt)
  )
)

(defun insertshort (blockname point /)
  (command "_insert" blockname point "" "" "")
)


(defun c:NestSel (/ ss def n l)
  ;; Â© Lee Mac  ~  03.06.10

  (while (setq def (tblnext "BLOCK" (not def)))
    (if	(ContainsNested
	  (tblobjname "BLOCK" (setq n (cdr (assoc 2 def))))
	)
      (setq l (cons n l))
    )
  )

  (if l
    (sssetfirst
      nil
      (ssget "_X"
	     (list '(0 . "INSERT") (cons 2 (lst->str l ",")))
      )
    )
  )
  (princ)
)

(defun ContainsNested (ent / foo)
  ;; Â© Lee Mac  ~  03.06.10

  (defun foo (e)
    (if	(setq e (entnext e))
      (cons e (foo e))
    )
  )

  (member "INSERT"
	  (mapcar
	    (function
	      (lambda (x)
		(cdr (assoc 0 (entget x)))
	      )
	    )
	    (foo ent)
	  )
  )
)

(defun lst->str	(lst del)
  ;; Â© Lee Mac  ~  03.06.10
  (if (cdr lst)
    (strcat (car lst) del (lst->str (cdr lst) del))
    (car lst)
  )
)



(defun blocknames (/ d n r)
  (while (setq d (tblnext "BLOCK" (null d)))
    (if	(wcmatch (setq n (cdr (assoc 2 d))) "~`**")
      (if (and (not (> (vl-string-search n "$") -1)) (isValidBlock n))
	(setq r (cons (tblobjname "BLOCK" n) r))
      )
    )
  )
  (vl-sort r 'sortFun)
)
(defun sortFun (a1 a2 /)
  (< (blockname a1) (blockname a2))
)

(defun layout (row startpt / ent blkname box xval yval zval boxwidth)
  (setq	xval (car startpt)
	yval (cadr startpt)
	zval (caddr startpt)
  )

  (foreach ent row
    (if	(not (= "_NONE" (setq blkname (blockname ent))))
      (progn
	(setq boxwidth (car (cadr (blockbb blkname))))
	(insertshort blkname (list xval yval zval))
	(if (not boxwidth)
	  (setq boxwidth 0)
	)

	(writetext (list (+ xval (* boxwidth 0.5)) (- yval 0.5) zval)
		   blkname
	)

	(setq xval (+ xval boxwidth 0.2))
      )
      ()
    )
  )
)

(defun spacerow	(entrow / minpt maxpt h)
  (setq h 0)
  (foreach ent entrow
    (progn
      (setq blkHeight (cadr (cadr (blockbb (blockname ent)))))
      (if (not blkHeight)
	(setq blkHeight 0)
      )
      (setq h (max h blkHeight))
    )
  )
  h
)

(defun writetext (pt1 tval /)
  (entmake (list '(0 . "TEXT")
		 '(8 . "TEXT")		; Change this for different Layer (type layer name in quote marks)
		 (cons 10 pt1)
		 (cons 40 (getvar "TEXTSIZE"))
					; Change this for different height (set at default text size, change all red parts to just a number, i.e. (cons 40 2.5)
		 (cons 1 tval)
		 '(50 . 0.0)
		 '(7 . "STANDARD")	; change this for different Text Style (type style name in quote marks)
		 '(71 . 0)
		 '(72 . 0)
		 '(73 . 0)
	   ) ;_  end list

  ) ;_  end entmake
)

(defun c:arrange (/ pt allblocks linelength row)
  (setq pt (list 0 0 0))
  (setq allblocks (blocknames))
  (setq linelength (fix (sqrt (length allblocks))))
  (setq result (split allblocks linelength))
					; result works is a list of block lists!
  (foreach row result
    (progn
      (layout row pt)
      (setq pt (list (car pt)
		     (+ (cadr pt) (* (spacerow row) 1.5))
		     (caddr pt)
	       )
      )
    )
  )
)

(defun blockbb (block / l1 l2 ll ur)
  (vlax-for obj
	    (vla-item (vla-get-blocks
			(vla-get-activedocument (vlax-get-acad-object))
		      )
		      block
	    )
    (if	(vlax-method-applicable-p obj 'getboundingbox)
      (if
	(not
	  (vl-catch-all-error-p
	    (vl-catch-all-apply 'vla-getboundingbox (list obj 'll 'ur))
	  )
	)


	 (setq l1 (cons (vlax-safearray->list ll) l1)
	       l2 (cons (vlax-safearray->list ur) l2)
	 )

      )
    )
  )
  (if l1
    (list
      (apply 'mapcar (cons 'min l1))
      (apply 'mapcar (cons 'max l2))
    )
  )
  (if
    (not l1)
     (setq l1 (list (list 0))
     )
  )
  (if
    (not l2)
     (setq l2 (list (list 0))
     )
  )

  (list
    (apply 'mapcar (cons 'min l1))
    (apply 'mapcar (cons 'max l2))
  )
)

(defun splitfunc (l n / subList)
  (while (and (> (length l) 0) (< (length subList) n))
					; add onto subList, and take away from l
    (setq subList (cons (car l) subList)
	  l	  (cdr l)
    )
  )
  (list (reverse subList) l)
)

(defun split (arr num / r res)
  (while (> (length (setq res (splitfunc arr num)
			  r   (cons (car res) r)
			  arr (cadr res)
		    )
	    )
	    0
	 )
  )
  (reverse r)
)

(defun c:testBlockEdit (/ adoc blocks blockref val dxf360 dxf330)
  (setq sss nil)
  (vlax-for val	(vla-get-Blocks
		  (vla-get-ActiveDocument (vlax-get-acad-object))
		)
    (progn
      (setq blockref (entget (vlax-vla-object->ename val)))
      (setq blkname (cdr (car blockref)))
      (if (assoc 360 blockRef)
	(setq dxf360 (entget (cdr (assoc 360 blockRef)))
	      dxf330 (entget (cdr (assoc 330 blockRef)))
	)
      )
      (princ)
    )
  )
)


					; Gets the 2d area of two points
(defun bbArea (bb / x y)
  (setq	x (- (car (cadr bb))
	     (car (car bb))
	  )
	y (- (cadr (cadr bb))
	     (cadr (car bb))
	  )
  )
  (* x y)
)

(defun maxAndMinBlockAreas
       (blocks / blkName bb area maxArea minArea x y)
  (setq	maxarea	-2147483647
	minarea	2147483647
  )
  (foreach blkName blocks
    (setq bb (blockbb blkName))

    (setq area (bbArea bb))

    (if	(< area minArea)
      (setq minArea area)
    )

    (if	(> area maxArea)
      (setq maxArea area)
    )
  )
  (list minArea maxArea)
)

					; OriginPoint is a three dimensional point
(defun drawCell	(originPt len width /)
  (command "_line"
	   originpt
	   (list (+ (car originPt) len) (cadr originPt) 0)
	   (list (+ (car originPt) len) (+ (cadr originPt) width) 0)
	   (list (car originPt) (+ (cadr originPt) width) 0)
	   originpt
	   ""
  )
)

(defun zeroZeroBoundingBox (bb /)
  (setq	xDiff (- (car (cadr bb)) (car (car bb)))
	yDiff (- (cadr (cadr bb)) (cadr (car bb)))
  )
  (list (list 0 0 0) (list xDiff yDiff 0))
)


(defun setAllLineWeight	(s weight / i n x e)
  (if (not (= (type s) "PICKSET"))
    (foreach o (vlax-safearray->list (vlax-variant-value s))
      (progn
	(vla-put-lineweight o weight)
      )
    )
    (progn
      (setq i 0
	    n (sslength s)
      )
      (if (> n 1)
	(princ)
      )
      (while (< i n)
	(setq e	(ssname s i)
	      x	(cdr (assoc 0 (entget e)))
	      i	(1+ i)
	)
	(vla-put-lineweight (vlax-ename->vla-object e) weight)
      )
    )
  )



)

					; Takes a point which needs to be fitted inside the maxPoint
					; Both must be 3d so it doesn't get mad when I use cadr
					; A dialiate point
(defun dialateInside (pointToScale	maxPoint /	  xRatio
		      yRatio   diff	x1	 x2	  y1
		      y2       aspect	xNew	 yNew
		     )
  (setq
    x1 (car pointToScale)
    x2 (car maxPoint)
    y1 (cadr pointToScale)
    y2 (cadr maxPoint)

  )
  (setq
    xRatio (/ x1 x2)
    yRatio (/ y1 y2)
  )

  (if (> xRatio yRatio)
    (progn
      (setq aspect (/ x1 y1))
      (setq diff (- x2 x1))
      (setq xNew (+ x1 diff))
      (setq yNew (/ xNew aspect))
    )

    (progn
      (setq aspect (/ y1 x1))
      (setq diff (- y2 y1))
      (setq yNew (+ y1 diff))
      (setq xNew (/ yNew aspect))
    )
  )
  (list xNew yNew 0)
)


;; Selection Set Bounding Box  -  Lee Mac
;; Returns a list of the lower-left and upper-right WCS coordinates of a
;; rectangular frame bounding all objects in a supplied selection set.
;; sel - [sel] Selection set for which to return bounding box

(defun LM:ssboundingbox	(sel / idx llp ls1 ls2 obj urp)
  (repeat (setq idx (sslength sel))
    (setq obj (vlax-ename->vla-object (ssname sel (setq idx (1- idx)))))
    (if	(and (vlax-method-applicable-p obj 'getboundingbox)
	     (not (vl-catch-all-error-p
		    (vl-catch-all-apply
		      'vla-getboundingbox
		      (list obj 'llp 'urp)
		    )
		  )
	     )
	)
      (setq ls1	(cons (vlax-safearray->list llp) ls1)
	    ls2	(cons (vlax-safearray->list urp) ls2)
      )
    )
  )
  (if (and ls1 ls2)
    (mapcar '(lambda (a b) (apply 'mapcar (cons a b)))
	    '(min max)
	    (list ls1 ls2)
    )
  )
)

(defun allLineWeights (/)
  (list	acLnWt005     acLnWt009	    acLnWt013	  acLnWt015
	acLnWt018     acLnWt020	    acLnWt025	  acLnWt030
	acLnWt035     acLnWt040	    acLnWt050	  acLnWt053
	acLnWt060     acLnWt070	    acLnWt080	  acLnWt090
	acLnWt100     acLnWt106	    acLnWt120	  acLnWt140
	acLnWt158     acLnWt200	    acLnWt211
       )
)

(defun createAllCellBlocks (/)
  (setq
    originPt (list 0 0 0)
    insertZoneBottomLeft
     (list 0.1 0.1 0)
    insertZoneTopRight
     (list 1.9 1.9 0)
  )
)

(defun transform2dPt (pt1 pt2)
  (list	(+ (car pt1) (car pt2))
	(+ (cadr pt1) (cadr pt2))
	0
  )
)
(defun cellBlock (cellWidth	    cellHeight	      cellOrigin
		  marginLeft	    marginBottom      textMarginLeft
		  textMarginBottom  blkName	      /
		  originPt	    insertZoneBottomLeft
		  insertZoneTopRight		      blkName
		  bb		    blkSs	      targetPt
		  xScaleRatio	    yScaleRatio	      newBb
		  insertZoneBoundingBox		      differencePoint
		 )
  (setq
    originPt cellOrigin
    insertZoneBottomLeft
     (transform2dPt
       cellOrigin
       (list marginLeft marginBottom 0)
     )
    textInsertPoint
     (transform2dPt
       cellOrigin
       (list textMarginLeft textMarginBottom 0)
     )
    insertZoneTopRight
     (transform2dPt
       cellOrigin
       (list (- cellWidth 0.1) (- cellHeight 0.1) 0)
     )
  )
					; I need this to translate left and up to center the block
  (setq	insertZoneBoundingBox
	 (list insertZoneBottomLeft
	       insertZoneTopRight
	       0
	 )
  )

					; Write the block name underneath
  (writeText textInsertPoint blkName)

  (setq	insertZoneBb
	 (zeroZeroBoundingBox
	   (list insertZoneBottomLeft insertZoneTopRight)
	 )
  )

					; Make a cell
  (drawCell originPt cellWidth cellHeight)
  (setq bb (zeroZeroBoundingBox (blockbb blkName)))
  (setq targetPt (dialateInside (cadr bb) (cadr insertZoneBb)))
					; These scale ratios are the same
  (setq	xScaleRatio (/ (car targetPt) (car (cadr bb)))
	yScaleRatio (/ (cadr targetpt) (cadr (cadr bb)))
  )





  (command "-insert"	   blkName	   insertZoneBottomLeft
	   xScaleRatio	   ""		   ""
	  )
					; I'm going to make a huge assumption and say I can get the reference to my block by WC selecting in the margin,
					; this might not always be the case, but there doesn't look to be an easy way to get an anonymous reference after I've inserted

  (setq blkSs (ssget "L"))
					; Here we have the new bounding box
  (setq newBb (zeroZeroBoundingBox (LM:ssboundingbox blkSs)))

					; Now compute how far we must move translate left an right to land in the middle
  (setq	differencePoint
	 (subtractPts
	   (cadr (zeroZeroBoundingBox insertZoneBoundingBox))
	   (cadr newBb)
	 )
  )
  (command "_.move"
	   blkSs
	   ""
	   "_non"
	   '(0. 0. 0.)
	   "_non"
	   (multiplyPts differencePoint (list 0.5 0.5 0))
  )



					; Now get the areas of the two

  (setq	oldArea	(* (car (cadr bb)) (cadr (cadr bb)))
	newArea	(* (car (cadr newBb)) (cadr (cadr newBb)))
  )
					; Now get the ratio between them
  (setq areaRatio (sqrt (/ oldArea newArea)))

  (if (> areaRatio 7)
    (princ)
  )
					; Then explode our block
  (setq blkSs (vla-explode (vlax-ename->vla-object (ssname blkSs 0))))

					; Then reselect
  (setAllLineWeight
    blkSs
    (lineWidthFromAreaRatio areaRatio)
  )
)

(defun c:testNewCellblock (/)
  (cellBlock 2 2 (list 0 0 0) 0.1 0.6 0.1 0.1 "1DC1")
)

; ICECREAM
					; Don't worry about this
(defun c:cellAllBlocks (/ pt allblocks linelength row)
					; Don't worry about this either
  (vl-load-com)
					; Setq assigns a value to a variable
  (setq pt (list 0 0 0))
  (setq	lineHeight
	 -1.3
	lineWidth 1.6
	rowsPerPage 6
	columnsPerPage 6
  )
					; If you want to call a function you do (functionName argument1 argument2)
  (setq allblocks (blocknames))
  (setq result (split allblocks columnsPerPage))

  (setq i 0)
					; result works is a list of block lists!0
  (foreach row result
					; Don't worry about progn
    (progn
      (foreach blkRef row
	(cellBlock lineWidth
		   1.3
		   pt
		   0.05
		   0.25
		   0.05
		   0.05
		   (blockname blkRef)
	)
	; This is where I move the point one cell width to the right
	(setq pt (transform2dPt pt (list lineWidth 0 0)))
	
      )
      ; This is where I move it down
      (setq i (+ i 1))
      ; If we're at the end of a page
      (if (= (rem i rowsPerPage) 0)
	(progn
	  ; Get current TEXTSIZE, set TEXTSIZE to .3, writetext then set the text size back
	  ;(setq tSize (getvar "TEXTSIZE"))
	  ;(setvar "TEXTSIZE" 0.25)
	  (writetext (transform2dPt pt (list (* -1 lineWidth columnsPerPage 0.98) -0.325 0)) (itoa (/ i rowsPerPage)))
	  ;(setvar "TEXTSIZE" tSize)
	  
	   
	  ; Aaand space our page
	  (setq pt (list 0 (+ (cadr pt) (* lineHeight 2)) 0))
	  )
	(setq pt (list 0 (+ (cadr pt) lineHeight) 0))
	)
      
    )
  )
)


(defun lineWidthFromAreaRatio (areaRatio	/
			       widths		estMaxTransform
			       areaRatio	placeOnLinewidthScale
			       width
			      )
  (setq widths (reverse (allLineWeights)))
  (setq estMaxTransform 2)

  (setq areaRatio (+ areaRatio 0.5))
  (setq placeOnLinewidthScale (fix (* (/ areaRatio 2) (length widths))))

  (if (not (nth placeOnLinewidthScale widths))
    (setq width (last widths))
    (setq width (nth placeOnLinewidthScale widths))
  )
)

(defun c:unnest	(/)
  (removeNestedBlocks (getstring "Which block?"))
)
(defun removeNestedBlocks (blkName /)
					; Erase everything so that we have a clean slate
  (command "_erase" (ssget "A"))
  (command "_insert" blkName (list 0 0 0) "" "" "")
  (explodeNestRecursive (ssname (ssget "L") 0))
  (command "_-block" blkName "Y" (list 0 0 0) (ssget "A") "")
)

(defun setBlockLayer (blkName layer /)
					; Erase everything so that we have a clean slate
  (command "_erase" (ssget "A"))
  (command "_insert" blkName (list 0 0 0) "" "" "")
  (setLayerRecursive (ssname (ssget "L") 0) layer)
  (command "_-block" blkName "Y" (list 0 0 0) (ssget "A") "")
)

(defun c:setAllBlocksLayer (/ layer)
  (setq	blocks (blocknames)
	layer  (getstring "What layer?")
  )
  (foreach block blocks
    (setBlockLayer (blockname block) layer)
  )
)

(defun c:unnestAllBlocks (/)
  (setq blocks (blocknames))
  (foreach block blocks
    (removeNestedBlocks (blockname block))
  )
)
					; Takes an entity name, explodes it and then does it again if any of the children of the explosion are blocks
(defun setLayerRecursive (entref layer /)
  (setq	objects	(vlax-safearray->list
		  (vlax-variant-value
		    (vla-explode (vlax-ename->vla-object entref))
		  )
		)
	ss	(ssadd)
  )
  (ssadd entref ss)
  (command "_erase" ss "")


  (foreach e objects
    (progn
      (vla-put-layer e layer)
      (if (= (cdr (assoc 0 (entget (vlax-vla-object->ename e))))
	     "INSERT"
	  )
	(setLayerRecursive (vlax-vla-object->ename e) layer)
      )
    )
  )
)


(defun c:setColorToByLayerAllBlocks (/)
  (setq blocks (blocknames))
  (foreach block blocks
    (setBlockLayer (blockname block))
  )
)
(defun setBlockLayer (blkName /)
					; Erase everything so that we have a clean slate
  (command "_erase" (ssget "A"))
  (command "_insert" blkName (list 0 0 0) "" "" "")
  (setColorRecursive (ssname (ssget "L") 0) acByLayer)
  (command "_-block" blkName "Y" (list 0 0 0) (ssget "A") "")
)

(defun setColorRecursive (entref color /)
  (setq	objects	(vlax-safearray->list
		  (vlax-variant-value
		    (vla-explode (vlax-ename->vla-object entref))
		  )
		)
	ss	(ssadd)
  )
  (ssadd entref ss)
  (command "_erase" ss "")

  (foreach e objects
    (progn
      (vla-put-color e color)
      (if (= (cdr (assoc 0 (entget (vlax-vla-object->ename e))))
	     "INSERT"
	  )
	(setColorRecursive (vlax-vla-object->ename e) color)
      )
    )
  )
)


(defun explodeNestRecursive (entref /)
  (setq	objects	(vlax-safearray->list
		  (vlax-variant-value
		    (vla-explode (vlax-ename->vla-object entref))
		  )
		)
	ss	(ssadd)
  )
  (ssadd entref ss)
  (command "_erase" ss "")


  (foreach e objects
    (progn
      (if (= (cdr (assoc 0 (entget (vlax-vla-object->ename e))))
	     "INSERT"
	  )
	(explodeNestRecursive (vlax-vla-object->ename e))
      )
    )
  )
)

(defun isValidBlock (blockName / badBlocks)
  (setq	badBlocks (list	"0°" "0°M" "45°" "45°M"	"90°" "90°M" "Hole"
			"M Hole")
  )
  (not (vl-position blockName badBlocks))
)
(defun subtractPts (pt1 pt2 /)
  (list	(- (car pt1) (car pt2))
	(- (cadr pt1) (cadr pt2))
	(- (nth 2 pt1) (nth 2 pt2))
  )
)
(defun multiplyPts (pt1 pt2 /)
  (list	(* (car pt1) (car pt2))
	(* (cadr pt1) (cadr pt2))
	(* (nth 2 pt1) (nth 2 pt2))
  )
)