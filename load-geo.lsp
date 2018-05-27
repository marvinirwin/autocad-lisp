;; String to List  -  Lee Mac
;; Separates a string using a given delimiter
;; str - [str] String to process
;; del - [str] Delimiter by which to separate the string
;; Returns: [lst] List of strings
 
(defun str->lst ( str del / pos )
  (if (/= nil str)
    (if (setq pos (vl-string-search del str))
        (cons (substr str 1 pos) (str->lst (substr str (+ pos 1 (strlen del))) del))
        (list str)
    )
    )
)

(defun parsegeoline (linelist / )
  (list
    (list (/ (atof (nth 0 linelist)) 25.4)
	  (/ (atof (nth 1 linelist)) 25.4)
	  0 )
    
    (list (/ (atof (nth 2 linelist)) 25.4)
	  (/ (atof (nth 3 linelist)) 25.4)
	  0 )
  )
  )

(defun readtoend (openf / lines l)
  (while (/= (setq l (read-line openf)) nil)
    (setq lines (cons l lines))
    (print l)
    )
  (reverse lines)
  )

; This returns a list of remaining lines,
; and the section which is a list of point pairs
(defun readsection (lines / spit geolines)
  ; Keep making the list of lines shorter until we get to the start of a section
  (while (and (not (= (length (setq split (str->lst (car lines) " "))) 4))
	      (> (length lines) 0))
    (setq lines (cdr lines))
    (print (length lines))
    )
  ; Once we're at 4 then parse each float as an int
  (while (and (= (length (setq split (str->lst (car lines) " "))) 4)
	      (> (length lines) 0))
    (setq geolines (cons (parsegeoline split) geolines))
    (setq lines (cdr lines))
    (print (length lines))
    )
  (list lines geolines)
  )

; Lee Mac's
(defun Line (p1 p2)
  (entmakex (list (cons 0 "LINE")
                  (cons 10 p1)
                  (cons 11 p2))))

(defun transform (p trp / )
  (list (+ (car p) (car trp))
	(+ (cadr p) (cadr trp))
	(+ (caddr p) (caddr trp))
	)
  )

(defun readgeo (lines / acadlines)
  (while (> (length lines) 0)
    (setq results (readsection lines))
    (setq acadlines (append acadlines (cadr results))
	   lines (car results))
    )
  (setq origin (getpoint "Select insertion origin"))
  (foreach l acadlines (line (transform (car l) origin) (transform (cadr l) origin)))
  )

(defun c:lgeo ( / )
  (setq file (getfiled "Select .geo file" "" "geo" 4))
  (readgeo (readtoend (open file "r")))
  )

