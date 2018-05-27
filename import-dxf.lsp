(defun c:importdxf (/ directory files)
(setq directory "C:\\Users\\Frodo\\Google Drive\\extrusion-list\\wblock")
(if (setq files (vl-directory-files directory "*.dxf" 1))
(foreach dxf files
(command "_.-insert"
(strcat directory "\\" dxf)
'(0. 0. 0.)
""
""
""
)
)
(princ (strcat "\n No DXF files found in " directory))
)
(princ)
)