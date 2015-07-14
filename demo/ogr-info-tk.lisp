;; Opens vector geographic files and displays information about layers
;; and features that they contain.
;;
;; This program is intended to serve as a demonstration and a
;; prototype to check how useful and convenient OGR library Lisp API
;; is in practice.
;;
;; This program depends on the LTK system that is available in
;; quicklisp and avoids external dependencies put forward by the GTK.
;;
;; Load:
;;
;; sbcl --load ogr-info-tk.lisp --eval '(ogr-info-demo:main)'
;; lx86cl --load ogr-info-tk.lisp --eval '(ogr-info-demo:main)'


;; TODO
;;
;; + open selected file
;; * better error reporting
;; * list layers
;; * draw features in selected area
;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :iterate)
  (ql:quickload :ltk)
  (ql:quickload :cl-ogr)
  (ql:quickload :cl-proj))

(defpackage :ogr-info-demo
  (:use :cl :iterate)
  (:export :main))

(in-package :ogr-info-demo)

;; --------------------------------------------------------

(defvar *layers-list* '())
(defvar *ds* nil
  "This is a datasource handle opened by the INSPECT-OGR-FILE.")
(defvar *cnv* nil
  "Canvas where all the drawing is performed.")

;; --------------------------------------------------------

(defun inspect-ogr-file (gis-file)
  "Open the data source for the given GIS-FILE and extract list of
layers from it."

  (format t "inspecting file: ~a~%" gis-file)
  (let ((ds (ogr:open-data-source gis-file)))
    (unless ds
      (error "Failed to open geographic file"))

    ;; reset globals.
    (when *ds*
      ;; gracefully close data source
      (ogr:destroy *ds*))

    (setf *ds* ds)
    (setf *layers-list* nil)

    (format t "it contains: ~a layers~%" (ogr:get-layer-count ds))

    (iter
      (for i from 0 below (ogr:get-layer-count ds))
      (for layer = (ogr:get-layer ds i))
      (format t "layer[~a]: ~a; spatial-ref: ~a~%" i (ogr:get-name layer)
	      (ogr:get-proj4 (ogr:get-spatial-ref layer)))
      (push (ogr:get-name layer) *layers-list*))))

;; --------------------------------------------------------

(defun gui-choose-file-dialog ()
  (let ((fname (ltk:get-open-file)))
    (format t "file: '~a'~%" fname)
    (when (and fname (> (length fname) 0))
      (inspect-ogr-file fname))))

;; --------------------------------------------------------

(defun gui-paint-layer (id)
  (let* ((layer (ogr:get-layer *ds* id))
	 (geom-type (ogr:get-geom-type layer)))
    (format t "painting layer ~a with spatial-ref: ~a~%" (ogr:get-name layer)
	    (ogr:get-proj4 (ogr:get-spatial-ref layer)))

    (case geom-type
      (:wkb-multi-point
       (format t "paint MULTI-POINT with ~a features~%" (ogr:get-feature-count layer))
       (iter (for i from 0 below (ogr:get-feature-count layer))
	     (for feature = (ogr:get-feature layer i))
	     (for geom = (ogr:get-geometry feature))
	     (for point = (ogr:get-geometry geom 0))
	     (format t "geom(~a)[~a] has ~a geometric items~%"
		     (ogr:get-type geom) i (ogr:get-geometry-count geom))
	     (multiple-value-bind (x y z)
		 (ogr:get-point point 0)
	       (format t "point[~a](~,2f, ~,2f, ~,2f)~%" i x y z)
	       (ltk:make-oval *cnv* x y (+ x 1.0) (+ y 1.0)))
	     (for sr = (ogr:get-spatial-ref point))
	     (format t "spatial-ref: ~a~%" (ogr:get-proj4 sr))
	     ))

      (:wkb-polygon
       (format t "paint POLYGON with ~a features~%" (ogr:get-feature-count layer))
       ;; (format t "feature ~a has ~a points~%" i (ogr:get-point-count geom))
       )
      (t
       (format t "unknown feature type ~a~%" geom-type)))))

;; --------------------------------------------------------

(defun gui-handle-select-layer (idx)
  "Handle selection of a layer from the layers list.

IDX is a list of selected layers."
  (format t "selected: ~a; ~a~%" idx (integerp (car idx)))
  (iter (for id in idx)
	(gui-paint-layer id)))

;; --------------------------------------------------------

(defun main ()
  (ogr:ogr-register-all)
  (ltk:with-ltk (:debug :develop)
    (let* ((lbl-main (make-instance 'ltk:label :text
				    "This application demonstrates basic features of the OGR library bindings"))
	   (lst-layers (make-instance 'ltk:listbox))
	   (cnv-layer (make-instance 'ltk:canvas :relief :sunken))
	   (btn-load
	    (make-instance 'ltk:button
			   :master nil
			   :text "Open a file"
			   :command (lambda ()
				      (ltk:listbox-clear lst-layers)
				      (gui-choose-file-dialog)
				      (ltk:listbox-append lst-layers *layers-list*)))))
      (ltk:bind lst-layers "<Double-1>"
		(lambda (evt)
		  (declare (ignore evt))
		  (gui-handle-select-layer (ltk:listbox-get-selection lst-layers))))

      (ltk:listbox-append lst-layers '("a"))
      (setf *cnv* cnv-layer)
      (ltk:pack lbl-main :side :top)
      (ltk:pack lst-layers :side :left)
      (ltk:pack cnv-layer :side :right)
      (ltk:pack btn-load :side :bottom))))

;; EOF
