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
;; sbcl --load ogr-info-tk.lisp --eval '(ogr-tk-demo:main)'
;; lx86cl --load ogr-info-tk.lisp --eval '(ogr-tk-demo:main)'
;; ecl -load ogr-info-tk.lisp -eval '(ogr-tk-demo:main)'

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
  (ql:quickload :cl-proj)
  (ql:quickload :vecto)
  (ql:quickload :external-program))

(defpackage :ogr-tk-demo
  (:use :cl :iterate)
  (:export :main))

(in-package :ogr-tk-demo)

;; --------------------------------------------------------

(defvar *layers-list* '())
(defvar *ds* nil
  "This is a datasource handle opened by the INSPECT-OGR-FILE.")
(defvar *cnv* nil
  "Canvas where all the drawing is performed.")

;; --------------------------------------------------------

(defvar *screen-x* 640 "Default viewport or screen width.")
(defvar *screen-y* 480 "Default viewport or screen height.")

;; --------------------------------------------------------

(defclass <paint-ctx> ()
  ((width :accessor width :initarg :width :type fixnum)
   (height :accessor height :initarg :height :type fixnum)
   (min-x :accessor min-x :initarg :min-x)
   (min-y :accessor min-y :initarg :min-y)
   (extent-x :accessor extent-x :initarg :extent-x)
   (extent-y :accessor extent-y :initarg :extent-y)
   (scale-x :accessor scale-x)
   (scale-y :accessor scale-y)
   (canvas :accessor canvas :initarg :canvas :type ltk:canvas)
   (seed :accessor seed :initform (make-random-state))))

;; --------------------------------------------------------

(defparameter *colors-brewer* (map 'list
				   #'(lambda (x)
				       (list (float (/ (first x) 255))
					     (float (/ (second x) 255))
					     (float (/ (third x) 255))))
				   '((165 0 38) (215 48 39) (244 109 67)
				     (253 174 97) (254 224 144) (224 243 248)
				     (171 217 233) (116 173 209) (69 117 180)
				     (49 54 149)

				     (64 0 75) (118 42 131) (153 112 171)
				     (194 165 207) (231 212 232) (247 247 247)
				     (217 240 211) (166 219 160) (90 174 97)
				     (27 120 55) (0 68 27)))
  "Color palette obtained from http://colorbrewer2.org/, a project of
Cynthia Brewer, Mark Harrower and The Pennsylvania State University.")

;; --------------------------------------------------------

(defmethod initialize-instance :after ((ctx <paint-ctx>) &key)
  (let ((extent-x (slot-value ctx 'extent-x))
	(extent-y (slot-value ctx 'extent-y))
	(width (slot-value ctx 'width))
	(height (slot-value ctx 'height)))

    (setf (slot-value ctx 'scale-x)
          (/ width extent-x)
	  (slot-value ctx 'scale-y)
	  (/ height extent-y))))

(defgeneric tx (ctx x)
  (:method ((ctx <paint-ctx>) x)
    ;; transform x coord to fit the screen
    (* (scale-x ctx) (- x (min-x ctx)))))

(defgeneric ty (ctx y)
  (:method ((ctx <paint-ctx>) y)
    ;; transform y coord to fit the screen
    (* (scale-y ctx)
       (- y (min-y ctx)))
    #+ltk-mode
    (- (height ctx)
       (* (scale-y ctx)
	  (- y (min-y ctx))))))

;; --------------------------------------------------------

(defgeneric paint (ctx geom)
  (:method ((ctx <paint-ctx>) (geom ogr:<POINT>))
    (format t "paint POINT~%")
    (multiple-value-bind (x y z)
	(ogr:get-point geom 0)
      (format t "  point[~a](~,2f, ~,2f, ~,2f)~%" 0 x y z)
      (vecto:set-rgb-fill 1.0 1.0 1.0)
      (vecto:centered-circle-path (tx ctx x) (ty ctx y) 1)
      (vecto:fill-path)
      #+ltk-mode
      (ltk:make-oval (canvas ctx)
		     (tx ctx x) (ty ctx y)
		     (+ (tx ctx x) 1.0)
		     (+ (ty ctx y) 1.0))))

  (:method ((ctx <paint-ctx>) (geom ogr:<LINE-STRING>))
    (format t "paint LINE-STRING with ~a points~%" (ogr:get-point-count geom))
    (iter
      (with color = (elt *colors-brewer* (random (length *colors-brewer*) (seed ctx))))
      (initially
       (vecto:set-line-width 1.1)
       (apply #'vecto:set-rgb-stroke color)
       (apply #'vecto:set-rgb-fill color)
       (vecto:move-to (tx ctx (ogr:get-x geom 0))
		      (ty ctx (ogr:get-y geom 0))))
      (for i from 1 below (ogr:get-point-count geom))
      (vecto:line-to (tx ctx (ogr:get-x geom i))
		     (ty ctx (ogr:get-y geom i)))
      (finally
       (vecto:close-subpath)
       (vecto:fill-and-stroke)))

    #+ltk-mode
    (let* ((points (iter
		     (for i from 0 below (ogr:get-point-count geom))
		     (appending (list (tx ctx (ogr:get-x geom i))
				      (ty ctx (ogr:get-y geom i)))))))
      (ltk:create-line (canvas ctx) points)))

  (:method ((ctx <paint-ctx>) (geom ogr:<POLYGON>))
    (format t "paint POLYGON with ~a points~%" (ogr:get-point-count geom))
    (iter (for i from 0 below (ogr:get-geometry-count geom))
	  (for poly = (ogr:get-geometry geom i))
	  (format t "  geom(~a) has ~a geometry objects: ~a~%"
		  (ogr:get-type poly) (ogr:get-geometry-count poly)
		  (iter (for j from 0 below (ogr:get-geometry-count poly))
			(collect (ogr:get-type (ogr:get-geometry poly j)))))
	  (paint ctx poly)

	  #+ignore
	  (iter (for j from 0 below (ogr:get-geometry-count geom))
		(paint (ogr:get-geometry geom j)))))

  (:method ((ctx <paint-ctx>) (geom ogr:<multi-point>))
    (format t "paint MULTI-POINT~%")
    ;; fix this cycle
    (iter (for i from 0 below (ogr:get-geometry-count geom))
	  (for point = (ogr:get-geometry geom i))
	  (format t "  geom(~a) has ~a geometric items~%"
		  (ogr:get-type geom) (ogr:get-geometry-count geom))
	  (paint ctx point)))

  (:method ((ctx <paint-ctx>) (geom ogr:<multi-line-string>))
    (format t "MULTI-LINE-STRING with ~a points~%" (ogr:get-point-count geom))
    )

  (:method ((ctx <paint-ctx>) (geom ogr:<multi-polygon>))
    (format t "paint MULTI-POLYGON~%")
    (iter (for i from 0 below (ogr:get-geometry-count geom))
	  (for poly = (ogr:get-geometry geom i))
	  (format t "  geom(~a) has ~a geometry objects: ~a~%"
		  (ogr:get-type poly) (ogr:get-geometry-count poly)
		  (iter (for j from 0 below (ogr:get-geometry-count poly))
			(collect (ogr:get-type (ogr:get-geometry poly j)))))
	  (paint ctx poly))))

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
	 (extent (ogr:get-extent layer))
	 (extent-x (- (ogr:max-x extent) (ogr:min-x extent)))
	 (extent-y (- (ogr:max-y extent) (ogr:min-y extent)))
	 (ctx (make-instance '<paint-ctx>
			     :width *screen-x*
			     :height *screen-y*
			     :min-x (ogr:min-x extent)
			     :min-y (ogr:min-y extent)
			     :extent-x extent-x
			     :extent-y extent-y
			     :canvas *cnv*)))

    (format t "painting layer ~a with spatial-ref: ~a~%"
	    (ogr:get-name layer)
	    (ogr:get-proj4 (ogr:get-spatial-ref layer)))

    (vecto:with-canvas (:width (width ctx) :height (height ctx))
      (iter (for i from 0 below (ogr:get-feature-count layer))
	    (for feature = (ogr:get-feature layer i))
	    (for geom = (ogr:get-geometry feature))
	    (paint ctx geom))

      (vecto:save-png "map-canvas.png"))
    ;; a bit of a hack since default Tcl/Tk does not accept PNG files,
    ;; we use ImageMagic to convert it to the GIF format
    (external-program:run "/bin/rm"
			  (list
			   "map-canvas.gif")
			  :error *standard-output*)
    (external-program:run "/usr/bin/convert"
			  (list
			   "map-canvas.png"
			   "map-canvas.gif")
			  :error *standard-output*)

    (let ((image (ltk:make-image)))
      (ltk:image-load image "map-canvas.gif")
      (ltk:create-image (canvas ctx) 0 0 :image image))))

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
	   (cnv-layer (make-instance 'ltk:canvas
				     :relief :sunken
				     :width *screen-x*
				     :height *screen-y*))
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
