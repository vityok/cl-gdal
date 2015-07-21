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
  (ql:quickload :cl-cairo2)
  (ql:quickload :cl-cairo2-xlib)
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

(defparameter *default-spatial-ref* "+proj=latlong +ellps=WGS84 +datum=WGS84"
  "Proj.4 representation of the Spatial Reference System when it is
not defined in the Layer.")

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
   (from-proj :accessor from-proj :initarg :from-proj)
   (to-proj :accessor to-proj :initarg :to-proj)
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
    #+vecto-mode
    (* (scale-y ctx)
       (- y (min-y ctx)))
    #-cairo-ltk-mode
    (- (height ctx)
       (* (scale-y ctx)
	  (- y (min-y ctx))))))

;; --------------------------------------------------------

(defun line-string-path (ctx geom)
  "Create a path for the given line-string so that it can be later
either striken or filled (or both) depending on the actual geometry
type."
  (format t "putting path for ~a with ~a points~%" geom (ogr:get-point-count geom))
  (ogr:with-points (x y z count) geom
    (format t "retrieved ~a points~%" count)

    (iter
      (initially
       (cairo:move-to (tx ctx (cffi:mem-aref x :double 0))
                      (ty ctx (cffi:mem-aref y :double 0))))
      (for i from 1 below (ogr:get-point-count geom))
      (cairo:line-to (tx ctx (cffi:mem-aref x :double i))
                     (ty ctx (cffi:mem-aref y :double i))))))

;; --------------------------------------------------------

(defgeneric paint (ctx geom)
  (:method ((ctx <paint-ctx>) (geom ogr:<POINT>))
    (format t "paint POINT~%")
    (multiple-value-bind (x y z)
	(ogr:get-point geom 0)
      (format t "  point[~a](~,2f, ~,2f, ~,2f)~%" 0 x y z)
      (cairo:set-source-rgb 1.0 1.0 1.0)
      (cairo:arc (tx ctx x) (ty ctx y) 2 0 (* 2 3.14))
      (cairo:fill-preserve)))

  (:method ((ctx <paint-ctx>) (geom ogr:<LINE-STRING>))
    (format t "paint LINE-STRING with ~a points~%" (ogr:get-point-count geom))
    (let ((color (elt *colors-brewer*
                      (random (length *colors-brewer*)
                              (seed ctx)))))
      (cairo:set-line-width 1.1)
      (apply #'cairo:set-source-rgb color)
      (line-string-path ctx geom)
      (cairo:stroke)))

  (:method ((ctx <paint-ctx>) (geom ogr:<POLYGON>))
    (format t "paint POLYGON with ~a points~%" (ogr:get-point-count geom))
    (iter (for i from 0 below (ogr:get-geometry-count geom))
	  (for poly = (ogr:get-geometry geom i))
	  (format t "  geom(~a) has ~a geometry objects: ~a~%"
		  (ogr:get-type poly) (ogr:get-geometry-count poly)
		  (iter (for j from 0 below (ogr:get-geometry-count poly))
			(collect (ogr:get-type (ogr:get-geometry poly j)))))
          (let ((color (elt *colors-brewer*
                            (random (length *colors-brewer*)
                                    (seed ctx)))))
            (cairo:set-line-width 1.1)
            (apply #'cairo:set-source-rgb color)
            (line-string-path ctx poly)
            (cairo:fill-preserve)
            (cairo:stroke))))

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
    (error "MULTI-LINE-STRING not implemented"))

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
	      (if (ogr:get-spatial-ref layer)
                  (ogr:get-proj4 (ogr:get-spatial-ref layer))
                  *default-spatial-ref*))
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
         (layer-spatial-ref (if (ogr:get-spatial-ref layer)
                                (ogr:get-proj4 (ogr:get-spatial-ref layer))
                                *default-spatial-ref*))
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
                             :from-proj (pj:pj-init-plus layer-spatial-ref)
                             :to-proj (pj:pj-init-plus "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs")
			     :canvas (cairo:create-image-surface :argb32 *screen-x* *screen-y*)))
	 (cairo-ctx (cairo:create-xlib-image-context *screen-x* *screen-y*)))

    (format t "painting layer ~a with spatial-ref: ~a~%"
	    (ogr:get-name layer)
	    layer-spatial-ref)


    (cairo:with-context (cairo-ctx)
      (iter (for i from 0 below (ogr:get-feature-count layer))
	    (for feature = (ogr:get-feature layer i))
	    (for geom = (ogr:get-geometry feature))
	    (paint ctx geom))
      ;; (cairo:destroy cairo-ctx)
      ;; (cairo:surface-write-to-png (canvas ctx) "map-canvas.png")
      )
    (format t "DONE painting layer ~a~%" id)))

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

      (ltk:listbox-append lst-layers '())
      (ltk:pack lbl-main :side :top)
      (ltk:pack lst-layers :side :left)
      (ltk:pack btn-load :side :bottom))))

;; EOF
