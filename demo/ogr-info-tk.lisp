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
(defvar *bb-min-x* 0.0d0)
(defvar *bb-min-y* 0.0d0)
(defvar *bb-max-x* 0.0d0)
(defvar *bb-max-y* 0.0d0)

(defparameter *default-spatial-ref* "+proj=latlong +ellps=WGS84 +datum=WGS84"
  "Proj.4 representation of the Spatial Reference System when it is
not defined in the Layer.")

(defparameter *target-spatial-ref*
  ;; a very simple version of the Mercator projection configured for
  ;; Ukraine (zone parameter)

  #+ignore
  "+proj=utm +zone=35 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

  ;; the so-called "web Mercator" projection as found on the internets
  "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"
  )

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
  (let ((width (slot-value ctx 'width))
	(height (slot-value ctx 'height)))

    ;; todo: what should be done when extent is 0?
    (if (= (slot-value ctx 'extent-x) 0) (setf (slot-value ctx 'extent-x) 180))
    (if (= (slot-value ctx 'extent-y) 0) (setf (slot-value ctx 'extent-y) 180))

    (setf (slot-value ctx 'scale-x)
          (/ width (slot-value ctx 'extent-x))
	  (slot-value ctx 'scale-y)
	  (/ height (slot-value ctx 'extent-y)))))

;; --------------------------------------------------------

(defun line-string-path (ctx geom)
  "Create a path for the given line-string so that it can be later
either striken or filled (or both) depending on the actual geometry
type."
  (declare (optimize debug))

  (format t "putting path for ~a with ~a points~%" geom (ogr:get-point-count geom))
  (ogr:with-points (x y z count) geom
    (format t "retrieved ~a points:~%" count)
    (iter
      (for i from 0 below (ogr:get-point-count geom))
      (setf
       (cffi:mem-aref x :double i) (pj:deg-to-rad (cffi:mem-aref x :double i))
       (cffi:mem-aref y :double i) (pj:deg-to-rad (cffi:mem-aref y :double i))
       (cffi:mem-aref z :double i) (pj:deg-to-rad (cffi:mem-aref z :double i))))

    (let ((ret (pj:pj-transform (from-proj ctx)
                                (to-proj ctx)
                                count 1 x y z)))
      (when (/= ret 0)
        (format t "transformation failed with error code: ~a ~%" ret)))

    (format t "transformed, starting to mark the path~%")

    (iter
      (initially
       (format t "move-to: ~a ~a~%" (cffi:mem-aref x :double 0) (cffi:mem-aref y :double 0))
       (cairo:move-to (cffi:mem-aref x :double 0)
                      (cffi:mem-aref y :double 0)))
      (for i from 1 below (ogr:get-point-count geom))
      (cairo:line-to (cffi:mem-aref x :double i)
                     (cffi:mem-aref y :double i)))))

;; --------------------------------------------------------

(defun paint-feature (ctx feature)
  (let ((geom (ogr:get-geometry feature)))
    (paint-geom ctx geom feature)))

(defgeneric paint-geom (ctx geom feature)
  (:method ((ctx <paint-ctx>) (geom ogr:<POINT>) feature)
    (declare (ignore feature))
    (format t "paint POINT~%")
    (multiple-value-bind (x y z)
	(ogr:get-point geom 0)
      (format t "  point[~a](~,2f, ~,2f, ~,2f)~%" 0 x y z)
      (cairo:set-source-rgb 1.0 1.0 1.0)
      (cairo:arc x y 2 0 (* 2 3.14))
      (cairo:fill-preserve)))

  (:method ((ctx <paint-ctx>) (geom ogr:<LINE-STRING>) feature)
    (format t "paint LINE-STRING with ~a points~%" (ogr:get-point-count geom))
    (let ((color (elt *colors-brewer*
                      (random (length *colors-brewer*)
                              (seed ctx)))))
      (cairo:set-line-width 1.1)
      (apply #'cairo:set-source-rgb color)
      (line-string-path ctx geom)
      (cairo:stroke)))

  (:method ((ctx <paint-ctx>) (geom ogr:<POLYGON>) feature)
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

  (:method ((ctx <paint-ctx>) (geom ogr:<MULTI-POINT>) feature)
    (format t "paint MULTI-POINT~%")
    ;; fix this cycle
    (iter (for i from 0 below (ogr:get-geometry-count geom))
	  (for point = (ogr:get-geometry geom i))
	  (format t "  geom(~a) has ~a geometric items~%"
		  (ogr:get-type geom) (ogr:get-geometry-count geom))
	  (paint-geom ctx point feature)))

  (:method ((ctx <paint-ctx>) (geom ogr:<MULTI-LINE-STRING>) feature)
    (format t "MULTI-LINE-STRING with ~a points~%" (ogr:get-point-count geom))
    (error "MULTI-LINE-STRING not implemented"))

  (:method ((ctx <paint-ctx>) (geom ogr:<MULTI-POLYGON>) feature)
    (format t "paint MULTI-POLYGON~%")
    (iter (for i from 0 below (ogr:get-geometry-count geom))
	  (for poly = (ogr:get-geometry geom i))
	  (format t "  geom(~a) has ~a geometry objects: ~a~%"
		  (ogr:get-type poly) (ogr:get-geometry-count poly)
		  (iter (for j from 0 below (ogr:get-geometry-count poly))
			(collect (ogr:get-type (ogr:get-geometry poly j)))))
	  (paint-geom ctx poly feature)))

  (:method ((ctx <paint-ctx>) (geom ogr:<GEOMETRY>) (feature ogr:<FEATURE>))
    (format t "fallback to GEOMETRY with ~a geometry fields~%" (ogr:ogr-f-get-geom-field-count (ogr:pointer feature)))
    (iter (for i from 0 below (ogr:ogr-f-get-geom-field-count (ogr:pointer feature)))
          (for geom-field = (ogr:ogr-f-get-geom-field-defn-ref (ogr:pointer feature) i))
          (format t "type: ~a~%" (ogr:ogr-gfld-get-type geom-field))
          (format t "spatial ref: ~a~%" (ogr:ogr-gfld-get-spatial-ref geom-field)))))

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
      (for extent = (ogr:get-extent layer))
      (format t "layer[~a]: ~a; spatial-ref: ~a~%" i (ogr:get-name layer)
	      (if (ogr:get-spatial-ref layer)
                  (ogr:get-proj4 (ogr:get-spatial-ref layer))
                  "N/A"))
      (format t "within bounding box: [(~a, ~a), (~a, ~a)]~%"
              (ogr:min-x extent) (ogr:min-y extent)
              (ogr:max-x extent) (ogr:max-y extent))
      (minimize (ogr:min-x extent) into bb-min-x)
      (maximize (ogr:max-x extent) into bb-max-x)
      (minimize (ogr:min-y extent) into bb-min-y)
      (maximize (ogr:max-y extent) into bb-max-y)
      (push (ogr:get-name layer) *layers-list*)
      (finally
       ;; if the datasource contains only single-point layers or there
       ;; is a single-point layer, make sure that the bounding box
       ;; extents will include all layers within this data source
       (setf *bb-min-x* bb-min-x
             *bb-min-y* bb-min-y
             *bb-max-x* bb-max-x
             *bb-max-y* bb-max-y)))
    
    (format t "SUMMARY BOUNDING BOX: [(~a, ~a), (~a, ~a)]~%"
            *bb-min-x* *bb-min-y*
            *bb-max-x* *bb-max-y*)))

;; --------------------------------------------------------

(defun gui-choose-file-dialog ()
  (let ((fname (ltk:get-open-file)))
    (format t "file: '~a'~%" fname)
    (when (and fname (> (length fname) 0))
      (inspect-ogr-file fname))))

;; --------------------------------------------------------

(defun adjust-canvas-transform (ctx)
  "Setup affine transformation on the canvas to help with placing
rendered points at appropriate locations."

  ;; we have to calculate scale and shift based on the envelope. There
  ;; is a problem though: if the geographic projection rotates a
  ;; little bit, our top-right conner might be not the rightmost as
  ;; the result.

  (let* ((pe (pj:geo-transform (from-proj ctx)
                               (to-proj ctx)
                               ;; render min and max points at once
                               `((,(min-x ctx) ,(min-y ctx) 0.0d0) ; left-bottom
                                 (,(+ (min-x ctx) (extent-x ctx) 0.0d0) ; right-top
                                   ,(+ (min-y ctx) (extent-y ctx)) 0.0d0)
                                 (,(min-x ctx) ,(+ (min-y ctx) (extent-y ctx)) 0.0d0) ; left-top
                                 (,(+ (min-x ctx) (extent-x ctx) 0.0d0) ; right-bottom
                                   ,(min-y ctx) 0.0d0))
                               :degs T))
         (p.minx (iter (for p in pe) (minimize (first p))))
         (p.miny (iter (for p in pe) (minimize (second p))))
         (p.maxx (iter (for p in pe) (maximize (first p))))
         (p.maxy (iter (for p in pe) (maximize (second p))))
         (p.width (- p.maxx p.minx))   ; width of the projected extent
         (p.height (- p.maxy p.miny))
         (scale.x (/ (width ctx) p.width))
         (scale.y (* -1.0d0 (/ (height ctx) p.height)))
         (shift.x (* -1.0d0 p.minx scale.x))
         ;; scale.y is negative by this time, negate again to get
         ;; positive
         (shift.y (+ (* -1.0d0 p.miny scale.y) (height ctx))))
    ;; (format t "got pe: ([~a; ~a], [~,2f; ~,2f])~%" p.minx p.miny p.maxx p.maxy)
    ;; (format t "scale[~,5f; ~,5f] shift[~,2f; ~,2f]~%" scale.x scale.y shift.x shift.y)
    (cairo:translate shift.x shift.y)
    (cairo:scale scale.x scale.y)
    (format t "trans-matrix: ~a~%" (cairo:get-trans-matrix))))

;; --------------------------------------------------------

(defun gui-paint-layer (id)
  (let* ((layer (ogr:get-layer *ds* id))
         (layer-spatial-ref (if (ogr:get-spatial-ref layer)
                                (ogr:get-proj4 (ogr:get-spatial-ref layer))
                                *default-spatial-ref*))
	 ;; (extent (ogr:get-extent layer))
	 (extent-x (- *bb-max-x* *bb-min-x*))
	 (extent-y (- *bb-max-y* *bb-min-y*))
	 (ctx (make-instance '<paint-ctx>
			     :width *screen-x*
			     :height *screen-y*
			     :min-x *bb-min-x*
			     :min-y *bb-min-y*
			     :extent-x extent-x
			     :extent-y extent-y
                             :from-proj (pj:pj-init-plus layer-spatial-ref)
                             :to-proj (pj:pj-init-plus *target-spatial-ref*)
			     :canvas (cairo:create-image-surface :argb32 *screen-x* *screen-y*)))
	 (cairo-ctx (cairo:create-xlib-image-context *screen-x* *screen-y*)))

    (format t "painting layer ~a with spatial-ref: ~a~%"
	    (ogr:get-name layer)
	    layer-spatial-ref)

    (cairo:with-context (cairo-ctx)
      (adjust-canvas-transform ctx)
      (iter (for i from 0 below (ogr:get-feature-count layer))
	    (for feature = (ogr:get-feature layer i))
            (paint-feature ctx feature))
      
      ;;    (for geom = (ogr:get-geometry feature))
      ;;    (paint-geom ctx geom))
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
