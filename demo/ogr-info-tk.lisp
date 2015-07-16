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
  (ql:quickload :cl-proj))

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

(defvar *screen-x* 800)
(defvar *screen-y* 600)

(defclass <paint-ctx> ()
  ((width :accessor width :initarg :width :type fixnum)
   (height :accessor height :initarg :height :type fixnum)
   (min-x :accessor min-x :initarg :min-x)
   (min-y :accessor min-y :initarg :min-y)
   (extent-x :accessor extent-x :initarg :extent-x)
   (extent-y :accessor extent-y :initarg :extent-y)
   (scale-x :accessor scale-x)
   (scale-y :accessor scale-y)
   (canvas :accessor canvas :initarg :canvas :type ltk:canvas)))

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
      (ltk:make-oval (canvas ctx)
		     (tx ctx x) (ty ctx y)
		     (+ (tx ctx x) 1.0)
		     (+ (ty ctx y) 1.0))))

  (:method ((ctx <paint-ctx>) (geom ogr:<LINE-STRING>))
    (format t "paint LINE-STRING with ~a points~%" (ogr:get-point-count geom))
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
		(paint (ogr:get-geometry geom j))))
    )

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

    (iter (for i from 0 below (ogr:get-feature-count layer))
	  (for feature = (ogr:get-feature layer i))
	  (for geom = (ogr:get-geometry feature))
	  (paint ctx geom))))

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
