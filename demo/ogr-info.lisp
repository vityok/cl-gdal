;; open vector geographic files and display information about layers
;; and features that they contain
;;
;; This program is intended to serve as a demonstration and a
;; prototype to check how useful and convenient OGR library Lisp API
;; is in practice.
;;
;; Load:
;;
;; sbcl --load ogr-info.lisp --eval '(ogr-info-demo:init)'
;; lx86cl --load ogr-info.lisp --eval '(ogr-info-demo:init)'


(ql:quickload :cffi)
(ql:quickload :cl-ogr)

(defpackage :ogr-info-demo
  (:nicknames :oi)
  (:use :cl)
  (:export #:inspect-ogr-file #:init))

(in-package :ogr-info-demo)

;; --------------------------------------------------------

(defun inspect-ogr-file (shp)
  (format t "inspecting file: ~a~%" shp)
  (let ((ds (ogr:open-data-source shp)))
    (unless ds
      (error "Failed to open file"))
    (format t "it contains: ~a layers~%" (ogr:get-layer-count ds))

    (loop for i from 0 below (ogr:get-layer-count ds) do
         (let ((layer (ogr:get-layer ds i)))
	   (cffi:with-foreign-object (envelope '(:struct ogr:ogr-envelope))
	     (ogr:ogr-l-get-extent (ogr:pointer layer) envelope 0)
	     (cffi:with-foreign-slots ((ogr:minx ogr:maxx ogr:miny ogr:maxy) envelope (:struct ogr:ogr-envelope))
	       (format t "LAYER[~a]:
  name=\"~a\",
  type=(~a),
  extents=[~,2F, ~,2F, ~,2F, ~,2F]~%"
		       i
		       (ogr:get-name layer)
		       (ogr:get-geom-type layer)
		       ogr:minx ogr:miny ogr:maxx ogr:maxy))
	     (format t "  spatial ref: ~a~%" (ogr:get-proj4 (ogr:get-spatial-ref layer)))

	     (format t "has ~a features~%"
		     (loop
			for feature = (ogr:get-next-feature layer)
			while feature
			count feature)))))))

;; --------------------------------------------------------

(defun init ()
  (ogr:ogr-register-all))

;; EOF
