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
  (let ((hDS (ogr:ogr-open shp 0 (cffi:null-pointer))))
    (when (cffi:null-pointer-p hds)
      (error "Failed to open file"))
    (format t "it contains: ~a layers~%" (ogr:ogr-ds-get-layer-count hds))

    (loop for i from 0 below (ogr:ogr-ds-get-layer-count hds) do
         (let ((layer (ogr:ogr-ds-get-layer hds i))
	       (envelope nil)) ; make cstruct ogr-envelope and pass pointer to it?
	   (ogr:ogr-l-get-extent layer envelope 0)
           (format t "layer[~a]: name=\"~a\", type=(~a)~%"
		   i
		   (ogr:ogr-l-get-name layer)
		   (ogr:ogr-l-get-geom-type layer))

	   (format t "has ~a features~%"
		   (loop
		      for feature = (ogr:ogr-l-get-next-feature layer) then (ogr:ogr-l-get-next-feature layer)
		      while (not (cffi:null-pointer-p feature))
		      count feature
		      do (ogr:ogr-f-destroy feature)))))))

;; --------------------------------------------------------

(defun init ()
  (ogr:ogr-register-all))

;; EOF