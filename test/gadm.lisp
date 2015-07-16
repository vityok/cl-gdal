;; Smoke-testing the bindings for the OGR library.
;;
;; These tests are focusing on read-only access to the geographic
;; files. Freely available shapefiles from the GADM database is used
;; as a data source for testing.
;;
;; The files can be acquired by running
;;
;; wget http://biogeo.ucdavis.edu/data/gadm2.5/shp/UKR_adm.zip

;; Running the tests:
;;
;; sbcl --load 'gadm.lisp' --eval '(ogr-gadm-test:run)' --quit
;; lx86cl --load 'gadm.lisp' --eval '(ogr-gadm-test:run)' --eval '(quit)'
;; ecl -load 'gadm.lisp' -eval '(ogr-gadm-test:run)' -eval '(quit)'

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cl-ogr)
  (ql:quickload :lisp-unit))

(defpackage :ogr-gadm-test
  (:use :cl :lisp-unit)
  (:export :run))

(in-package :ogr-gadm-test)

;; --------------------------------------------------------

(defvar *ds* nil)
(defparameter *gadm-0* "data/UKR_adm0.shp")

;; --------------------------------------------------------

(define-test basic-test
    (setf *ds* (ogr:open-data-source *gadm-0*))

  (assert-equal 1 (ogr:get-layer-count *ds*))

  (let ((layer (ogr:get-layer *ds* 0)))
    (assert-equal "UKR_adm0" (ogr:get-name layer))
    (assert-equal :wkb-polygon (ogr:get-geom-type layer))
    (assert-equal "+proj=longlat +datum=WGS84 +no_defs "
		  (ogr:get-proj4 (ogr:get-spatial-ref layer)))
    (assert-equal 1 (ogr:get-feature-count layer))))

  ;; extents=[22.14, 44.39, 40.22, 52.38]

;; --------------------------------------------------------

(setf lisp-unit:*print-summary* T
      lisp-unit:*print-failures* T
      lisp-unit:*print-errors* T)
(lisp-unit:use-debugger T)

(defun run ()
  (ogr:ogr-register-all)
  (lisp-unit:print-errors
   (lisp-unit:run-tests :all (find-package 'ogr-gadm-test))))

;; EOF
