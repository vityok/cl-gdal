
(cffi:define-foreign-library libgdal
    (:unix (:or "libgdal.so.1" "libgdal.so"))
  (t (:default "libgdal")))

(cffi:use-foreign-library libgdal)

(defpackage :cl-gdal
  (:nicknames :gdal)
  (:use :cl)
  (:export #:data-source))

;; EOF