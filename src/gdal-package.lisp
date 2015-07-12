;;; -*- package: CL-USER; Syntax: Common-lisp; Base: 10 -*-

(cffi:define-foreign-library libgdal
    (:unix (:or "libgdal.so.1" "libgdal.so"
		"libgdal1.6.0.so.1"
		"libgdal1.6.0.so.1.13.3"))
  (t (:default "libgdal")))

(cffi:use-foreign-library libgdal)

;; --------------------------------------------------------

(defpackage :cl-gdal
  (:nicknames :gdal)
  (:use :cl)
  (:export #:gdal-all-register))

;; EOF