;;; -*- mode: lisp; -*-

(in-package :cl-user)

(defpackage :cl-gdal-asd
  (:use :cl :asdf))

(in-package :cl-gdal-asd)

(defsystem :cl-gdal
  :version "2013.08.25"	       ; YYYY.MM.DD -- digits to suit the ASDF
  :licence "BSD"
  :description "CL-GDAL is a minimal Common Lisp wrapper for GDAL and OGR libraries."
  :author "Victor Anyakin <anyakinvictor@yahoo.com>"
  :long-description
  "A minimal wrapper for GDAL and OGR libraries to perform basic IO operations."
  :components
  ((:module "src"
	    :components ((:file "ogr-package")
			 (:file "ogr" :depends-on ("ogr-package")))))
  :depends-on (:cffi))



