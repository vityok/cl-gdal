;;; -*- mode: lisp; -*-

(in-package :cl-user)

(defpackage :cl-ogr-asd
  (:use :cl :asdf))

(in-package :cl-ogr-asd)

(defsystem :cl-ogr
    :version "2013.08.25"      ; YYYY.MM.DD -- digits to suit the ASDF
    :licence "BSD"
    :description "CL-OGR is a minimal Common Lisp wrapper for the OGR library."
    :author "Victor Anyakin <anyakinvictor@yahoo.com>"
    :long-description
    "A minimal Common Lisp wrapper for OGR library to perform basic IO operations.

The OGR Simple Features Library is a C++ open source library (and
commandline tools) providing read (and sometimes write) access to a
variety of vector file formats including ESRI Shapefiles, S-57, SDTS,
PostGIS, Oracle Spatial, and Mapinfo mid/mif and TAB formats.

OGR is a part of the GDAL library."
    :serial t
    :components
    ((:module "src"
	      :components ((:file "ogr-package")
			   (:file "ogr")
			   (:file "ogr-datasource")
			   (:file "ogr-geometry"))))
    :depends-on (:cffi
		 :trivial-garbage))

;; EOF