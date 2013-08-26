;; Read data tutorial translation into the Common Lisp
;;
;; http://www.gdal.org/ogr/ogr_apitut.html


(ql:quickload :ogr)
(ql:quickload :cffi)

(defun main ()
  "This is how the tutorial might look in Common Lisp using the bindings."

  (ogr:OGR-Register-All)
  (let ((hDS (ogr:OGR-Open "point.shp" NIL (cffi:null-pointer))) ; OGRDataSourceH
	(hLayer)			; OGRLayerH
	(hFeature))			; OGRFeatureH

    (when (cffi:null-pointer-p hds)
      (error "Open failed"))

    (setf hLayer (ogr:OGR-DS-Get-Layer-By-Name hDS "point"))

    (OGR-L-Reset-Reading hLayer)

    (loop :for hFeature = (ogr:OGR-L-Get-Next-Feature hLayer)
       :until (cffi:null-pointer-p hFeature) :do
       (let ((hFDefn (ogr:OGR-L-Get-Layer-Defn hLayer))	; OGRFeatureDefnH
	     (iField)					; int
	     (hGeometry))				; OGRGeometryH

	 (loop :for iField = 0 :below (ogr:OGR-FD-Get-Field-Count hFDefn)
	    :for hFieldDefn = (ogr:OGR-FD-Get-Field-Defn hFDefn iField ) :do
	    (case (ogr:OGR-Fld-Get-Type hFieldDefn)
	      (:OFTInteger
	       (format t "%d," (ogr:OGR-F-Get-Field-As-Integer hFeature iField)))
	      (:OFTReal
	       (format t "%.3f," (ogr:OGR-F-Get-Field-As-Double hFeature iField)))
	      (:OFTString
	       (format t "%s," (ogr:OGR-F-Get-Field-As-String hFeature iField)))
	      (t
	       (format t "%s," (ogr:OGR-F-Get-Field-As-String hFeature iField)))))

	 (setf hGeometry (ogr:OGR-F-Get-Geometry-Ref hFeature))
	 (if (and (not (cffi:null-pointer-p hGeometry))
		  (= (ogr:wkbFlatten (ogr:OGR-G-Get-Geometry-Type hGeometry))
		     :wkbPoint))
	     (format t "%.3f,%3.f~%" (ogr:OGR-G-GetX hGeometry 0) (ogr:OGR-G-GetY hGeometry 0) )
	     (format t "no point geometry~%"))

	 (ogr:OGR-F-Destroy hFeature)))

    (OGR-DS-Destroy hDS)))

;; EOF