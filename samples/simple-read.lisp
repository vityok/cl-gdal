;; Read data tutorial translation into the Common Lisp
;;
;; http://www.gdal.org/ogr/ogr_apitut.html


(ql:quickload :cl-ogr)
(ql:quickload :cffi)

(defun main (&optional (shp "point.shp") (layer-name "point"))
  "This is how the tutorial might look in Common Lisp using the bindings."

  (ogr:OGR-Register-All)
  (let ((hDS (ogr:OGR-Open shp 0 (cffi:null-pointer))) ; OGRDataSourceH
	(hLayer))				       ; OGRLayerH

    (when (cffi:null-pointer-p hds)
      (error "Open failed"))

    (format t "Number of layers in the data source: ~a~%"
	    (ogr:ogr-ds-get-layer-count hds))

    (setf hLayer (ogr:OGR-DS-Get-Layer-By-Name hDS layer-name))

    (when (cffi:null-pointer-p hLayer)
      (error "No such layer: ~a" layer-name))

    (ogr:OGR-L-Reset-Reading hLayer)

    (loop :for hFeature = (ogr:OGR-L-Get-Next-Feature hLayer)
       :until (cffi:null-pointer-p hFeature) :do
       (let ((hFDefn (ogr:OGR-L-Get-Layer-Defn hLayer))	; OGRFeatureDefnH
	     (hGeometry))				; OGRGeometryH

	 (loop :for iField :from 0 :below (ogr:OGR-FD-Get-Field-Count hFDefn)
	    :for hFieldDefn = (ogr:OGR-FD-Get-Field-Defn hFDefn iField ) :do
	    (case (ogr:OGR-Fld-Get-Type hFieldDefn)
	      (:OFTInteger
	       (format t "~a," (ogr:OGR-F-Get-Field-As-Integer hFeature iField)))
	      (:OFTReal
	       (format t "~a," (ogr:OGR-F-Get-Field-As-Double hFeature iField)))
	      (:OFTString
	       (format t "~a," (ogr:OGR-F-Get-Field-As-String hFeature iField)))
	      (t
	       (format t "~a," (ogr:OGR-F-Get-Field-As-String hFeature iField)))))

	 (setf hGeometry (ogr:OGR-F-Get-Geometry-Ref hFeature))
	 (if (and (not (cffi:null-pointer-p hGeometry))
		  (eql (ogr:wkb-flatten (ogr:OGR-G-Get-Geometry-Type hGeometry))
		       :wkbPoint))
	     (format t "~a,~a~%" (ogr:OGR-G-GetX hGeometry 0) (ogr:OGR-G-GetY hGeometry 0) )
	     (format t "no point geometry~%"))

	 (ogr:OGR-F-Destroy hFeature)))

    (ogr:OGR-DS-Destroy hDS)))

;; EOF