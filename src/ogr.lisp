;;; -*- package: OGR; Syntax: Common-lisp; Base: 10 -*-

;; based on the auto-generated documentation at:
;; http://www.gdal.org/ogr/ogr__api_8h.html

(in-package :ogr)

;; --------------------------------------------------------

(defclass ogr-class ()
  ((pointer
    :type (or null cffi:foreign-pointer)
    :initarg :pointer
    :accessor pointer
    :initform nil)))

(defclass data-source (ogr-class)
  ;; original documentation at: http://www.gdal.org/ogr/classOGRDataSource.html
  (:documentation "This class represents a data source.

  A data source potentially consists of many layers (OGRLayer). A data
  source normally consists of one, or a related set of files, though
  the name doesn't have to be a real item in the file system.

  When an OGRDataSource is destroyed, all it's associated OGRLayers
  objects are also destroyed.")
  ())

(defclass layer (ogr-class)
  ((data-source
    :type (or null data-source)
    :initarg :data-source
    :accessor data-source
    :initform nil)))

(defclass geometry (ogr-class)
  ((data-source
    :type (or null data-source)
    :initarg :data-source
    :accessor data-source
    :initform nil)))

;; --------------------------------------------------------

(cffi:defcfun ("OGRRegisterAll" ogr-register-all) :void
  "Register all drivers.")

(export 'ogr-register-all)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_L_ResetReading" ogr-l-reset-reading) :void

  "Reset feature reading to start on the first feature.

This affects GetNextFeature().

This function is the same as the C++ method OGRLayer::ResetReading().

@argument[hLayer]{handle to the layer on which features are read.}"

  (hLayer :pointer))			; OGRLayerH

(export 'ogr-l-reset-reading)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_L_GetNextFeature" ogr-l-get-next-feature) :pointer ; OGRFeatureH

  "Fetch the next available feature from this layer.

The returned feature becomes the responsiblity of the caller to delete
with OGR_F_Destroy(). It is critical that all features associated with
an OGRLayer (more specifically an OGRFeatureDefn) be deleted before
that layer/datasource is deleted.

Only features matching the current spatial filter (set with
SetSpatialFilter()) will be returned.

This function implements sequential access to the features of a
layer. The OGR_L_ResetReading() function can be used to start at the
beginning again.

This function is the same as the C++ method OGRLayer::GetNextFeature().

Parameters:	hLayer 	handle to the layer from which feature are read.

@return{an handle to a feature, or NULL if no more features are available.}"

  (hLayer :pointer))			; OGRLayerH

(export 'ogr-l-get-next-feature)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_L_GetLayerDefn" OGR-L-Get-Layer-Defn) :pointer
  "Fetch the schema information for this layer.

The returned handle to the OGRFeatureDefn is owned by the OGRLayer,
and should not be modified or freed by the application. It
encapsulates the attribute schema of the features of the layer.

This function is the same as the C++ method OGRLayer::GetLayerDefn().
Parameters:	hLayer 	handle to the layer to get the schema information.

@return{OGRFeatureDefnH an handle to the feature definition.}"
  (hLayer :pointer))

(export 'OGR-L-Get-Layer-Defn)			;

;; --------------------------------------------------------

(cffi:defcfun ("OGR_FD_GetFieldCount" OGR-FD-Get-Field-Count) :int

  "Fetch number of fields on the passed feature definition.

This function is the same as the C++ OGRFeatureDefn::GetFieldCount().
Parameters:	hDefn 	handle to the feature definition to get the fields count from.

@return{count of fields.}"
  (hDefn :pointer))			; OGRFeatureDefnH

(export 'OGR-FD-Get-Field-Count)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_FD_GetFieldDefn" OGR-FD-Get-Field-Defn) :pointer

  "Fetch field definition of the passed feature definition.

This function is the same as the C++ method OGRFeatureDefn::GetFieldDefn().

Starting with GDAL 1.7.0, this method will also issue an error if the index is not valid.
Parameters:	hDefn 	handle to the feature definition to get the field definition from.
	iField 	the field to fetch, between 0 and GetFieldCount()-1.

@return{OGRFieldDefnH an handle to an internal field definition object
or NULL if invalid index. This object should not be modified or freed
by the application.}"
  (hDefn :pointer)			; OGRFeatureDefnH
  (iField :int))

(export 'OGR-FD-Get-Field-Defn)

;; --------------------------------------------------------

(cffi:defcenum OGR-Field-Type
  "List of feature field types. This list is likely to be extended in
the future ... avoid coding applications based on the assumption that
all field types can be known. "

  (:OFTInteger 0)		     ; Simple 32bit integer
  (:OFTIntegerList 1)		     ; List of 32bit integers
  (:OFTReal 2)			     ; Double Precision floating point
  (:OFTRealList 3)		     ; List of doubles
  (:OFTString 4)		     ; String of ASCII chars
  (:OFTStringList 5)		     ; Array of strings
  (:OFTWideString 6)		     ; deprecated
  (:OFTWideStringList 7)	     ; deprecated
  (:OFTBinary 8)		     ; Raw Binary data
  (:OFTDate 9)			     ; Date
  (:OFTTime 10)			     ; Time
  (:OFTDateTime 11))		     ; Date and Time

;; --------------------------------------------------------

(cffi:defcenum OGR-Err

    (:NONE                0)
  (:NOT_ENOUGH_DATA     1)
  (:NOT_ENOUGH_MEMORY   2)
  (:UNSUPPORTED_GEOMETRY_TYPE 3)
  (:UNSUPPORTED_OPERATION 4)
  (:CORRUPT_DATA        5)
  (:FAILURE             6)
  (:UNSUPPORTED_SRS     7)
  (:INVALID_HANDLE      8))


;; --------------------------------------------------------

(cffi:defcfun ("OGR_Fld_GetType" OGR-Fld-Get-Type) :int

  "Fetch type of this field.

This function is the same as the CPP method OGRFieldDefn::GetType().
Parameters:	hDefn 	handle to the field definition to get type from.

@return{OGRFieldType field type.}"
  (hDefn :pointer))			; OGRFieldDefnH

(export 'OGR-Fld-Get-Type)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_F_GetFieldAsInteger" OGR-F-Get-Field-As-Integer) :int

  "Fetch field value as integer.

OFTString features will be translated using atoi(). OFTReal fields
will be cast to integer. Other field types, or errors will result in a
return value of zero.

This function is the same as the C++ method OGRFeature::GetFieldAsInteger().
Parameters:	hFeat 	handle to the feature that owned the field.
	iField 	the field to fetch, from 0 to GetFieldCount()-1.

@return{the field value.}"
  (hFeat :pointer)			; OGRFeatureH
  (iField :int))

(export 'OGR-F-Get-Field-As-Integer)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_F_GetFieldAsDouble" OGR-F-Get-Field-As-Double) :double 	


  "Fetch field value as a double.

OFTString features will be translated using atof(). OFTInteger fields
will be cast to double. Other field types, or errors will result in a
return value of zero.

This function is the same as the C++ method OGRFeature::GetFieldAsDouble().
Parameters:	hFeat 	handle to the feature that owned the field.
	iField 	the field to fetch, from 0 to GetFieldCount()-1.

Returns:
the field value."

  (hFeat :pointer)			; OGRFeatureH
  (iField :int))

(export 'OGR-F-Get-Field-As-Double)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_F_GetFieldAsString" OGR-F-Get-Field-As-String) :string 	

  "Fetch field value as a string.

OFTReal and OFTInteger fields will be translated to string using
sprintf(), but not necessarily using the established formatting
rules. Other field types, or errors will result in a return value of
zero.

This function is the same as the C++ method OGRFeature::GetFieldAsString().
Parameters:	hFeat 	handle to the feature that owned the field.
	iField 	the field to fetch, from 0 to GetFieldCount()-1.

@return{the field value. This string is internal, and should not be
modified, or freed. Its lifetime may be very brief.}"
  (hFeat :pointer)			; OGRFeatureH
  (iField :int))

(export 'OGR-F-Get-Field-As-String)

;; --------------------------------------------------------

(cffi:defcfun  ("OGR_F_GetGeometryRef" OGR-F-Get-Geometry-Ref) :pointer ; OGRGeometryH	

  "Fetch an handle to feature geometry.

This function is the same as the C++ method OGRFeature::GetGeometryRef().
Parameters:	hFeat 	handle to the feature to get geometry from.

@return{an handle to internal feature geometry. This object should not
be modified.}"
  (hFeat :pointer))			; OGRFeatureH

(export 'OGR-F-Get-Geometry-Ref)

;; --------------------------------------------------------

(cffi:defcenum OGR-wkb-Geometry-Type
    "List of well known binary geometry types. These are used within the
BLOBs but are also returned from OGRGeometry::getGeometryType() to
identify the type of a geometry object."
  (:wkbUnknown 0)	; unknown type, non-standard
  (:wkbPoint 1)		; 0-dimensional geometric object, standard WKB
  (:wkbLineString 2)	; 1-dimensional geometric object with linear
					; interpolation between Points, standard WKB
  (:wkbPolygon 3) ; planar 2-dimensional geometric object defined by 1
					; exterior boundary and 0 or more interior
					; boundaries, standard WKB
  (:wkbMultiPoint 4)      ; GeometryCollection of Points, standard WKB
  (:wkbMultiLineString 5) ; GeometryCollection of LineStrings, standard WKB
  (:wkbMultiPolygon 6)	; GeometryCollection of Polygons, standard WKB
  (:wkbGeometryCollection 7)   ; geometric object that is a collection
					; of 1 or more geometric objects,
					; standard WKB
  (:wkbNone 100)	    ; non-standard, for pure attribute records
  (:wkbLinearRing 101)	    ; non-standard, just for createGeometry()
  (:wkbPoint25D #x80000001) ; 2.5D extension as per 99-402
  (:wkbLineString25D #x80000002)	; 2.5D extension as per 99-402
  (:wkbPolygon25D #x80000003)		; 2.5D extension as per 99-402
  (:wkbMultiPoint25D #x80000004)	; 2.5D extension as per 99-402
  (:wkbMultiLineString25D #x80000005)	; 2.5D extension as per 99-402
  (:wkbMultiPolygon25D #x80000006)	; 2.5D extension as per 99-402
  (:wkbGeometryCollection25D #x80000007)) ; 2.5D extension as per 99-402

;; --------------------------------------------------------

(cffi:defcfun ("OGR_F_Destroy" OGR-F-Destroy) :void 	

  "Destroy feature.

The feature is deleted, but within the context of the GDAL/OGR
heap. This is necessary when higher level applications use GDAL/OGR
from a DLL and they want to delete a feature created within the
DLL. If the delete is done in the calling application the memory will
be freed onto the application heap which is inappropriate.

This function is the same as the C++ method OGRFeature::DestroyFeature().
Parameters:	hFeat 	handle to the feature to destroy."
  (hFeat :pointer))			; OGRFeatureH

(export 'OGR-F-Destroy)

;; --------------------------------------------------------

(defconstant +wkb25DBit+ #x80000000)

(defun wkb-flatten (x)
  "The wkb-flatten function is used above to convert the type for a
:wkbPoint25D (a point with a z coordinate) into the base 2D geometry
type code (:wkbPoint). For each 2D geometry type there is a
corresponding 2.5D type code. The 2D and 2.5D geometry cases are
handled by the same C++ class, so our code will handle 2D or 3D cases
properly."

  (let ((%x (if (keywordp x)
		(cffi:foreign-enum-value 'OGR-wkb-Geometry-Type x)
		x)))
    (cffi:foreign-enum-keyword 'OGR-wkb-Geometry-Type
			       (logand %x (lognot +wkb25DBit+)))))
(export 'wkb-flatten)


;; --------------------------------------------------------




;; EOF