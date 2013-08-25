;;; -*- package: OGR; Syntax: Common-lisp; Base: 10 -*-

(in-package :ogr)

;; --------------------------------------------------------

(cffi:defcfun ("OGRRegisterAll" ogr-register-all) :void
  "Register all drivers.")

;; --------------------------------------------------------

(cffi:defctype data-source-h (:pointer :void)
  "OGRDataSourceH")

;; --------------------------------------------------------

(cffi:defcfun  ("OGROpen" ogr-open) :pointer
  (psz-name :string)
  (p-update :int)
  (path-driver-list :pointer)) ;; OGRSFDriverH

;; --------------------------------------------------------

(cffi:defcfun  ("OGR_DS_GetLayerByName" ogr-ds-get-layer-by-name) :pointer ; OGRLayerH
  "Fetch a layer by name.

The returned layer remains owned by the OGRDataSource and should not
be deleted by the application.

This function is the same as the C++ method OGRDataSource::GetLayerByName().

@argument[hDS]{handle to the data source from which to get the layer.}

@argument[pszLayerName]{Layer the layer name of the layer to fetch.}

@return{an handle to the layer, or NULL if the layer is not found or
an error occurs.}"
  (hDS :pointer)
  (pszLayerName :pointer))

;; --------------------------------------------------------

(cffi:defcfun ("OGR_L_ResetReading" ogr-l-reset-reading) :void

  "Reset feature reading to start on the first feature.

This affects GetNextFeature().

This function is the same as the C++ method OGRLayer::ResetReading().

@argument[hLayer]{handle to the layer on which features are read.}"

  (hLayer :pointer))			; OGRLayerH

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

;; --------------------------------------------------------

(cffi:defcfun ("OGR_L_GetLayerDefn" OGR-L-Get-Layer-Defn) :pointer
  "Fetch the schema information for this layer.

The returned handle to the OGRFeatureDefn is owned by the OGRLayer,
and should not be modified or freed by the application. It
encapsulates the attribute schema of the features of the layer.

This function is the same as the C++ method OGRLayer::GetLayerDefn().
Parameters:	hLayer 	handle to the layer to get the schema information.

@return{OGRFeatureDefnH an handle to the feature definition.}"
  (hLayer :pointer))			;

;; --------------------------------------------------------

(cffi:defcfun ("OGR_FD_GetFieldCount" OGR-FD-Get-Field-Count) :int

  "Fetch number of fields on the passed feature definition.

This function is the same as the C++ OGRFeatureDefn::GetFieldCount().
Parameters:	hDefn 	handle to the feature definition to get the fields count from.

@return{count of fields.}"
  (hDefn :pointer))			; OGRFeatureDefnH

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

(cffi:defcfun ("OGR_Fld_GetType" OGR_Fld_GetType) :int

  "Fetch type of this field.

This function is the same as the CPP method OGRFieldDefn::GetType().
Parameters:	hDefn 	handle to the field definition to get type from.

@return{OGRFieldType field type.}"
  (hDefn :pointer))			; OGRFieldDefnH

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

;; --------------------------------------------------------

(cffi:defcfun  ("OGR_F_GetGeometryRef" OGR_F_GetGeometryRef) :pointer ; OGRGeometryH	

  "Fetch an handle to feature geometry.

This function is the same as the C++ method OGRFeature::GetGeometryRef().
Parameters:	hFeat 	handle to the feature to get geometry from.

@return{an handle to internal feature geometry. This object should not
be modified.}"
  (hFeat :pointer))			; OGRFeatureH

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

(cffi:defcfun ("OGR_G_GetGeometryType" OGR-G-Get-Geometry-Type) :int ; OGRwkbGeometryType

  "Fetch geometry type.

Note that the geometry type may include the 2.5D flag. To get a 2D
flattened version of the geometry type apply the wkbFlatten() macro to
the return result.

This function is the same as the CPP method OGRGeometry::getGeometryType().
Parameters:	hGeom 	handle on the geometry to get type from.

@return{the geometry type code.}"
  (hGeom :pointer))			; OGRGeometryH

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

;; --------------------------------------------------------

(cffi:defcfun ("OGR_DS_Destroy" OGR-DS-Destroy) :void

  "Closes opened datasource and releases allocated resources.

This method is the same as the C++ method OGRDataSource::DestroyDataSource().
@argument[hDataSource]{handle to allocated datasource object.}"
  (hDataSource :pointer))		; OGRDataSourceH

;; EOF