;;; -*- package: OGR; Syntax: Common-lisp; Base: 10 -*-

;; OGRGeometry <ogr_api.h>

(in-package :ogr)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_GetX" ogr-g-getx) :double
  (geom :pointer)
  (i :int))
(export 'ogr-g-getx)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_GetY" ogr-g-gety) :double
  (geom :pointer)
  (i :int))
(export 'ogr-g-gety)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_GetZ" ogr-g-getz) :double
  (geom :pointer)
  (i :int))
(export 'ogr-g-getz)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_CreateFromWkb" OGR-G-Create-From-Wkb) :int
  "Create a geometry object of the appropriate type from it's well
 known binary representation.

 Note that if nBytes is passed as zero, no checking can be done on
 whether the pabyData is sufficient. This can result in a crash if the
 input data is corrupt. This function returns no indication of the
 number of bytes from the data source actually used to represent the
 returned geometry object. Use OGR_G_WkbSize() on the returned
 geometry to establish the number of bytes it required in WKB format.

 The OGRGeometryFactory::createFromWkb() CPP method is the same as this function.

 @argument[pabyData]{pointer to the input BLOB data.}

 @argument[hSRS]{handle to the spatial reference to be assigned to the
 created geometry object. This may be NULL.}

 @argument[phGeometry]{the newly created geometry object will be
 assigned to the indicated handle on return. This will be NULL in case
 of failure. If not NULL, *phGeometry should be freed with
 OGR_G_DestroyGeometry() after use.}

 @argument[nBytes]{the number of bytes of data available in pabyData,
 or -1 if it is not known, but assumed to be sufficient.}

 @return{OGRERR_NONE if all goes well, otherwise any of
 OGRERR_NOT_ENOUGH_DATA, OGRERR_UNSUPPORTED_GEOMETRY_TYPE, or
 OGRERR_CORRUPT_DATA may be returned.}"
  (pabyData :string)
  (hSRS :pointer)			; OGRSpatialReferenceH
  (phGeometry :pointer)			; OGRGeometryH *
  (nBytes :int))
(export 'OGR-G-Create-From-Wkb)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_CreateFromWkt" OGR-G-Create-From-Wkt) :int
  "Create a geometry object of the appropriate type from it's well known text representation.

 The OGRGeometryFactory::createFromWkt CPP method is the same as this function.

 @argument[ppszData]{input zero terminated string containing well
 known text representation of the geometry to be created. The pointer
 is updated to point just beyond that last character consumed.}

 @argument[hSRS]{handle to the spatial reference to be assigned to the
 created geometry object. This may be NULL.}

 @argument[phGeometry]{the newly created geometry object will be
 assigned to the indicated handle on return. This will be NULL if the
 method fails. If not NULL, *phGeometry should be freed with
 OGR_G_DestroyGeometry() after use.}

 @return{OGRERR_NONE if all goes well, otherwise any of
 OGRERR_NOT_ENOUGH_DATA, OGRERR_UNSUPPORTED_GEOMETRY_TYPE, or
 OGRERR_CORRUPT_DATA may be returned.}"
  (ppszData :pointer)			; char **
  (hSRS :pointer)			; OGRSpatialReferenceH
  (phGeometry :pointer))		; OGRGeometryH *
(export 'OGR-G-Create-From-Wkt)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_DestroyGeometry" OGR-G-Destroy-Geometry) :void
  "Destroy geometry object."
  (geom :pointer))

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_CreateGeometry" %OGR-G-Create-Geometry) :pointer ; OGRGeometryH
  (eGeometryType :int))			; OGR-wkb-Geometry-Type

(defun OGR-G-Create-Geometry (eGeometryType)
  "Create an empty geometry of desired type.

 This is equivalent to allocating the desired geometry with new, but
 the allocation is guaranteed to take place in the context of the
 GDAL/OGR heap.

 This function is the same as the CPP method
 OGRGeometryFactory::createGeometry.

 @argument[eGeometryType]{the type code of the geometry to be created.}

 @return{handle to the newly create geometry or NULL on
 failure. Should be freed with OGR_G_DestroyGeometry() after use.}"
  (%ogr-g-create-geometry (cffi:foreign-enum-value 'ogr-wkb-geometry-type
						   eGeometryType)))
(export 'ogr-g-create-geometry)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_ApproximateArcAngles" OGR-G-Approximate-Arc-Angles) :pointer ;OGRGeometryH
  "Stroke arc to linestring.

Stroke an arc of a circle to a linestring based on a center point,
radius, start angle and end angle, all angles in degrees.

If the dfMaxAngleStepSizeDegrees is zero, then a default value will be
used. This is currently 4 degrees unless the user has overridden the
value with the OGR_ARC_STEPSIZE configuration variable.

See also:
CPLSetConfigOption()

Parameters:	dfCenterX 	center X
	dfCenterY 	center Y
	dfZ 	center Z
	dfPrimaryRadius 	X radius of ellipse.
	dfSecondaryRadius 	Y radius of ellipse.
	dfRotation 	rotation of the ellipse clockwise.
	dfStartAngle 	angle to first point on arc (clockwise of X-positive)
	dfEndAngle 	angle to last point on arc (clockwise of X-positive)
	dfMaxAngleStepSizeDegrees 	the largest step in degrees along the arc, zero to use the default setting.

Returns:
OGRLineString geometry representing an approximation of the arc.
Since:
OGR 1.8.0"

  (dfCenterX :double)
  (dfCenterY :double)
  (dfZ :double)
  (dfPrimaryRadius :double)
  (dfSecondaryRadius :double)
  (dfRotation :double)
  (dfStartAngle :double)
  (dfEndAngle :double)
  (dfMaxAngleStepSizeDegrees :double))
(export 'OGR-G-Approximate-Arc-Angles)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_ForceToPolygon" OGR-G-Force-To-Polygon) :pointer ; OGRGeometryH
  "Convert to polygon.

 This function is the same as the C++ method
 OGRGeometryFactory::forceToPolygon().

 @argument[hGeom]{handle to the geometry to convert (ownership
 surrendered).}

 @return{the converted geometry (ownership to caller).}

 Since: GDAL/OGR 1.8.0"
  (hGeom :pointer))			; OGRGeometryH
(export 'OGR-G-Force-To-Polygon)

;; --------------------------------------------------------

(cffi:defcfun  ("OGR_G_ForceToMultiLineString" OGR_G_ForceToMultiLineString) :pointer ; OGRGeometryH
  "Convert to multilinestring.

 This function is the same as the C++ method
 OGRGeometryFactory::forceToMultiLineString().

 @argument[hGeom]{handle to the geometry to convert (ownership surrendered).}

 @return{the converted geometry (ownership to caller).}

 Since: GDAL/OGR 1.8.0"
  (hGeom :pointer))			; OGRGeometryH

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_ForceToMultiPolygon" OGR-G-Force-To-Multi-Polygon) :pointer ; OGRGeometryH
  "Convert to multipolygon.

 This function is the same as the C++ method
 OGRGeometryFactory::forceToMultiPolygon().

 @argument[hGeom]{handle to the geometry to convert (ownership
 surrendered).}

 @return{the converted geometry (ownership to caller).}

 Since: GDAL/OGR 1.8.0"
  (hGeom :pointer))			; OGRGeometryH

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_ForceToMultiPoint" OGR-G-Force-To-Multi-Point) :pointer ; OGRGeometryH
  "Convert to multipoint.

 This function is the same as the C++ method
 OGRGeometryFactory::forceToMultiPoint().

 @argument[hGeom]{handle to the geometry to convert (ownership
 surrendered).}

 @return{the converted geometry (ownership to caller).}

 Since: GDAL/OGR 1.8.0"
  (hGeom :pointer))			; OGRGeometryH

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_ForceToMultiLineString" OGR-G-Force-To-Multi-Line-String) :pointer ; OGRGeometryH
  "Convert to multilinestring.

 This function is the same as the C++ method
 OGRGeometryFactory::forceToMultiLineString().

 @argument[hGeom]{handle to the geometry to convert (ownership surrendered).}

 @return{the converted geometry (ownership to caller).}

 Since: GDAL/OGR 1.8.0"
  (hGeom :pointer))			; OGRGeometryH

;; --------------------------------------------------------


(cffi:defcfun ("OGR_G_GetDimension" OGR-G-Get-Dimension) :int
  "Get the dimension of this geometry.

 This function corresponds to the SFCOM IGeometry::GetDimension()
 method. It indicates the dimension of the geometry, but does not
 indicate the dimension of the underlying space (as indicated by
 OGR_G_GetCoordinateDimension() function).

 This function is the same as the CPP method
 OGRGeometry::getDimension().

 @argument[hGeom]{handle on the geometry to get the dimension from.}
 @return{0 for points, 1 for lines and 2 for surfaces.}"
  (hGeom :pointer))			; OGRGeometryH

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_GetCoordinateDimension" OGR-G-Get-Coordinate-Dimension) :int
  "Get the dimension of the coordinates in this geometry.

 This function corresponds to the SFCOM IGeometry::GetDimension()
 method.

 This function is the same as the CPP method
 OGRGeometry::getCoordinateDimension().

 @argument[hGeom]{handle on the geometry to get the dimension of the
 coordinates from.}

 @return{in practice this will return 2 or 3. It can also return 0 in
 the case of an empty point.}"
  (hGeom :pointer))			; OGRGeometryH

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_SetCoordinateDimension" OGR-G-Set-Coordinate-Dimension) :void
  "Set the coordinate dimension.

 This method sets the explicit coordinate dimension. Setting the
 coordinate dimension of a geometry to 2 should zero out any existing
 Z values. Setting the dimension of a geometry collection will not
 necessarily affect the children geometries.

 @argument[hGeom]{handle on the geometry to set the dimension of the
 coordinates.}

 @argument[nNewDimension]{New coordinate dimension value, either 2 or
 3.}"
  (hGeom :pointer)			; OGRGeometryH
  (nNewDimension :int))

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_Clone" OGR-G-Clone) :pointer ; OGRGeometryH
  "Make a copy of this object.

 This function relates to the SFCOM IGeometry::clone() method.

 This function is the same as the CPP method OGRGeometry::clone().

 @argument[hGeom]{handle on the geometry to clone from.}

 @return{an handle on the copy of the geometry with the spatial
 reference system as the original.}"
  (hGeom :pointer))			; OGRGeometryH

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_GetEnvelope" OGR-G-Get-Envelope) :void
  "Computes and returns the bounding envelope for this geometry in
 the passed psEnvelope structure.

 This function is the same as the CPP method OGRGeometry::getEnvelope().

 @argument[hGeom]{handle of the geometry to get envelope from.}
 @argument[psEnvelope]{the structure in which to place the results.}"

  (hGeom :pointer)			; OGRGeometryH
  (psEnvelope :pointer))		; OGREnvelope*


;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_GetEnvelope3D" OGR-G-Get-Envelope3D) :void
  "Computes and returns the bounding envelope (3D) for this geometry in
 the passed psEnvelope structure.

 This function is the same as the CPP method
 OGRGeometry::getEnvelope().

 @argument[hGeom]{handle of the geometry to get envelope from.}
 @argument[psEnvelope]{the structure in which to place the results.}

 Since: OGR 1.9.0"
  (hGeom :pointer)			; OGRGeometryH
  (psEnvelope :pointer))		; OGREnvelope3D*
	

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_ImportFromWkb" OGR-G-Import-From-Wkb) :int ; OGRErr
  "Assign geometry from well known binary data.

 The object must have already been instantiated as the correct derived
 type of geometry object to match the binaries type.

 This function relates to the SFCOM IWks::ImportFromWKB() method.

 This function is the same as the CPP method OGRGeometry::importFromWkb().

 @argument[hGeom]{handle on the geometry to assign the well know
 binary data to.}
 @argument[pabyData]{the binary input data.}
 @argument[nSize]{the size of pabyData in bytes, or zero if not known.}

 @return{NONE if all goes well, otherwise any
 of :NOT_ENOUGH_DATA, :UNSUPPORTED_GEOMETRY_TYPE, or :CORRUPT_DATA may
 be returned.}"
  (hGeom :pointer)			; OGRGeometryH
  (pabyData :string)			; unsigned char * 	
  (nSize :int))

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_ExportToWkb" OGR-G-Export-To-Wkb) :int ; OGRErr
  "Convert a geometry into well known binary format.

 This function relates to the SFCOM IWks::ExportToWKB() method.

 This function is the same as the CPP method OGRGeometry::exportToWkb().

 @argument[hGeom]{handle on the geometry to convert to a well know
 binary data from.}
 @argument[eOrder]{One of wkbXDR or wkbNDR indicating MSB or LSB byte
 order respectively.}
 @argument[pabyDstBuffer]{a buffer into which the binary
 representation is written. This buffer must be at least
 OGR_G_WkbSize() byte in size.}
 @return{Currently OGRERR_NONE is always returned.}"
  (hGeom :pointer)			; OGRGeometryH
  (eOrder :int)				; OGRwkbByteOrder
  (pabyDstBuffer :string))		; unsigned char *


;; --------------------------------------------------------

(cffi:defcfun int OGR_G_WkbSize
  "Returns size of related binary representation.

 This function returns the exact number of bytes required to hold the
 well known binary representation of this geometry object. Its
 computation may be slightly expensive for complex geometries.

 This function relates to the SFCOM IWks::WkbSize() method.

 This function is the same as the CPP method OGRGeometry::WkbSize().

 @argument[hGeom]{handle on the geometry to get the binary size from.}

 @return{size of binary representation in bytes.}"
  (hGeom :pointer))			; OGRGeometryH
;; --------------------------------------------------------

(cffi:defcfun OGRErr OGR_G_ImportFromWkt


Assign geometry from well known text data.

The object must have already been instantiated as the correct derived type of geometry object to match the text type.

This function relates to the SFCOM IWks::ImportFromWKT() method.

This function is the same as the CPP method OGRGeometry::importFromWkt().
@argument[hGeom 	handle on the geometry to assign well know text data to.
	ppszSrcText 	pointer to a pointer to the source text. The pointer is updated to pointer after the consumed text.

@return{OGRERR_NONE if all goes well, otherwise any of OGRERR_NOT_ENOUGH_DATA, OGRERR_UNSUPPORTED_GEOMETRY_TYPE, or OGRERR_CORRUPT_DATA may be returned.
(	OGRGeometryH 	hGeom,
		char ** 	ppszSrcText
	)

;; --------------------------------------------------------

(cffi:defcfun OGRErr OGR_G_ExportToWkt


Convert a geometry into well known text format.

This function relates to the SFCOM IWks::ExportToWKT() method.

This function is the same as the CPP method OGRGeometry::exportToWkt().
@argument[hGeom 	handle on the geometry to convert to a text format from.
	ppszSrcText 	a text buffer is allocated by the program, and assigned to the passed pointer. After use, *ppszDstText should be freed with OGRFree().

@return{Currently OGRERR_NONE is always returned.
(	OGRGeometryH 	hGeom,
		char ** 	ppszSrcText
	)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_GetGeometryType" %OGR-G-Get-Geometry-Type) :int ; OGRwkbGeometryType
  (hGeom :pointer))			; OGRGeometryH

(defun OGR-G-Get-Geometry-Type (hGeom)
  "Fetch geometry type.

 Note that the geometry type may include the 2.5D flag. To get a 2D
 flattened version of the geometry type apply the wkbFlatten() macro
 to the return result.

 This function is the same as the CPP method
 OGRGeometry::getGeometryType().

 @argument[hGeom]{handle on the geometry to get type from.}

 @return{the geometry type code.}"
  (cffi:foreign-enum-keyword 'OGR-wkb-Geometry-Type
			     (%OGR-G-Get-Geometry-Type hGeom)))
(export 'OGR-G-Get-Geometry-Type)

;; --------------------------------------------------------

(cffi:defcfun const char* OGR_G_GetGeometryName


Fetch WKT name for geometry type.

There is no SFCOM analog to this function.

This function is the same as the CPP method OGRGeometry::getGeometryName().
@argument[hGeom 	handle on the geometry to get name from.

@return{name used for this geometry type in well known text format.

(	OGRGeometryH 	hGeom	 )

;; TODO
;; --------------------------------------------------------

(cffi:defcfun void OGR_F_DumpReadable


Dump this feature in a human readable form.

This dumps the attributes, and geometry; however, it doesn't definition information (other than field types and names), nor does it report the geometry spatial reference system.

This function is the same as the C++ method OGRFeature::DumpReadable().
@argument[hFeat 	handle to the feature to dump.
	fpOut 	the stream to write to, such as strout.
(	OGRFeatureH 	hFeat,
		FILE * 	fpOut
	)

;; --------------------------------------------------------

(cffi:defcfun void OGR_G_FlattenTo2D


Convert geometry to strictly 2D. In a sense this converts all Z coordinates to 0.0.

This function is the same as the CPP method OGRGeometry::flattenTo2D().
@argument[hGeom 	handle on the geometry to convert.
(	OGRGeometryH 	hGeom	 )

;; --------------------------------------------------------

(cffi:defcfun void OGR_G_CloseRings


Force rings to be closed.

If this geometry, or any contained geometries has polygon rings that are not closed, they will be closed by adding the starting point at the end.
@argument[hGeom 	handle to the geometry.
(	OGRGeometryH 	hGeom	 )

;; --------------------------------------------------------

(cffi:defcfun OGRGeometryH OGR_G_CreateFromGML


Create geometry from GML.

This method translates a fragment of GML containing only the geometry portion into a corresponding OGRGeometry. There are many limitations on the forms of GML geometries supported by this parser, but they are too numerous to list here.

The following GML2 elements are parsed : Point, LineString, Polygon, MultiPoint, MultiLineString, MultiPolygon, MultiGeometry.

(OGR >= 1.8.0) The following GML3 elements are parsed : Surface, MultiSurface, PolygonPatch, Triangle, Rectangle, Curve, MultiCurve, CompositeCurve, LineStringSegment, Arc, Circle, CompositeSurface, OrientableSurface, Solid, Tin, TriangulatedSurface.

Arc and Circle elements are stroked to linestring, by using a 4 degrees step, unless the user has overridden the value with the OGR_ARC_STEPSIZE configuration variable.

The C++ method OGRGeometryFactory::createFromGML() is the same as this function.
@argument[pszGML 	The GML fragment for the geometry.

@return{a geometry on succes, or NULL on error.
(	const char * 	pszGML	 )

;; --------------------------------------------------------

(cffi:defcfun char* OGR_G_ExportToGML


Convert a geometry into GML format.

The GML geometry is expressed directly in terms of GML basic data types assuming the this is available in the gml namespace. The returned string should be freed with CPLFree() when no longer required.

This method is the same as the C++ method OGRGeometry::exportToGML().
@argument[hGeometry 	handle to the geometry.

@return{A GML fragment or NULL in case of error.
(	OGRGeometryH 	hGeometry	 )

;; --------------------------------------------------------

(cffi:defcfun char* OGR_G_ExportToGMLEx


Convert a geometry into GML format.

The GML geometry is expressed directly in terms of GML basic data types assuming the this is available in the gml namespace. The returned string should be freed with CPLFree() when no longer required.

The supported options in OGR 1.8.0 are :
 FORMAT=GML3. Otherwise it will default to GML 2.1.2 output.
 GML3_LINESTRING_ELEMENT=curve. (Only valid for FORMAT=GML3) To use gml:Curve element for linestrings. Otherwise gml:LineString will be used .
 GML3_LONGSRS=YES/NO. (Only valid for FORMAT=GML3) Default to YES. If YES, SRS with EPSG authority will be written with the "urn:ogc:def:crs:EPSG::" prefix. In the case, if the SRS is a geographic SRS without explicit AXIS order, but that the same SRS authority code imported with ImportFromEPSGA() should be treated as lat/long, then the function will take care of coordinate order swapping. If set to NO, SRS with EPSG authority will be written with the "EPSG:" prefix, even if they are in lat/long order.
 GMLID=astring. If specified, a gml:id attribute will be written in the top-level geometry element with the provided value. Required for GML 3.2 compatibility.

This method is the same as the C++ method OGRGeometry::exportToGML().
@argument[hGeometry 	handle to the geometry.
	papszOptions 	NULL-terminated list of options.

@return{A GML fragment or NULL in case of error.
Since:
OGR 1.8.0
(	OGRGeometryH 	hGeometry,
		char ** 	papszOptions
	)

;; --------------------------------------------------------

(cffi:defcfun char* OGR_G_ExportToKML


Convert a geometry into KML format.

The returned string should be freed with CPLFree() when no longer required.

This method is the same as the C++ method OGRGeometry::exportToKML().
@argument[hGeometry 	handle to the geometry.
	pszAltitudeMode 	value to write in altitudeMode element, or NULL.

@return{A KML fragment or NULL in case of error.
(	OGRGeometryH 	hGeometry,
		const char * 	pszAltitudeMode
	)

;; --------------------------------------------------------
(cffi:defcfun char* OGR_G_ExportToJson


Convert a geometry into GeoJSON format.

The returned string should be freed with CPLFree() when no longer required.

This method is the same as the C++ method OGRGeometry::exportToJson().
@argument[hGeometry 	handle to the geometry.

@return{A GeoJSON fragment or NULL in case of error.
(	OGRGeometryH 	hGeometry	 )

;; --------------------------------------------------------
(cffi:defcfun char* OGR_G_ExportToJsonEx


Convert a geometry into GeoJSON format.

The returned string should be freed with CPLFree() when no longer required.

This method is the same as the C++ method OGRGeometry::exportToJson().
@argument[hGeometry 	handle to the geometry.
	papszOptions 	a null terminated list of options. For now, only COORDINATE_PRECISION=int_number where int_number is the maximum number of figures after decimal separator to write in coordinates.

@return{A GeoJSON fragment or NULL in case of error.
Since:
OGR 1.9.0
(	OGRGeometryH 	hGeometry,
		char ** 	papszOptions
	)

;; --------------------------------------------------------
(cffi:defcfun void OGR_G_AssignSpatialReference


Assign spatial reference to this object.

Any existing spatial reference is replaced, but under no circumstances does this result in the object being reprojected. It is just changing the interpretation of the existing geometry. Note that assigning a spatial reference increments the reference count on the OGRSpatialReference, but does not copy it.

This is similar to the SFCOM IGeometry::put_SpatialReference() method.

This function is the same as the CPP method OGRGeometry::assignSpatialReference.
@argument[hGeom 	handle on the geometry to apply the new spatial reference system.
	hSRS 	handle on the new spatial reference system to apply.
(	OGRGeometryH 	hGeom,
		OGRSpatialReferenceH 	hSRS
	)


;; --------------------------------------------------------
(cffi:defcfun OGRSpatialReferenceH OGR_G_GetSpatialReference 	(	OGRGeometryH 	hGeom	 )


Returns spatial reference system for geometry.

This function relates to the SFCOM IGeometry::get_SpatialReference() method.

This function is the same as the CPP method OGRGeometry::getSpatialReference().
@argument[hGeom 	handle on the geometry to get spatial reference from.

@return{a reference to the spatial reference geometry.

;; --------------------------------------------------------
(cffi:defcfun OGRErr OGR_G_Transform 	(	OGRGeometryH 	hGeom,
		OGRCoordinateTransformationH 	hTransform
	)


Apply arbitrary coordinate transformation to geometry.

This function will transform the coordinates of a geometry from their current spatial reference system to a new target spatial reference system. Normally this means reprojecting the vectors, but it could include datum shifts, and changes of units.

Note that this function does not require that the geometry already have a spatial reference system. It will be assumed that they can be treated as having the source spatial reference system of the OGRCoordinateTransformation object, and the actual SRS of the geometry will be ignored. On successful completion the output OGRSpatialReference of the OGRCoordinateTransformation will be assigned to the geometry.

This function is the same as the CPP method OGRGeometry::transform.
@argument[hGeom 	handle on the geometry to apply the transform to.
	hTransform 	handle on the transformation to apply.

@return{OGRERR_NONE on success or an error code.

;; --------------------------------------------------------
(cffi:defcfun OGRErr OGR_G_TransformTo 	(	OGRGeometryH 	hGeom,
		OGRSpatialReferenceH 	hSRS
	)


Transform geometry to new spatial reference system.

This function will transform the coordinates of a geometry from their current spatial reference system to a new target spatial reference system. Normally this means reprojecting the vectors, but it could include datum shifts, and changes of units.

This function will only work if the geometry already has an assigned spatial reference system, and if it is transformable to the target coordinate system.

Because this function requires internal creation and initialization of an OGRCoordinateTransformation object it is significantly more expensive to use this function to transform many geometries than it is to create the OGRCoordinateTransformation in advance, and call transform() with that transformation. This function exists primarily for convenience when only transforming a single geometry.

This function is the same as the CPP method OGRGeometry::transformTo.
@argument[hGeom 	handle on the geometry to apply the transform to.
	hSRS 	handle on the spatial reference system to apply.

@return{OGRERR_NONE on success, or an error code.

;; --------------------------------------------------------
(cffi:defcfun OGRGeometryH OGR_G_Simplify 	(	OGRGeometryH 	hThis,
		double 	dTolerance
	)


Compute a simplified geometry.

This function is the same as the C++ method OGRGeometry::Simplify().

This function is built on the GEOS library, check it for the definition of the geometry operation. If OGR is built without the GEOS library, this function will always fail, issuing a CPLE_NotSupported error.
@argument[hThis 	the geometry.
	dTolerance 	the distance tolerance for the simplification.

@return{the simplified geometry or NULL if an error occurs.
Since:
OGR 1.8.0

;; --------------------------------------------------------

(cffi:defcfun OGRGeometryH OGR_G_SimplifyPreserveTopology 	(	OGRGeometryH 	hThis,
		double 	dTolerance
	)


Simplify the geometry while preserving topology.

This function is the same as the C++ method OGRGeometry::SimplifyPreserveTopology().

This function is built on the GEOS library, check it for the definition of the geometry operation. If OGR is built without the GEOS library, this function will always fail, issuing a CPLE_NotSupported error.
@argument[hThis 	the geometry.
	dTolerance 	the distance tolerance for the simplification.

@return{the simplified geometry or NULL if an error occurs.
Since:
OGR 1.9.0

;; --------------------------------------------------------
(cffi:defcfun void OGR_G_Segmentize 	(	OGRGeometryH 	hGeom,
		double 	dfMaxLength
	)


Modify the geometry such it has no segment longer then the given distance.

Interpolated points will have Z and M values (if needed) set to 0. Distance computation is performed in 2d only

This function is the same as the CPP method OGRGeometry::segmentize().
@argument[hGeom 	handle on the geometry to segmentize
	dfMaxLength 	the maximum distance between 2 points after segmentization

;; --------------------------------------------------------
(cffi:defcfun int OGR_G_Intersects 	(	OGRGeometryH 	hGeom,
		OGRGeometryH 	hOtherGeom
	)


Do these features intersect?

Currently this is not implemented in a rigerous fashion, and generally just tests whether the envelopes of the two features intersect. Eventually this will be made rigerous.

This function is the same as the CPP method OGRGeometry::Intersects.
@argument[hGeom 	handle on the first geometry.
	hOtherGeom 	handle on the other geometry to test against.

@return{TRUE if the geometries intersect, otherwise FALSE.

;; --------------------------------------------------------
(cffi:defcfun int OGR_G_Equals 	(	OGRGeometryH 	hGeom,
		OGRGeometryH 	hOther
	)


Returns TRUE if two geometries are equivalent.

This function is the same as the CPP method OGRGeometry::Equals() method.
@argument[hGeom 	handle on the first geometry.
	hOther 	handle on the other geometry to test against.

@return{TRUE if equivalent or FALSE otherwise.
;; --------------------------------------------------------
(cffi:defcfun int OGR_G_Disjoint 	(	OGRGeometryH 	hThis,
		OGRGeometryH 	hOther
	)


Test for disjointness.

Tests if this geometry and the other geometry are disjoint.

This function is the same as the C++ method OGRGeometry::Disjoint().

This function is built on the GEOS library, check it for the definition of the geometry operation. If OGR is built without the GEOS library, this function will always fail, issuing a CPLE_NotSupported error.
@argument[hThis 	the geometry to compare.
	hOther 	the other geometry to compare.

@return{TRUE if they are disjoint, otherwise FALSE.

;; --------------------------------------------------------
(cffi:defcfun int OGR_G_Touches 	(	OGRGeometryH 	hThis,
		OGRGeometryH 	hOther
	)


Test for touching.

Tests if this geometry and the other geometry are touching.

This function is the same as the C++ method OGRGeometry::Touches().

This function is built on the GEOS library, check it for the definition of the geometry operation. If OGR is built without the GEOS library, this function will always fail, issuing a CPLE_NotSupported error.
@argument[hThis 	the geometry to compare.
	hOther 	the other geometry to compare.

@return{TRUE if they are touching, otherwise FALSE.

;; --------------------------------------------------------
(cffi:defcfun int OGR_G_Crosses 	(	OGRGeometryH 	hThis,
		OGRGeometryH 	hOther
	)


Test for crossing.

Tests if this geometry and the other geometry are crossing.

This function is the same as the C++ method OGRGeometry::Crosses().

This function is built on the GEOS library, check it for the definition of the geometry operation. If OGR is built without the GEOS library, this function will always fail, issuing a CPLE_NotSupported error.
@argument[hThis 	the geometry to compare.
	hOther 	the other geometry to compare.

@return{TRUE if they are crossing, otherwise FALSE.

;; --------------------------------------------------------
(cffi:defcfun int OGR_G_Within 	(	OGRGeometryH 	hThis,
		OGRGeometryH 	hOther
	)


Test for containment.

Tests if this geometry is within the other geometry.

This function is the same as the C++ method OGRGeometry::Within().

This function is built on the GEOS library, check it for the definition of the geometry operation. If OGR is built without the GEOS library, this function will always fail, issuing a CPLE_NotSupported error.
@argument[hThis 	the geometry to compare.
	hOther 	the other geometry to compare.

@return{TRUE if hThis is within hOther, otherwise FALSE.

;; --------------------------------------------------------
(cffi:defcfun int OGR_G_Contains 	(	OGRGeometryH 	hThis,
		OGRGeometryH 	hOther
	)


Test for containment.

Tests if this geometry contains the other geometry.

This function is the same as the C++ method OGRGeometry::Contains().

This function is built on the GEOS library, check it for the definition of the geometry operation. If OGR is built without the GEOS library, this function will always fail, issuing a CPLE_NotSupported error.
@argument[hThis 	the geometry to compare.
	hOther 	the other geometry to compare.

@return{TRUE if hThis contains hOther geometry, otherwise FALSE.

;; --------------------------------------------------------
(cffi:defcfun int OGR_G_Overlaps 	(	OGRGeometryH 	hThis,
		OGRGeometryH 	hOther
	)


Test for overlap.

Tests if this geometry and the other geometry overlap, that is their intersection has a non-zero area.

This function is the same as the C++ method OGRGeometry::Overlaps().

This function is built on the GEOS library, check it for the definition of the geometry operation. If OGR is built without the GEOS library, this function will always fail, issuing a CPLE_NotSupported error.
@argument[hThis 	the geometry to compare.
	hOther 	the other geometry to compare.

@return{TRUE if they are overlapping, otherwise FALSE.

;; --------------------------------------------------------
(cffi:defcfun OGRGeometryH OGR_G_Boundary 	(	OGRGeometryH 	hTarget	 )


Compute boundary.

A new geometry object is created and returned containing the boundary of the geometry on which the method is invoked.

This function is the same as the C++ method OGR_G_Boundary().

This function is built on the GEOS library, check it for the definition of the geometry operation. If OGR is built without the GEOS library, this function will always fail, issuing a CPLE_NotSupported error.
@argument[hTarget 	The Geometry to calculate the boundary of.

@return{a handle to a newly allocated geometry now owned by the caller, or NULL on failure.
Since:
OGR 1.8.0

;; --------------------------------------------------------
(cffi:defcfun OGRGeometryH OGR_G_ConvexHull 	(	OGRGeometryH 	hTarget	 )


Compute convex hull.

A new geometry object is created and returned containing the convex hull of the geometry on which the method is invoked.

This function is the same as the C++ method OGRGeometry::ConvexHull().

This function is built on the GEOS library, check it for the definition of the geometry operation. If OGR is built without the GEOS library, this function will always fail, issuing a CPLE_NotSupported error.
@argument[hTarget 	The Geometry to calculate the convex hull of.

@return{a handle to a newly allocated geometry now owned by the caller, or NULL on failure.

;; --------------------------------------------------------
(cffi:defcfun OGRGeometryH OGR_G_Buffer 	(	OGRGeometryH 	hTarget,
		double 	dfDist,
		int 	nQuadSegs
	)


Compute buffer of geometry.

Builds a new geometry containing the buffer region around the geometry on which it is invoked. The buffer is a polygon containing the region within the buffer distance of the original geometry.

Some buffer sections are properly described as curves, but are converted to approximate polygons. The nQuadSegs parameter can be used to control how many segements should be used to define a 90 degree curve - a quadrant of a circle. A value of 30 is a reasonable default. Large values result in large numbers of vertices in the resulting buffer geometry while small numbers reduce the accuracy of the result.

This function is the same as the C++ method OGRGeometry::Buffer().

This function is built on the GEOS library, check it for the definition of the geometry operation. If OGR is built without the GEOS library, this function will always fail, issuing a CPLE_NotSupported error.
@argument[hTarget 	the geometry.
	dfDist 	the buffer distance to be applied.
	nQuadSegs 	the number of segments used to approximate a 90 degree (quadrant) of curvature.

@return{the newly created geometry, or NULL if an error occurs.

;; --------------------------------------------------------
(cffi:defcfun OGRGeometryH OGR_G_Intersection 	(	OGRGeometryH 	hThis,
		OGRGeometryH 	hOther
	)


Compute intersection.

Generates a new geometry which is the region of intersection of the two geometries operated on. The OGR_G_Intersects() function can be used to test if two geometries intersect.

This function is the same as the C++ method OGRGeometry::Intersection().

This function is built on the GEOS library, check it for the definition of the geometry operation. If OGR is built without the GEOS library, this function will always fail, issuing a CPLE_NotSupported error.
@argument[hThis 	the geometry.
	hOther 	the other geometry.

@return{a new geometry representing the intersection or NULL if there is no intersection or an error occurs.

;; --------------------------------------------------------
(cffi:defcfun OGRGeometryH OGR_G_Union 	(	OGRGeometryH 	hThis,
		OGRGeometryH 	hOther
	)


Compute union.

Generates a new geometry which is the region of union of the two geometries operated on.

This function is the same as the C++ method OGRGeometry::Union().

This function is built on the GEOS library, check it for the definition of the geometry operation. If OGR is built without the GEOS library, this function will always fail, issuing a CPLE_NotSupported error.
@argument[hThis 	the geometry.
	hOther 	the other geometry.

@return{a new geometry representing the union or NULL if an error occurs.
;; --------------------------------------------------------
(cffi:defcfun OGRGeometryH OGR_G_UnionCascaded 	(	OGRGeometryH 	hThis	 )


Compute union using cascading.

This function is the same as the C++ method OGRGeometry::UnionCascaded().

This function is built on the GEOS library, check it for the definition of the geometry operation. If OGR is built without the GEOS library, this function will always fail, issuing a CPLE_NotSupported error.
@argument[hThis 	the geometry.

@return{a new geometry representing the union or NULL if an error occurs.

;; --------------------------------------------------------
(cffi:defcfun OGRGeometryH OGR_G_PointOnSurface 	(	OGRGeometryH 	hGeom	 )


Returns a point guaranteed to lie on the surface.

This method relates to the SFCOM ISurface::get_PointOnSurface() method however the current implementation based on GEOS can operate on other geometry types than the types that are supported by SQL/MM-Part 3 : surfaces (polygons) and multisurfaces (multipolygons).

This method is built on the GEOS library, check it for the definition of the geometry operation. If OGR is built without the GEOS library, this method will always fail, issuing a CPLE_NotSupported error.
@argument[hGeom 	the geometry to operate on.

@return{a point guaranteed to lie on the surface or NULL if an error occured.
Since:
OGR 1.10
;; --------------------------------------------------------
(cffi:defcfun OGRGeometryH OGR_G_Difference 	(	OGRGeometryH 	hThis,
		OGRGeometryH 	hOther
	)


Compute difference.

Generates a new geometry which is the region of this geometry with the region of the other geometry removed.

This function is the same as the C++ method OGRGeometry::Difference().

This function is built on the GEOS library, check it for the definition of the geometry operation. If OGR is built without the GEOS library, this function will always fail, issuing a CPLE_NotSupported error.
@argument[hThis 	the geometry.
	hOther 	the other geometry.

@return{a new geometry representing the difference or NULL if the difference is empty or an error occurs.
;; --------------------------------------------------------
(cffi:defcfun OGRGeometryH OGR_G_SymDifference 	(	OGRGeometryH 	hThis,
		OGRGeometryH 	hOther
	)


Compute symmetric difference.

Generates a new geometry which is the symmetric difference of this geometry and the other geometry.

This function is the same as the C++ method OGRGeometry::SymmetricDifference().

This function is built on the GEOS library, check it for the definition of the geometry operation. If OGR is built without the GEOS library, this function will always fail, issuing a CPLE_NotSupported error.
@argument[hThis 	the geometry.
	hOther 	the other geometry.

@return{a new geometry representing the symmetric difference or NULL if the difference is empty or an error occurs.
Since:
OGR 1.8.0
;; --------------------------------------------------------
(cffi:defcfun double OGR_G_Distance 	(	OGRGeometryH 	hFirst,
		OGRGeometryH 	hOther
	)


Compute distance between two geometries.

Returns the shortest distance between the two geometries.

This function is the same as the C++ method OGRGeometry::Distance().

This function is built on the GEOS library, check it for the definition of the geometry operation. If OGR is built without the GEOS library, this function will always fail, issuing a CPLE_NotSupported error.
@argument[hFirst 	the first geometry to compare against.
	hOther 	the other geometry to compare against.

@return{the distance between the geometries or -1 if an error occurs.

;; --------------------------------------------------------
(cffi:defcfun double OGR_G_Length 	(	OGRGeometryH 	hGeom	 )


Compute length of a geometry.

Computes the area for OGRCurve or MultiCurve objects. Undefined for all other geometry types (returns zero).

This function utilizes the C++ get_Length() method.
@argument[hGeom 	the geometry to operate on.

@return{the lenght or 0.0 for unsupported geometry types.
Since:
OGR 1.8.0
;; --------------------------------------------------------
(cffi:defcfun double OGR_G_Area 	(	OGRGeometryH 	hGeom	 )


Compute geometry area.

Computes the area for an OGRLinearRing, OGRPolygon or OGRMultiPolygon. Undefined for all other geometry types (returns zero).

This function utilizes the C++ get_Area() methods such as OGRPolygon::get_Area().
@argument[hGeom 	the geometry to operate on.

@return{the area or 0.0 for unsupported geometry types.
Since:
OGR 1.8.0
;; --------------------------------------------------------

(cffi:defcfun int OGR_G_Centroid 	(	OGRGeometryH 	hGeom,
		OGRGeometryH 	hCentroidPoint
	)


Compute the geometry centroid.

The centroid location is applied to the passed in OGRPoint object. The centroid is not necessarily within the geometry.

This method relates to the SFCOM ISurface::get_Centroid() method however the current implementation based on GEOS can operate on other geometry types such as multipoint, linestring, geometrycollection such as multipolygons. OGC SF SQL 1.1 defines the operation for surfaces (polygons). SQL/MM-Part 3 defines the operation for surfaces and multisurfaces (multipolygons).

This function is the same as the C++ method OGRGeometry::Centroid().

This function is built on the GEOS library, check it for the definition of the geometry operation. If OGR is built without the GEOS library, this function will always fail, issuing a CPLE_NotSupported error.
@return{OGRERR_NONE on success or OGRERR_FAILURE on error.

;; --------------------------------------------------------

(cffi:defcfun void OGR_G_Empty 	(	OGRGeometryH 	hGeom	 )


Clear geometry information. This restores the geometry to it's initial state after construction, and before assignment of actual geometry.

This function relates to the SFCOM IGeometry::Empty() method.

This function is the same as the CPP method OGRGeometry::empty().
@argument[hGeom 	handle on the geometry to empty.

;; --------------------------------------------------------
(cffi:defcfun int OGR_G_IsEmpty 	(	OGRGeometryH 	hGeom	 )


Test if the geometry is empty.

This method is the same as the CPP method OGRGeometry::IsEmpty().
@argument[hGeom 	The Geometry to test.

@return{TRUE if the geometry has no points, otherwise FALSE.
;; --------------------------------------------------------
(cffi:defcfun int OGR_G_IsValid 	(	OGRGeometryH 	hGeom	 )


Test if the geometry is valid.

This function is the same as the C++ method OGRGeometry::IsValid().

This function is built on the GEOS library, check it for the definition of the geometry operation. If OGR is built without the GEOS library, this function will always return FALSE.
@argument[hGeom 	The Geometry to test.

@return{TRUE if the geometry has no points, otherwise FALSE.
;; --------------------------------------------------------
(cffi:defcfun int OGR_G_IsSimple 	(	OGRGeometryH 	hGeom	 )


Returns TRUE if the geometry is simple.

Returns TRUE if the geometry has no anomalous geometric points, such as self intersection or self tangency. The description of each instantiable geometric class will include the specific conditions that cause an instance of that class to be classified as not simple.

This function is the same as the c++ method OGRGeometry::IsSimple() method.

If OGR is built without the GEOS library, this function will always return FALSE.
@argument[hGeom 	The Geometry to test.

@return{TRUE if object is simple, otherwise FALSE.
;; --------------------------------------------------------
(cffi:defcfun int OGR_G_IsRing 	(	OGRGeometryH 	hGeom	 )


Test if the geometry is a ring.

This function is the same as the C++ method OGRGeometry::IsRing().

This function is built on the GEOS library, check it for the definition of the geometry operation. If OGR is built without the GEOS library, this function will always return FALSE.
@argument[hGeom 	The Geometry to test.

@return{TRUE if the geometry has no points, otherwise FALSE.
;; --------------------------------------------------------
(cffi:defcfun OGRGeometryH OGR_G_Polygonize 	(	OGRGeometryH 	hTarget	 )


Polygonizes a set of sparse edges.

A new geometry object is created and returned containing a collection of reassembled Polygons: NULL will be returned if the input collection doesn't corresponds to a MultiLinestring, or when reassembling Edges into Polygons is impossible due to topogical inconsistencies.

This function is the same as the C++ method OGRGeometry::Polygonize().

This function is built on the GEOS library, check it for the definition of the geometry operation. If OGR is built without the GEOS library, this function will always fail, issuing a CPLE_NotSupported error.
@argument[hTarget 	The Geometry to be polygonized.

@return{a handle to a newly allocated geometry now owned by the caller, or NULL on failure.
Since:
OGR 1.9.0
;; --------------------------------------------------------
(cffi:defcfun OGRGeometryH OGR_G_SymmetricDifference 	(	OGRGeometryH 	hThis,
		OGRGeometryH 	hOther
	)


Compute symmetric difference (deprecated).
Deprecated:
;; --------------------------------------------------------
(cffi:defcfun double OGR_G_GetArea 	(	OGRGeometryH 	hGeom	 )


Compute geometry area (deprecated).
Deprecated:
;; --------------------------------------------------------
(cffi:defcfun OGRGeometryH OGR_G_GetBoundary 	(	OGRGeometryH 	hTarget	 )


Compute boundary (deprecated).
Deprecated:
;; --------------------------------------------------------
(cffi:defcfun int OGR_G_GetPointCount 	(	OGRGeometryH 	hGeom	 )


Fetch number of points from a geometry.

Only wkbPoint[25D] or wkbLineString[25D] may return a valid value. Other geometry types will silently return 0.
@argument[hGeom 	handle to the geometry from which to get the number of points.

@return{the number of points.
;; --------------------------------------------------------

(cffi:defcfun int OGR_G_GetPoints 	(	OGRGeometryH 	hGeom,
		void * 	pabyX,
		int 	nXStride,
		void * 	pabyY,
		int 	nYStride,
		void * 	pabyZ,
		int 	nZStride
	)


Returns all points of line string.

This method copies all points into user arrays. The user provides the stride between 2 consecutives elements of the array.

On some CPU architectures, care must be taken so that the arrays are properly aligned.
@argument[hGeom 	handle to the geometry from which to get the coordinates.
	pabyX 	a buffer of at least (sizeof(double) * nXStride * nPointCount) bytes, may be NULL.
	nXStride 	the number of bytes between 2 elements of pabyX.
	pabyY 	a buffer of at least (sizeof(double) * nYStride * nPointCount) bytes, may be NULL.
	nYStride 	the number of bytes between 2 elements of pabyY.
	pabyZ 	a buffer of at last size (sizeof(double) * nZStride * nPointCount) bytes, may be NULL.
	nZStride 	the number of bytes between 2 elements of pabyZ.

@return{the number of points
Since:
OGR 1.9.0

;; --------------------------------------------------------
(cffi:defcfun void OGR_G_GetPoint 	(	OGRGeometryH 	hGeom,
		int 	i,
		double * 	pdfX,
		double * 	pdfY,
		double * 	pdfZ
	)


Fetch a point in line string or a point geometry.
@argument[hGeom 	handle to the geometry from which to get the coordinates.
	i 	the vertex to fetch, from 0 to getNumPoints()-1, zero for a point.
	pdfX 	value of x coordinate.
	pdfY 	value of y coordinate.
	pdfZ 	value of z coordinate.

;; --------------------------------------------------------
(cffi:defcfun void OGR_G_SetPoint 	(	OGRGeometryH 	hGeom,
		int 	i,
		double 	dfX,
		double 	dfY,
		double 	dfZ
	)


Set the location of a vertex in a point or linestring geometry.

If iPoint is larger than the number of existing points in the linestring, the point count will be increased to accomodate the request.
@argument[hGeom 	handle to the geometry to add a vertex to.
	i 	the index of the vertex to assign (zero based) or zero for a point.
	dfX 	input X coordinate to assign.
	dfY 	input Y coordinate to assign.
	dfZ 	input Z coordinate to assign (defaults to zero).

;; --------------------------------------------------------
(cffi:defcfun void OGR_G_SetPoint_2D 	(	OGRGeometryH 	hGeom,
		int 	i,
		double 	dfX,
		double 	dfY
	)


Set the location of a vertex in a point or linestring geometry.

If iPoint is larger than the number of existing points in the linestring, the point count will be increased to accomodate the request.
@argument[hGeom 	handle to the geometry to add a vertex to.
	i 	the index of the vertex to assign (zero based) or zero for a point.
	dfX 	input X coordinate to assign.
	dfY 	input Y coordinate to assign.

;; --------------------------------------------------------

(cffi:defcfun void OGR_G_AddPoint 	(	OGRGeometryH 	hGeom,
		double 	dfX,
		double 	dfY,
		double 	dfZ
	)


Add a point to a geometry (line string or point).

The vertex count of the line string is increased by one, and assigned from the passed location value.
@argument[hGeom 	handle to the geometry to add a point to.
	dfX 	x coordinate of point to add.
	dfY 	y coordinate of point to add.
	dfZ 	z coordinate of point to add.

;; --------------------------------------------------------

(cffi:defcfun void OGR_G_AddPoint_2D 	(	OGRGeometryH 	hGeom,
		double 	dfX,
		double 	dfY
	)


Add a point to a geometry (line string or point).

The vertex count of the line string is increased by one, and assigned from the passed location value.
@argument[hGeom 	handle to the geometry to add a point to.
	dfX 	x coordinate of point to add.
	dfY 	y coordinate of point to add.

;; --------------------------------------------------------
(cffi:defcfun int OGR_G_GetGeometryCount 	(	OGRGeometryH 	hGeom	 )


Fetch the number of elements in a geometry or number of geometries in container.

Only geometries of type wkbPolygon[25D], wkbMultiPoint[25D], wkbMultiLineString[25D], wkbMultiPolygon[25D] or wkbGeometryCollection[25D] may return a valid value. Other geometry types will silently return 0.

For a polygon, the returned number is the number of rings (exterior ring + interior rings).
@argument[hGeom 	single geometry or geometry container from which to get the number of elements.

@return{the number of elements.
;; --------------------------------------------------------
(cffi:defcfun OGRGeometryH OGR_G_GetGeometryRef 	(	OGRGeometryH 	hGeom,
		int 	iSubGeom
	)


Fetch geometry from a geometry container.

This function returns an handle to a geometry within the container. The returned geometry remains owned by the container, and should not be modified. The handle is only valid untill the next change to the geometry container. Use OGR_G_Clone() to make a copy.

This function relates to the SFCOM IGeometryCollection::get_Geometry() method.

This function is the same as the CPP method OGRGeometryCollection::getGeometryRef().

For a polygon, OGR_G_GetGeometryRef(iSubGeom) returns the exterior ring if iSubGeom == 0, and the interior rings for iSubGeom > 0.
@argument[hGeom 	handle to the geometry container from which to get a geometry from.
	iSubGeom 	the index of the geometry to fetch, between 0 and getNumGeometries() - 1.

@return{handle to the requested geometry.

;; --------------------------------------------------------

(cffi:defcfun OGRErr OGR_G_AddGeometry 	(	OGRGeometryH 	hGeom,
		OGRGeometryH 	hNewSubGeom
	)


Add a geometry to a geometry container.

Some subclasses of OGRGeometryCollection restrict the types of geometry that can be added, and may return an error. The passed geometry is cloned to make an internal copy.

There is no SFCOM analog to this method.

This function is the same as the CPP method OGRGeometryCollection::addGeometry.

For a polygon, hNewSubGeom must be a linearring. If the polygon is empty, the first added subgeometry will be the exterior ring. The next ones will be the interior rings.
@argument[hGeom 	existing geometry container.
	hNewSubGeom 	geometry to add to the container.

@return{OGRERR_NONE if successful, or OGRERR_UNSUPPORTED_GEOMETRY_TYPE if the geometry type is illegal for the type of existing geometry.

;; --------------------------------------------------------

(cffi:defcfun OGRErr OGR_G_AddGeometryDirectly 	(	OGRGeometryH 	hGeom,
		OGRGeometryH 	hNewSubGeom
	)


Add a geometry directly to an existing geometry container.

Some subclasses of OGRGeometryCollection restrict the types of geometry that can be added, and may return an error. Ownership of the passed geometry is taken by the container rather than cloning as addGeometry() does.

This function is the same as the CPP method OGRGeometryCollection::addGeometryDirectly.

There is no SFCOM analog to this method.

For a polygon, hNewSubGeom must be a linearring. If the polygon is empty, the first added subgeometry will be the exterior ring. The next ones will be the interior rings.
@argument[hGeom 	existing geometry.
	hNewSubGeom 	geometry to add to the existing geometry.

@return{OGRERR_NONE if successful, or OGRERR_UNSUPPORTED_GEOMETRY_TYPE if the geometry type is illegal for the type of geometry container.

;; --------------------------------------------------------

(cffi:defcfun OGRErr OGR_G_RemoveGeometry 	(	OGRGeometryH 	hGeom,
		int 	iGeom,
		int 	bDelete
	)


Remove a geometry from an exiting geometry container.

Removing a geometry will cause the geometry count to drop by one, and all "higher" geometries will shuffle down one in index.

There is no SFCOM analog to this method.

This function is the same as the CPP method OGRGeometryCollection::removeGeometry().
@argument[hGeom 	the existing geometry to delete from.
	iGeom 	the index of the geometry to delete. A value of -1 is a special flag meaning that all geometries should be removed.
	bDelete 	if TRUE the geometry will be destroyed, otherwise it will not. The default is TRUE as the existing geometry is considered to own the geometries in it.

@return{OGRERR_NONE if successful, or OGRERR_FAILURE if the index is out of range.

;; EOF