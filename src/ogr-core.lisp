;;; -*- package: OGR; Syntax: Common-lisp; Base: 10 -*-

;; based on the auto-generated documentation at:
;; http://www.gdal.org/ogr/ogr__api_8h.html

(in-package :cl-ogr)

;; --------------------------------------------------------

(cffi:defctype ogr-layer-h :pointer "OGRLayerH")

(cffi:defctype ogr-data-source-h :pointer "OGRDataSourceH")

(cffi:defctype ogr-geometry-h :pointer "OGRGeometryH")

(cffi:defctype ogr-feature-h :pointer "OGRFeatureH")

(cffi:defctype ogr-feature-defn-h :pointer "OGRFeatureDefnH")

(cffi:defctype ogr-spatial-reference-h :pointer "OGRSpatialReferenceH")

(cffi:defctype ogr-field-defn-h :pointer "OGRFieldDefnH")

(cffi:defctype ogr-geom-field-defn-h :pointer "OGRGeomFieldDefnH")

(cffi:defctype ogr-style-mgr-h :pointer "OGRStyleMgrH")

(cffi:defctype ogr-style-table-h :pointer "OGRStyleTableH")

(cffi:defctype ogr-style-tool-h :pointer "OGRStyleToolH")

(cffi:defctype ogr-sf-driver-h :pointer "OGRSFDriverH")

;; --------------------------------------------------------

(defclass ogr-class ()
  ((pointer
    :type (or null cffi:foreign-pointer)
    :initarg :pointer
    :accessor pointer
    :initform nil)))

(defclass data-source (ogr-class)
  ;; original documentation at: http://www.gdal.org/ogr/classOGRDataSource.html
  ()
  (:documentation "This class represents a data source.

  A data source potentially consists of many layers (OGRLayer). A data
  source normally consists of one, or a related set of files, though
  the name doesn't have to be a real item in the file system.

  When an OGRDataSource is destroyed, all it's associated OGRLayers
  objects are also destroyed."))

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

(cffi:defcenum ogr-field-type
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

(cffi:defcenum ogr-err
    "Errors are defined as macro constants, but we define is an
enumeration with set constant values."
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

(cffi:defcenum ogr-wkb-geometry-type
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

(cffi:defcenum ogr-justification)

;; --------------------------------------------------------

(cffi:defcenum ogr-st-class-id
    "ogr_style_tool_class_id: OGRSTClassId;"
  (:OGRSTCNone    0)
  (:OGRSTCPen     1)
  (:OGRSTCBrush   2)
  (:OGRSTCSymbol  3)
  (:OGRSTCLabel   4)
  (:OGRSTCVector  5))

;; --------------------------------------------------------

(cffi:defcenum OGR-ST-Unit-Id
    "ogr_style_tool_units_id: OGRSTUnitId"
  (:OGRSTUGround  0)
  (:OGRSTUPixel   1)
  (:OGRSTUPoints  2)
  (:OGRSTUMM      3)
  (:OGRSTUCM      4)
  (:OGRSTUInches  5))

;; --------------------------------------------------------

(cffi:defcenum ogr-st-pen-param
    "ogr_style_tool_param_pen_id: OGRSTPenParam"
  (:OGRSTPenColor        0)
  (:OGRSTPenWidth        1)
  (:OGRSTPenPattern      2)
  (:OGRSTPenId           3)
  (:OGRSTPenPerOffset    4)
  (:OGRSTPenCap          5)
  (:OGRSTPenJoin         6)
  (:OGRSTPenPriority     7)
  (:OGRSTPenLast         8))

;; --------------------------------------------------------

(cffi:defcenum ogr-st-brush-param
    "ogr_style_tool_param_brush_id: OGRSTBrushParam"
  (:OGRSTBrushFColor     0)
  (:OGRSTBrushBColor     1)
  (:OGRSTBrushId         2)
  (:OGRSTBrushAngle      3)
  (:OGRSTBrushSize       4)
  (:OGRSTBrushDx         5)
  (:OGRSTBrushDy         6)
  (:OGRSTBrushPriority   7)
  (:OGRSTBrushLast       8))

;; --------------------------------------------------------

(cffi:defcenum ogr-st-symbol-param
    "ogr_style_tool_param_symbol_id: OGRSTSymbolParam"
  (:OGRSTSymbolId        0)
  (:OGRSTSymbolAngle     1)
  (:OGRSTSymbolColor     2)
  (:OGRSTSymbolSize      3)
  (:OGRSTSymbolDx        4)
  (:OGRSTSymbolDy        5)
  (:OGRSTSymbolStep      6)
  (:OGRSTSymbolPerp      7)
  (:OGRSTSymbolOffset    8)
  (:OGRSTSymbolPriority  9)
  (:OGRSTSymbolFontName  10)
  (:OGRSTSymbolOColor    11)
  (:OGRSTSymbolLast      12))

;; --------------------------------------------------------

(cffi:defcenum ogr-st-label-param
    "ogr_style_tool_param_label_id: OGRSTLabelParam"
  (:OGRSTLabelFontName   0)
  (:OGRSTLabelSize       1)
  (:OGRSTLabelTextString 2)
  (:OGRSTLabelAngle      3)
  (:OGRSTLabelFColor     4)
  (:OGRSTLabelBColor     5)
  (:OGRSTLabelPlacement  6)
  (:OGRSTLabelAnchor     7)
  (:OGRSTLabelDx         8)
  (:OGRSTLabelDy         9)
  (:OGRSTLabelPerp       10)
  (:OGRSTLabelBold       11)
  (:OGRSTLabelItalic     12)
  (:OGRSTLabelUnderline  13)
  (:OGRSTLabelPriority   14)
  (:OGRSTLabelStrikeout  15)
  (:OGRSTLabelStretch    16)
  (:OGRSTLabelAdjHor     17)
  (:OGRSTLabelAdjVert    18)
  (:OGRSTLabelHColor     19)
  (:OGRSTLabelOColor     20)
  (:OGRSTLabelLast       21))

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

(cffi:defcfun ("OGRCleanupAll" ogr-cleanup-all) :void
  "Cleanup all OGR related resources.

 This function will destroy the OGRSFDriverRegistrar along with all
 registered drivers, and then cleanup long lived
 OSR (OGRSpatialReference) and CPL resources. This may be called in an
 application when OGR services are no longer needed. It is not
 normally required, but by freeing all dynamically allocated memory it
 can make memory leak testing easier.

 In addition to destroying the OGRDriverRegistrar, this function also
 calls:

 OSRCleanup()
 CPLFinderClean()
 VSICleanupFileManager()
 CPLFreeConfig()
 CPLCleanupTLS()")

;; --------------------------------------------------------
;; EOF
