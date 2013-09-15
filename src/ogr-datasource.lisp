;;; -*- package: OGR; Syntax: Common-lisp; Base: 10 -*-

;; OGRDataSource <ogr_api.h>

(in-package :ogr)

;; --------------------------------------------------------

(cffi:defctype data-source-h (:pointer :void)
  "OGRDataSourceH")

;; --------------------------------------------------------

(cffi:defcfun  ("OGROpen" ogr-open) :pointer ; OGRDataSourceH
  (psz-name :string)
  (p-update :int)
  (path-driver-list :pointer)) ;; OGRSFDriverH
(export 'ogr-open)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_DS_GetLayerCount" OGR-DS-Get-Layer-Count) :int
  (ds :pointer ))
(export 'ogr-ds-get-layer-count)

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
  (pszLayerName :string))
(export 'ogr-ds-get-layer-by-name)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_DS_GetLayer" OGR-DS-Get-Layer) :pointer ; OGRLayerH
  (hDS :pointer)			; OGRDataSourceH
  (i :int))
(export 'ogr-ds-get-layer)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_DS_Destroy" OGR-DS-Destroy) :void
  "Closes opened datasource and releases allocated resources.

This method is the same as the C++ method OGRDataSource::DestroyDataSource().
@argument[hDataSource]{handle to allocated datasource object.}"
  (hDataSource :pointer))		; OGRDataSourceH
(export 'OGR-DS-Destroy)

;; --------------------------------------------------------
;; CLOS
;; --------------------------------------------------------

(defun open-data-source (name &optional (update 0) (drivers (cffi:null-pointer)))
  (let ((ds-pointer (ogr-open name update drivers)))
    (when (cffi:null-pointer-p ds-pointer)
      (error "Failed to open OGRDataSource: ~a" name))
    (make-instance 'data-source
		   :pointer ds-pointer)))

(export 'open-data-source)

;; --------------------------------------------------------

(defgeneric get-layer-count (ds)
  (:documentation "")
  (:method ((ds data-source))
    (OGR-DS-Get-Layer-Count (pointer ds))))

(export 'get-layer-count)

;; --------------------------------------------------------

(defgeneric get-layer-by-name (ds name)
  (:method ((ds data-source) (name string))
    (let ((layer-pointer (ogr-ds-get-layer-by-name (pointer ds)
						   name)))
      (unless (cffi:null-pointer-p layer-pointer)
	(make-instance 'layer
		       :pointer layer-pointer
		       :data-source ds)))))
(export 'get-layer-by-name)

;; --------------------------------------------------------

(defgeneric get-layer (ds idx)
  (:method ((ds data-source) (idx fixnum))
    (let ((layer-pointer (ogr-ds-get-layer (pointer ds)
					   idx)))
      (unless (cffi:null-pointer-p layer-pointer)
	(make-instance 'layer
		       :pointer layer-pointer
		       :data-source ds)))))
(export 'get-layer)

;; --------------------------------------------------------

#|
const char * 	OGR_DS_GetName (OGRDataSourceH)
 	Returns the name of the data source. 
int 	OGR_DS_GetLayerCount (OGRDataSourceH)
 	Get the number of layers in this data source. 

OGRErr 	OGR_DS_DeleteLayer (OGRDataSourceH, int)
 	Delete the indicated layer from the datasource. 

OGRSFDriverH 	OGR_DS_GetDriver (OGRDataSourceH)
 	Returns the driver that the dataset was opened with. 

OGRLayerH 	OGR_DS_CreateLayer (OGRDataSourceH, const char *, OGRSpatialReferenceH, OGRwkbGeometryType, char **)
 	This function attempts to create a new layer on the data source with the indicated name, coordinate system, geometry type. 

OGRLayerH 	OGR_DS_CopyLayer (OGRDataSourceH, OGRLayerH, const char *, char **)
 	Duplicate an existing layer. 

int 	OGR_DS_TestCapability (OGRDataSourceH, const char *)
 	Test if capability is available. 

OGRLayerH 	OGR_DS_ExecuteSQL (OGRDataSourceH, const char *, OGRGeometryH, const char *)
 	Execute an SQL statement against the data store. 

void 	OGR_DS_ReleaseResultSet (OGRDataSourceH, OGRLayerH)
 	Release results of OGR_DS_ExecuteSQL(). 

OGRErr 	OGR_DS_SyncToDisk (OGRDataSourceH)
 	Flush pending changes to disk.

OGRErr 	OGRReleaseDataSource (OGRDataSourceH)
 	Drop a reference to this datasource, and if the reference count drops to zero close (destroy) the datasource. 

void 	OGRRegisterDriver (OGRSFDriverH)
 	Add a driver to the list of registered drivers. 

void 	OGRDeregisterDriver (OGRSFDriverH)
 	Remove the passed driver from the list of registered drivers. 

int 	OGRGetDriverCount (void)
 	Fetch the number of registered drivers. 

OGRSFDriverH 	OGRGetDriver (int)
 	Fetch the indicated driver. 

OGRSFDriverH 	OGRGetDriverByName (const char *)
 	Fetch the indicated driver. 

int 	OGRGetOpenDSCount (void)
 	Return the number of opened datasources. 

OGRDataSourceH 	OGRGetOpenDS (int iDS)
 	Return the iDS th datasource opened. 

 void 	OGRRegisterAll (void)
 	Register all drivers. 

void 	OGRCleanupAll (void)
 	Cleanup all OGR related resources.
|#

;; EOF