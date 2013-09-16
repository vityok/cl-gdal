const char * 	OGR_L_GetName (OGRLayerH)
 	Return the layer name. 
OGRwkbGeometryType 	OGR_L_GetGeomType (OGRLayerH)
 	Return the layer geometry type. 
OGRGeometryH 	OGR_L_GetSpatialFilter (OGRLayerH)
 	This function returns the current spatial filter for this layer. 
void 	OGR_L_SetSpatialFilter (OGRLayerH, OGRGeometryH)
 	Set a new spatial filter. 
void 	OGR_L_SetSpatialFilterRect (OGRLayerH, double, double, double, double)
 	Set a new rectangular spatial filter. 
void 	OGR_L_SetSpatialFilterEx (OGRLayerH, int iGeomField, OGRGeometryH hGeom)
 	Set a new spatial filter. 
void 	OGR_L_SetSpatialFilterRectEx (OGRLayerH, int iGeomField, double dfMinX, double dfMinY, double dfMaxX, double dfMaxY)
 	Set a new rectangular spatial filter. 
OGRErr 	OGR_L_SetAttributeFilter (OGRLayerH, const char *)
 	Set a new attribute query. 
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

OGRErr 	OGR_L_SetNextByIndex (OGRLayerH, long)
 	Move read cursor to the nIndex'th feature in the current resultset. 
OGRFeatureH 	OGR_L_GetFeature (OGRLayerH, long)
 	Fetch a feature by its identifier. 
OGRErr 	OGR_L_SetFeature (OGRLayerH, OGRFeatureH)
 	Rewrite an existing feature. 
OGRErr 	OGR_L_CreateFeature (OGRLayerH, OGRFeatureH)
 	Create and write a new feature within a layer. 
OGRErr 	OGR_L_DeleteFeature (OGRLayerH, long)
 	Delete feature from layer. 

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
(export 'OGR-L-Get-Layer-Defn)

;; --------------------------------------------------------

OGRSpatialReferenceH 	OGR_L_GetSpatialRef (OGRLayerH)
 	Fetch the spatial reference system for this layer. 
int 	OGR_L_GetFeatureCount (OGRLayerH, int)
 	Fetch the feature count in this layer. 
OGRErr 	OGR_L_GetExtent (OGRLayerH, OGREnvelope *, int)
 	Fetch the extent of this layer. 
OGRErr 	OGR_L_GetExtentEx (OGRLayerH, int iGeomField, OGREnvelope *psExtent, int bForce)
 	Fetch the extent of this layer, on the specified geometry field. 
int 	OGR_L_TestCapability (OGRLayerH, const char *)
 	Test if this layer supported the named capability. 
OGRErr 	OGR_L_CreateField (OGRLayerH, OGRFieldDefnH, int)
 	Create a new field on a layer. 
OGRErr 	OGR_L_CreateGeomField (OGRLayerH hLayer, OGRGeomFieldDefnH hFieldDefn, int bForce)
 	Create a new geometry field on a layer. 
OGRErr 	OGR_L_DeleteField (OGRLayerH, int iField)
 	Create a new field on a layer. 
OGRErr 	OGR_L_ReorderFields (OGRLayerH, int *panMap)
 	Reorder all the fields of a layer. 
OGRErr 	OGR_L_ReorderField (OGRLayerH, int iOldFieldPos, int iNewFieldPos)
 	Reorder an existing field on a layer. 
OGRErr 	OGR_L_AlterFieldDefn (OGRLayerH, int iField, OGRFieldDefnH hNewFieldDefn, int nFlags)
 	Alter the definition of an existing field on a layer. 
OGRErr 	OGR_L_StartTransaction (OGRLayerH)
 	For datasources which support transactions, StartTransaction creates a transaction. 
OGRErr 	OGR_L_CommitTransaction (OGRLayerH)
 	For datasources which support transactions, CommitTransaction commits a transaction. 
OGRErr 	OGR_L_RollbackTransaction (OGRLayerH)
 	For datasources which support transactions, RollbackTransaction will roll back a datasource to its state before the start of the current transaction. If no transaction is active, or the rollback fails, will return OGRERR_FAILURE. Datasources which do not support transactions will always return OGRERR_NONE. 
OGRErr 	OGR_L_SyncToDisk (OGRLayerH)
 	Flush pending changes to disk. 
const char * 	OGR_L_GetFIDColumn (OGRLayerH)
 	This method returns the name of the underlying database column being used as the FID column, or "" if not supported. 
const char * 	OGR_L_GetGeometryColumn (OGRLayerH)
 	This method returns the name of the underlying database column being used as the geometry column, or "" if not supported. 
OGRErr 	OGR_L_SetIgnoredFields (OGRLayerH, const char **)
 	Set which fields can be omitted when retrieving features from the layer. 
OGRErr 	OGR_L_Intersection (OGRLayerH, OGRLayerH, OGRLayerH, char **, GDALProgressFunc, void *)
 	Intersection of two layers. 
OGRErr 	OGR_L_Union (OGRLayerH, OGRLayerH, OGRLayerH, char **, GDALProgressFunc, void *)
 	Union of two layers. 
OGRErr 	OGR_L_SymDifference (OGRLayerH, OGRLayerH, OGRLayerH, char **, GDALProgressFunc, void *)
 	Symmetrical difference of two layers. 
OGRErr 	OGR_L_Identity (OGRLayerH, OGRLayerH, OGRLayerH, char **, GDALProgressFunc, void *)
 	Identify the features of this layer with the ones from the identity layer. 
OGRErr 	OGR_L_Update (OGRLayerH, OGRLayerH, OGRLayerH, char **, GDALProgressFunc, void *)
 	Update this layer with features from the update layer. 
OGRErr 	OGR_L_Clip (OGRLayerH, OGRLayerH, OGRLayerH, char **, GDALProgressFunc, void *)
 	Clip off areas that are not covered by the method layer. 
OGRErr 	OGR_L_Erase (OGRLayerH, OGRLayerH, OGRLayerH, char **, GDALProgressFunc, void *)
 	Remove areas that are covered by the method layer.