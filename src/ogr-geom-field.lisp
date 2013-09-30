OGRGeomFieldDefnH 	OGR_GFld_Create (const char *, OGRwkbGeometryType) CPL_WARN_UNUSED_RESULT
 	Create a new field geometry definition. 
void 	OGR_GFld_Destroy (OGRGeomFieldDefnH)
 	Destroy a geometry field definition. 
void 	OGR_GFld_SetName (OGRGeomFieldDefnH, const char *)
 	Reset the name of this field. 
const char * 	OGR_GFld_GetNameRef (OGRGeomFieldDefnH)
 	Fetch name of this field. 
OGRwkbGeometryType 	OGR_GFld_GetType (OGRGeomFieldDefnH)
 	Fetch geometry type of this field. 
void 	OGR_GFld_SetType (OGRGeomFieldDefnH, OGRwkbGeometryType)
 	Set the geometry type of this field. This should never be done to an OGRGeomFieldDefn that is already part of an OGRFeatureDefn. 
OGRSpatialReferenceH 	OGR_GFld_GetSpatialRef (OGRGeomFieldDefnH)
 	Fetch spatial reference system of this field. 
void 	OGR_GFld_SetSpatialRef (OGRGeomFieldDefnH, OGRSpatialReferenceH hSRS)
 	Set the spatial reference of this field. 
int 	OGR_GFld_IsIgnored (OGRGeomFieldDefnH hDefn)
 	Return whether this field should be omitted when fetching features. 
void 	OGR_GFld_SetIgnored (OGRGeomFieldDefnH hDefn, int)
 	Set whether this field should be omitted when fetching features.