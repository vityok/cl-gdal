OGRFeatureDefnH 	OGR_FD_Create (const char *) CPL_WARN_UNUSED_RESULT
 	Create a new feature definition object to hold the field definitions. 
void 	OGR_FD_Destroy (OGRFeatureDefnH)
 	Destroy a feature definition object and release all memory associated with it. 
void 	OGR_FD_Release (OGRFeatureDefnH)
 	Drop a reference, and destroy if unreferenced. 
const char * 	OGR_FD_GetName (OGRFeatureDefnH)
 	Get name of the OGRFeatureDefn passed as an argument. 
int 	OGR_FD_GetFieldCount (OGRFeatureDefnH)
 	Fetch number of fields on the passed feature definition. 
OGRFieldDefnH 	OGR_FD_GetFieldDefn (OGRFeatureDefnH, int)
 	Fetch field definition of the passed feature definition. 
int 	OGR_FD_GetFieldIndex (OGRFeatureDefnH, const char *)
 	Find field by name. 
void 	OGR_FD_AddFieldDefn (OGRFeatureDefnH, OGRFieldDefnH)
 	Add a new field definition to the passed feature definition. 
OGRErr 	OGR_FD_DeleteFieldDefn (OGRFeatureDefnH hDefn, int iField)
 	Delete an existing field definition. 
OGRwkbGeometryType 	OGR_FD_GetGeomType (OGRFeatureDefnH)
 	Fetch the geometry base type of the passed feature definition. 
void 	OGR_FD_SetGeomType (OGRFeatureDefnH, OGRwkbGeometryType)
 	Assign the base geometry type for the passed layer (the same as the feature definition). 
int 	OGR_FD_IsGeometryIgnored (OGRFeatureDefnH)
 	Determine whether the geometry can be omitted when fetching features. 
void 	OGR_FD_SetGeometryIgnored (OGRFeatureDefnH, int)
 	Set whether the geometry can be omitted when fetching features. 
int 	OGR_FD_IsStyleIgnored (OGRFeatureDefnH)
 	Determine whether the style can be omitted when fetching features. 
void 	OGR_FD_SetStyleIgnored (OGRFeatureDefnH, int)
 	Set whether the style can be omitted when fetching features. 
int 	OGR_FD_Reference (OGRFeatureDefnH)
 	Increments the reference count by one. 
int 	OGR_FD_Dereference (OGRFeatureDefnH)
 	Decrements the reference count by one. 
int 	OGR_FD_GetReferenceCount (OGRFeatureDefnH)
 	Fetch current reference count. 
int 	OGR_FD_GetGeomFieldCount (OGRFeatureDefnH hFDefn)
 	Fetch number of geometry fields on the passed feature definition. 
OGRGeomFieldDefnH 	OGR_FD_GetGeomFieldDefn (OGRFeatureDefnH hFDefn, int i)
 	Fetch geometry field definition of the passed feature definition. 
int 	OGR_FD_GetGeomFieldIndex (OGRFeatureDefnH hFDefn, const char *pszName)
 	Find geometry field by name. 
void 	OGR_FD_AddGeomFieldDefn (OGRFeatureDefnH hFDefn, OGRGeomFieldDefnH hGFldDefn)
 	Add a new field definition to the passed feature definition. 
OGRErr 	OGR_FD_DeleteGeomFieldDefn (OGRFeatureDefnH hFDefn, int iGeomField)
 	Delete an existing geometry field definition. 
int 	OGR_FD_IsSame (OGRFeatureDefnH hFDefn, OGRFeatureDefnH hOtherFDefn)
 	Test if the feature definition is identical to the other one.