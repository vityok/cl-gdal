OGRFeatureH 	OGR_F_Create (OGRFeatureDefnH) CPL_WARN_UNUSED_RESULT
 	Feature factory. 
void 	OGR_F_Destroy (OGRFeatureH)
 	Destroy feature. 
OGRFeatureDefnH 	OGR_F_GetDefnRef (OGRFeatureH)
 	Fetch feature definition. 
OGRErr 	OGR_F_SetGeometryDirectly (OGRFeatureH, OGRGeometryH)
 	Set feature geometry. 
OGRErr 	OGR_F_SetGeometry (OGRFeatureH, OGRGeometryH)
 	Set feature geometry. 
OGRGeometryH 	OGR_F_GetGeometryRef (OGRFeatureH)
 	Fetch an handle to feature geometry. 
OGRGeometryH 	OGR_F_StealGeometry (OGRFeatureH)
 	Take away ownership of geometry. 
OGRFeatureH 	OGR_F_Clone (OGRFeatureH)
 	Duplicate feature. 
int 	OGR_F_Equal (OGRFeatureH, OGRFeatureH)
 	Test if two features are the same. 
int 	OGR_F_GetFieldCount (OGRFeatureH)
 	Fetch number of fields on this feature This will always be the same as the field count for the OGRFeatureDefn. 
OGRFieldDefnH 	OGR_F_GetFieldDefnRef (OGRFeatureH, int)
 	Fetch definition for this field. 
int 	OGR_F_GetFieldIndex (OGRFeatureH, const char *)
 	Fetch the field index given field name. 
int 	OGR_F_IsFieldSet (OGRFeatureH, int)
 	Test if a field has ever been assigned a value or not. 
void 	OGR_F_UnsetField (OGRFeatureH, int)
 	Clear a field, marking it as unset. 
OGRField * 	OGR_F_GetRawFieldRef (OGRFeatureH, int)
 	Fetch an handle to the internal field value given the index. 
int 	OGR_F_GetFieldAsInteger (OGRFeatureH, int)
 	Fetch field value as integer. 
double 	OGR_F_GetFieldAsDouble (OGRFeatureH, int)
 	Fetch field value as a double. 
const char * 	OGR_F_GetFieldAsString (OGRFeatureH, int)
 	Fetch field value as a string. 
const int * 	OGR_F_GetFieldAsIntegerList (OGRFeatureH, int, int *)
 	Fetch field value as a list of integers. 
const double * 	OGR_F_GetFieldAsDoubleList (OGRFeatureH, int, int *)
 	Fetch field value as a list of doubles. 
char ** 	OGR_F_GetFieldAsStringList (OGRFeatureH, int)
 	Fetch field value as a list of strings. 
GByte * 	OGR_F_GetFieldAsBinary (OGRFeatureH, int, int *)
 	Fetch field value as binary. 
int 	OGR_F_GetFieldAsDateTime (OGRFeatureH, int, int *, int *, int *, int *, int *, int *, int *)
 	Fetch field value as date and time. 
void 	OGR_F_SetFieldInteger (OGRFeatureH, int, int)
 	Set field to integer value. 
void 	OGR_F_SetFieldDouble (OGRFeatureH, int, double)
 	Set field to double value. 
void 	OGR_F_SetFieldString (OGRFeatureH, int, const char *)
 	Set field to string value. 
void 	OGR_F_SetFieldIntegerList (OGRFeatureH, int, int, int *)
 	Set field to list of integers value. 
void 	OGR_F_SetFieldDoubleList (OGRFeatureH, int, int, double *)
 	Set field to list of doubles value. 
void 	OGR_F_SetFieldStringList (OGRFeatureH, int, char **)
 	Set field to list of strings value. 
void 	OGR_F_SetFieldRaw (OGRFeatureH, int, OGRField *)
 	Set field. 
void 	OGR_F_SetFieldBinary (OGRFeatureH, int, int, GByte *)
 	Set field to binary data. 
void 	OGR_F_SetFieldDateTime (OGRFeatureH, int, int, int, int, int, int, int, int)
 	Set field to datetime. 
int 	OGR_F_GetGeomFieldCount (OGRFeatureH hFeat)
 	Fetch number of geometry fields on this feature This will always be the same as the geometry field count for the OGRFeatureDefn. 
OGRGeomFieldDefnH 	OGR_F_GetGeomFieldDefnRef (OGRFeatureH hFeat, int iField)
 	Fetch definition for this geometry field. 
int 	OGR_F_GetGeomFieldIndex (OGRFeatureH hFeat, const char *pszName)
 	Fetch the geometry field index given geometry field name. 
OGRGeometryH 	OGR_F_GetGeomFieldRef (OGRFeatureH hFeat, int iField)
 	Fetch an handle to feature geometry. 
OGRErr 	OGR_F_SetGeomFieldDirectly (OGRFeatureH hFeat, int iField, OGRGeometryH hGeom)
 	Set feature geometry of a specified geometry field. 
OGRErr 	OGR_F_SetGeomField (OGRFeatureH hFeat, int iField, OGRGeometryH hGeom)
 	Set feature geometry of a specified geometry field. 
long 	OGR_F_GetFID (OGRFeatureH)
 	Get feature identifier. 
OGRErr 	OGR_F_SetFID (OGRFeatureH, long)
 	Set the feature identifier. 
void 	OGR_F_DumpReadable (OGRFeatureH, FILE *)
 	Dump this feature in a human readable form. 
OGRErr 	OGR_F_SetFrom (OGRFeatureH, OGRFeatureH, int)
 	Set one feature from another. 
OGRErr 	OGR_F_SetFromWithMap (OGRFeatureH, OGRFeatureH, int, int *)
 	Set one feature from another. 
const char * 	OGR_F_GetStyleString (OGRFeatureH)
 	Fetch style string for this feature. 
void 	OGR_F_SetStyleString (OGRFeatureH, const char *)
 	Set feature style string. This method operate exactly as OGR_F_SetStyleStringDirectly() except that it does not assume ownership of the passed string, but instead makes a copy of it. 
void 	OGR_F_SetStyleStringDirectly (OGRFeatureH, char *)
 	Set feature style string. This method operate exactly as OGR_F_SetStyleString() except that it assumes ownership of the passed string.