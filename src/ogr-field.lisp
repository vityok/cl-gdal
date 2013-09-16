OGRFieldDefnH 	OGR_Fld_Create (const char *, OGRFieldType) CPL_WARN_UNUSED_RESULT
 	Create a new field definition. 
void 	OGR_Fld_Destroy (OGRFieldDefnH)
 	Destroy a field definition. 
void 	OGR_Fld_SetName (OGRFieldDefnH, const char *)
 	Reset the name of this field. 
const char * 	OGR_Fld_GetNameRef (OGRFieldDefnH)
 	Fetch name of this field. 

;; --------------------------------------------------------

(cffi:defcfun ("OGR_Fld_GetType" OGR-Fld-Get-Type) :int

  "Fetch type of this field.

 This function is the same as the CPP method OGRFieldDefn::GetType().
 Parameters:	hDefn 	handle to the field definition to get type from.

 @return{OGRFieldType field type.}"
  (hDefn :pointer))			; OGRFieldDefnH
(export 'OGR-Fld-Get-Type)

;; --------------------------------------------------------

void 	OGR_Fld_SetType (OGRFieldDefnH, OGRFieldType)
 	Set the type of this field. This should never be done to an OGRFieldDefn that is already part of an OGRFeatureDefn. 
OGRJustification 	OGR_Fld_GetJustify (OGRFieldDefnH)
 	Get the justification for this field. 
void 	OGR_Fld_SetJustify (OGRFieldDefnH, OGRJustification)
 	Set the justification for this field. 
int 	OGR_Fld_GetWidth (OGRFieldDefnH)
 	Get the formatting width for this field. 
void 	OGR_Fld_SetWidth (OGRFieldDefnH, int)
 	Set the formatting width for this field in characters. 
int 	OGR_Fld_GetPrecision (OGRFieldDefnH)
 	Get the formatting precision for this field. This should normally be zero for fields of types other than OFTReal. 
void 	OGR_Fld_SetPrecision (OGRFieldDefnH, int)
 	Set the formatting precision for this field in characters. 
void 	OGR_Fld_Set (OGRFieldDefnH, const char *, OGRFieldType, int, int, OGRJustification)
 	Set defining parameters for a field in one call. 
int 	OGR_Fld_IsIgnored (OGRFieldDefnH hDefn)
 	Return whether this field should be omitted when fetching features. 
void 	OGR_Fld_SetIgnored (OGRFieldDefnH hDefn, int)
 	Set whether this field should be omitted when fetching features.

const char * 	OGR_GetFieldTypeName (OGRFieldType)
 	Fetch human readable name for a field type.