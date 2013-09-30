OGRStyleMgrH 	OGR_SM_Create (OGRStyleTableH hStyleTable) CPL_WARN_UNUSED_RESULT
 	OGRStyleMgr factory. 
void 	OGR_SM_Destroy (OGRStyleMgrH hSM)
 	Destroy Style Manager. 
const char * 	OGR_SM_InitFromFeature (OGRStyleMgrH hSM, OGRFeatureH hFeat)
 	Initialize style manager from the style string of a feature. 
int 	OGR_SM_InitStyleString (OGRStyleMgrH hSM, const char *pszStyleString)
 	Initialize style manager from the style string. 
int 	OGR_SM_GetPartCount (OGRStyleMgrH hSM, const char *pszStyleString)
 	Get the number of parts in a style. 
OGRStyleToolH 	OGR_SM_GetPart (OGRStyleMgrH hSM, int nPartId, const char *pszStyleString)
 	Fetch a part (style tool) from the current style. 
int 	OGR_SM_AddPart (OGRStyleMgrH hSM, OGRStyleToolH hST)
 	Add a part (style tool) to the current style. 
int 	OGR_SM_AddStyle (OGRStyleMgrH hSM, const char *pszStyleName, const char *pszStyleString)
 	Add a style to the current style table.