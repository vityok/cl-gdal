OGRStyleToolH 	OGR_ST_Create (OGRSTClassId eClassId) CPL_WARN_UNUSED_RESULT
 	OGRStyleTool factory. 
void 	OGR_ST_Destroy (OGRStyleToolH hST)
 	Destroy Style Tool. 
OGRSTClassId 	OGR_ST_GetType (OGRStyleToolH hST)
 	Determine type of Style Tool. 
OGRSTUnitId 	OGR_ST_GetUnit (OGRStyleToolH hST)
 	Get Style Tool units. 
void 	OGR_ST_SetUnit (OGRStyleToolH hST, OGRSTUnitId eUnit, double dfGroundPaperScale)
 	Set Style Tool units. 
const char * 	OGR_ST_GetParamStr (OGRStyleToolH hST, int eParam, int *bValueIsNull)
 	Get Style Tool parameter value as string. 
int 	OGR_ST_GetParamNum (OGRStyleToolH hST, int eParam, int *bValueIsNull)
 	Get Style Tool parameter value as an integer. 
double 	OGR_ST_GetParamDbl (OGRStyleToolH hST, int eParam, int *bValueIsNull)
 	Get Style Tool parameter value as a double. 
void 	OGR_ST_SetParamStr (OGRStyleToolH hST, int eParam, const char *pszValue)
 	Set Style Tool parameter value from a string. 
void 	OGR_ST_SetParamNum (OGRStyleToolH hST, int eParam, int nValue)
 	Set Style Tool parameter value from an integer. 
void 	OGR_ST_SetParamDbl (OGRStyleToolH hST, int eParam, double dfValue)
 	Set Style Tool parameter value from a double. 
const char * 	OGR_ST_GetStyleString (OGRStyleToolH hST)
 	Get the style string for this Style Tool. 
int 	OGR_ST_GetRGBFromString (OGRStyleToolH hST, const char *pszColor, int *pnRed, int *pnGreen, int *pnBlue, int *pnAlpha)
 	Return the r,g,b,a components of a color encoded in #RRGGBB[AA] format.