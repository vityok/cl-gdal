OGRStyleTableH 	OGR_STBL_Create (void) CPL_WARN_UNUSED_RESULT
 	OGRStyleTable factory. 
void 	OGR_STBL_Destroy (OGRStyleTableH hSTBL)
 	Destroy Style Table. 
int 	OGR_STBL_SaveStyleTable (OGRStyleTableH hStyleTable, const char *pszFilename)
 	Save a style table to a file. 
int 	OGR_STBL_LoadStyleTable (OGRStyleTableH hStyleTable, const char *pszFilename)
 	Load a style table from a file. 
const char * 	OGR_STBL_Find (OGRStyleTableH hStyleTable, const char *pszName)
 	Get a style string by name. 
void 	OGR_STBL_ResetStyleStringReading (OGRStyleTableH hStyleTable)
 	Reset the next style pointer to 0. 
const char * 	OGR_STBL_GetNextStyle (OGRStyleTableH hStyleTable)
 	Get the next style string from the table. 
const char * 	OGR_STBL_GetLastStyleName (OGRStyleTableH hStyleTable)