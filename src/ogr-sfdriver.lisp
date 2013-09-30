const char * 	OGR_Dr_GetName (OGRSFDriverH)
 	Fetch name of driver (file format). This name should be relatively short (10-40 characters), and should reflect the underlying file format. For instance "ESRI Shapefile". 
OGRDataSourceH 	OGR_Dr_Open (OGRSFDriverH, const char *, int) CPL_WARN_UNUSED_RESULT
 	Attempt to open file with this driver. 
int 	OGR_Dr_TestCapability (OGRSFDriverH, const char *)
 	Test if capability is available. 
OGRDataSourceH 	OGR_Dr_CreateDataSource (OGRSFDriverH, const char *, char **) CPL_WARN_UNUSED_RESULT
 	This function attempts to create a new data source based on the passed driver. 
OGRDataSourceH 	OGR_Dr_CopyDataSource (OGRSFDriverH, OGRDataSourceH, const char *, char **) CPL_WARN_UNUSED_RESULT
 	This function creates a new datasource by copying all the layers from the source datasource. 
OGRErr 	OGR_Dr_DeleteDataSource (OGRSFDriverH, const char *)
 	Delete a datasource.

