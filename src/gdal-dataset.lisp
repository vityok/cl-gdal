;;; -*- package: CL-GDAL; Syntax: Common-lisp; Base: 10 -*-

(in-package :cl-gdal)

;; --------------------------------------------------------

(cffi:defcfun ("GDALClose" GDAL-Close) :void
  "Close GDAL dataset.

 For non-shared datasets (opened with GDALOpen()) the dataset is
 closed using the C++ \"delete\" operator, recovering all dataset
 related resources. For shared datasets (opened with GDALOpenShared())
 the dataset is dereferenced, and closed only if the referenced count
 has dropped below 1.

Parameters:	hDS 	The dataset to close."
  (hDS gdal-dataset-h))			;

;; --------------------------------------------------------

(cffi:defcfun ("GDALOpen" GDALOpen) gdal-dataset-h


Open a raster file as a GDALDataset.

This function will try to open the passed file, or virtual dataset name by invoking the Open method of each registered GDALDriver in turn. The first successful open will result in a returned dataset. If all drivers fail then NULL is returned and an error is issued.

Several recommendations :
 If you open a dataset object with GA_Update access, it is not recommended to open a new dataset on the same underlying file.
 The returned dataset should only be accessed by one thread at a time. If you want to use it from different threads, you must add all necessary code (mutexes, etc.) to avoid concurrent use of the object. (Some drivers, such as GeoTIFF, maintain internal state variables that are updated each time a new block is read, thus preventing concurrent use.)

For drivers supporting the VSI virtual file API, it is possible to open a file in a .zip archive (see VSIInstallZipFileHandler()), in a .tar/.tar.gz/.tgz archive (see VSIInstallTarFileHandler()) or on a HTTP / FTP server (see VSIInstallCurlFileHandler())

In some situations (dealing with unverified data), the datasets can be opened in another process through the GDAL API Proxy mechanism.
See also:
GDALOpenShared()
Parameters:	pszFilename 	the name of the file to access. In the case of exotic drivers this may not refer to a physical file, but instead contain information for the driver on how to access a dataset. It should be in UTF-8 encoding.
	eAccess 	the desired access, either GA_Update or GA_ReadOnly. Many drivers support only read only access.

Returns:
A gdal-dataset-h handle or NULL on failure. For C++ applications this handle can be cast to a GDALDataset *.
(	const char * 	pszFilename,
		GDALAccess 	eAccess
	)
;; --------------------------------------------------------

(cffi:defcfun ("GDALOpenShared" GDALOpenShared) gdal-dataset-h


Open a raster file as a GDALDataset.

This function works the same as GDALOpen(), but allows the sharing of GDALDataset handles for a dataset with other callers to GDALOpenShared().

In particular, GDALOpenShared() will first consult it's list of currently open and shared GDALDataset's, and if the GetDescription() name for one exactly matches the pszFilename passed to GDALOpenShared() it will be referenced and returned.

Starting with GDAL 1.6.0, if GDALOpenShared() is called on the same pszFilename from two different threads, a different GDALDataset object will be returned as it is not safe to use the same dataset from different threads, unless the user does explicitely use mutexes in its code.

For drivers supporting the VSI virtual file API, it is possible to open a file in a .zip archive (see VSIInstallZipFileHandler()), in a .tar/.tar.gz/.tgz archive (see VSIInstallTarFileHandler()) or on a HTTP / FTP server (see VSIInstallCurlFileHandler())

In some situations (dealing with unverified data), the datasets can be opened in another process through the GDAL API Proxy mechanism.
See also:
GDALOpen()
Parameters:	pszFilename 	the name of the file to access. In the case of exotic drivers this may not refer to a physical file, but instead contain information for the driver on how to access a dataset. It should be in UTF-8 encoding.
	eAccess 	the desired access, either GA_Update or GA_ReadOnly. Many drivers support only read only access.

Returns:
A gdal-dataset-h handle or NULL on failure. For C++ applications this handle can be cast to a GDALDataset *.
(	const char * 	pszFilename,
		GDALAccess 	eAccess
	)
;; --------------------------------------------------------

(cffi:defcfun ("GDALCreateDatasetMaskBand" GDALCreateDatasetMaskBand) cpl-err


Adds a mask band to the dataset.
See also:
GDALDataset::CreateMaskBand()
(	gdal-dataset-h 	hDS,
		int 	nFlags
	)
;; --------------------------------------------------------

(cffi:defcfun ("GDALAddBand" GDALAddBand) cpl-err


Add a band to a dataset.
See also:
GDALDataset::AddBand().
(	gdal-dataset-h 	hDataset,
		GDALDataType 	eType,
		char ** 	papszOptions
	)
;; --------------------------------------------------------

(cffi:defcfun ("GDALDatasetAdviseRead" GDALDatasetAdviseRead) cpl-err


Advise driver of upcoming read requests.
See also:
GDALDataset::AdviseRead()
(	gdal-dataset-h 	hDS,
		int 	nXOff,
		int 	nYOff,
		int 	nXSize,
		int 	nYSize,
		int 	nBufXSize,
		int 	nBufYSize,
		GDALDataType 	eDT,
		int 	nBandCount,
		int * 	panBandMap,
		char ** 	papszOptions
	)
;; --------------------------------------------------------

(cffi:defcfun ("GDALDatasetCopyWholeRaster" GDALDatasetCopyWholeRaster) cpl-err


Copy all dataset raster data.

This function copies the complete raster contents of one dataset to another similarly configured dataset. The source and destination dataset must have the same number of bands, and the same width and height. The bands do not have to have the same data type.

This function is primarily intended to support implementation of driver specific CreateCopy() functions. It implements efficient copying, in particular \"chunking\" the copy in substantial blocks and, if appropriate, performing the transfer in a pixel interleaved fashion.

Currently the only papszOptions value supported are : \"INTERLEAVE=PIXEL\" to force pixel interleaved operation and \"COMPRESSED=YES\" to force alignment on target dataset block sizes to achieve best compression. More options may be supported in the future.

Parameters:	hSrcDS 	the source dataset
	hDstDS 	the destination dataset
	papszOptions 	transfer hints in \"StringList\" Name=Value format.
	pfnProgress 	progress reporting function.
	pProgressData 	callback data for progress function.

Returns:
CE_None on success, or CE_Failure on failure.
(	gdal-dataset-h 	hSrcDS,
		gdal-dataset-h 	hDstDS,
		char ** 	papszOptions,
		GDALProgressFunc 	pfnProgress,
		void * 	pProgressData
	)
;; --------------------------------------------------------

(cffi:defcfun ("GDALDatasetRasterIO" GDALDatasetRasterIO) cpl-err


Read/write a region of image data from multiple bands.
See also:
GDALDataset::RasterIO()
(	gdal-dataset-h 	hDS,
		GDALRWFlag 	eRWFlag,
		int 	nXOff,
		int 	nYOff,
		int 	nXSize,
		int 	nYSize,
		void * 	pData,
		int 	nBufXSize,
		int 	nBufYSize,
		GDALDataType 	eBufType,
		int 	nBandCount,
		int * 	panBandMap,
		int 	nPixelSpace,
		int 	nLineSpace,
		int 	nBandSpace
	)
;; --------------------------------------------------------

(cffi:defcfun ("GDALFlushCache" GDALFlushCache) void


Flush all write cached data to disk.
See also:
GDALDataset::FlushCache().
(	gdal-dataset-h 	hDS	 )

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetAccess" GDALGetAccess) int


Return access flag.
See also:
GDALDataset::GetAccess()
(	gdal-dataset-h 	hDS	 )
;; --------------------------------------------------------

(cffi:defcfun ("GDALGetFileList" GDALGetFileList) char**


Fetch files forming dataset.
See also:
GDALDataset::GetFileList()
(	gdal-dataset-h 	hDS	 )
;; --------------------------------------------------------

(cffi:defcfun ("GDALGetGCPCount" GDALGetGCPCount) int


Get number of GCPs.
See also:
GDALDataset::GetGCPCount()
(	gdal-dataset-h 	hDS	 )

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetGCPProjection" GDALGetGCPProjection) const char*


Get output projection for GCPs.
See also:
GDALDataset::GetGCPProjection()
(	gdal-dataset-h 	hDS	 )

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetGCPs" GDALGetGCPs) const GDAL_GCP*


Fetch GCPs.
See also:
GDALDataset::GetGCPs()
(	gdal-dataset-h 	hDS	 )

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetGeoTransform" GDALGetGeoTransform) cpl-err


Fetch the affine transformation coefficients.
See also:
GDALDataset::GetGeoTransform()
(	gdal-dataset-h 	hDS,
		double * 	padfTransform
	)

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetInternalHandle" GDALGetInternalHandle) void*


Fetch a format specific internally meaningful handle.
See also:
GDALDataset::GetInternalHandle()
(	gdal-dataset-h 	hDS,
		const char * 	pszRequest
	)

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetProjectionRef" GDALGetProjectionRef) const char*


Fetch the projection definition string for this dataset.
See also:
GDALDataset::GetProjectionRef()
(	gdal-dataset-h 	hDS	 )

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetRasterBand" GDALGetRasterBand) GDALRasterBandH


Fetch a band object for a dataset.
See also:
GDALDataset::GetRasterBand().
(	gdal-dataset-h 	hDS,
		int 	nBandId
	)
;; --------------------------------------------------------

(cffi:defcfun ("GDALSetGCPs" GDALSetGCPs) 


Assign GCPs.
See also:
GDALDataset::SetGCPs()
(	gdal-dataset-h 	hDS,
		int 	nGCPCount,
		const GDAL_GCP * 	pasGCPList,
		const char * 	pszGCPProjection
	)

;; --------------------------------------------------------

(cffi:defcfun ("GDALSetGeoTransform" GDALSetGeoTransform) cpl-err


Set the affine transformation coefficients.
See also:
GDALDataset::SetGeoTransform()
(	gdal-dataset-h 	hDS,
		double * 	padfTransform
	)
;; --------------------------------------------------------

(cffi:defcfun ("GDALDereferenceDataset" GDALDereferenceDataset) int


Subtract one from dataset reference count.
See also:
GDALDataset::Dereference()
(	gdal-dataset-h 	hDataset	 )
;; --------------------------------------------------------

(cffi:defcfun ("GDALGetDatasetDriver" GDALGetDatasetDriver) gdal-driver-h


Fetch the driver to which this dataset relates.
See also:
GDALDataset::GetDriver()
(	gdal-dataset-h 	hDataset	 )

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetOpenDatasets" GDALGetOpenDatasets) void


Fetch all open GDAL dataset handles.
See also:
GDALDataset::GetOpenDatasets()
(	gdal-dataset-h ** 	ppahDSList,
		int * 	pnCount
	)
;; --------------------------------------------------------

(cffi:defcfun ("GDALGetRasterXSize" GDALGetRasterXSize) int


Fetch raster width in pixels.
See also:
GDALDataset::GetRasterXSize().
(	gdal-dataset-h 	hDataset	 )
;; --------------------------------------------------------

(cffi:defcfun ("GDALGetRasterYSize" GDALGetRasterYSize) int


Fetch raster height in pixels.
See also:
GDALDataset::GetRasterYSize().
(	gdal-dataset-h 	hDataset	 )

;; --------------------------------------------------------

(cffi:defcfun ("GDALReferenceDataset" GDALReferenceDataset) int


Add one to dataset reference count.
See also:
GDALDataset::Reference()
(	gdal-dataset-h 	hDataset	 )

;; --------------------------------------------------------

(cffi:defcfun ("GDALSetProjection" GDALSetProjection) cpl-err


Set the projection reference string for this dataset.
See also:
GDALDataset::SetProjection()
(	gdal-dataset-h 	hDS,
		const char * 	pszProjection
	)
;; --------------------------------------------------------

(cffi:defcfun ("GDALBuildOverviews" GDALBuildOverviews) cpl-err


Build raster overview(s).
See also:
GDALDataset::BuildOverviews()

(	gdal-dataset-h 	hDataset,
		const char * 	pszResampling,
		int 	nOverviews,
		int * 	panOverviewList,
		int 	nListBands,
		int * 	panBandList,
		GDALProgressFunc 	pfnProgress,
		void * 	pProgressData
	)
;; EOF