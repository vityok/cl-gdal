;;; -*- package: CL-GDAL; Syntax: Common-lisp; Base: 10 -*-

(in-package :cl-gdal)

;; --------------------------------------------------------

(cffi:defcfun ("GDALAddDerivedBandPixelFunc" gdal-add-derived-band-pixel-func) cpl-err
  "This adds a pixel function to the global list of available pixel
 functions for derived bands.

 Pixel functions must be registered in this way before a derived band
 tries to access data.

 Derived bands are stored with only the name of the pixel function
 that it will apply, and if a pixel function matching the name is not
 found the IRasterIO() call will do nothing.

 @argument[pszFuncName]{Name used to access pixel function}
 @argument[pfnNewFunction]{Pixel function associated with name. An
 existing pixel function registered with the same name will be
 replaced with the new one.}

 @return{CE_None, invalid (NULL) parameters are currently ignored.}"
  (pszFuncName :string)
  (pfnNewFunctio GDALDerivedPixelFunc))

;; --------------------------------------------------------

(cffi:defcfun ("GDALApplyGeoTransform" gdal-apply-geo-transform) :void
  "Apply GeoTransform to x/y coordinate.

 Applies the following computation, converting a (pixel,line)
 coordinate into a georeferenced (geo_x,geo_y) location.

 *pdfGeoX = padfGeoTransform[0] + dfPixel * padfGeoTransform[1] + dfLine * padfGeoTransform[2];
 *pdfGeoY = padfGeoTransform[3] + dfPixel * padfGeoTransform[4] + dfLine * padfGeoTransform[5];

 @argument[padfGeoTransform]{Six coefficient GeoTransform to apply.}
 @argument[dfPixel]{Input pixel position.}
 @argument[dfLine]{Input line position.}
 @argument[pdfGeoX]{output location where geo_x (easting/longitude) location is placed.}
 @argument[pdfGeoY]{output location where geo_y (northing/latitude) location is placed.}"
  (padfGeoTransform (:pointer :double))
  (dfPixel :double)
  (dfLine :double)
  (pdfGeoX (:pointer :double))
  (pdfGeoY (:pointer :double)))

;; --------------------------------------------------------

(cffi:defcfun ("GDALCheckVersion" gdal-check-version) :int
  "Return TRUE if GDAL library version at runtime matches
 nVersionMajor.nVersionMinor.

 The purpose of this method is to ensure that calling code will run
 with the GDAL version it is compiled for. It is primarly intented for
 external plugins.

 @argument[nVersionMajor]{Major version to be tested against}
 @argument[nVersionMinor]{Minor version to be tested against}
 @argument[pszCallingComponentName]{If not NULL, in case of version
 mismatch, the method will issue a failure mentionning the name of the
 calling component.}

 @return{TRUE if GDAL library version at runtime matches
 nVersionMajor.nVersionMinor, FALSE otherwise.}"
  (nVersionMajor :int)
  (nVersionMinor :int)
  (pszCallingComponentNam :string))

;; --------------------------------------------------------

(cffi:defcfun ("GDALCloneColorTable" gdal-clone-color-table) GDALColorTableH
  "Make a copy of a color table.

This function is the same as the C++ method GDALColorTable::Clone()"
  (hTable GDALColorTableH))

;; --------------------------------------------------------

(cffi:defcfun ("GDALCopyBits" gdal-copy-bits) :void
  "Bitwise word copying.

 A function for moving sets of partial bytes around. Loosely speaking
 this is a bitswise analog to GDALCopyWords().

 It copies nStepCount \"words\" where each word is nBitCount bits
 long. The nSrcStep and nDstStep are the number of bits from the start
 of one word to the next (same as nBitCount if they are packed). The
 nSrcOffset and nDstOffset are the offset into the source and
 destination buffers to start at, also measured in bits.

 All bit offsets are assumed to start from the high order bit in a
 byte (ie. most significant bit first). Currently this function is not
 very optimized, but it may be improved for some common cases in the
 future as needed.

 @argument[pabySrcData]{the source data buffer.}
 @argument[nSrcOffset]{the offset (in bits) in pabySrcData to the
 start of the first word to copy.}
 @argument[nSrcStep]{the offset in bits from the start one source word
 to the start of the next.}
 @argument[pabyDstData]{the destination data buffer.}
 @argument[nDstOffset]{the offset (in bits) in pabyDstData to the
 start of the first word to copy over.}
 @argument[nDstStep]{the offset in bits from the start one word to the
 start of the next.}
 @argument[nBitCount]{the number of bits in a word to be copied.}
 @argument[nStepCount]{the number of words to copy.}"
  (pabySrcData (:pointer GByte))
  (nSrcOffset :int)
  (nSrcStep :int)
  (pabyDstData (:pointer GByte))
  (nDstOffset :int)
  (nDstStep :int)
  (nBitCount :int)
  (nStepCoun :int))

;; --------------------------------------------------------

(cffi:defcfun ("GDALCopyDatasetFiles" gdal-copy-dataset-files) CPLErr


Copy the files of a dataset.
See also:
GDALDriver::CopyFiles()
(	(hDriver GDALDriverH)
		(pszNewName :string)
		(pszOldNam :string)
	)

;; --------------------------------------------------------

(cffi:defcfun ("GDALCopyWords" GDALCopyWords) :void


Copy pixel words from buffer to buffer.

 This function is used to copy pixel word values from one memory
 buffer to another, with support for conversion between data types,
 and differing step factors. The data type conversion is done using
 the normal GDAL rules. Values assigned to a lower range integer type
 are clipped. For instance assigning GDT_Int16 values to a GDT_Byte
 buffer will cause values less the 0 to be set to 0, and values larger
 than 255 to be set to 255. Assignment from floating point to integer
 uses default C type casting semantics. Assignment from non-complex to
 complex will result in the imaginary part being set to zero on
 output. Assigment from complex to non-complex will result in the
 complex portion being lost and the real component being
 preserved (not magnitidue!).

 No assumptions are made about the source or destination words
 occuring on word boundaries. It is assumed that all values are in
 native machine byte order.

 @argument[pSrcData]{Pointer to source data to be converted.}
 @argument[eSrcType]{the source data type (see GDALDataType enum)}
 @argument[nSrcPixelOffset]{Source pixel offset, in bytes}
 @argument[pDstData]{Pointer to buffer where destination data should go}
 @argument[eDstType]{the destination data type (see GDALDataType enum)}
 @argument[nDstPixelOffset]{Destination pixel offset, in bytes}
 @argument[nWordCount]{number of words to be copied}

 Note: When adding a new data type to GDAL, you must do the following
 to support it properly within the GDALCopyWords function: 1. Add the
 data type to the switch on eSrcType in GDALCopyWords. This should
 invoke the appropriate GDALCopyWordsFromT wrapper. 2. Add the data
 type to the switch on eDstType in GDALCopyWordsFromT. This should
 call the appropriate GDALCopyWordsT template. 3. If appropriate,
 overload the appropriate CopyWord template in the above
 namespace. This will ensure that any conversion issues are
 handled (cases like the float -> int32 case, where the min/max)
 values are subject to roundoff error.

(	void * pSrcData,
		(eSrcType GDALDataType)
		(nSrcPixelOffset int)
		void * pDstData,
		(eDstType GDALDataType)
		(nDstPixelOffset int)
		(nWordCoun int)
	)

;; --------------------------------------------------------

(cffi:defcfun ("GDALCreate" GDALCreate) GDALDatasetH


Create a new dataset with this driver.
See also:
GDALDriver::Create()
(	(hDriver GDALDriverH)
		(pszFilename :string)
		(nXSize int)
		(nYSize int)
		(nBands int)
		(eBandType GDALDataType)
		char ** papszOptions
	)

;; --------------------------------------------------------

(cffi:defcfun ("GDALCreateColorRamp" GDALCreateColorRamp) :void


Create color ramp.

This function is the same as the C++ method GDALColorTable::CreateColorRamp()
(	(hTable GDALColorTableH)
		(nStartIndex int)
		const GDALColorEntry * psStartColor,
		(nEndIndex int)
		const GDALColorEntry * psEndColor
	)

;; --------------------------------------------------------

(cffi:defcfun ("GDALCreateColorTable" GDALCreateColorTable) GDALColorTableH


Construct a new color table.

This function is the same as the C++ method GDALColorTable::GDALColorTable()
(	(eInterp	  GDALPaletteInterp)

;; --------------------------------------------------------

(cffi:defcfun ("GDALCreateCopy" GDALCreateCopy) GDALDatasetH


Create a copy of a dataset.
See also:
GDALDriver::CreateCopy()
(	(hDriver GDALDriverH)
		(pszFilename :string)
		(hSrcDS GDALDatasetH)
		(bStrict int)
		char ** papszOptions,
		(pfnProgress GDALProgressFunc)
		void * pProgressData
	)

;; --------------------------------------------------------

(cffi:defcfun ("GDALCreateRasterAttributeTable" GDALCreateRasterAttributeTable) GDALRasterAttributeTableH


Construct empty table.

 This function is the same as the C++ method
 GDALDefaultRasterAttributeTable::GDALDefaultRasterAttributeTable()

(	void 	 )

;; --------------------------------------------------------

(cffi:defcfun ("GDALDataTypeIsComplex" GDALDataTypeIsComplex) int


Is data type complex?

 @return{TRUE if the passed type is complex (one of GDT_CInt16,
 GDT_CInt32, GDT_CFloat32 or GDT_CFloat64), that is it consists of a
 real and imaginary component.}

(	(eDataType	  GDALDataType)

;; --------------------------------------------------------

(cffi:defcfun ("GDALDataTypeUnion" GDALDataTypeUnion) GDALDataType


 Return the smallest data type that can fully express both input data
 types.


 @argument[eType1]{first data type.}
 @argument[eType2]{second data type.}

 @return{a data type able to express eType1 and eType2.}

(	(eType1 GDALDataType)
		(eType GDALDataType)
	)
;; --------------------------------------------------------

(cffi:defcfun ("GDALDecToPackedDMS" GDALDecToPackedDMS) double


Convert decimal degrees into packed DMS value (DDDMMMSSS.SS).

See CPLDecToPackedDMS().
(	(dfDec	  double)

;; --------------------------------------------------------

(cffi:defcfun ("GDALDeleteDataset" GDALDeleteDataset) CPLErr


Delete named dataset.
See also:
GDALDriver::Delete()
(	(hDriver GDALDriverH)
		(pszFilenam :string)
	)

;; --------------------------------------------------------

(cffi:defcfun ("GDALDeregisterDriver" GDALDeregisterDriver) :void


Deregister the passed driver.
See also:
GDALDriverManager::GetDeregisterDriver()
(	(hDriver	  GDALDriverH)

;; --------------------------------------------------------

(cffi:defcfun ("GDALDestroyColorTable" GDALDestroyColorTable) :void


Destroys a color table.

This function is the same as the C++ method GDALColorTable::~GDALColorTable()
(	(hTable	  GDALColorTableH)

;; --------------------------------------------------------

(cffi:defcfun ("GDALDestroyDriver" GDALDestroyDriver) :void


Destroy a GDALDriver.

 This is roughly equivelent to deleting the driver, but is guaranteed
 to take place in the GDAL heap. It is important this that function
 not be called on a driver that is registered with the
 GDALDriverManager.

@argument[hDriver]{the driver to destroy.}
(	(hDriver	  GDALDriverH)

;; --------------------------------------------------------

(cffi:defcfun ("GDALDestroyDriverManager" GDALDestroyDriverManager) :void


Destroy the driver manager.

Incidently unloads all managed drivers.

 NOTE: This function is not thread safe. It should not be called while
 other threads are actively using GDAL.

(	void 	 )

;; --------------------------------------------------------

(cffi:defcfun ("GDALDestroyRasterAttributeTable" GDALDestroyRasterAttributeTable) :void


Destroys a RAT.

 This function is the same as the C++ method
 GDALRasterAttributeTable::~GDALRasterAttributeTable()

(	(hRAT	  GDALRasterAttributeTableH)

;; --------------------------------------------------------

(cffi:defcfun ("GDALDumpOpenDatasets" GDALDumpOpenDatasets) int


List open datasets.

 Dumps a list of all open datasets (shared or not) to the indicated
 text file (may be stdout or stderr). This function is primarily
 intended to assist in debugging \"dataset leaks\" and reference
 counting issues. The information reported includes the dataset name,
 referenced count, shared status, driver name, size, and band count.

(	FILE * fp	 )

;; --------------------------------------------------------

(cffi:defcfun ("GDALFlushCacheBlock" GDALFlushCacheBlock) int


Try to flush one cached raster block.

 This function will search the first unlocked raster block and will
 flush it to release the associated memory.

 @return{TRUE if one block was flushed, FALSE if there are no cached
 blocks or if they are currently locked.}

(	void 	 )

;; --------------------------------------------------------

(cffi:defcfun ("GDALGCPsToGeoTransform" GDALGCPsToGeoTransform) int

Generate Geotransform from GCPs.

 Given a set of GCPs perform first order fit as a geotransform.

 Due to imprecision in the calculations the fit algorithm will often
 return non-zero rotational coefficients even if given perfectly
 non-rotated inputs. A special case has been implemented for corner
 corner coordinates given in TL, TR, BR, BL order. So when using this
 to get a geotransform from 4 corner coordinates, pass them in this
 order.

 @argument[nGCPCount]{the number of GCPs being passed in.}
 @argument[pasGCPs]{the list of GCP structures.}
 @argument[padfGeoTransform]{the six double array in which the affine
 geotransformation will be returned.}
 @argument[bApproxOK]{If FALSE the function will fail if the
 geotransform is not essentially an exact fit (within 0.25 pixel) for
 all GCPs.}

 @return{TRUE on success or FALSE if there aren't enough points to
 prepare a geotransform, the pointers are ill-determined or if
 bApproxOK is FALSE and the fit is poor.}

(	(nGCPCount int)
		const GDAL_GCP * pasGCPs,
		(:pointer :double) padfGeoTransform,
		(bApproxO int)
	)

;; --------------------------------------------------------

(cffi:defcfun ("GDALGeneralCmdLineProcessor" GDALGeneralCmdLineProcessor) int


General utility option processing.

 This function is intended to provide a variety of generic commandline
 options for all GDAL commandline utilities. It takes care of the
 following commandline options:

--version: report version of GDAL in use.
--build: report build info about GDAL in use.
--license: report GDAL license info.
--formats: report all format drivers configured.
--format [format]: report details of one format driver.
--optfile filename: expand an option file into the argument list.
--config key value: set system configuration option.
--debug [on/off/value]: set debug level.
--mempreload dir: preload directory contents into /vsimem
--pause: Pause for user input (allows time to attach debugger)
--locale [locale]: Install a locale using setlocale() (debugging)
--help-general: report detailed help on general options.

 The argument array is replaced \"in place\" and should be freed with
 CSLDestroy() when no longer needed. The typical usage looks something
 like the following. Note that the formats should be registered so
 that the --formats and --format options will work properly.

int main( int argc, char ** argv ) { GDALAllRegister();

argc = GDALGeneralCmdLineProcessor( argc, &argv, 0 ); if( argc < 1 ) exit( -argc );

 @argument[nArgc]{number of values in the argument list.}
 @argument[ppapszArgv]{pointer to the argument list array (will be updated in place).}
 @argument[nOptions]{unused for now.}

 @return{updated nArgc argument count. Return of 0 requests terminate
 without error, return of -1 requests exit with error code.}

(	(nArgc int)
		char *** ppapszArgv,
		(nOption int)
	)

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetAsyncStatusTypeByName" GDALGetAsyncStatusTypeByName) GDALAsyncStatusType


Get AsyncStatusType by symbolic name.

 Returns a data type corresponding to the given symbolic name. This
 function is opposite to the GDALGetAsyncStatusTypeName().

@argument[pszName]{string containing the symbolic name of the type.}

@return{GDAL AsyncStatus type.
(	(pszName	  :string)

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetAsyncStatusTypeName" GDALGetAsyncStatusTypeName) :string


Get name of AsyncStatus data type.

 Returns a symbolic name for the AsyncStatus data type. This is
 essentially the the enumerated item name with the GARIO_ prefix
 removed. So GARIO_COMPLETE returns \"COMPLETE\". The returned strings
 are static strings and should not be modified or freed by the
 application. These strings are useful for reporting datatypes in
 debug statements, errors and other user output.

@argument[eAsyncStatusType]{type to get name of.}

@return{string corresponding to type.
(	(eAsyncStatusType	  GDALAsyncStatusType)

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetCacheMax" GDALGetCacheMax) int


Get maximum cache memory.

 Gets the maximum amount of memory available to the GDALRasterBlock
 caching system for caching GDAL read/write imagery.

 The first type this function is called, it will read the
 GDAL_CACHEMAX configuation option to initialize the maximum cache
 memory.

 This function cannot return a value higher than 2 GB. Use
 GDALGetCacheMax64() to get a non-truncated value.

 @return{maximum in bytes.}
(	void 	 )

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetCacheMax64" GDALGetCacheMax64) GIntBig


Get maximum cache memory.

 Gets the maximum amount of memory available to the GDALRasterBlock
 caching system for caching GDAL read/write imagery.

 The first type this function is called, it will read the
 GDAL_CACHEMAX configuation option to initialize the maximum cache
 memory.

@return{maximum in bytes.}

Since:
GDAL 1.8.0
(	void 	 )

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetCacheUsed" GDALGetCacheUsed) int


Get cache memory used.

 @return{the number of bytes of memory currently in use by the
 GDALRasterBlock memory caching.}

(	void 	 )

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetCacheUsed64" GDALGetCacheUsed64) GIntBig


Get cache memory used.

 @return{the number of bytes of memory currently in use by the
 GDALRasterBlock memory caching.}

Since:
GDAL 1.8.0
(	void 	 )

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetColorEntry" GDALGetColorEntry) const GDALColorEntry*


Fetch a color entry from table.

This function is the same as the C++ method GDALColorTable::GetColorEntry()

(	(hTable GDALColorTableH)
		(i int)
	)

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetColorEntryAsRGB" GDALGetColorEntryAsRGB) int


Fetch a table entry in RGB format.

This function is the same as the C++ method GDALColorTable::GetColorEntryAsRGB()
(	(hTable GDALColorTableH)
		(i int)
		GDALColorEntry * poEntry
	)

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetColorEntryCount" GDALGetColorEntryCount) int


Get number of color entries in table.

This function is the same as the C++ method GDALColorTable::GetColorEntryCount()
(	(hTable	  GDALColorTableH)

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetColorInterpretationByName" GDALGetColorInterpretationByName) GDALColorInterp


Get color interpreation by symbolic name.

 Returns a color interpreation corresponding to the given symbolic
 name. This function is opposite to the
 GDALGetColorInterpretationName().

@argument[pszName]{string containing the symbolic name of the color interpretation.}

 @return{GDAL color interpretation.}

Since:
GDAL 1.7.0
(	(pszName	  :string)

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetColorInterpretationName" GDALGetColorInterpretationName) :string


Get name of color interpretation.

 Returns a symbolic name for the color interpretation. This is derived
 from the enumerated item name with the GCI_ prefix removed, but there
 are some variations. So GCI_GrayIndex returns \"Gray\" and GCI_RedBand
 returns \"Red\". The returned strings are static strings and should not
 be modified or freed by the application.

@argument[eInterp]{color interpretation to get name of.}

 @return{string corresponding to color interpretation or NULL pointer
 if invalid enumerator given.}

(	(eInterp	  GDALColorInterp)

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetDataTypeByName" GDALGetDataTypeByName) GDALDataType


Get data type by symbolic name.

 Returns a data type corresponding to the given symbolic name. This
 function is opposite to the GDALGetDataTypeName().

 @argument[pszName]{string containing the symbolic name of the type.}

@return{GDAL data type.}

(	(pszName :string)

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetDataTypeName" GDALGetDataTypeName) :string


Get name of data type.

 Returns a symbolic name for the data type. This is essentially the
 the enumerated item name with the GDT_ prefix removed. So GDT_Byte
 returns \"Byte\". The returned strings are static strings and should
 not be modified or freed by the application. These strings are useful
 for reporting datatypes in debug statements, errors and other user
 output.

 @argument[eDataType]{type to get name of.}

 @return{string corresponding to existing data type or NULL pointer if
 invalid type given.}

(	(eDataType	  GDALDataType)

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetDataTypeSize" GDALGetDataTypeSize) int


Get data type size in bits.

Returns the size of a a GDT_* type in bits, not bytes!

 @argument[eDataType]{type, such as GDT_Byte.}

 @return{the number of bits or zero if it is not recognised.}

(	(eDataType	  GDALDataType)

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetDescription" GDALGetDescription) :string


Fetch object description.
See also:
GDALMajorObject::GetDescription()
(	(hObject GDALMajorObjectH)

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetDriver" GDALGetDriver) GDALDriverH


"Fetch driver by index.
See also:
GDALDriverManager::GetDriver()"
	(iDrive int))

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetDriverByName" GDALGetDriverByName) GDALDriverH


Fetch a driver based on the short name.
See also:
GDALDriverManager::GetDriverByName()
	(pszNam :string))

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetDriverCount" GDALGetDriverCount) int


Fetch the number of registered drivers.
See also:
GDALDriverManager::GetDriverCount()
(	void 	 )

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetDriverCreationOptionList" GDALGetDriverCreationOptionList) :string


Return the list of creation options of the driver.

 Return the list of creation options of the driver used by Create()
 and CreateCopy() as an XML string

 @argument[hDriver]{the handle of the driver}

 @return{an XML string that describes the list of creation options or
 empty string. The returned string should not be freed and is owned by
 the driver.}

	(hDrive GDALDriverH))

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetDriverHelpTopic" GDALGetDriverHelpTopic) :string


"Return the URL to the help that describes the driver.

That URL is relative to the GDAL documentation directory.

 For the GeoTIFF driver, this is \"frmt_gtiff.html\"

 @argument[hDriver]{the handle of the driver}

 @return{the URL to the help that describes the driver or NULL. The
 returned string should not be freed and is owned by the driver.}"

	(hDrive GDALDriverH))

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetDriverLongName" GDALGetDriverLongName) :string


Return the long name of a driver.

 For the GeoTIFF driver, this is \"GeoTIFF\"

 @argument[hDriver]{the handle of the driver}

 @return{the long name of the driver or empty string. The returned
 string should not be freed and is owned by the driver.}

	(hDriver GDALDriverH))

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetDriverShortName" GDALGetDriverShortName) :string


Return the short name of a driver.

This is the string that can be passed to the GDALGetDriverByName() function.

 For the GeoTIFF driver, this is \"GTiff\"

 @argument[hDriver]{the handle of the driver}

 @return{the short name of the driver. The returned string should not
 be freed and is owned by the driver.}

	(hDrive GDALDriverH))

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetMetadata" GDALGetMetadata) (:pointer :string)
  "Fetch metadata."
  (hObject GDALMajorObjectH)
  (pszDomai :string))

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetMetadataItem" GDALGetMetadataItem) :string
  "Fetch single metadata item."
  (hObject GDALMajorObjectH)
  (pszName :string)
  (pszDomai :string))

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetPaletteInterpretation" GDALGetPaletteInterpretation) GDALPaletteInterp
"Fetch palette interpretation.

 This function is the same as the C++ method
 GDALColorTable::GetPaletteInterpretation()"
	(hTabl GDALColorTableH))

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetPaletteInterpretationName" GDALGetPaletteInterpretationName) :string


Get name of palette interpretation.

 Returns a symbolic name for the palette interpretation. This is the
 the enumerated item name with the GPI_ prefix removed. So GPI_Gray
 returns \"Gray\". The returned strings are static strings and should
 not be modified or freed by the application.

 @argument[eInterp]{palette interpretation to get name of.}

 @return{string corresponding to palette interpretation.}
	(eInter GDALPaletteInterp))

;; --------------------------------------------------------

(cffi:defcfun ("GDALIdentifyDriver" GDALIdentifyDriver) GDALDriverH


Identify the driver that can open a raster file.

 This function will try to identify the driver that can open the
 passed file name by invoking the Identify method of each registered
 GDALDriver in turn. The first driver that successful identifies the
 file name will be returned. If all drivers fail then NULL is
 returned.

 In order to reduce the need for such searches touch the operating
 system file system machinery, it is possible to give an optional list
 of files. This is the list of all files at the same level in the file
 system as the target file, including the target file. The filenames
 will not include any path components, are an essentially just the
 output of CPLReadDir() on the parent directory. If the target object
 does not have filesystem semantics then the file list should be NULL.

 @argument[pszFilename]{the name of the file to access. In the case of
 exotic drivers this may not refer to a physical file, but instead
 contain information for the driver on how to access a dataset.}
 @argument[papszFileList]{an array of strings, whose last element is
 the NULL pointer. These strings are filenames that are auxiliary to
 the main filename. The passed value may be NULL.}

 @return{A GDALDriverH handle or NULL on failure. For C++ applications
 this handle can be cast to a GDALDriver *.}

	(pszFilename :string)
		char ** papszFileList
	)

;; --------------------------------------------------------

(cffi:defcfun ("GDALInvGeoTransform" GDALInvGeoTransform) int


Invert Geotransform.

 This function will invert a standard 3x2 set of GeoTransform
 coefficients. This converts the equation from being pixel to geo to
 being geo to pixel.

 @argument[gt_in][Input geotransform (six doubles - unaltered).}
 @argument[gt_out]{Output geotransform (six doubles - updated).}

 @return{TRUE on success or FALSE if the equation is uninvertable.}

(	(:pointer :double) gt_in,
		(:pointer :double) gt_out
	)

;; --------------------------------------------------------

(cffi:defcfun ("GDALLoadWorldFile" GDALLoadWorldFile) int


Read ESRI world file.

 This function reads an ESRI style world file, and formats a
 geotransform from its contents.

 The world file contains an affine transformation with the parameters
 in a different order than in a geotransform array.

 geotransform[1] : width of pixel
 geotransform[4] : rotational coefficient, zero for north up images.
 geotransform[2] : rotational coefficient, zero for north up images.
 geotransform[5] : height of pixel (but negative)
 geotransform[0] + 0.5 * geotransform[1] + 0.5 * geotransform[2] : x offset to center of top left pixel.
 geotransform[3] + 0.5 * geotransform[4] + 0.5 * geotransform[5] : y offset to center of top left pixel.

 @argument[pszFilename]{the world file name.}
 @argument[padfGeoTransform]{the six double array into which the
 geotransformation should be placed.}

@return{TRUE on success or FALSE on failure.}

	(pszFilename :string)
		(:pointer :double) padfGeoTransform
	)

;; --------------------------------------------------------

(cffi:defcfun ("GDALPackedDMSToDec" GDALPackedDMSToDec) double


Convert a packed DMS value (DDDMMMSSS.SS) into decimal degrees.

See CPLPackedDMSToDec().
	(dfPacke double))

;; --------------------------------------------------------

(cffi:defcfun ("GDALRasterBandCopyWholeRaster" GDALRasterBandCopyWholeRaster) CPLErr


Copy all raster band raster data.

 This function copies the complete raster contents of one band to
 another similarly configured band. The source and destination bands
 must have the same width and height. The bands do not have to have
 the same data type.

 It implements efficient copying, in particular \"chunking\" the copy
 in substantial blocks.

 Currently the only papszOptions value supported is :
 \"COMPRESSED=YES\" to force alignment on target dataset block sizes
 to achieve best compression. More options may be supported in the
 future.

 @argument[hSrcBand]{the source band}
 @argument[hDstBand]{the destination band}
 @argument[papszOptions]{transfer hints in \"StringList\" Name=Value format.}
 @argument[pfnProgress]{progress reporting function.}
 @argument[pProgressData]{callback data for progress function.}

 @return{CE_None on success, or CE_Failure on failure.}
(	(hSrcBand GDALRasterBandH)
		(hDstBand GDALRasterBandH)
		char ** papszOptions,
		(pfnProgress GDALProgressFunc)
		void * pProgressData
	)

;; --------------------------------------------------------

(cffi:defcfun ("GDALRATChangesAreWrittenToFile" GDALRATChangesAreWrittenToFile) int


 Determine whether changes made to this RAT are reflected directly in
 the dataset.

 This function is the same as the C++ method
 GDALRasterAttributeTable::ChangesAreWrittenToFile()

	(hRAT GDALRasterAttributeTableH))

;; --------------------------------------------------------

(cffi:defcfun ("GDALRATClone" GDALRATClone) GDALRasterAttributeTableH


Copy Raster Attribute Table.

This function is the same as the C++ method GDALRasterAttributeTable::Clone()
	(hRAT GDALRasterAttributeTableH))

;; --------------------------------------------------------

(cffi:defcfun ("GDALRATCreateColumn" GDALRATCreateColumn) CPLErr


Create new column.

This function is the same as the C++ method GDALRasterAttributeTable::CreateColumn()
	(hRAT GDALRasterAttributeTableH)
		(pszFieldName :string)
		(eFieldType GDALRATFieldType)
		(eFieldUsage GDALRATFieldUsage))

;; --------------------------------------------------------

(cffi:defcfun ("GDALRATDumpReadable" GDALRATDumpReadable) :void


Dump RAT in readable form.

This function is the same as the C++ method GDALRasterAttributeTable::DumpReadable()
(	(hRAT GDALRasterAttributeTableH)
		FILE * fp
	)

;; --------------------------------------------------------

(cffi:defcfun ("GDALRATGetColOfUsage" GDALRATGetColOfUsage) int
  "Fetch column index for given usage.

 This function is the same as the C++ method
 GDALRasterAttributeTable::GetColOfUsage()"
  (hRAT GDALRasterAttributeTableH)
  (eUsage GDALRATFieldUsage))

;; --------------------------------------------------------

(cffi:defcfun ("GDALRATGetColumnCount" GDALRATGetColumnCount) int


Fetch table column count.

 This function is the same as the C++ method
 GDALRasterAttributeTable::GetColumnCount()

	(hRAT	  GDALRasterAttributeTableH))

;; --------------------------------------------------------

(cffi:defcfun ("GDALRATGetLinearBinning" GDALRATGetLinearBinning) int


Get linear binning information.

This function is the same as the C++ method GDALRasterAttributeTable::GetLinearBinning()

(hRAT GDALRasterAttributeTableH)
		(:pointer :double) pdfRow0Min,
		(:pointer :double) pdfBinSize
	)

;; --------------------------------------------------------

(cffi:defcfun ("GDALRATGetNameOfCol" GDALRATGetNameOfCol) :string
  "Fetch name of indicated column.

This function is the same as the C++ method GDALRasterAttributeTable::GetNameOfCol()"
  (hRAT GDALRasterAttributeTableH)
  (iCol int))

;; --------------------------------------------------------

(cffi:defcfun ("GDALRATGetRowCount" GDALRATGetRowCount) int
Fetch row count.

This function is the same as the C++ method GDALRasterAttributeTable::GetRowCount()
	(hRAT	  GDALRasterAttributeTableH))

;; --------------------------------------------------------

(cffi:defcfun ("GDALRATGetRowOfValue" GDALRATGetRowOfValue) int
Get row for pixel value.

This function is the same as the C++ method GDALRasterAttributeTable::GetRowOfValue()
	(hRAT GDALRasterAttributeTableH)
		(dfValue double))

;; --------------------------------------------------------

(cffi:defcfun ("GDALRATGetTypeOfCol" GDALRATGetTypeOfCol) GDALRATFieldType
Fetch column type.

This function is the same as the C++ method GDALRasterAttributeTable::GetTypeOfCol()

	(hRAT GDALRasterAttributeTableH)
		(iCol int))

;; --------------------------------------------------------

(cffi:defcfun ("GDALRATGetUsageOfCol" GDALRATGetUsageOfCol) GDALRATFieldUsage

Fetch column usage value.

This function is the same as the C++ method GDALRasterAttributeTable::GetUsageOfColetNameOfCol()

	(hRAT GDALRasterAttributeTableH)
		(iCol int))

;; --------------------------------------------------------

(cffi:defcfun ("GDALRATGetValueAsDouble" GDALRATGetValueAsDouble) double
  "Fetch field value as a double.

This function is the same as the C++ method GDALRasterAttributeTable::GetValueAsDouble()"
  (hRAT GDALRasterAttributeTableH)
  (iRow int)
  (iField int))

;; --------------------------------------------------------

(cffi:defcfun ("GDALRATGetValueAsInt" GDALRATGetValueAsInt) int
  "Fetch field value as a integer.

This function is the same as the C++ method GDALRasterAttributeTable::GetValueAsInt()"
  (hRAT GDALRasterAttributeTableH)
  (iRow int)
  (iField int))

;; --------------------------------------------------------

(cffi:defcfun ("GDALRATGetValueAsString" GDALRATGetValueAsString) :string
  "Fetch field value as a string.

This function is the same as the C++ method GDALRasterAttributeTable::GetValueAsString()"
  (hRAT GDALRasterAttributeTableH)
  (iRow int)
  (iField int))

;; --------------------------------------------------------

(cffi:defcfun ("GDALRATInitializeFromColorTable" GDALRATInitializeFromColorTable) CPLErr
  "Initialize from color table.

This function is the same as the C++ method GDALRasterAttributeTable::InitializeFromColorTable()"
  (hRAT GDALRasterAttributeTableH)
  (hCT GDALColorTableH))

;; --------------------------------------------------------

(cffi:defcfun ("GDALRATSetLinearBinning" GDALRATSetLinearBinning) CPLErr
  "Set linear binning information.

This function is the same as the C++ method GDALRasterAttributeTable::SetLinearBinning()"
  (hRAT GDALRasterAttributeTableH)
  (dfRow0Min double)
  (dfBinSize double))

;; --------------------------------------------------------

(cffi:defcfun ("GDALRATSetRowCount" GDALRATSetRowCount) :void
  "Set row count.

This function is the same as the C++ method GDALRasterAttributeTable::SetRowCount()"
  (hRAT GDALRasterAttributeTableH)
  (nNewCount int))

;; --------------------------------------------------------

(cffi:defcfun ("GDALRATSetValueAsDouble" GDALRATSetValueAsDouble) :void
  "Set field value from double.

This function is the same as the C++ method GDALRasterAttributeTable::SetValue()"
  (hRAT GDALRasterAttributeTableH)
  (iRow int)
  (iField int)
  (dfValue double))

;; --------------------------------------------------------

(cffi:defcfun ("GDALRATSetValueAsInt" GDALRATSetValueAsInt) :void
  "Set field value from integer.

This function is the same as the C++ method GDALRasterAttributeTable::SetValue()"
  (hRAT GDALRasterAttributeTableH)
  (iRow int)
  (iField int)
  (nValue int))

;; --------------------------------------------------------

(cffi:defcfun ("GDALRATSetValueAsString" GDALRATSetValueAsString) :void
  "Set field value from string.

This function is the same as the C++ method GDALRasterAttributeTable::SetValue()"
  (hRAT GDALRasterAttributeTableH)
  (iRow int)
  (iField int)
  (pszValue :string))

;; --------------------------------------------------------

(cffi:defcfun ("GDALRATTranslateToColorTable" GDALRATTranslateToColorTable) GDALColorTableH
  "Translate to a color table.

This function is the same as the C++ method GDALRasterAttributeTable::TranslateToColorTable()"
  (hRAT GDALRasterAttributeTableH)
  (nEntryCount int))

;; --------------------------------------------------------

(cffi:defcfun ("GDALRATValuesIOAsDouble" GDALRATValuesIOAsDouble) CPLErr
  "Read or Write a block of doubles to/from the Attribute Table.

This function is the same as the C++ method GDALRasterAttributeTable::ValuesIO()"
  (hRAT GDALRasterAttributeTableH)
  (eRWFlag GDALRWFlag)
  (iField int)
  (iStartRow int)
  (iLength int)
  (pdfData (:pointer :double)))

;; --------------------------------------------------------

(cffi:defcfun ("GDALRATValuesIOAsInteger" GDALRATValuesIOAsInteger) CPLErr
  "Read or Write a block of ints to/from the Attribute Table.

This function is the same as the C++ method GDALRasterAttributeTable::ValuesIO()"
  (hRAT GDALRasterAttributeTableH)
  (eRWFlag GDALRWFlag)
  (iField int)
  (iStartRow int)
  (iLength int)
  (pnData (:pointer :int)))

;; --------------------------------------------------------

(cffi:defcfun ("GDALRATValuesIOAsString" GDALRATValuesIOAsString) CPLErr
Read or Write a block of strings to/from the Attribute Table.

This function is the same as the C++ method GDALRasterAttributeTable::ValuesIO()
	(hRAT GDALRasterAttributeTableH)
		(eRWFlag GDALRWFlag)
		(iField int)
		(iStartRow int)
		(iLength int)
		char ** papszStrList
	)

;; --------------------------------------------------------

(cffi:defcfun ("GDALReadWorldFile" GDALReadWorldFile) int
  "Read ESRI world file.

 This function reads an ESRI style world file, and formats a
 geotransform from its contents. It does the same as
 GDALLoadWorldFile() function, but it will form the filename for the
 worldfile from the filename of the raster file referred and the
 suggested extension. If no extension is provided, the code will
 internally try the unix style and windows style world file extensions
 (eg. for .tif these would be .tfw and .tifw).

 The world file contains an affine transformation with the parameters
 in a different order than in a geotransform array.

 geotransform[1] : width of pixel
 geotransform[4] : rotational coefficient, zero for north up images.
 geotransform[2] : rotational coefficient, zero for north up images.
 geotransform[5] : height of pixel (but negative)
 geotransform[0] + 0.5 * geotransform[1] + 0.5 * geotransform[2] : x offset to center of top left pixel.
 geotransform[3] + 0.5 * geotransform[4] + 0.5 * geotransform[5] : y offset to center of top left pixel.

 @argument[pszBaseFilename]{the target raster file.}
 @argument[pszExtension]{the extension to use (ie. \".wld\") or NULL
 to derive it from the pszBaseFilename}
 @argument[padfGeoTransform]{the six double array into which the
 geotransformation should be placed.}

@return{TRUE on success or FALSE on failure.}"
  (pszBaseFilename :string)
  (pszExtension :string)
  (padfGeoTransform (:pointer :double)))

;; --------------------------------------------------------

(cffi:defcfun ("GDALRegenerateOverviews" GDALRegenerateOverviews) CPLErr
Generate downsampled overviews.

 This function will generate one or more overview images from a base
 image using the requested downsampling algorithm. It's primary use is
 for generating overviews via GDALDataset::BuildOverviews(), but it
 can also be used to generate downsampled images in one file from
 another outside the overview architecture.

 The output bands need to exist in advance.

 The full set of resampling algorithms is documented in
 GDALDataset::BuildOverviews().

 This function will honour properly NODATA_VALUES tuples (special
 dataset metadata) so that only a given RGB triplet (in case of a RGB
 image) will be considered as the nodata value and not each value of
 the triplet independantly per band.

 @argument[hSrcBand]{the source (base level) band.}
 @argument[nOverviewCount]{the number of downsampled bands being generated.}
 @argument[pahOvrBands]{the list of downsampled bands to be generated.}
 @argument[pszResampling]{Resampling algorithm (eg. \"AVERAGE\").}
 @argument[pfnProgress]{progress report function.}
 @argument[pProgressData]{progress function callback data.}

@return{CE_None on success or CE_Failure on failure.}

	(hSrcBand GDALRasterBandH)
		(nOverviewCount int)
		GDALRasterBandH * pahOvrBands,
		(pszResampling :string)
		(pfnProgress GDALProgressFunc)
		void * pProgressData
	)

;; --------------------------------------------------------

(cffi:defcfun ("GDALRegisterDriver" GDALRegisterDriver) int


Register a driver for use.
See also:
GDALDriverManager::GetRegisterDriver()
	(hDrive GDALDriverH))

;; --------------------------------------------------------

(cffi:defcfun ("GDALRenameDataset" GDALRenameDataset) CPLErr
Rename a dataset.
See also:
GDALDriver::Rename()
(	GDALDriverH hDriver,
		:string pszNewName,
		:string pszOldName
	)

;; --------------------------------------------------------

void GDALSetCacheMax


Set maximum cache memory.

 This function sets the maximum amount of memory that GDAL is
 permitted to use for GDALRasterBlock caching. The unit of the value
 is bytes.

 The maximum value is 2GB, due to the use of a signed 32 bit
 integer. Use GDALSetCacheMax64() to be able to set a higher value.

@argument[nNewSizeInBytes]{the maximum number of bytes for caching.}
(	int nNewSizeInBytes	 )

;; --------------------------------------------------------

(cffi:defcfun ("GDALSetCacheMax64" GDALSetCacheMax64) :void
  "Set maximum cache memory.

 This function sets the maximum amount of memory that GDAL is
 permitted to use for GDALRasterBlock caching. The unit of the value
 is bytes.

 Note: On 32 bit platforms, the maximum amount of memory that can be
 addressed by a process might be 2 GB or 3 GB, depending on the
 operating system capabilities. This function will not make any
 attempt to check the consistency of the passed value with the
 effective capabilities of the OS.

@argument[nNewSizeInBytes]{the maximum number of bytes for caching.}

Since:
GDAL 1.8.0"
  (nNewSizeInBytes GIntBig))

;; --------------------------------------------------------

(cffi:defcfun ("GDALSetColorEntry" GDALSetColorEntry) :void
Set entry in color table.

This function is the same as the C++ method GDALColorTable::SetColorEntry()
	(hTable GDALColorTableH)
		(i int)
		const GDALColorEntry * poEntry
	)

;; --------------------------------------------------------

(cffi:defcfun ("GDALSetDescription" GDALSetDescription) :void
  "Set object description.
See also:
GDALMajorObject::SetDescription()"
  (hObject GDALMajorObjectH)
  (pszNewDesc :string))

;; --------------------------------------------------------

(cffi:defcfun ("GDALSetMetadata" gdal-set-metadata) CPLErr
  "Set metadata.
See also:
GDALMajorObject::SetMetadata()"
  (hObject GDALMajorObjectH)
  (papszMD (:pointer :string))
  (pszDomain :string))

;; --------------------------------------------------------

(cffi:defcfun ("GDALSetMetadataItem" GDALSetMetadataItem) CPLErr
  "Set single metadata item.
See also:
GDALMajorObject::SetMetadataItem()"
  (hObject GDALMajorObjectH)
  (pszName :string)
  (pszValue :string)
  (pszDomain :string))

;; --------------------------------------------------------

(cffi:defcfun ("GDALSwapWords" GDALSwapWords) :void
  "Byte swap words in-place.

 This function will byte swap a set of 2, 4 or 8 byte words \"in place\"
 in a memory array. No assumption is made that the words being swapped
 are word aligned in memory. Use the CPL_LSB and CPL_MSB macros from
 cpl_port.h to determine if the current platform is big endian or
 little endian. Use The macros like CPL_SWAP32() to byte swap single
 values without the overhead of a function call.

 @argument[pData]{pointer to start of data buffer.}
 @argument[nWordSize]{size of words being swapped in bytes. Normally 2, 4 or 8.}
 @argument[nWordCount]{the number of words to be swapped in this call.}
 @argument[nWordSkip]{the byte offset from the start of one word to
 the start of the next. For packed buffers this is the same as
 nWordSize.}"

  (pData (:pointer :void))
  (nWordSize int)
  (nWordCount int)
  (nWordSkip int))

;; --------------------------------------------------------

(cffi:defcfun ("GDALValidateCreationOptions" GDALValidateCreationOptions) int
  "Validate the list of creation options that are handled by a driver.

 This is a helper method primarily used by Create() and CreateCopy()
 to validate that the passed in list of creation options is compatible
 with the GDAL_DMD_CREATIONOPTIONLIST metadata item defined by some
 drivers.

See also:
GDALGetDriverCreationOptionList()

 If the GDAL_DMD_CREATIONOPTIONLIST metadata item is not defined, this
 function will return TRUE. Otherwise it will check that the keys and
 values in the list of creation options are compatible with the
 capabilities declared by the GDAL_DMD_CREATIONOPTIONLIST metadata
 item. In case of incompatibility a (non fatal) warning will be emited
 and FALSE will be returned.

 @argument[hDriver]{the handle of the driver with whom the lists of
 creation option must be validated}
 @argument[papszCreationOptions]{the list of creation options. An
 array of strings, whose last element is a NULL pointer}

 @return{TRUE if the list of creation options is compatible with the
 Create() and CreateCopy() method of the driver, FALSE otherwise.}"

  (hDriver GDALDriverH)
  (papszCreationOptions (:pointer :string)))

;; --------------------------------------------------------

(cffi:defcfun ("GDALVersionInfo" GDALVersionInfo) :string
  "Get runtime version information.

Available pszRequest values:
 \"VERSION_NUM\": Returns GDAL_VERSION_NUM formatted as a string. ie. \"1170\" Note: starting with GDAL 1.10, this string will be longer than 4 characters.
 \"RELEASE_DATE\": Returns GDAL_RELEASE_DATE formatted as a string. ie. \"20020416\".
 \"RELEASE_NAME\": Returns the GDAL_RELEASE_NAME. ie. \"1.1.7\"
 \"--version\": Returns one line version message suitable for use in response to --version requests. ie. \"GDAL 1.1.7, released 2002/04/16\"
 \"LICENSE\": Returns the content of the LICENSE.TXT file from the GDAL_DATA directory. Before GDAL 1.7.0, the returned string was leaking memory but this is now resolved. So the result should not been freed by the caller.
 \"BUILD_INFO\": List of NAME=VALUE pairs separated by newlines with information on build time options.

@argument[pszRequest]{the type of version info desired, as listed above.}

 @return{an internal string containing the requested information.}"
  (pszRequest :string))

;; --------------------------------------------------------

(cffi:defcfun ("GDALWriteWorldFile" GDALWriteWorldFile) int
  "Write ESRI world file.

This function writes an ESRI style world file from the passed geotransform.

 The world file contains an affine transformation with the parameters
 in a different order than in a geotransform array.

 geotransform[1] : width of pixel
 geotransform[4] : rotational coefficient, zero for north up images.
 geotransform[2] : rotational coefficient, zero for north up images.
 geotransform[5] : height of pixel (but negative)
 geotransform[0] + 0.5 * geotransform[1] + 0.5 * geotransform[2] : x offset to center of top left pixel.
 geotransform[3] + 0.5 * geotransform[4] + 0.5 * geotransform[5] : y offset to center of top left pixel.

 @argument[pszBaseFilename]{the target raster file.}
 @argument[pszExtension]{the extension to use (ie. \".wld\"). Must not
 be NULL}
 @argument[padfGeoTransform]{the six double array from which the
 geotransformation should be read.}

@return{TRUE on success or FALSE on failure.}"
  (pszBaseFilename :string)
  (pszExtension :string)
  (padfGeoTransform (:pointer :double)))

;; EOF
