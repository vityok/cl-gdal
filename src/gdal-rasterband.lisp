;;; -*- package: CL-GDAL; Syntax: Common-lisp; Base: 10 -*-

(in-package :cl-gdal)

;; --------------------------------------------------------

(cffi:defcfun ("GDALComputeRasterMinMax" GDAL-Compute-Raster-Min-Max) :void
"Compute the min/max values for a band."
(hBand gdal-raster-band-h)
		(bApproxOK 	:int)
		double 	adfMinMax[2]
	)

;; --------------------------------------------------------

(cffi:defcfun ("GDALComputeRasterStatistics" GDALComputeRasterStatistics) CPLErr
"Compute image statistics."
(hBand gdal-raster-band-h)
		(:int 	bApproxOK)
		(:pointer :double) 	pdfMin,
		(:pointer :double) 	pdfMax,
		(:pointer :double) 	pdfMean,
		(:pointer :double) 	pdfStdDev,
		((pfnProgress :pointer) GDALProgressFunc)
		(pProgressData 	:pointer))

;; --------------------------------------------------------

(cffi:defcfun ("GDALCreateMaskBand" GDALCreateMaskBand) CPLErr
"Adds a mask band to the current band."
(hBand gdal-raster-band-h)
		(nFlags :int))

;; --------------------------------------------------------

(cffi:defcfun ("GDALFillRaster" GDALFillRaster) CPLErr
"Fill this band with a constant value."
(hBand gdal-raster-band-h)
		double 	dfRealValue,
		double 	dfImaginaryValue
	)

;; --------------------------------------------------------

(cffi:defcfun ("GDALFlushRasterCache" GDALFlushRasterCache) CPLErr


"Flush raster data cache."
(hBand gdal-raster-band-h	 )

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetBandDataset" GDALGetBandDataset) GDALDatasetH
"Fetch the owning dataset handle."
(hBand gdal-raster-band-h	 )

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetBandNumber" GDALGetBandNumber) int
"Fetch the band number."
(hBand gdal-raster-band-h	 )

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetBlockSize" GDALGetBlockSize) void
"Fetch the \"natural\" block size of this band."
(hBand gdal-raster-band-h)
		int * 	pnXSize,
		int * 	pnYSize
	)

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetDefaultHistogram" GDALGetDefaultHistogram) CPLErr
"Fetch default raster histogram."
(hBand gdal-raster-band-h)
		(:pointer :double) 	pdfMin,
		(:pointer :double) 	pdfMax,
		int * 	pnBuckets,
		int ** 	ppanHistogram,
		int 	bForce,
		GDALProgressFunc 	pfnProgress,
		:pointer 	pProgressData
	)

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetDefaultRAT" GDALGetDefaultRAT) GDALRasterAttributeTableH
"Fetch default Raster Attribute Table."
(hBand gdal-raster-band-h	 )

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetMaskBand" GDALGetMaskBand) gdal-raster-band-h
"Return the mask band associated with the band."
(hBand gdal-raster-band-h	 )

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetMaskFlags" GDALGetMaskFlags) int
"Return the status flags of the mask band associated with the band."
(hBand gdal-raster-band-h	 )

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetOverview" GDALGetOverview) gdal-raster-band-h
"Fetch overview raster band object."
(hBand gdal-raster-band-h)
		int 	i
	)

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetOverviewCount" GDALGetOverviewCount) int
"Return the number of overview layers available."
(hBand gdal-raster-band-h	 )

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetRasterAccess" GDALGetRasterAccess) GDALAccess
"Find out if we have update permission for this band."
(hBand gdal-raster-band-h	 )

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetRasterBandXSize" GDALGetRasterBandXSize) int
"Fetch XSize of raster."
(hBand gdal-raster-band-h	 )

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetRasterBandYSize" GDALGetRasterBandYSize) int
"Fetch YSize of raster."
(hBand gdal-raster-band-h	 )

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetRasterCategoryNames" GDALGetRasterCategoryNames) (:pointer :string)
"Fetch the list of category names for this raster."
(hBand gdal-raster-band-h	 )

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetRasterColorInterpretation" GDALGetRasterColorInterpretation) GDALColorInterp
"How should this band be interpreted as color?"
(hBand gdal-raster-band-h	 )

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetRasterColorTable" GDALGetRasterColorTable) gdal-color-table-h
"Fetch the color table associated with band."
(hBand gdal-raster-band-h	 )

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetRasterDataType" GDALGetRasterDataType) GDALDataType
"Fetch the pixel data type for this band."
(hBand gdal-raster-band-h	 )

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetRasterHistogram" GDALGetRasterHistogram) CPLErr
"Compute raster histogram."
(hBand gdal-raster-band-h)
		double 	dfMin,
		double 	dfMax,
		int 	nBuckets,
		int * 	panHistogram,
		int 	bIncludeOutOfRange,
		int 	bApproxOK,
		GDALProgressFunc 	pfnProgress,
		:pointer 	pProgressData
	)

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetRasterMaximum" GDALGetRasterMaximum) double
"Fetch the maximum value for this band."
(hBand gdal-raster-band-h)
		int * 	pbSuccess
	)

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetRasterMinimum" GDALGetRasterMinimum) double
"Fetch the minimum value for this band."
(hBand gdal-raster-band-h)
		int * 	pbSuccess
	)

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetRasterNoDataValue" GDALGetRasterNoDataValue) double
"Fetch the no data value for this band."
(hBand gdal-raster-band-h)
		int * 	pbSuccess
	)

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetRasterOffset" GDALGetRasterOffset) double
"Fetch the raster value offset."
(hBand gdal-raster-band-h)
		int * 	pbSuccess
	)

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetRasterSampleOverview" GDALGetRasterSampleOverview) gdal-raster-band-h
"Fetch best sampling overview."
(hBand gdal-raster-band-h)
		int 	nDesiredSamples
	)

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetRasterScale" GDALGetRasterScale) double
"Fetch the raster value scale."
(hBand gdal-raster-band-h)
		int * 	pbSuccess
	)

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetRasterStatistics" GDALGetRasterStatistics) CPLErr
"Fetch image statistics."
(hBand gdal-raster-band-h)
		int 	bApproxOK,
		int 	bForce,
		(:pointer :double) 	pdfMin,
		(:pointer :double) 	pdfMax,
		(:pointer :double) 	pdfMean,
		(:pointer :double) 	pdfStdDev
	)

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetRasterUnitType" GDALGetRasterUnitType) :string
"Return raster unit type."
(hBand gdal-raster-band-h))

;; --------------------------------------------------------

(cffi:defcfun ("GDALHasArbitraryOverviews" GDALHasArbitraryOverviews) :int
  "Check for arbitrary overviews."
  (hBand gdal-raster-band-h))

;; --------------------------------------------------------

(cffi:defcfun ("GDALRasterAdviseRead" GDALRasterAdviseRead) CPLErr
"Advise driver of upcoming read requests."
(hBand gdal-raster-band-h)
		int 	nXOff,
		int 	nYOff,
		int 	nXSize,
		int 	nYSize,
		int 	nBufXSize,
		int 	nBufYSize,
		GDALDataType 	eDT,
		char ** 	papszOptions
	)

;; --------------------------------------------------------

(cffi:defcfun ("GDALRasterIO" GDALRasterIO) CPLErr
"Read/write a region of image data for this band."
(hBand gdal-raster-band-h)
		GDALRWFlag 	eRWFlag,
		int 	nXOff,
		int 	nYOff,
		int 	nXSize,
		int 	nYSize,
		:pointer 	pData,
		int 	nBufXSize,
		int 	nBufYSize,
		GDALDataType 	eBufType,
		int 	nPixelSpace,
		int 	nLineSpace
	)

;; --------------------------------------------------------

(cffi:defcfun ("GDALReadBlock" GDALReadBlock) CPLErr
"Read a block of image data efficiently."
(hBand gdal-raster-band-h)
		int 	nXOff,
		int 	nYOff,
		:pointer 	pData
	)

;; --------------------------------------------------------

(cffi:defcfun ("GDALSetDefaultHistogram" GDALSetDefaultHistogram) CPLErr
"Set default histogram."
(hBand gdal-raster-band-h)
		double 	dfMin,
		double 	dfMax,
		int 	nBuckets,
		int * 	panHistogram
	)

;; --------------------------------------------------------

(cffi:defcfun ("GDALSetDefaultRAT" GDALSetDefaultRAT) CPLErr
"Set default Raster Attribute Table."
(hBand gdal-raster-band-h)
		GDALRasterAttributeTableH 	hRAT
	)

;; --------------------------------------------------------

(cffi:defcfun ("GDALSetRasterCategoryNames" GDALSetRasterCategoryNames) CPLErr
"Set the category names for this band."
(hBand gdal-raster-band-h)
		char ** 	papszNames
	)

;; --------------------------------------------------------

(cffi:defcfun ("GDALSetRasterColorInterpretation" GDALSetRasterColorInterpretation) CPLErr
"Set color interpretation of a band."
(hBand gdal-raster-band-h)
		GDALColorInterp 	eColorInterp
	)

;; --------------------------------------------------------

(cffi:defcfun ("GDALSetRasterColorTable" GDALSetRasterColorTable) CPLErr
  "Set the raster color table."
  (hBand gdal-raster-band-h)
  (hCT gdal-color-table-h))

;; --------------------------------------------------------

(cffi:defcfun ("GDALSetRasterNoDataValue" GDALSetRasterNoDataValue) CPLErr
"Set the no data value for this band."
(hBand gdal-raster-band-h)
		double 	dfValue
	)

;; --------------------------------------------------------

(cffi:defcfun ("GDALSetRasterOffset" GDALSetRasterOffset) CPLErr
"Set scaling offset."
(hBand gdal-raster-band-h)
		double 	dfNewOffset
	)

;; --------------------------------------------------------

(cffi:defcfun ("GDALSetRasterScale" GDALSetRasterScale) CPLErr
"Set scaling ratio."
(hBand gdal-raster-band-h)
		double 	dfNewOffset
	)

;; --------------------------------------------------------

(cffi:defcfun ("GDALSetRasterStatistics" GDALSetRasterStatistics) CPLErr
"Set statistics on band."
(hBand gdal-raster-band-h)
		double 	dfMin,
		double 	dfMax,
		double 	dfMean,
		double 	dfStdDev
	)

;; --------------------------------------------------------

(cffi:defcfun ("GDALSetRasterUnitType" GDALSetRasterUnitType) CPLErr
"Set unit type.

Since: GDAL 1.8.0"
(hBand gdal-raster-band-h)
		:string 	pszNewValue
	)

;; --------------------------------------------------------

(cffi:defcfun ("GDALWriteBlock" GDALWriteBlock) CPLErr
"Write a block of image data efficiently."
(hBand gdal-raster-band-h)
		int 	nXOff,
		int 	nYOff,
		:pointer 	pData
	)

;; EOF
