;;; -*- package: CL-GDAL; Syntax: Common-lisp; Base: 10 -*-

(in-package :cl-gdal)

;; --------------------------------------------------------

(cffi:defcfun ("GDALComputeRasterMinMax" GDAL-Compute-Raster-Min-Max) :void
  "Compute the min/max values for a band."
  (hBand gdal-raster-band-h)
  (bApproxOK :int)
  (adfMinMax (:pointer :double)) ;; array of 2
  )

;; --------------------------------------------------------

(cffi:defcfun ("GDALComputeRasterStatistics" gdal-compute-raster-statistics) cpl-err
  "Compute image statistics."
  (hBand gdal-raster-band-h)
  (bApproxOK :int)
  (pdfMin (:pointer :double))
  (pdfMax (:pointer :double))
  (pdfMean (:pointer :double))
  (pdfStdDev (:pointer :double))
  (pfnProgress :pointer) ;; GDALProgressFunc
  (pProgressData :pointer))

;; --------------------------------------------------------

(cffi:defcfun ("GDALCreateMaskBand" GDALCreateMaskBand) cpl-err
  "Adds a mask band to the current band."
  (hBand gdal-raster-band-h)
  (nFlags :int))

;; --------------------------------------------------------

(cffi:defcfun ("GDALFillRaster" GDAL-Fill-Raster) cpl-err
  "Fill this band with a constant value."
  (hBand gdal-raster-band-h)
  (dfRealValue :double)
  (dfImaginaryValue :double))

;; --------------------------------------------------------

(cffi:defcfun ("GDALFlushRasterCache" GDAL-Flush-Raster-Cache) cpl-err
  "Flush raster data cache."
  (hBand gdal-raster-band-h))

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetBandDataset" GDAL-Get-Band-Dataset) gdal-dataset-h
  "Fetch the owning dataset handle."
  (hBand gdal-raster-band-h))

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetBandNumber" gdal-get-band-number) :int
  "Fetch the band number."
  (hBand gdal-raster-band-h))

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetBlockSize" gdal-get-block-size) :void
  "Fetch the \"natural\" block size of this band."
  (hBand gdal-raster-band-h)
  (pnXSize (:pointer :int))
  (pnYSize (:pointer :int)))

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetDefaultHistogram" gdal-get-default-histogram) cpl-err
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

(cffi:defcfun ("GDALGetDefaultRAT" gdal-get-default-rat) gdal-raster-attribute-table-h
  "Fetch default Raster Attribute Table."
  (hBand gdal-raster-band-h))

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetMaskBand" gdal-get-mask-band) gdal-raster-band-h
  "Return the mask band associated with the band."
  (hBand gdal-raster-band-h))

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetMaskFlags" gdal-get-mask-flags) :int
  "Return the status flags of the mask band associated with the band."
  (hBand gdal-raster-band-h))

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetOverview" gdal-get-overview) gdal-raster-band-h
  "Fetch overview raster band object."
  (hBand gdal-raster-band-h)
  (i :int))

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetOverviewCount" gdal-get-overview-count) :int
  "Return the number of overview layers available."
  (hBand gdal-raster-band-h))

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetRasterAccess" gdal-get-raster-access) gdal-access
  "Find out if we have update permission for this band."
  (hBand gdal-raster-band-h))

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetRasterBandXSize" gdal-get-raster-band-x-size) :int
  "Fetch XSize of raster."
  (hBand gdal-raster-band-h))

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetRasterBandYSize" gdal-get-raster-band-y-size) :int
  "Fetch YSize of raster."
  (hBand gdal-raster-band-h))

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

(cffi:defcfun ("GDALSetRasterStatistics" gdal-set-raster-statistics) cpl-err
  "Set statistics on band."
  (hBand gdal-raster-band-h)
  (dfMin :double)
  (dfMax :double)
  (dfMean :double)
  (dfStdDe :double))

;; --------------------------------------------------------

(cffi:defcfun ("GDALSetRasterUnitType" gdal-set-raster-unit-type) cpl-err
  "Set unit type.

Since: GDAL 1.8.0"
  (hBand gdal-raster-band-h)
  (pszNewValue :string))

;; --------------------------------------------------------

(cffi:defcfun ("GDALWriteBlock" gdal-write-block) cpl-err
  "Write a block of image data efficiently."
  (hBand gdal-raster-band-h)
  (nXOff :int)
  (nYOff :int)
  (pDat :pointer))

;; EOF
