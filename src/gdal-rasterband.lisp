;;; -*- package: CL-GDAL; Syntax: Common-lisp; Base: 10 -*-

(in-package :cl-gdal)

;; --------------------------------------------------------
void GDALComputeRasterMinMax 	(	GDALRasterBandH 	hBand,
		int 	bApproxOK,
		double 	adfMinMax[2]
	)


Compute the min/max values for a band.
See also:
GDALRasterBand::ComputeRasterMinMax()
;; --------------------------------------------------------

CPLErr GDALComputeRasterStatistics 	(	GDALRasterBandH 	hBand,
		int 	bApproxOK,
		double * 	pdfMin,
		double * 	pdfMax,
		double * 	pdfMean,
		double * 	pdfStdDev,
		GDALProgressFunc 	pfnProgress,
		void * 	pProgressData
	)


Compute image statistics.
See also:
GDALRasterBand::ComputeStatistics()
;; --------------------------------------------------------
CPLErr GDALCreateMaskBand 	(	GDALRasterBandH 	hBand,
		int 	nFlags
	)


Adds a mask band to the current band.
See also:
GDALRasterBand::CreateMaskBand()

;; --------------------------------------------------------

CPLErr GDALFillRaster 	(	GDALRasterBandH 	hBand,
		double 	dfRealValue,
		double 	dfImaginaryValue
	)


Fill this band with a constant value.
See also:
GDALRasterBand::Fill()

;; --------------------------------------------------------

CPLErr GDALFlushRasterCache 	(	GDALRasterBandH 	hBand	 )


Flush raster data cache.
See also:
GDALRasterBand::FlushCache()

;; --------------------------------------------------------

GDALDatasetH GDALGetBandDataset 	(	GDALRasterBandH 	hBand	 )


Fetch the owning dataset handle.
See also:
GDALRasterBand::GetDataset()
;; --------------------------------------------------------
int GDALGetBandNumber 	(	GDALRasterBandH 	hBand	 )


Fetch the band number.
See also:
GDALRasterBand::GetBand()
;; --------------------------------------------------------
void GDALGetBlockSize 	(	GDALRasterBandH 	hBand,
		int * 	pnXSize,
		int * 	pnYSize
	)


Fetch the "natural" block size of this band.
See also:
GDALRasterBand::GetBlockSize()

;; --------------------------------------------------------

CPLErr GDALGetDefaultHistogram 	(	GDALRasterBandH 	hBand,
		double * 	pdfMin,
		double * 	pdfMax,
		int * 	pnBuckets,
		int ** 	ppanHistogram,
		int 	bForce,
		GDALProgressFunc 	pfnProgress,
		void * 	pProgressData
	)


Fetch default raster histogram.
See also:
GDALRasterBand::GetDefaultHistogram()

;; --------------------------------------------------------

GDALRasterAttributeTableH GDALGetDefaultRAT 	(	GDALRasterBandH 	hBand	 )


Fetch default Raster Attribute Table.
See also:
GDALRasterBand::GetDefaultRAT()

;; --------------------------------------------------------

GDALRasterBandH GDALGetMaskBand 	(	GDALRasterBandH 	hBand	 )


Return the mask band associated with the band.
See also:
GDALRasterBand::GetMaskBand()
;; --------------------------------------------------------
int GDALGetMaskFlags 	(	GDALRasterBandH 	hBand	 )


Return the status flags of the mask band associated with the band.
See also:
GDALRasterBand::GetMaskFlags()
;; --------------------------------------------------------

GDALRasterBandH GDALGetOverview 	(	GDALRasterBandH 	hBand,
		int 	i
	)


Fetch overview raster band object.
See also:
GDALRasterBand::GetOverview()
;; --------------------------------------------------------
int GDALGetOverviewCount 	(	GDALRasterBandH 	hBand	 )


Return the number of overview layers available.
See also:
GDALRasterBand::GetOverviewCount()
;; --------------------------------------------------------
GDALAccess GDALGetRasterAccess 	(	GDALRasterBandH 	hBand	 )


Find out if we have update permission for this band.
See also:
GDALRasterBand::GetAccess()
;; --------------------------------------------------------
int GDALGetRasterBandXSize 	(	GDALRasterBandH 	hBand	 )


Fetch XSize of raster.
See also:
GDALRasterBand::GetXSize()
;; --------------------------------------------------------
int GDALGetRasterBandYSize 	(	GDALRasterBandH 	hBand	 )


Fetch YSize of raster.
See also:
GDALRasterBand::GetYSize()
;; --------------------------------------------------------
char** GDALGetRasterCategoryNames 	(	GDALRasterBandH 	hBand	 )


Fetch the list of category names for this raster.
See also:
GDALRasterBand::GetCategoryNames()
;; --------------------------------------------------------
GDALColorInterp GDALGetRasterColorInterpretation 	(	GDALRasterBandH 	hBand	 )


How should this band be interpreted as color?
See also:
GDALRasterBand::GetColorInterpretation()
;; --------------------------------------------------------
GDALColorTableH GDALGetRasterColorTable 	(	GDALRasterBandH 	hBand	 )


Fetch the color table associated with band.
See also:
GDALRasterBand::GetColorTable()
GDALDataset::GetRasterCount().
;; --------------------------------------------------------
GDALDataType GDALGetRasterDataType 	(	GDALRasterBandH 	hBand	 )


Fetch the pixel data type for this band.
See also:
GDALRasterBand::GetRasterDataType()
;; --------------------------------------------------------
CPLErr GDALGetRasterHistogram 	(	GDALRasterBandH 	hBand,
		double 	dfMin,
		double 	dfMax,
		int 	nBuckets,
		int * 	panHistogram,
		int 	bIncludeOutOfRange,
		int 	bApproxOK,
		GDALProgressFunc 	pfnProgress,
		void * 	pProgressData
	)


Compute raster histogram.
See also:
GDALRasterBand::GetHistogram()
;; --------------------------------------------------------
double GDALGetRasterMaximum 	(	GDALRasterBandH 	hBand,
		int * 	pbSuccess
	)


Fetch the maximum value for this band.
See also:
GDALRasterBand::GetMaximum()
;; --------------------------------------------------------
double GDALGetRasterMinimum 	(	GDALRasterBandH 	hBand,
		int * 	pbSuccess
	)


Fetch the minimum value for this band.
See also:
GDALRasterBand::GetMinimum()
;; --------------------------------------------------------
double GDALGetRasterNoDataValue 	(	GDALRasterBandH 	hBand,
		int * 	pbSuccess
	)


Fetch the no data value for this band.
See also:
GDALRasterBand::GetNoDataValue()
;; --------------------------------------------------------
double GDALGetRasterOffset 	(	GDALRasterBandH 	hBand,
		int * 	pbSuccess
	)


Fetch the raster value offset.
See also:
GDALRasterBand::GetOffset()
;; --------------------------------------------------------

GDALRasterBandH GDALGetRasterSampleOverview 	(	GDALRasterBandH 	hBand,
		int 	nDesiredSamples
	)


Fetch best sampling overview.
See also:
GDALRasterBand::GetRasterSampleOverview()
;; --------------------------------------------------------
double GDALGetRasterScale 	(	GDALRasterBandH 	hBand,
		int * 	pbSuccess
	)


Fetch the raster value scale.
See also:
GDALRasterBand::GetScale()
;; --------------------------------------------------------
CPLErr GDALGetRasterStatistics 	(	GDALRasterBandH 	hBand,
		int 	bApproxOK,
		int 	bForce,
		double * 	pdfMin,
		double * 	pdfMax,
		double * 	pdfMean,
		double * 	pdfStdDev
	)


Fetch image statistics.
See also:
GDALRasterBand::GetStatistics()
;; --------------------------------------------------------
const char* GDALGetRasterUnitType 	(	GDALRasterBandH 	hBand	 )


Return raster unit type.
See also:
GDALRasterBand::GetUnitType()
;; --------------------------------------------------------
int GDALHasArbitraryOverviews 	(	GDALRasterBandH 	hBand	 )


Check for arbitrary overviews.
See also:
GDALRasterBand::HasArbitraryOverviews()
;; --------------------------------------------------------
CPLErr GDALRasterAdviseRead 	(	GDALRasterBandH 	hBand,
		int 	nXOff,
		int 	nYOff,
		int 	nXSize,
		int 	nYSize,
		int 	nBufXSize,
		int 	nBufYSize,
		GDALDataType 	eDT,
		char ** 	papszOptions
	)


Advise driver of upcoming read requests.
See also:
GDALRasterBand::AdviseRead()
;; --------------------------------------------------------
CPLErr GDALRasterIO 	(	GDALRasterBandH 	hBand,
		GDALRWFlag 	eRWFlag,
		int 	nXOff,
		int 	nYOff,
		int 	nXSize,
		int 	nYSize,
		void * 	pData,
		int 	nBufXSize,
		int 	nBufYSize,
		GDALDataType 	eBufType,
		int 	nPixelSpace,
		int 	nLineSpace
	)


Read/write a region of image data for this band.
See also:
GDALRasterBand::RasterIO()
;; --------------------------------------------------------
CPLErr GDALReadBlock 	(	GDALRasterBandH 	hBand,
		int 	nXOff,
		int 	nYOff,
		void * 	pData
	)


Read a block of image data efficiently.
See also:
GDALRasterBand::ReadBlock()
;; --------------------------------------------------------
CPLErr GDALSetDefaultHistogram 	(	GDALRasterBandH 	hBand,
		double 	dfMin,
		double 	dfMax,
		int 	nBuckets,
		int * 	panHistogram
	)


Set default histogram.
See also:
GDALRasterBand::SetDefaultHistogram()
;; --------------------------------------------------------

CPLErr GDALSetDefaultRAT 	(	GDALRasterBandH 	hBand,
		GDALRasterAttributeTableH 	hRAT
	)


Set default Raster Attribute Table.
See also:
GDALRasterBand::GDALSetDefaultRAT()
;; --------------------------------------------------------
CPLErr GDALSetRasterCategoryNames 	(	GDALRasterBandH 	hBand,
		char ** 	papszNames
	)


Set the category names for this band.
See also:
GDALRasterBand::SetCategoryNames()
;; --------------------------------------------------------

CPLErr GDALSetRasterColorInterpretation 	(	GDALRasterBandH 	hBand,
		GDALColorInterp 	eColorInterp
	)


Set color interpretation of a band.
See also:
GDALRasterBand::SetColorInterpretation()
;; --------------------------------------------------------
CPLErr GDALSetRasterColorTable 	(	GDALRasterBandH 	hBand,
		GDALColorTableH 	hCT
	)


Set the raster color table.
See also:
GDALRasterBand::SetColorTable()

CPLErr GDALSetRasterNoDataValue 	(	GDALRasterBandH 	hBand,
		double 	dfValue
	)


Set the no data value for this band.
See also:
GDALRasterBand::SetNoDataValue()
;; --------------------------------------------------------
CPLErr GDALSetRasterOffset 	(	GDALRasterBandH 	hBand,
		double 	dfNewOffset
	)


Set scaling offset.
See also:
GDALRasterBand::SetOffset()
;; --------------------------------------------------------
CPLErr GDALSetRasterScale 	(	GDALRasterBandH 	hBand,
		double 	dfNewOffset
	)


Set scaling ratio.
See also:
GDALRasterBand::SetScale()
;; --------------------------------------------------------
CPLErr GDALSetRasterStatistics 	(	GDALRasterBandH 	hBand,
		double 	dfMin,
		double 	dfMax,
		double 	dfMean,
		double 	dfStdDev
	)


Set statistics on band.
See also:
GDALRasterBand::SetStatistics()
;; --------------------------------------------------------
CPLErr GDALSetRasterUnitType 	(	GDALRasterBandH 	hBand,
		const char * 	pszNewValue
	)


Set unit type.
See also:
GDALRasterBand::SetUnitType()
Since: GDAL 1.8.0

;; --------------------------------------------------------

CPLErr GDALWriteBlock 	(	GDALRasterBandH 	hBand,
		int 	nXOff,
		int 	nYOff,
		void * 	pData
	)


Write a block of image data efficiently.
See also:
GDALRasterBand::WriteBlock()
;; EOF