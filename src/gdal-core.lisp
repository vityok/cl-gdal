;;; -*- package: GDAL; Syntax: Common-lisp; Base: 10 -*-

(in-package :cl-gdal)

;; --------------------------------------------------------

(cffi:defcenum GDAL-Access
  "Flag indicating read/write, or read-only access to data."
  :GA_ReadOnly				; Read only (no update) access
  :GA_Update)				; Read/write access.

;; --------------------------------------------------------

(cffi:defcenum GDAL-Color-Interp
  "Types of color interpretation for raster bands."
  :GCI_GrayIndex	       ; Greyscale
  :GCI_PaletteIndex	       ; Paletted (see associated color table)
  :GCI_RedBand		       ; Red band of RGBA image
  :GCI_GreenBand	       ; Green band of RGBA image
  :GCI_BlueBand		       ; Blue band of RGBA image
  :GCI_AlphaBand	       ; Alpha (0=transparent, 255=opaque)
  :GCI_HueBand		       ; Hue band of HLS image
  :GCI_SaturationBand	       ; Saturation band of HLS image
  :GCI_LightnessBand	       ; Lightness band of HLS image
  :GCI_CyanBand		       ; Cyan band of CMYK image
  :GCI_MagentaBand	       ; Magenta band of CMYK image
  :GCI_YellowBand	       ; Yellow band of CMYK image
  :GCI_BlackBand	       ; Black band of CMLY image
  :GCI_YCbCr_YBand	       ; Y Luminance
  :GCI_YCbCr_CbBand	       ; Cb Chroma
  :GCI_YCbCr_CrBand	       ; Cr Chroma
  :GCI_Max)		       ; Max current value

;; --------------------------------------------------------

(cffi:defcenum GDAL-Data-Type
  "Pixel data types"
  :GDT_Unknown			     ; Unknown or unspecified type
  :GDT_Byte			     ; Eight bit unsigned integer
  :GDT_UInt16			     ; Sixteen bit unsigned integer
  :GDT_Int16			     ; Sixteen bit signed integer
  :GDT_UInt32			     ; Thirty two bit unsigned integer
  :GDT_Int32			     ; Thirty two bit signed integer
  :GDT_Float32			     ; Thirty two bit floating point
  :GDT_Float64			     ; Sixty four bit floating point
  :GDT_CInt16			     ; Complex Int16
  :GDT_CInt32			     ; Complex Int32
  :GDT_CFloat32			     ; Complex Float32
  :GDT_CFloat64)		     ; Complex Float64

;; --------------------------------------------------------

(cffi:defcenum GDAL-Palette-Interp
  "Types of color interpretations for a GDALColorTable."
  :GPI_Gray   ; Grayscale (in GDALColorEntry.c1)
  :GPI_RGB    ; Red, Green, Blue and Alpha in (in c1, c2, c3 and c4)
  :GPI_CMYK   ; Cyan, Magenta, Yellow and Black (in c1, c2, c3 and c4)
  :GPI_HLS)   ; Hue, Lightness and Saturation (in c1, c2, and c3)

;; --------------------------------------------------------

(cffi:defcenum GDAL-RAT-Field-Type
  "Field type of raster attribute table."
  :GFT_Integer			       ; Integer field
  :GFT_Real			       ; Floating point (double) field
  :GFT_String)			       ; String field

;; --------------------------------------------------------

(cffi:defcenum GDAL-RAT-Field-Usage
  "Field usage of raster attribute table."
  :GFU_Generic			    ; General purpose field.
  :GFU_PixelCount		    ; Histogram pixel count
  :GFU_Name			    ; Class name
  :GFU_Min			    ; Class range minimum
  :GFU_Max			    ; Class range maximum
  :GFU_MinMax			    ; Class value (min=max)
  :GFU_Red			    ; Red class color (0-255)
  :GFU_Green			    ; Green class color (0-255)
  :GFU_Blue			    ; Blue class color (0-255)
  :GFU_Alpha			    ; Alpha (0=transparent,255=opaque)
  :GFU_RedMin			    ; Color Range Red Minimum
  :GFU_GreenMin			    ; Color Range Green Minimum
  :GFU_BlueMin			    ; Color Range Blue Minimum
  :GFU_AlphaMin			    ; Color Range Alpha Minimum
  :GFU_RedMax			    ; Color Range Red Maximum
  :GFU_GreenMax			    ; Color Range Green Maximum
  :GFU_BlueMax			    ; Color Range Blue Maximum
  :GFU_AlphaMax			    ; Color Range Alpha Maximum
  :GFU_MaxCount)		    ; Maximum GFU value

;; --------------------------------------------------------

(cffi:defcenum GDAL-RW-Flag
  "Read/Write flag for RasterIO() method"
  :GF_Read				; Read data
  :GF_Write)				; Write data

;; EOF