;;; -*- package: CL-OGR; Syntax: Common-lisp; Base: 10 -*-

(in-package :cl-ogr)

;; --------------------------------------------------------

OGRStyleToolH OGR_ST_Create 	(	OGRSTClassId 	eClassId	 )


OGRStyleTool factory.

This function is a constructor for OGRStyleTool derived classes.

 @argument[eClassId]{subclass of style tool to create. One of
 OGRSTCPen (1), OGRSTCBrush (2), OGRSTCSymbol (3) or OGRSTCLabel (4).}

 @return{an handle to the new style tool object or NULL if the creation failed.}

;; --------------------------------------------------------

void OGR_ST_Destroy 	(	OGRStyleToolH 	hST	 )


Destroy Style Tool.
@argument[hST 	handle to the style tool to destroy.

;; --------------------------------------------------------

double OGR_ST_GetParamDbl 	(	OGRStyleToolH 	hST,
		int 	eParam,
		int * 	bValueIsNull
	)


Get Style Tool parameter value as a double.

Maps to the OGRStyleTool subclasses' GetParamDbl() methods.
@argument[hST 	handle to the style tool.

 @argument[eParam]{the parameter id from the enumeration corresponding
 to the type of this style tool (one of the OGRSTPenParam,
 OGRSTBrushParam, OGRSTSymbolParam or OGRSTLabelParam enumerations)}
 @argument[bValueIsNull]{pointer to an integer that will be set to
 TRUE or FALSE to indicate whether the parameter value is NULL.}

@return{the parameter value as double and sets bValueIsNull.}

;; --------------------------------------------------------

int OGR_ST_GetParamNum 	(	OGRStyleToolH 	hST,
		int 	eParam,
		int * 	bValueIsNull
	)


Get Style Tool parameter value as an integer.

Maps to the OGRStyleTool subclasses' GetParamNum() methods.
@argument[hST 	handle to the style tool.

 @argument[eParam]{the parameter id from the enumeration corresponding
 to the type of this style tool (one of the OGRSTPenParam,
 OGRSTBrushParam, OGRSTSymbolParam or OGRSTLabelParam enumerations)}
 @argument[bValueIsNull]{pointer to an integer that will be set to
 TRUE or FALSE to indicate whether the parameter value is NULL.}

 @return{the parameter value as integer and sets bValueIsNull.}

;; --------------------------------------------------------

const char* OGR_ST_GetParamStr 	(	OGRStyleToolH 	hST,
		int 	eParam,
		int * 	bValueIsNull
	)


Get Style Tool parameter value as string.

Maps to the OGRStyleTool subclasses' GetParamStr() methods.

@argument[hST 	handle to the style tool.

 @argument[eParam]{the parameter id from the enumeration corresponding
 to the type of this style tool (one of the OGRSTPenParam,
 OGRSTBrushParam, OGRSTSymbolParam or OGRSTLabelParam enumerations)}
 @argument[bValueIsNull]{pointer to an integer that will be set to
 TRUE or FALSE to indicate whether the parameter value is NULL.}

@return{the parameter value as string and sets bValueIsNull.}

;; --------------------------------------------------------

int OGR_ST_GetRGBFromString 	(	OGRStyleToolH 	hST,
		:string 	pszColor,
		int * 	pnRed,
		int * 	pnGreen,
		int * 	pnBlue,
		int * 	pnAlpha
	)


Return the r,g,b,a components of a color encoded in #RRGGBB[AA] format.

Maps to OGRStyleTool::GetRGBFromString().

@argument[hST 	handle to the style tool.
 @argument[pszColor]{the color to parse}
 @argument[pnRed]{pointer to an int in which the red value will be returned}
 @argument[pnGreen]{pointer to an int in which the green value will be returned}
 @argument[pnBlue]{pointer to an int in which the blue value will be returned}
 @argument[pnAlpha]{pointer to an int in which the (optional) alpha
 value will be returned}

@return{TRUE if the color could be succesfully parsed, or FALSE in case of errors.}

;; --------------------------------------------------------

const char* OGR_ST_GetStyleString 	(	OGRStyleToolH 	hST	 )


Get the style string for this Style Tool.

Maps to the OGRStyleTool subclasses' GetStyleString() methods.

@argument[hST 	handle to the style tool.

@return{the style string for this style tool or "" if the hST is invalid.}

;; --------------------------------------------------------

OGRSTClassId OGR_ST_GetType 	(	OGRStyleToolH 	hST	 )


Determine type of Style Tool.
@argument[hST 	handle to the style tool.

 @return{the style tool type, one of OGRSTCPen (1), OGRSTCBrush (2),
 OGRSTCSymbol (3) or OGRSTCLabel (4). Returns OGRSTCNone (0) if the
 OGRStyleToolH is invalid.}

;; --------------------------------------------------------

OGRSTUnitId OGR_ST_GetUnit 	(	OGRStyleToolH 	hST	 )


Get Style Tool units.
@argument[hST 	handle to the style tool.

@return{the style tool units.}

;; --------------------------------------------------------

void OGR_ST_SetParamDbl 	(	OGRStyleToolH 	hST,
		int 	eParam,
		double 	dfValue
	)


Set Style Tool parameter value from a double.

Maps to the OGRStyleTool subclasses' SetParamDbl() methods.
@argument[hST 	handle to the style tool.

 @argument[eParam]{the parameter id from the enumeration corresponding
 to the type of this style tool (one of the OGRSTPenParam,
 OGRSTBrushParam, OGRSTSymbolParam or OGRSTLabelParam enumerations)}
 @argument[dfValue]{the new parameter value}

;; --------------------------------------------------------

void OGR_ST_SetParamNum 	(	OGRStyleToolH 	hST,
		int 	eParam,
		int 	nValue
	)


Set Style Tool parameter value from an integer.

Maps to the OGRStyleTool subclasses' SetParamNum() methods.

@argument[hST 	handle to the style tool.

 @argument[eParam]{the parameter id from the enumeration corresponding
 to the type of this style tool (one of the OGRSTPenParam,
 OGRSTBrushParam, OGRSTSymbolParam or OGRSTLabelParam enumerations)}
 @argument[nValue]{the new parameter value}

;; --------------------------------------------------------

void OGR_ST_SetParamStr 	(	OGRStyleToolH 	hST,
		int 	eParam,
		:string 	pszValue
	)


Set Style Tool parameter value from a string.

Maps to the OGRStyleTool subclasses' SetParamStr() methods.

@argument[hST 	handle to the style tool.

 @argument[eParam]{the parameter id from the enumeration corresponding
 to the type of this style tool (one of the OGRSTPenParam,
 OGRSTBrushParam, OGRSTSymbolParam or OGRSTLabelParam enumerations)}
 @argument[pszValue]{the new parameter value}

;; --------------------------------------------------------

void OGR_ST_SetUnit 	(	OGRStyleToolH 	hST,
		OGRSTUnitId 	eUnit,
		double 	dfGroundPaperScale
	)


Set Style Tool units.

This function is the same as OGRStyleTool::SetUnit()

@argument[hST 	handle to the style tool.
 @argument[eUnit]{the new unit.}
 @argument[dfGroundPaperScale]{ground to paper scale factor.}

;; EOF
