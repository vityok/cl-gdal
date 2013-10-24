;;; -*- package: CL-OGR; Syntax: Common-lisp; Base: 10 -*-

(in-package :cl-ogr)

;; --------------------------------------------------------

int OGR_SM_AddPart 	(	OGRStyleMgrH 	hSM,
		OGRStyleToolH 	hST
	)


Add a part (style tool) to the current style.

This function is the same as the C++ method OGRStyleMgr::AddPart().
@argument[hSM 	handle to the style manager.
 @argument[hST]{the style tool defining the part to add.}

@return{TRUE on success, FALSE on errors.

;; --------------------------------------------------------

int OGR_SM_AddStyle 	(	OGRStyleMgrH 	hSM,
		:string 	pszStyleName,
		:string 	pszStyleString
	)


Add a style to the current style table.

This function is the same as the C++ method OGRStyleMgr::AddStyle().
@argument[hSM 	handle to the style manager.
 @argument[pszStyleName]{the name of the style to add.}
 @argument[pszStyleString]{the style string to use, or NULL to use the
 style stored in the manager.}

@return{TRUE on success, FALSE on errors.

;; --------------------------------------------------------

OGRStyleMgrH OGR_SM_Create 	(	OGRStyleTableH 	hStyleTable	 )


OGRStyleMgr factory.

This function is the same as the C++ method OGRStyleMgr::OGRStyleMgr().
@argument[hStyleTable 	pointer to OGRStyleTable or NULL if not working with a style table.

@return{an handle to the new style manager object.}

;; --------------------------------------------------------

void OGR_SM_Destroy 	(	OGRStyleMgrH 	hSM	 )


Destroy Style Manager.

This function is the same as the C++ method OGRStyleMgr::~OGRStyleMgr().
@argument[hSM 	handle to the style manager to destroy.

;; --------------------------------------------------------

OGRStyleToolH OGR_SM_GetPart 	(	OGRStyleMgrH 	hSM,
		int 	nPartId,
		:string 	pszStyleString
	)


Fetch a part (style tool) from the current style.

This function is the same as the C++ method OGRStyleMgr::GetPart().

This function instanciates a new object that should be freed with OGR_ST_Destroy().
@argument[hSM 	handle to the style manager.
 @argument[nPartId]{the part number (0-based index).}
 @argument[pszStyleString]{(optional) the style string on which to
 operate. If NULL then the current style string stored in the style
 manager is used.}

 @return{OGRStyleToolH of the requested part (style tools) or NULL on error.}

;; --------------------------------------------------------

int OGR_SM_GetPartCount 	(	OGRStyleMgrH 	hSM,
		:string 	pszStyleString
	)


Get the number of parts in a style.

This function is the same as the C++ method OGRStyleMgr::GetPartCount().
@argument[hSM 	handle to the style manager.
 @argument[pszStyleString]{(optional) the style string on which to
 operate. If NULL then the current style string stored in the style
 manager is used.}

@return{the number of parts (style tools) in the style.

;; --------------------------------------------------------

const char* OGR_SM_InitFromFeature 	(	OGRStyleMgrH 	hSM,
		OGRFeatureH 	hFeat
	)


Initialize style manager from the style string of a feature.

This function is the same as the C++ method OGRStyleMgr::InitFromFeature().
@argument[hSM 	handle to the style manager.
 @argument[hFeat]{handle to the new feature from which to read the style.}

 @return{a reference to the style string read from the feature, or
 NULL in case of error.}

;; --------------------------------------------------------

int OGR_SM_InitStyleString 	(	OGRStyleMgrH 	hSM,
		:string 	pszStyleString
	)


Initialize style manager from the style string.

This function is the same as the C++ method OGRStyleMgr::InitStyleString().
@argument[hSM 	handle to the style manager.
 @argument[pszStyleString]{the style string to use (can be NULL).}

@return{TRUE on success, FALSE on errors.}

;; EOF
