;;; -*- package: CL-OGR; Syntax: Common-lisp; Base: 10 -*-

(in-package :cl-ogr)

;; --------------------------------------------------------

OGRStyleTableH OGR_STBL_Create 	(	void 		 )


OGRStyleTable factory.

This function is the same as the C++ method OGRStyleTable::OGRStyleTable().
@return{an handle to the new style table object.

;; --------------------------------------------------------

void OGR_STBL_Destroy 	(	OGRStyleTableH 	hSTBL	 )


Destroy Style Table.
@argument[hSTBL 	handle to the style table to destroy.

;; --------------------------------------------------------

const char* OGR_STBL_Find 	(	OGRStyleTableH 	hStyleTable,
		:string 	pszName
	)


Get a style string by name.

This function is the same as the C++ method OGRStyleTable::Find().
@argument[hStyleTable 	handle to the style table.
 @argument[pszName]{the name of the style string to find.}

@return{the style string matching the name or NULL if not found or error.

;; --------------------------------------------------------

const char* OGR_STBL_GetLastStyleName 	(	OGRStyleTableH 	hStyleTable	 )


 Get the style name of the last style string fetched with
 OGR_STBL_GetNextStyle.

 This function is the same as the C++ method
 OGRStyleTable::GetStyleName().

@argument[hStyleTable 	handle to the style table.

@return{the Name of the last style string or NULL on error.}

;; --------------------------------------------------------

const char* OGR_STBL_GetNextStyle 	(	OGRStyleTableH 	hStyleTable	 )


Get the next style string from the table.

This function is the same as the C++ method OGRStyleTable::GetNextStyle().
@argument[hStyleTable 	handle to the style table.

@return{the next style string or NULL on error.

;; --------------------------------------------------------

int OGR_STBL_LoadStyleTable 	(	OGRStyleTableH 	hStyleTable,
		:string 	pszFilename
	)


Load a style table from a file.

This function is the same as the C++ method OGRStyleTable::LoadStyleTable().
@argument[hStyleTable 	handle to the style table.
 @argument[pszFilename]{the name of the file to load from.}

@return{TRUE on success, FALSE on error

;; --------------------------------------------------------

void OGR_STBL_ResetStyleStringReading 	(	OGRStyleTableH 	hStyleTable	 )


Reset the next style pointer to 0.

This function is the same as the C++ method OGRStyleTable::ResetStyleStringReading().
@argument[hStyleTable 	handle to the style table.

;; --------------------------------------------------------

int OGR_STBL_SaveStyleTable 	(	OGRStyleTableH 	hStyleTable,
		:string 	pszFilename
	)


Save a style table to a file.

 This function is the same as the C++ method OGRStyleTable::SaveStyleTable().

@argument[hStyleTable 	handle to the style table.
 @argument[pszFilename]{the name of the file to save to.}

@return{TRUE on success, FALSE on error}

;; EOF
