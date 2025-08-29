/*
 * proTbcLoad.h --
 *
 *  Declarations of the interfaces exported by the tbcload package.
 *
 * Copyright (c) 1998-2000 Ajuba Solutions
 * Copyright (c) 2000, 2017 ActiveState Software Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * RCS: @(#) $Id: proTbcLoad.h,v 1.4 2000/10/31 23:30:51 welch Exp $
 */

#ifndef _PROTBCLOAD_H
#define _PROTBCLOAD_H

#include "tcl.h"

#ifdef BUILD_tbcload
#undef TCL_STORAGE_CLASS
#define TCL_STORAGE_CLASS DLLEXPORT
#endif

/*
 *----------------------------------------------------------------
 * Procedures exported by cmpRead.c and cmpRPkg.c
 *----------------------------------------------------------------
 */

EXTERN int Tbcload_EvalObjCmd(void* dummy, Tcl_Interp* interp, Tcl_Size objc, Tcl_Obj* const objv[]);
EXTERN int Tbcload_ProcObjCmd(void* dummy, Tcl_Interp* interp, Tcl_Size objc, Tcl_Obj* const objv[]);
EXTERN const char* TbcloadGetPackageName(void);

EXTERN int Tbcload_Init(Tcl_Interp* interp);
EXTERN int Tbcload_SafeInit(Tcl_Interp* interp);

#undef TCL_STORAGE_CLASS
#define TCL_STORAGE_CLASS DLLIMPORT

#endif /* _PROTBCLOAD_H */
