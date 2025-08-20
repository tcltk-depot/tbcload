/*
 * cmpRPkg.c --
 *
 *  This file contains the C interfaces to the Tcl load command for the
 *  Reader package: the Reader_Init function.
 *
 * Copyright (c) 1998-2000 Ajuba Solutions
 * Copyright (c) 2000, 2017 ActiveState Software Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * RCS: @(#) $Id: cmpRPkg.c,v 1.2 2000/05/30 22:19:06 wart Exp $
 */

#include "cmpInt.h"
#include "proTbcLoad.h"

/*
 * name and version of this package
 */

static char packageName[]    = PACKAGE_NAME;
static char packageVersion[] = PACKAGE_VERSION;

/*
 * Name of the commands exported by this package
 */

static char evalCommand[] = CMP_EVAL_COMMAND;
static char procCommand[] = CMP_PROC_COMMAND;

/*
 * this struct describes an entry in the table of command names and command
 * procs
 */

typedef struct CmdTable
{
  char *cmdName;
  Tcl_ObjCmdProc *proc;
  int exportIt;
} CmdTable;

/*
 * Declarations for functions defined in this file.
 */

static int  TbcloadInitInternal (Tcl_Interp *interp, int isSafe);
static int  RegisterCommand (Tcl_Interp* interp, char *namespace, const CmdTable *cmdTablePtr);

/*
 * List of commands to create when the package is loaded; must go after the
 * declarations of the enable command procedure.
 */

static const CmdTable commands[] =
  {
    { evalCommand,  Tbcload_EvalObjCmd, 1 },
    { procCommand,  Tbcload_ProcObjCmd, 1 },
    { 0, 0, 0 }
  };

static const CmdTable safeCommands[] =
  {
    { evalCommand,  Tbcload_EvalObjCmd, 1 },
    { procCommand,  Tbcload_ProcObjCmd, 1 },
    { 0, 0, 0 }
  };

/*
 *----------------------------------------------------------------------
 *
 * Tbcload_Init --
 *
 *  This procedure initializes the Loader package.
 *  The initialization consists of add ing the compiled script loader to the
 *  set of registered script loaders.
 *
 * Results:
 *  A standard Tcl result.
 *
 * Side effects:
 *  None.
 *
 *----------------------------------------------------------------------
 */

int
Tbcload_Init(Tcl_Interp *interp)
{
  return TbcloadInitInternal(interp, 0);
}

/*
 *----------------------------------------------------------------------
 *
 * Tbcload_SafeInit --
 *
 *  This procedure initializes the Loader package.
 *  The initialization consists of add ing the compiled script loader to the
 *  set of registered script loaders.
 *
 * Results:
 *  A standard Tcl result.
 *
 * Side effects:
 *  None.
 *
 *----------------------------------------------------------------------
 */

int
Tbcload_SafeInit(Tcl_Interp *interp)
{
  return TbcloadInitInternal(interp, 1);
}

/*
 *----------------------------------------------------------------------
 *
 * RegisterCommand --
 *
 *  This procedure registers a command in the context of the given namespace.
 *
 * Results:
 *  A standard Tcl result.
 *
 * Side effects:
 *  None.
 *
 *----------------------------------------------------------------------
 */

static int RegisterCommand(Tcl_Interp *interp, char *namespace, const CmdTable *cmdTablePtr)
{
  char buf[128];

  if (cmdTablePtr->exportIt) {
    sprintf(buf, "namespace eval %s { namespace export %s }", namespace, cmdTablePtr->cmdName);
    if (Tcl_Eval(interp, buf) != TCL_OK)
      return TCL_ERROR;
  }

  sprintf(buf, "%s::%s", namespace, cmdTablePtr->cmdName);
  Tcl_CreateObjCommand(interp, buf, cmdTablePtr->proc, 0, 0);

  return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * TbcloadInitInternal --
 *
 *  This procedure initializes the Loader package.
 *  The isSafe flag is 1 if the interpreter is safe, 0 otherwise.
 *
 * Results:
 *  A standard Tcl result.
 *
 * Side effects:
 *  None.
 *
 *----------------------------------------------------------------------
 */

static int
TbcloadInitInternal(Tcl_Interp *interp, int isSafe)
{
  const CmdTable *cmdTablePtr;

  if (TbcloadInit(interp) != TCL_OK) {
    return TCL_ERROR;
  }

  cmdTablePtr = (isSafe) ? &safeCommands[0] : &commands[0];
  for ( ; cmdTablePtr->cmdName ; cmdTablePtr++) {
    if (RegisterCommand(interp, packageName, cmdTablePtr) != TCL_OK) {
      return TCL_ERROR;
    }
  }

  return Tcl_PkgProvide(interp, packageName, packageVersion);
}

/*
 *----------------------------------------------------------------------
 *
 * TbcloadGetPackageName --
 *
 *  Returns the package name for the loader package.
 *
 * Results:
 *  See above.
 *
 * Side effects:
 *  None.
 *
 *----------------------------------------------------------------------
 */

const char *
TbcloadGetPackageName()
{
  return packageName;
}
