/*
 * cmpRPkg.c --
 *
 *  Loader package initialization and command/variable registration (reader).
 *  Tcl 9 modernization:
 *    - No string eval for namespace/export
 *    - Idempotent namespace handling
 *    - Const-correct tables, modern ObjCmd signatures via headers
 *    - Safe string assembly via Tcl_DString
 */

#include <string.h>
#include "cmpInt.h"
#include "proTbcLoad.h"

/* Package identity (TEA) */
static char packageName[] = PACKAGE_NAME;       /* "tbcload" */
static char packageVersion[] = PACKAGE_VERSION; /* e.g. "1.9.0" */

/* Command names (from headers) */
static char evalCommand[] = CMP_EVAL_COMMAND; /* "bceval" */
static char procCommand[] = CMP_PROC_COMMAND; /* "bcproc"  */

/* Command table */
typedef struct CmdTable
{
    const char* cmdName;   /* unqualified, e.g. "bceval" */
    Tcl_ObjCmdProc2* proc; /* implementation */
    int exportIt;          /* nonzero => export */
} CmdTable;

/* Forward decls */
static int TbcloadInitInternal(Tcl_Interp* interp, int isSafe);
static int RegisterCommand(Tcl_Interp* interp, const char* nsName, const CmdTable* cmd);

/* Command lists */
static const CmdTable commands[] = {{evalCommand, Tbcload_EvalObjCmd, 1}, {procCommand, Tbcload_ProcObjCmd, 1}, {NULL, NULL, 0}};

static const CmdTable safeCommands[] = {
    {evalCommand, Tbcload_EvalObjCmd, 1}, {procCommand, Tbcload_ProcObjCmd, 1}, {NULL, NULL, 0}};

/* --- helpers --- */
static Tcl_Namespace* GetOrCreateNamespace(Tcl_Interp* interp, const char* nsName)
{
    Tcl_Namespace* ns = Tcl_FindNamespace(interp, nsName, NULL, TCL_GLOBAL_ONLY);
    if (!ns)
    {
        ns = Tcl_CreateNamespace(interp, nsName, NULL, NULL);
    }
    return ns;
}

static int RegisterCommand(Tcl_Interp* interp, const char* nsName, const CmdTable* cmd)
{
    Tcl_Namespace* ns = GetOrCreateNamespace(interp, nsName);
    if (!ns)
        return TCL_ERROR;

    Tcl_DString fq;
    Tcl_DStringInit(&fq);
    Tcl_DStringAppend(&fq, nsName, -1);
    Tcl_DStringAppend(&fq, "::", 2);
    Tcl_DStringAppend(&fq, cmd->cmdName, -1);

    Tcl_CreateObjCommand2(interp, Tcl_DStringValue(&fq), cmd->proc, NULL, NULL);
    Tcl_DStringFree(&fq);

    if (cmd->exportIt)
    {
        if (Tcl_Export(interp, ns, cmd->cmdName, 0) != TCL_OK)
            return TCL_ERROR;
    }
    return TCL_OK;
}

/* --- public inits --- */

int Tbcload_Init(Tcl_Interp* interp)
{
    return TbcloadInitInternal(interp, 0);
}

int Tbcload_SafeInit(Tcl_Interp* interp)
{
    return TbcloadInitInternal(interp, 1);
}

/* Common initializer */
static int TbcloadInitInternal(Tcl_Interp* interp, int isSafe)
{
#ifdef USE_TCL_STUBS
    if (!Tcl_InitStubs(interp, "9.0", 1))
    {
        return TCL_ERROR;
    }
#else
    if (Tcl_PkgRequire(interp, "Tcl", "9.0", 1) == NULL)
    {
        return TCL_ERROR;
    }
#endif

    /* Initialize loader core (from tbcload) */
    if (TbcloadInit(interp) != TCL_OK)
    { /* internal helper provided by tbcload */
        return TCL_ERROR;
    }

    /* Use packageName "tbcload" also as namespace */
    const char* nsName = packageName;

    const CmdTable* ct = isSafe ? &safeCommands[0] : &commands[0];
    for (; ct->cmdName; ct++)
    {
        if (RegisterCommand(interp, nsName, ct) != TCL_OK)
        {
            return TCL_ERROR;
        }
    }

    return Tcl_PkgProvide(interp, packageName, packageVersion);
}

/* Utility */
const char* TbcloadGetPackageName(void)
{
    return packageName;
}
