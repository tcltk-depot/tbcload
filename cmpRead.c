/*
 * cmpRead.c --
 *
 *  This file contains the code used by the compiled file script loader to
 *  load a compiled script. The script loader is registered in the Init
 *  procedure of the "Loader" package.
 *
 * Copyright (c) 1998-2000 Ajuba Solutions
 * Copyright (c) 2000, 2017 ActiveState Software Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * RCS: @(#) $Id: cmpRead.c,v 1.11 2002/11/28 16:53:10 andreas_kupries Exp $
 */

#include "cmpInt.h"
#include "proTbcLoad.h"

/*
 * This structure contains signature information from a compiled file.
 */
typedef struct ImageSignature
{
    Tcl_Size formatNumber;    /* version number of the .tbc file format */
    Tcl_Size cmpMajorVersion; /* major version of the compiler package that generated the compiled image */
    Tcl_Size cmpMinorVersion; /* minor version of the compiler package that generated the compiled image */
    Tcl_Size tclMajorVersion; /* major version of the interpreter that generated the compiled image */
    Tcl_Size tclMinorVersion; /* minor version of the interpreter that generated the compiled image */
} ImageSignature;

/*
 * The extraction environment contains the state of an extraction for a compiled
 * ByteCode structure.
 */
typedef struct ExtractionEnv
{
    char* imageBase;    /* base of the compiled image bytes */
    char* imageEnd;     /* address immediately following the last byte in the compiled image */
    char* curImagePtr;  /* pointer to the part of the compiled image we are parsing */
    ByteCode* codePtr;  /* the ByteCode structure we are populating */
    Tcl_Size codeSize;  /* the size of the Bytecode structure */
    LocMapSizes locMap; /* this structure holds the sizes of the location map arrays as extracted from the image header */
    ImageSignature sig; /* Image signature from file */
} ExtractionEnv;

/*
 * This struct holds the decoding context for a run of ExtractByteSequence
 */
typedef struct A85DecodeContext
{
    Tcl_Size bytesToDecode; /* number of bytes left to decode */
    unsigned char* curPtr;  /* pointer to the next available location in the decode buffer */
    int curChar;            /* index of the next base-85 digit to be stored in the decode buffer */
    int decodeBuf[5];       /* buffer to hold the 5-tuple we are currently collecting. */
} A85DecodeContext;

/*
 * The current format version number.  This will be set in
 * TbcloadInit to the correct value for the current Tcl interpreter.
 */
static Tcl_Size formatVersion = 3;

/*
 * The current Tcl version that we are loaded in.
 */
static Tcl_Size tclMajorVersion = 0, tclMinorVersion = 0;

/*
 * No source available message. The source field in ByteCode structures that
 * are extracted from compiled images is initialized with this string, and
 * will be used to generate call stacks and error info.
 * Also the string representation of procedure body objects is initialized
 * with this value.
 * Must be kept to under 128 bytes in length (because of the way the source
 * code map is automatically generated in the ByteCode structs).
 * There are two parts to the "source" for a compiled script:
 *  - a comment fo humans to look at (for example in an errorInfo trace)
 *  - a bit of code that throws an error; this is used to trigger an error
 *    in some situations; for example, if the following construct is used
 *  proc A {args} { set x 5 ; return "hello world $x" }
 *  proc B {args} [info body A]
 *    a call to B will trigger the error.
 */
static char noSourceCode[] = "# Compiled -- no source code available\nerror \"called a copy of a compiled script\"";
static Tcl_Size noSourceCodeSize = sizeof(noSourceCode);

/*
 * Marker string appended by Tbcload_EvalObjCmd to the errorInfo, for use by
 * the catch code to strip out error info that we don't want.
 */
#if USE_CATCH_WRAPPER
static char errorInfoMarker[] = CMP_ERRORINFO_MARKER;
#endif

/*
 * Map between ExceptionRangeType enums and type codes.
 * This map must be kept consistent with the equivalent one in cmpWrite.c.
 */
static ExcRangeMap excRangeMap[] = {
    {LOOP_EXCEPTION_RANGE, CMP_LOOP_EXCEPTION_RANGE}, {CATCH_EXCEPTION_RANGE, CMP_CATCH_EXCEPTION_RANGE}, {0, '\0'}};

/*
 * The list of VAR_ flag values to check when emitting. The order is
 * is important an must be kept consistent with the equivalent list in
 * cmpWrite.c
 */
static int varFlagsList[] = {0, 0, 0, 0, 0, 0, 0, 0, VAR_ARGUMENT, VAR_TEMPORARY, 0};
static int varFlagsListSize = sizeof(varFlagsList) / sizeof(varFlagsList[0]);

/*
 * We use a modified encoding scheme which avoids the Tcl special characters
 * $, {, }, [, ], and \.
 * Because of this, we need to use a table instead of generating the character
 * codes arithmetically.
 * The decode map maps character values in the range 0 - 127 to the
 * corresponding base-85 digit's value; characters that are not base-85
 * digits have a map value of -1 if they are considered whitespace, and -2 if
 * they are illegal characters in the sequence (they should not appear).
 * The decoder skips whitespace while decoding.
 */
#define times85(x) ((((((x << 2) + x) << 2) + x) << 2) + x)

#define A85_WHITESPACE -1
#define A85_ILLEGAL_CHAR -2
#define A85_Z -3

static int decodeMap[] = {
    A85_ILLEGAL_CHAR, /* ^@ */
    A85_ILLEGAL_CHAR, /* ^A */
    A85_ILLEGAL_CHAR, /* ^B */
    A85_ILLEGAL_CHAR, /* ^C */
    A85_ILLEGAL_CHAR, /* ^D */
    A85_ILLEGAL_CHAR, /* ^E */
    A85_ILLEGAL_CHAR, /* ^F */
    A85_ILLEGAL_CHAR, /* ^G */
    A85_ILLEGAL_CHAR, /* ^H */
    A85_WHITESPACE,   /* \t */
    A85_WHITESPACE,   /* \n */
    A85_ILLEGAL_CHAR, /* ^K */
    A85_ILLEGAL_CHAR, /* ^L */
    A85_ILLEGAL_CHAR, /* ^M */
    A85_ILLEGAL_CHAR, /* ^N */
    A85_ILLEGAL_CHAR, /* ^O */
    A85_ILLEGAL_CHAR, /* ^P */
    A85_ILLEGAL_CHAR, /* ^Q */
    A85_ILLEGAL_CHAR, /* ^R */
    A85_ILLEGAL_CHAR, /* ^S */
    A85_ILLEGAL_CHAR, /* ^T */
    A85_ILLEGAL_CHAR, /* ^U */
    A85_ILLEGAL_CHAR, /* ^V */
    A85_ILLEGAL_CHAR, /* ^W */
    A85_ILLEGAL_CHAR, /* ^X */
    A85_ILLEGAL_CHAR, /* ^Y */
    A85_ILLEGAL_CHAR, /* ^Z */
    A85_ILLEGAL_CHAR, /* ^[ */
    A85_ILLEGAL_CHAR, /* ^\ */
    A85_ILLEGAL_CHAR, /* ^] */
    A85_ILLEGAL_CHAR, /* ^^ */
    A85_ILLEGAL_CHAR, /* ^_ */
    A85_WHITESPACE,   /*   */
    0,                /* ! */
    A85_ILLEGAL_CHAR, /* " (for hilit: ") */
    2,                /* # */
    A85_ILLEGAL_CHAR, /* $ */
    4,                /* % */
    5,                /* & */
    6,                /* ' */
    7,                /* ( */
    8,                /* ) */
    9,                /* * */
    10,               /* + */
    11,               /* , */
    12,               /* - */
    13,               /* . */
    14,               /* / */
    15,               /* 0 */
    16,               /* 1 */
    17,               /* 2 */
    18,               /* 3 */
    19,               /* 4 */
    20,               /* 5 */
    21,               /* 6 */
    22,               /* 7 */
    23,               /* 8 */
    24,               /* 9 */
    25,               /* : */
    26,               /* ; */
    27,               /* < */
    28,               /* = */
    29,               /* > */
    30,               /* ? */
    31,               /* @ */
    32,               /* A */
    33,               /* B */
    34,               /* C */
    35,               /* D */
    36,               /* E */
    37,               /* F */
    38,               /* G */
    39,               /* H */
    40,               /* I */
    41,               /* J */
    42,               /* K */
    43,               /* L */
    44,               /* M */
    45,               /* N */
    46,               /* O */
    47,               /* P */
    48,               /* Q */
    49,               /* R */
    50,               /* S */
    51,               /* T */
    52,               /* U */
    53,               /* V */
    54,               /* W */
    55,               /* X */
    56,               /* Y */
    57,               /* Z */
    A85_ILLEGAL_CHAR, /* [ */
    A85_ILLEGAL_CHAR, /* \ */
    A85_ILLEGAL_CHAR, /* ] */
    61,               /* ^ */
    62,               /* _ */
    63,               /* ` */
    64,               /* a */
    65,               /* b */
    66,               /* c */
    67,               /* d */
    68,               /* e */
    69,               /* f */
    70,               /* g */
    71,               /* h */
    72,               /* i */
    73,               /* j */
    74,               /* k */
    75,               /* l */
    76,               /* m */
    77,               /* n */
    78,               /* o */
    79,               /* p */
    80,               /* q */
    81,               /* r */
    82,               /* s */
    83,               /* t */
    84,               /* u */
    1,                /* v (replaces ") " */
    3,                /* w (replaces $) */
    58,               /* x (replaces [) */
    59,               /* y (replaces \) */
    A85_Z,            /* z */
    A85_ILLEGAL_CHAR, /* { */
    60,               /* | (replaces ]) */
    A85_ILLEGAL_CHAR, /* } */
    A85_ILLEGAL_CHAR, /* ~ */
};

/*
 * These Tcl_ObjType pointers are initialized the first time that the package
 * is loaded; we do it this way because the actual object types are not
 * exported by the TCL DLL, and therefore if we use the address of the
 * standard types we get an undefined symbol at link time.
 */
static const Tcl_ObjType* cmpTclProProcBodyType = 0;
static const Tcl_ObjType* cmpByteCodeType = 0;
static const Tcl_ObjType* cmpDoubleType = 0;
static const Tcl_ObjType* cmpIntType = 0;

/*
 * Same thing for AuxDataTypes.
 */
static const AuxDataType* cmpJumptableInfoType = 0;
static const AuxDataType* cmpDictUpdateInfoType = 0;
static const AuxDataType* cmpNewForeachInfoType = 0;

static int didLoadTypes = 0;

/*
 * error message to generate when we run into the end of the buffer and we
 * are expecting more stuff
 */
static char prematureEnd[] = "bytecode terminated prematurely";

/*
 * Compatibility layer declarations and static storage follow.
 */
static int compatibilityLayerInit = 0;

/*
 * This procedure is used internally to look up symbols in the current
 * executable, to initialize the compatibility layer properly
 */
extern void* TbcloadGetSymbolAddress(const char* symbolName);

/*
 * The factory for procbody objects.
 */
typedef Tcl_Obj*(ProcBodyFactory)(Proc* procPtr);
static ProcBodyFactory* procBodyFactory = 0;

/*
 * The cleanup proc for procbody objects.
 */
typedef void(ProcBodyCleanup)(Proc* procPtr);
static ProcBodyCleanup* procBodyCleanup = 0;

/*
 *
 * The real tbcload::bcproc command implementation, Tcl_ProcObjCmd
 *
 */
static Tcl_ObjCmdProc* bcprocCmdProc = NULL;

/*
 * Prototypes for procedures defined later in this file:
 */
static int A85DecodeByte(Tcl_Interp* interp, int code, A85DecodeContext* ctxPtr);
static void A85InitDecodeContext(Tcl_Size numBytes, unsigned char* decodeBuf, A85DecodeContext* ctxPtr);
static int AllocAndExtractByteSequence(
    Tcl_Interp* interp, ExtractionEnv* envPtr, int addNull, unsigned char** seqPtrPtr, Tcl_Size* seqSizePtr);
static int AllocAndExtractString(Tcl_Interp* interp, ExtractionEnv* envPtr, int addNull, char** strPtrPtr, Tcl_Size* strSizePtr);
static void AppendErrorLocation(Tcl_Interp* interp, ExtractionEnv* envPtr);
static int CheckSignature(Tcl_Interp* interp, ImageSignature* signaturePtr);
static void CleanupByteCode(ExtractionEnv* envPtr);
static void CleanupExtractEnv(ExtractionEnv* envPtr);
static Tcl_Obj* CreateSimpleObject(ExtractionEnv* envPtr);
static int ExcRangeFromName(Tcl_Size name, ExceptionRangeType* typePtr);
static int ExtractAuxDataArray(
    Tcl_Interp* interp, Tcl_Size numAuxDataItems, ExtractionEnv* envPtr, AuxData* auxDataArray, Tcl_Size auxDataArraySize);
static int ExtractByteCode(Tcl_Interp* interp, ExtractionEnv* envPtr);
static int
ExtractByteSequence(Tcl_Interp* interp, Tcl_Size length, ExtractionEnv* envPtr, unsigned char* seqPtr, Tcl_Size seqSize);
static Tcl_Obj* ExtractCompiledFile(Tcl_Interp* interp, char* codePtr, Tcl_Size codeLength);
static CompiledLocal* ExtractCompiledLocal(Tcl_Interp* interp, ExtractionEnv* envPtr);
static int ExtractDictUpdateInfo(Tcl_Interp* interp, ExtractionEnv* envPtr, AuxData* auxDataPtr);
static int ExtractExcRangeArray(
    Tcl_Interp* interp, Tcl_Size numExceptRanges, ExtractionEnv* envPtr, ExceptionRange* excArray, Tcl_Size excArraySize);
static int ExtractTclSize(Tcl_Interp* interp, ExtractionEnv* envPtr, Tcl_Size* valuePtr);
static int ExtractJumptableInfo(Tcl_Interp* interp, ExtractionEnv* envPtr, AuxData* auxDataPtr);
static int ExtractNewForeachInfo(Tcl_Interp* interp, ExtractionEnv* envPtr, AuxData* auxDataPtr);
static int
ExtractObjArray(Tcl_Interp* interp, Tcl_Size numLitObjects, ExtractionEnv* envPtr, Tcl_Obj** objArray, Tcl_Size objArraySize);
static Tcl_Obj* ExtractObject(Tcl_Interp* interp, ExtractionEnv* envPtr);
static Tcl_Obj* ExtractProcBody(Tcl_Interp* interp, ByteCode* codePtr, ExtractionEnv* envPtr);
static char* ExtractSignature(Tcl_Interp* interp, char* codePtr, char* codeEnd, ImageSignature* signaturePtr);
static int ExtractString(Tcl_Interp* interp, Tcl_Size length, ExtractionEnv* envPtr, char* strPtr, Tcl_Size strSize);
static char* FindEnd(char* first, char* last);
static int InitByteCode(Tcl_Interp* interp, ExtractionEnv* envPtr);
static int InitCompatibilityLayer(Tcl_Interp* interp);
static void InitExtractEnv(char* codeBase, char* codeEnd, ExtractionEnv* envPtr);
static void InitTypes();

/*
 *----------------------------------------------------------------------
 *
 * Tbcload_EvalObjCmd --
 *
 *  Eval the Tcl_Obj given as its first argument. If this Tcl_Obj contains
 *  the string representation of a compiled script, it builds a ByteCode
 *  from it, and executes it.
 *
 * Results:
 *  Returns a standard TCL result code.
 *
 * Side effects:
 *  None.
 *
 *----------------------------------------------------------------------
 */

int Tbcload_EvalObjCmd(void* dummy, Tcl_Interp* interp, Tcl_Size objc, Tcl_Obj* const objv[])
{
    ImageSignature sig;
    Tcl_Obj* cmdObjPtr;
    char* scriptPtr;
    Tcl_Size scriptLength;
    int result;

    if (objc < 2)
    {
        Tcl_WrongNumArgs(interp, 1, objv, "bytestream");
        return TCL_ERROR;
    }

    /*
     * Check the signature. If OK, then extract the whole file.
     * If the script is empty, return TCL_OK here.
     */
    scriptPtr = Tcl_GetStringFromObj(objv[1], &scriptLength);

    if ((scriptLength < 1) || (scriptPtr == NULL))
    {
        return TCL_OK;
    }

    if ((ExtractSignature(interp, (char*)scriptPtr, (char*)(scriptPtr + scriptLength), &sig) == NULL) ||
        (CheckSignature(interp, &sig) != TCL_OK))
    {
        return TCL_ERROR;
    }

    cmdObjPtr = ExtractCompiledFile(interp, scriptPtr, scriptLength);
    if (cmdObjPtr == NULL)
    {
        return TCL_ERROR;
    }
    result = Tcl_EvalObjEx(interp, cmdObjPtr, 0);
    Tcl_DecrRefCount(cmdObjPtr);

#if USE_CATCH_WRAPPER
    if (result == TCL_ERROR)
    {
        Tcl_AppendObjToErrorInfo(interp, Tcl_NewStringObj("\n", -1));
        Tcl_AppendObjToErrorInfo(interp, Tcl_NewStringObj(errorInfoMarker, -1));
    }
#endif

    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * Tbcload_ProcObjCmd --
 *
 *  Wrapper procedure, dispatches the call to the appropriate implementation.
 *
 * Results:
 *  A standard Tcl object result value.
 *
 * Side effects:
 *  A new procedure gets created.
 *
 *----------------------------------------------------------------------
 */

int Tbcload_ProcObjCmd(void* dummy, Tcl_Interp* interp, Tcl_Size objc, Tcl_Obj* const objv[])
{
    return (*bcprocCmdProc)(dummy, interp, objc, objv);
}

/*
 *----------------------------------------------------------------------
 *
 * ExtractByteCode --
 *
 *  Extracts the contents of a ByteCode structure from a compiled image.
 *
 * Results:
 *  Returns TCL_OK on success, TCL_ERROR on failure.
 *
 * Side effects:
 *  Populates the ByteCode with the value extracted from the file.
 *  Updates the contents of the ExtractionEnv structure: stores the new
 *  address of the next available byte.
 *  May also modify the interpreter's result in case of failure.
 *
 *----------------------------------------------------------------------
 */

static int ExtractByteCode(Tcl_Interp* interp, ExtractionEnv* envPtr)
{
    ByteCode* codePtr;
    LocMapSizes* locMapPtr = &envPtr->locMap;

    if (InitByteCode(interp, envPtr) != TCL_OK)
    {
        goto error;
    }

    codePtr = envPtr->codePtr;

    if ((ExtractByteSequence(interp, codePtr->numCodeBytes, envPtr, codePtr->codeStart, codePtr->numCodeBytes) != TCL_OK) ||
        (ExtractByteSequence(interp, locMapPtr->codeDeltaSize, envPtr, codePtr->codeDeltaStart, locMapPtr->codeDeltaSize) !=
         TCL_OK) ||
        (ExtractByteSequence(interp, locMapPtr->codeLengthSize, envPtr, codePtr->codeLengthStart, locMapPtr->codeLengthSize) !=
         TCL_OK))
    {
        goto error;
    }

    if ((locMapPtr->srcDeltaSize >= 0) &&
        (ExtractByteSequence(interp, locMapPtr->srcDeltaSize, envPtr, codePtr->srcDeltaStart, locMapPtr->srcDeltaSize) != TCL_OK))
    {
        goto error;
    }

    if ((locMapPtr->srcLengthSize >= 0) &&
        (ExtractByteSequence(interp, locMapPtr->srcLengthSize, envPtr, codePtr->srcLengthStart, locMapPtr->srcLengthSize) !=
         TCL_OK))
    {
        goto error;
    }

    if ((ExtractObjArray(interp, codePtr->numLitObjects, envPtr, codePtr->objArrayPtr, codePtr->numLitObjects) != TCL_OK) ||
        (ExtractExcRangeArray(interp, codePtr->numExceptRanges, envPtr, codePtr->exceptArrayPtr, codePtr->numExceptRanges) !=
         TCL_OK) ||
        (ExtractAuxDataArray(interp, codePtr->numAuxDataItems, envPtr, codePtr->auxDataArrayPtr, codePtr->numAuxDataItems) !=
         TCL_OK))
    {
        goto error;
    }

    /*
     * If the source map arrays were not included in the .tbc file (which is
     * typically the case), generate them here; each command will start at 0
     * and have length noSourceCodeSize
     */

    if (locMapPtr->srcDeltaSize < 0)
    {
        memset(codePtr->srcDeltaStart, 0, (size_t)codePtr->numCommands);
    }

    if (locMapPtr->srcLengthSize < 0)
    {
        memset(codePtr->srcLengthStart, (char)noSourceCodeSize, (size_t)codePtr->numCommands);
    }

    return TCL_OK;

error:
    CleanupByteCode(envPtr);
    return TCL_ERROR;
}

/*
 *----------------------------------------------------------------------
 *
 * InitByteCode --
 *
 *  Allocates and initializes a ByteCode structure using the values stored
 *  in a compiled image.
 *
 * Results:
 *  Returns TCL_OK on success, TCL_ERROR on failure.
 *
 * Side effects:
 *  Populates parts of the ByteCode with the value extracted from the file;
 *  also sets it up so that CleanupByteCode can clean it up even if the
 *  structure was only partially built.
 *  Updates the contents of the ExtractionEnv structure: stores the new
 *  address of the next available byte and the pointer to the newly allocated
 *  ByteCode struct.
 *  May also modify the interpreter's result in case of failure.
 *
 *----------------------------------------------------------------------
 */

static int InitByteCode(Tcl_Interp* interp, ExtractionEnv* envPtr)
{
    Interp* iPtr = (Interp*)interp;
    ByteCode* byteCodePtr;
    unsigned char* p;
    Tcl_Size size;
    Tcl_Size numCommands, numSrcBytes, numCodeBytes, numLitObjects, numExceptRanges;
    Tcl_Size numAuxDataItems, numCmdLocBytes, maxExceptDepth, maxStackDepth;
    Tcl_Size objArrayBytes, exceptArrayBytes, auxDataArrayBytes;
    LocMapSizes* locMapPtr = &envPtr->locMap;
    Namespace* namespacePtr;

    CleanupByteCode(envPtr);

    /*
     * Determine the size of the ByteCode struct, malloc it, then initialize
     * the components that were not saved with the file image because it
     * doesn't make sense to do so: pointer to the interpreter, epoch, etc...
     * To determine the size of the ByteCode struct, we read in the .tbc
     * control line, which has enough enformation to calculate the required
     * size.
     */

    if ((ExtractTclSize(interp, envPtr, &numCommands) != TCL_OK) || (ExtractTclSize(interp, envPtr, &numSrcBytes) != TCL_OK) ||
        (ExtractTclSize(interp, envPtr, &numCodeBytes) != TCL_OK) || (ExtractTclSize(interp, envPtr, &numLitObjects) != TCL_OK) ||
        (ExtractTclSize(interp, envPtr, &numExceptRanges) != TCL_OK) ||
        (ExtractTclSize(interp, envPtr, &numAuxDataItems) != TCL_OK) ||
        (ExtractTclSize(interp, envPtr, &numCmdLocBytes) != TCL_OK) ||
        (ExtractTclSize(interp, envPtr, &maxExceptDepth) != TCL_OK) ||
        (ExtractTclSize(interp, envPtr, &maxStackDepth) != TCL_OK) ||
        (ExtractTclSize(interp, envPtr, &(locMapPtr->codeDeltaSize)) != TCL_OK) ||
        (ExtractTclSize(interp, envPtr, &(locMapPtr->codeLengthSize)) != TCL_OK) ||
        (ExtractTclSize(interp, envPtr, &(locMapPtr->srcDeltaSize)) != TCL_OK) ||
        (ExtractTclSize(interp, envPtr, &(locMapPtr->srcLengthSize)) != TCL_OK))
    {
        return TCL_ERROR;
    }

    objArrayBytes = (numLitObjects * sizeof(Tcl_Obj*));
    exceptArrayBytes = (numExceptRanges * sizeof(ExceptionRange));
    auxDataArrayBytes = (numAuxDataItems * sizeof(AuxData));

    /*
     * generate the numCmdLocBytes from the values of the location and
     * source arrays, which could be different here from the original,
     * because the .tbc file was written without sources
     */

    numCmdLocBytes = locMapPtr->codeDeltaSize + locMapPtr->codeLengthSize;

    if (locMapPtr->srcDeltaSize < 0)
    {
        /*
         * The source arrays have as many entries as the number of commands,
         * because both start (0 for all) and delta (noSourceCodeSize for all)
         * values fit in one byte
         */
        numCmdLocBytes += numCommands;
    }
    else
    {
        numCmdLocBytes += locMapPtr->srcDeltaSize;
    }
    if (locMapPtr->srcLengthSize < 0)
    {
        numCmdLocBytes += numCommands;
    }
    else
    {
        numCmdLocBytes += locMapPtr->srcLengthSize;
    }

    size = sizeof(ByteCode);
    size += TCL_ALIGN(numCodeBytes);     /* align object array */
    size += TCL_ALIGN(objArrayBytes);    /* align exception range array */
    size += TCL_ALIGN(exceptArrayBytes); /* align AuxData array */
    size += auxDataArrayBytes;
    size += numCmdLocBytes;

    /*
     * initialize the reference count on the ByteCode to 1 because we have a
     * reference to it in the extraction environment struct.
     */

    if (iPtr->varFramePtr != NULL)
    {
        namespacePtr = iPtr->varFramePtr->nsPtr;
    }
    else
    {
        namespacePtr = iPtr->globalNsPtr;
    }

    p = (unsigned char*)Tcl_Alloc((size_t)size);
    byteCodePtr = (ByteCode*)p;
    memset(byteCodePtr, 0, (size_t)size);
    byteCodePtr->interpHandle = TclHandlePreserve(iPtr->handle);
    byteCodePtr->compileEpoch = iPtr->compileEpoch;
    byteCodePtr->nsPtr = namespacePtr;
    byteCodePtr->nsEpoch = namespacePtr->resolverEpoch;
    byteCodePtr->refCount = 1;
    byteCodePtr->flags = TCL_BYTECODE_PRECOMPILED;
    byteCodePtr->procPtr = NULL;

    envPtr->codeSize = size;
    envPtr->codePtr = byteCodePtr;

    byteCodePtr->structureSize = 0;
    byteCodePtr->numCommands = numCommands;
    byteCodePtr->numSrcBytes = numSrcBytes;
    byteCodePtr->numCodeBytes = numCodeBytes;
    byteCodePtr->numLitObjects = numLitObjects;
    byteCodePtr->numExceptRanges = numExceptRanges;
    byteCodePtr->numAuxDataItems = numAuxDataItems;
    byteCodePtr->numCmdLocBytes = numCmdLocBytes;
    byteCodePtr->maxExceptDepth = maxExceptDepth;
    byteCodePtr->maxStackDepth = maxStackDepth;
    byteCodePtr->source = noSourceCode;
    byteCodePtr->numSrcBytes = noSourceCodeSize;

    /*
     * The assignements to p must be kept consistent with the ones in
     * TclInitByteCodeObj, so that the arrays are aligned as expected.
     */

    p += sizeof(ByteCode);
    byteCodePtr->codeStart = p;
    memset(p, 0, (size_t)numCodeBytes);

    p += TCL_ALIGN(numCodeBytes);
    if (numLitObjects > 0)
    {
        byteCodePtr->objArrayPtr = (Tcl_Obj**)p;
        memset(p, 0, (size_t)objArrayBytes);
    }
    else
    {
        byteCodePtr->objArrayPtr = (Tcl_Obj**)0;
    }

    p += TCL_ALIGN(objArrayBytes);
    if (numExceptRanges > 0)
    {
        byteCodePtr->exceptArrayPtr = (ExceptionRange*)p;
        memset(p, 0, (size_t)exceptArrayBytes);
    }
    else
    {
        byteCodePtr->exceptArrayPtr = (ExceptionRange*)0;
    }

    p += TCL_ALIGN(exceptArrayBytes);
    if (numAuxDataItems > 0)
    {
        byteCodePtr->auxDataArrayPtr = (AuxData*)p;
        memset(p, 0, (size_t)auxDataArrayBytes);
    }
    else
    {
        byteCodePtr->auxDataArrayPtr = (AuxData*)0;
    }

    p += auxDataArrayBytes;
    byteCodePtr->codeDeltaStart = p;
    p += locMapPtr->codeDeltaSize;
    byteCodePtr->codeLengthStart = p;
    p += locMapPtr->codeLengthSize;
    byteCodePtr->srcDeltaStart = p;
    if (locMapPtr->srcDeltaSize < 0)
    {
        p += byteCodePtr->numCommands;
    }
    else
    {
        p += locMapPtr->srcDeltaSize;
    }
    byteCodePtr->srcLengthStart = p;

    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * CleanupByteCode --
 *
 *  Cleans up and frees the ByteCode structure in the ExtractionEnv.
 *  Generally this will only be called in the case when an error
 *  occurs extracting the ByteCode, while the ByteCodes will generally
 *  get cleaned up by tclCompile.c:TclCleanupByteCode, which has a
 *  special case for precompiled flagged ByteCode structures.
 *
 * Results:
 *  None.
 *
 * Side effects:
 *  Frees a number of arrays and TCL objects that may have been allocated
 *  while the ByteCode was assembled from the compiled image..
 *
 *----------------------------------------------------------------------
 */

static void CleanupByteCode(ExtractionEnv* envPtr)
{
    ByteCode* byteCodePtr = envPtr->codePtr;

    if (byteCodePtr)
    {
        byteCodePtr->refCount--;
        if (byteCodePtr->refCount < 1)
        {
            if (byteCodePtr->numLitObjects > 0)
            {
                Tcl_Obj** objArrayPtr = byteCodePtr->objArrayPtr;
                Tcl_Obj* objPtr;
                Tcl_Size i;

                for (i = 0; i < byteCodePtr->numLitObjects; i++)
                {
                    objPtr = *objArrayPtr;
                    objArrayPtr += 1;
                    if (objPtr)
                    {
                        Tcl_DecrRefCount(objPtr);
                    }
                }
            }
            Tcl_Free((char*)byteCodePtr);
        }
        envPtr->codePtr = NULL;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * ExtractTclSize --
 *
 *  Extracts a Tcl_Size value from a compiled image. Skips whitespace, then gets
 *  the value from the next word. If successful, places the value in the location
 *  pointed to by valuePtr.
 *
 * Results:
 *  Returns TCL_OK on success, TCL_ERROR on failure.
 *
 * Side effects:
 *  Loads the Tcl_Size at valuePtr with the value extracted from the file.
 *  Updates the contents of the ExtractionEnv structure: stores the new
 *  address of the next available byte.
 *  May also modify the interpreter's result in case of failure.
 *
 *----------------------------------------------------------------------
 */

static int ExtractTclSize(Tcl_Interp* interp, ExtractionEnv* envPtr, Tcl_Size* valuePtr)
{
    const char* codePtr = envPtr->curImagePtr;
    const char* codeEnd = envPtr->imageEnd;

    /* Skip leading whitespace */
    while (codePtr < codeEnd && isspace(UCHAR(*codePtr)))
        codePtr++;
    if (codePtr >= codeEnd)
    {
        Tcl_SetObjResult(interp, Tcl_NewStringObj(prematureEnd, -1));
        return TCL_ERROR;
    }

    /* Find token end without mutating the buffer */
    const char* endPtr = FindEnd((char*)codePtr, (char*)codeEnd);

    /* Slice token into a Tcl_Obj and parse as Tcl_Size */
    Tcl_Obj* tok = Tcl_NewStringObj(codePtr, (Tcl_Size)(endPtr - codePtr));
    if (Tcl_GetSizeIntFromObj(interp, tok, valuePtr) != TCL_OK)
    {
        AppendErrorLocation(interp, envPtr);
        return TCL_ERROR;
    }

    /* Advance scanner */
    envPtr->curImagePtr = (char*)endPtr;
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ExtractByteSequence --
 *
 *  Extracts a byte sequence from the compiled image, place it into the array
 *  at seqPtr. The length argument can be specified as -1, in which case the
 *  procedure extracts the length from the byte sequence header, or as a
 *  number that specifies the expected length of the sequence. In the latter
 *  case, the length obtained from the byte sequence header is compared to
 *  the value passed in, and an error is returned if the two don't match.
 *  The seqSize argument specifies the size of the seqPtr array.
 *  The byte sequence is assumed to have been encoded with a modified
 *  ASCII85 filter, as described in cmpWrite.c.
 *
 * Results:
 *  Returns TCL_OK on success, TCL_ERROR on failure.
 *
 * Side effects:
 *  Loads the byte sequence into the array starting at seqPtr.
 *  Updates the contents of the ExtractionEnv structure: stores the new
 *  address of the next available byte.
 *  May also modify the interpreter's result in case of failure.
 *
 *----------------------------------------------------------------------
 */

static int
ExtractByteSequence(Tcl_Interp* interp, Tcl_Size length, ExtractionEnv* envPtr, unsigned char* seqPtr, Tcl_Size seqSize)
{
    char* imagePtr;
    char* imageEnd;
    Tcl_Size hLen;
    int code;
    A85DecodeContext decodeCtx;
    A85DecodeContext* ctxPtr = &decodeCtx;

    /*
     * read the length from the header; we need to do this even in cases where
     * the length is passed in.
     * Then do the length checks if necessary.
     */

    if (ExtractTclSize(interp, envPtr, &hLen) != TCL_OK)
    {
        return TCL_ERROR;
    }

    if (length < 0)
    {
        length = hLen;
    }
    else if (length != hLen)
    {
        {
            Tcl_Obj* res = Tcl_GetObjResult(interp);
            Tcl_AppendToObj(res, "inconsistent byte sequence length", -1);
        }
        AppendErrorLocation(interp, envPtr);
        return TCL_ERROR;
    }

    if (length > seqSize)
    {
        {
            Tcl_Obj* res = Tcl_GetObjResult(interp);
            Tcl_AppendToObj(res, "byte sequence too big for storage", -1);
        }
        AppendErrorLocation(interp, envPtr);
        return TCL_ERROR;
    }

    imagePtr = envPtr->curImagePtr;
    imageEnd = envPtr->imageEnd;
    A85InitDecodeContext(length, seqPtr, ctxPtr);

    while (decodeCtx.bytesToDecode > 0)
    {
        if (imagePtr == imageEnd)
        {
            envPtr->curImagePtr = imagePtr;
            {
                Tcl_Obj* res = Tcl_GetObjResult(interp);
                Tcl_AppendToObj(res, prematureEnd, -1);
            }
            return TCL_ERROR;
        }

        code = decodeMap[(int)*imagePtr];
        imagePtr += 1;

        if (code == A85_ILLEGAL_CHAR)
        {
            envPtr->curImagePtr = imagePtr - 1;
            {
                Tcl_Obj* res = Tcl_GetObjResult(interp);
                Tcl_AppendToObj(res, "malformed byte sequence", -1);
            }
            AppendErrorLocation(interp, envPtr);
            return TCL_ERROR;
        }
        else if (code != A85_WHITESPACE)
        {
            if (A85DecodeByte(interp, code, ctxPtr) != TCL_OK)
            {
                envPtr->curImagePtr = imagePtr - 1;
                AppendErrorLocation(interp, envPtr);
                return TCL_ERROR;
            }
        }
    }

    envPtr->curImagePtr = imagePtr;

    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ExtractString --
 *
 *  Extracts a string from the compiled image, place it into the array
 *  at strPtr. The length argument must be specified, because strings are
 *  written out without count fields.
 *
 * Results:
 *  Returns TCL_OK on success, TCL_ERROR on failure.
 *
 * Side effects:
 *  Loads the string into the array starting at strPtr.
 *  Updates the contents of the ExtractionEnv structure: stores the new
 *  address of the next available byte.
 *  May also modify the interpreter's result in case of failure.
 *
 *----------------------------------------------------------------------
 */

static int ExtractString(Tcl_Interp* interp, Tcl_Size length, ExtractionEnv* envPtr, char* strPtr, Tcl_Size strSize)
{
    char* imagePtr;

    imagePtr = envPtr->curImagePtr;

    if ((imagePtr + strSize) > envPtr->imageEnd)
    {
        {
            Tcl_Obj* res = Tcl_GetObjResult(interp);
            Tcl_AppendToObj(res, prematureEnd, -1);
        }
        return TCL_ERROR;
    }

    memcpy(strPtr, imagePtr, (size_t)strSize);

    envPtr->curImagePtr += strSize;

    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * AllocAndExtractByteSequence --
 *
 *  Get the size of the byte sequence from the compiled image, alloc a buffer
 *  large enough for it, then extract it. Places the allocated pointer in
 *  *seqPtrPtr, and the size of the byte sequence in *seqSizePtr.
 *  If 'addNull' is true, then the procedure adds a terminating NULL char
 *  to the sequence; this terminating char is not counted in the size value
 *  that is returned.
 *
 * Results:
 *  Returns TCL_OK on success, TCL_ERROR on failure.
 *
 * Side effects:
 *  Loads the byte sequence into the array starting at *seqPtr.
 *  Updates the contents of the ExtractionEnv structure: stores the new
 *  address of the next available byte.
 *  May also modify the interpreter's result in case of failure.
 *
 *----------------------------------------------------------------------
 */

static int AllocAndExtractByteSequence(
    Tcl_Interp* interp, ExtractionEnv* envPtr, int addNull, unsigned char** seqPtrPtr, Tcl_Size* seqSizePtr)
{
    char* curImagePtr;
    Tcl_Size hLen, allocLen;
    unsigned char* seqBuf;

    /*
     * read the length from the header; we need to do this so that we can
     * allocate the buffer. Then, let's move the extraction environment back
     * to where it was at the start of the call, so that we can call
     * ExtractByteSequence.
     */

    curImagePtr = envPtr->curImagePtr;
    if (ExtractTclSize(interp, envPtr, &hLen) != TCL_OK)
    {
        return TCL_ERROR;
    }
    envPtr->curImagePtr = curImagePtr;

    allocLen = (addNull) ? hLen + 1 : hLen;
    seqBuf = (unsigned char*)Tcl_Alloc((size_t)allocLen);
    if (ExtractByteSequence(interp, hLen, envPtr, seqBuf, hLen) != TCL_OK)
    {
        Tcl_Free((char*)seqBuf);
        return TCL_ERROR;
    }

    *seqPtrPtr = seqBuf;
    *seqSizePtr = hLen;

    if (addNull)
    {
        seqBuf[hLen] = 0;
    }

    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * AllocAndExtractString --
 *
 *  Get the size of the string from the compiled image, alloc a buffer
 *  large enough for it, then extract it. Places the allocated pointer in
 *  *strPtrPtr, and the size of the string in *strSizePtr.
 *  If 'addNull' is true, then the procedure adds a terminating NULL char
 *  to the sequence; this terminating char is not counted in the size value
 *  that is returned.
 *
 * Results:
 *  Returns TCL_OK on success, TCL_ERROR on failure.
 *
 * Side effects:
 *  Loads the string into the array starting at *seqPtrPtr.
 *  Updates the contents of the ExtractionEnv structure: stores the new
 *  address of the next available byte.
 *  May also modify the interpreter's result in case of failure.
 *
 *----------------------------------------------------------------------
 */

static int AllocAndExtractString(Tcl_Interp* interp, ExtractionEnv* envPtr, int addNull, char** strPtrPtr, Tcl_Size* strSizePtr)
{
    Tcl_Size hLen, allocLen;
    char* strBuf;
    char* imagePtr;
    char* imageEnd;

    /*
     * read the length from the header; we don't need to move the extraction
     * environment back, because ExtractString does not expect a count field.
     * But make sure we skip to just past end-of-line
     */

    if (ExtractTclSize(interp, envPtr, &hLen) != TCL_OK)
    {
        return TCL_ERROR;
    }

    /*
     * skip to EOL, but not beyond, because that belongs to the string
     */

    imagePtr = envPtr->curImagePtr;
    imageEnd = envPtr->imageEnd;
    for (;;)
    {
        if (imagePtr == imageEnd)
        {
            {
                Tcl_Obj* res = Tcl_GetObjResult(interp);
                Tcl_AppendToObj(res, prematureEnd, -1);
            }
            return TCL_ERROR;
        }
        if (*imagePtr == '\n')
        {
            break;
        }
        imagePtr += 1;
    }

    envPtr->curImagePtr = imagePtr + 1;

    allocLen = (addNull) ? hLen + 1 : hLen;
    strBuf = (char*)Tcl_Alloc((size_t)allocLen);
    if (ExtractString(interp, hLen, envPtr, strBuf, hLen) != TCL_OK)
    {
        Tcl_Free(strBuf);
        return TCL_ERROR;
    }

    *strPtrPtr = strBuf;
    *strSizePtr = hLen;

    if (addNull)
    {
        strBuf[hLen] = '\0';
    }

    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ExtractObjArray --
 *
 *  Extracts a number of objects from the compiled image, instantiate them,
 *  and place pointers to them in the array at objArray. The numLitObjects
 *  argument can be specified as -1, in which case the procedure extracts
 *  the number of objects from the array header, or as a number that specifies
 *  the expected number of objects in the array. In the latter case, the value
 *  obtained from the array header is compared to the value passed in, and an
 *  error is returned if the two don't match.
 *  The objArraySize argument specifies the size of the objArray array.
 *
 * Results:
 *  Returns TCL_OK on success, TCL_ERROR on failure.
 *
 * Side effects:
 *  Populates the object array with pointers to objects extracted from the
 *  file.
 *  Creates the objects listed in the compiled image.
 *  Updates the contents of the ExtractionEnv structure: stores the new
 *  address of the next available byte.
 *  May also modify the interpreter's result in case of failure.
 *
 *----------------------------------------------------------------------
 */

static int
ExtractObjArray(Tcl_Interp* interp, Tcl_Size numLitObjects, ExtractionEnv* envPtr, Tcl_Obj** objArray, Tcl_Size objArraySize)
{
    Tcl_Size hnumLitObjects;
    Tcl_Size iObj;
    Tcl_Obj* objPtr;

    /*
     * read the number of objects from the header; we need to do this even
     * in cases where the number is passed in.
     * Then do the checks if necessary.
     */
    if (ExtractTclSize(interp, envPtr, &hnumLitObjects) != TCL_OK)
    {
        return TCL_ERROR;
    }

    if (numLitObjects < 0)
    {
        numLitObjects = hnumLitObjects;
    }
    else if (numLitObjects != hnumLitObjects)
    {
        {
            Tcl_Obj* res = Tcl_GetObjResult(interp);
            Tcl_AppendToObj(res, "inconsistent object array size", -1);
        }
        return TCL_ERROR;
    }

    if (numLitObjects > objArraySize)
    {
        {
            Tcl_Obj* res = Tcl_GetObjResult(interp);
            Tcl_AppendToObj(res, "object array too big for storage", -1);
        }
        return TCL_ERROR;
    }

    for (iObj = 0; iObj < numLitObjects; iObj++)
    {
        objPtr = ExtractObject(interp, envPtr);
        if (!objPtr)
        {
            return TCL_ERROR;
        }
        objArray[iObj] = objPtr;
    }

    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ExtractObject --
 *
 *  Extracts an object from the compiled image, instantiate it, and returns it.
 *  Note that the object's reference count is initialized to 1.
 *
 * Results:
 *  Returns a Tcl_Obj pointer, NULL on error.
 *
 * Side effects:
 *  Creates the objects listed in the compiled image.
 *  Updates the contents of the ExtractionEnv structure: stores the new
 *  address of the next available byte.
 *  May also modify the interpreter's result in case of failure.
 *
 *----------------------------------------------------------------------
 */

static Tcl_Obj* ExtractObject(Tcl_Interp* interp, ExtractionEnv* envPtr)
{
    Tcl_Obj* objPtr = NULL;
    char* imagePtr;
    char* imageEnd;
    char* objString;
    char typeCode;
    Tcl_Size objStringLength;
    const Tcl_ObjType* objTypePtr = NULL;

    imagePtr = envPtr->curImagePtr;
    imageEnd = envPtr->imageEnd;

    /*
     * skip whitespace, get the typecode
     */

    for (;;)
    {
        if (imagePtr == imageEnd)
        {
            {
                Tcl_Obj* res = Tcl_GetObjResult(interp);
                Tcl_AppendToObj(res, prematureEnd, -1);
            }
            return NULL;
        }
        if (!isspace(UCHAR(*imagePtr)))
        {
            break;
        }
        imagePtr += 1;
    }

    typeCode = *imagePtr;
    imagePtr += 1;
    envPtr->curImagePtr = imagePtr;

    /*
     * process by object type.
     */

    if (typeCode == CMP_STRING_CODE)
    {
        if (AllocAndExtractString(interp, envPtr, 1, &objString, &objStringLength) != TCL_OK)
        {
            return NULL;
        }

        objPtr = Tcl_NewStringObj(objString, objStringLength);
        Tcl_Free(objString);
        Tcl_IncrRefCount(objPtr);
    }
    else if (typeCode == CMP_XSTRING_CODE)
    {
        if (AllocAndExtractByteSequence(interp, envPtr, 1, (unsigned char**)&objString, &objStringLength) != TCL_OK)
        {
            return NULL;
        }
        objPtr = Tcl_NewStringObj(objString, objStringLength);
        Tcl_Free(objString);
        Tcl_IncrRefCount(objPtr);
    }
    else
    {
        /*
         * skip whitespace
         */

        for (;;)
        {
            if (imagePtr == imageEnd)
            {
                {
                    Tcl_Obj* res = Tcl_GetObjResult(interp);
                    Tcl_AppendToObj(res, prematureEnd, -1);
                }
                return NULL;
            }
            if (!isspace(UCHAR(*imagePtr)))
            {
                break;
            }
            imagePtr += 1;
        }

        envPtr->curImagePtr = imagePtr;

        if (typeCode == CMP_INT_CODE)
        {
            objPtr = CreateSimpleObject(envPtr);
            objTypePtr = cmpIntType;
        }
        else if (typeCode == CMP_DOUBLE_CODE)
        {
            objPtr = CreateSimpleObject(envPtr);
            objTypePtr = cmpDoubleType;
        }
        else if (typeCode == CMP_BOOLEAN_CODE)
        {
            objPtr = CreateSimpleObject(envPtr);
            objTypePtr = NULL;
        }
        else if (typeCode == CMP_BYTECODE_CODE)
        {
            /*
             * This block is a copy of most of the code in
             * ExtractCompiledFile; unfortunately, we can't share the two
             * because here I need the final values of the local extraction
             * environment so that I can update the outer one.
             */

            ExtractionEnv localExEnv;

            InitExtractEnv(imagePtr, imageEnd, &localExEnv);
            localExEnv.sig = envPtr->sig;

            if (ExtractByteCode(interp, &localExEnv) != TCL_OK)
            {
                localExEnv.codePtr = NULL;
                CleanupExtractEnv(&localExEnv);
                return NULL;
            }

            /*
             * create the new object.
             * We give its string representation a dummy value, so that
             * commands like "info body" don't cause a panic. The reason
             * "info body" does is this: "info body" simply looks up the
             * body object and returns on the stack. Later, when we access
             * the object as a string, Tcl_GetStringFromObj calls the
             * type's updateStringProc, which for bytecodes panics.
             * So put a dummy to avoid the call.
             * The side effects of this need to be investigated further.
             */

            objPtr = Tcl_NewStringObj(noSourceCode, -1);
            Tcl_IncrRefCount(objPtr);
            Tcl_ObjInternalRep ir;
            ir.twoPtrValue.ptr1 = (void*)localExEnv.codePtr;
            ir.twoPtrValue.ptr2 = NULL;
            Tcl_StoreInternalRep(objPtr, cmpByteCodeType, &ir);
            localExEnv.codePtr->refCount++;

            /*
             * skip over the ByteCode representation we just read in
             */

            envPtr->curImagePtr = localExEnv.curImagePtr;

            localExEnv.codePtr = NULL;
            CleanupExtractEnv(&localExEnv);
        }
        else if (typeCode == CMP_PROCBODY_CODE)
        {
            /*
             * A ProcBodyType Tcl_Obj contains a ByteCode dump and a
             * number of fields in a Proc struct.
             *
             * First, extract the ByteCode.
             */

            ExtractionEnv localExEnv;
            InitExtractEnv(imagePtr, imageEnd, &localExEnv);
            localExEnv.sig = envPtr->sig;

            if (ExtractByteCode(interp, &localExEnv) != TCL_OK)
            {
                localExEnv.codePtr = NULL;
                CleanupExtractEnv(&localExEnv);
                return NULL;
            }

            /*
             * skip over the ByteCode representation we just read in, then
             * finish reading in the object.
             */

            envPtr->curImagePtr = localExEnv.curImagePtr;

            objPtr = ExtractProcBody(interp, localExEnv.codePtr, envPtr);
            if (objPtr)
            {
                Tcl_IncrRefCount(objPtr);
            }

            localExEnv.codePtr = NULL;
            CleanupExtractEnv(&localExEnv);
        }
        else
        {
            char errBuf[2];
            errBuf[0] = typeCode;
            errBuf[1] = '\0';
            {
                Tcl_Obj* res = Tcl_GetObjResult(interp);
                Tcl_AppendToObj(res, "unknown object type \"", -1);
                Tcl_AppendToObj(res, errBuf, -1);
                Tcl_AppendToObj(res, "\"", -1);
            }
            AppendErrorLocation(interp, envPtr);
            return NULL;
        }

        if (objTypePtr && (Tcl_ConvertToType(interp, objPtr, (Tcl_ObjType*)objTypePtr) != TCL_OK))
        {
            Tcl_DecrRefCount(objPtr);
            return NULL;
        }
    }

    return objPtr;
}

/*
 *----------------------------------------------------------------------
 *
 * CreateSimpleObject --
 *
 *  Creates a Tcl object whose string representation is the Tcl word at the
 *  current extraction pointer.
 *  Note that the procedure increases the reference count on the object.
 *
 * Results:
 *  Returns a newly created Tcl object.
 *
 * Side effects:
 *  Updates the contents of the extraction environment.
 *
 *----------------------------------------------------------------------
 */

static Tcl_Obj* CreateSimpleObject(ExtractionEnv* envPtr)
{
    char* endPtr;
    char* imagePtr = envPtr->curImagePtr;
    Tcl_Obj* objPtr;

    endPtr = FindEnd(imagePtr, envPtr->imageEnd);

    objPtr = Tcl_NewStringObj(imagePtr, endPtr - imagePtr);
    Tcl_IncrRefCount(objPtr);

    envPtr->curImagePtr = endPtr;

    return objPtr;
}

/*
 *----------------------------------------------------------------------
 *
 * ExtractExcRangeArray --
 *
 *  Extracts the exception range array for a ByteCode struct from a compiled
 *  image. The numExceptRanges argument can be specified as -1, in which case the
 *  procedure extracts the number of exception ranges from the array header,
 *  or as a number that specifies the expected number of exception ranges in
 *  the array. In the latter case, the value obtained from the array header is
 *  compared to the value passed in, and an error is returned if the two don't
 *  match.
 *  The excArraySize argument specifies the size of the excArray array.
 *
 * Results:
 *  Returns TCL_OK on success, TCL_ERROR on failure.
 *
 * Side effects:
 *  Populates the exception ranges array with values extracted from the image.
 *  Updates the contents of the ExtractionEnv structure: stores the new
 *  address of the next available byte.
 *  May also modify the interpreter's result in case of failure.
 *
 *----------------------------------------------------------------------
 */

static int ExtractExcRangeArray(
    Tcl_Interp* interp, Tcl_Size numExceptRanges, ExtractionEnv* envPtr, ExceptionRange* excArray, Tcl_Size excArraySize)
{
    char* imagePtr;
    char* imageEnd;
    Tcl_Size hnumExceptRanges, iRange;
    char codeType;
    ExceptionRange* excPtr = excArray;

    /*
     * read the number of exception ranges from the header; we need to do
     * this even in cases where the number is passed in.
     * Then do the checks if necessary.
     */

    if (ExtractTclSize(interp, envPtr, &hnumExceptRanges) != TCL_OK)
    {
        return TCL_ERROR;
    }

    if (numExceptRanges < 0)
    {
        numExceptRanges = hnumExceptRanges;
    }
    else if (numExceptRanges != hnumExceptRanges)
    {
        {
            Tcl_Obj* res = Tcl_GetObjResult(interp);
            Tcl_AppendToObj(res, "inconsistent exception ranges array size", -1);
        }
        AppendErrorLocation(interp, envPtr);
        return TCL_ERROR;
    }

    if (numExceptRanges > excArraySize)
    {
        {
            Tcl_Obj* res = Tcl_GetObjResult(interp);
            Tcl_AppendToObj(res, "exception ranges array too big for storage", -1);
        }
        AppendErrorLocation(interp, envPtr);
        return TCL_ERROR;
    }

    imagePtr = envPtr->curImagePtr;
    imageEnd = envPtr->imageEnd;

    for (iRange = 0; iRange < numExceptRanges; iRange++)
    {
        /*
         * skip whitespace.
         */

        for (;;)
        {
            if (imagePtr == imageEnd)
            {
                {
                    Tcl_Obj* res = Tcl_GetObjResult(interp);
                    Tcl_AppendToObj(res, prematureEnd, -1);
                }
                return TCL_ERROR;
            }
            if (!isspace(UCHAR(*imagePtr)))
            {
                break;
            }
            imagePtr += 1;
        }

        codeType = *imagePtr;
        imagePtr += 1;
        envPtr->curImagePtr = imagePtr;

        /*
         * look up the type, then read in all the Tcl_Size values.
         */

        if (ExcRangeFromName((Tcl_Size)codeType, &excPtr->type) < 0)
        {
            char errBuf[2];
            errBuf[0] = codeType;
            errBuf[1] = '\0';
            {
                Tcl_Obj* res = Tcl_GetObjResult(interp);
                Tcl_AppendToObj(res, "unknown exception range type: ", -1);
                Tcl_AppendToObj(res, errBuf, -1);
            }
            AppendErrorLocation(interp, envPtr);
            return TCL_ERROR;
        }

        if ((ExtractTclSize(interp, envPtr, &excPtr->nestingLevel) != TCL_OK) ||
            (ExtractTclSize(interp, envPtr, &excPtr->codeOffset) != TCL_OK) ||
            (ExtractTclSize(interp, envPtr, &excPtr->numCodeBytes) != TCL_OK) ||
            (ExtractTclSize(interp, envPtr, &excPtr->breakOffset) != TCL_OK) ||
            (ExtractTclSize(interp, envPtr, &excPtr->continueOffset) != TCL_OK) ||
            (ExtractTclSize(interp, envPtr, &excPtr->catchOffset) != TCL_OK))
        {
            return TCL_ERROR;
        }

        excPtr += 1;
        imagePtr = envPtr->curImagePtr;
    }

    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ExtractAuxDataArray --
 *
 *  Extracts the AuxData array for a ByteCode struct from a compiled image.
 *  The numAuxDataItems argument can be specified as -1, in which case the
 *  procedure extracts the number of AuxData items from the array header,
 *  or as a number that specifies the expected number of AuxData items in
 *  the array. In the latter case, the value obtained from the array header is
 *  compared to the value passed in, and an error is returned if the two don't
 *  match.
 *  The auxDataArraySize argument specifies the size of the auxDataArray array.
 *
 * Results:
 *  Returns TCL_OK on success, TCL_ERROR on failure.
 *
 * Side effects:
 *  Populates the AuxData array with values extracted from the image.
 *  Updates the contents of the ExtractionEnv structure: stores the new
 *  address of the next available byte.
 *  May also modify the interpreter's result in case of failure.
 *
 *----------------------------------------------------------------------
 */

static int ExtractAuxDataArray(
    Tcl_Interp* interp, Tcl_Size numAuxDataItems, ExtractionEnv* envPtr, AuxData* auxDataArray, Tcl_Size auxDataArraySize)
{
    char* imagePtr;
    char* imageEnd;
    Tcl_Size hNumAuxDataItems, iAuxData;
    AuxData* auxPtr = auxDataArray;
    char typeCode;
    int result;

    /*
     * read the number of AuxData items from the header; we need to do
     * this even in cases where the number is passed in.
     * Then do the checks if necessary.
     */

    if (ExtractTclSize(interp, envPtr, &hNumAuxDataItems) != TCL_OK)
    {
        return TCL_ERROR;
    }

    if (numAuxDataItems < 0)
    {
        numAuxDataItems = hNumAuxDataItems;
    }
    else if (numAuxDataItems != hNumAuxDataItems)
    {
        {
            Tcl_Obj* res = Tcl_GetObjResult(interp);
            Tcl_AppendToObj(res, "inconsistent aux data array size", -1);
        }
        AppendErrorLocation(interp, envPtr);
        return TCL_ERROR;
    }

    if (numAuxDataItems > auxDataArraySize)
    {
        {
            Tcl_Obj* res = Tcl_GetObjResult(interp);
            Tcl_AppendToObj(res, "aux data array too big for storage", -1);
        }
        AppendErrorLocation(interp, envPtr);
        return TCL_ERROR;
    }

    imagePtr = envPtr->curImagePtr;
    imageEnd = envPtr->imageEnd;

    for (iAuxData = 0; iAuxData < numAuxDataItems; iAuxData++)
    {
        /*
         * skip whitespace
         */

        for (;;)
        {
            if (imagePtr == imageEnd)
            {
                {
                    Tcl_Obj* res = Tcl_GetObjResult(interp);
                    Tcl_AppendToObj(res, prematureEnd, -1);
                }
                return TCL_ERROR;
            }
            if (!isspace(UCHAR(*imagePtr)))
            {
                break;
            }
            imagePtr += 1;
        }

        typeCode = *imagePtr;
        imagePtr += 1;
        envPtr->curImagePtr = imagePtr;

        /*
         * look up the type, then dispatch to an extractor routine based on it
         */

        if (typeCode == CMP_JUMPTABLE_INFO)
        {
            result = ExtractJumptableInfo(interp, envPtr, auxPtr);
            if (result != TCL_OK)
            {
                return result;
            }
        }
        else if (typeCode == CMP_DICTUPDATE_INFO)
        {
            result = ExtractDictUpdateInfo(interp, envPtr, auxPtr);
            if (result != TCL_OK)
            {
                return result;
            }
        }
        else if (typeCode == CMP_NEW_FOREACH_INFO)
        {
            result = ExtractNewForeachInfo(interp, envPtr, auxPtr);
            if (result != TCL_OK)
            {
                return result;
            }
        }
        else
        {
            char errBuf[2];
            errBuf[0] = typeCode;
            errBuf[1] = '\0';
            {
                Tcl_Obj* res = Tcl_GetObjResult(interp);
                Tcl_AppendToObj(res, "unknown aux data type: ", -1);
                Tcl_AppendToObj(res, errBuf, -1);
            }
            AppendErrorLocation(interp, envPtr);
            return TCL_ERROR;
        }

        auxPtr += 1;
        imagePtr = envPtr->curImagePtr;
    }

    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ExtractSignature --
 *
 *  Extracts the signature data from the image at codePtr.
 *
 * Results:
 *  Returns the next available address in the image after the signature,
 *  0 on failure.
 *
 * Side effects:
 *  Populates the ImageSignature at signaturePtr with values extracted from
 *  the file. May also modify the interpreter's result in case of failure.
 *
 *----------------------------------------------------------------------
 */

static char* ExtractSignature(Tcl_Interp* interp, char* codePtr, char* codeEnd, ImageSignature* signaturePtr)
{
    static char badMsg[] = "bad image signature in bytecode";
    char savedChar, *savedCharPtr;
    Tcl_Size numScanned;

    /*
     * skip whitespace to the first nonempty line in the image; this skips
     * over whitespace that could have been inserted by the construct
     *  loader::bceval {
     *  ...
     *  }
     */

    for (;;)
    {
        if (codePtr == codeEnd)
        {
            return NULL;
        }
        if (!isspace(UCHAR(*codePtr)))
        {
            break;
        }
        codePtr += 1;
    }

    /*
     * Find the end the string at the first EOL, which delimits the
     * signature line. The saved pointer will also be used for the return
     * value.
     */

    savedCharPtr = strchr(codePtr, '\n');
    if (!savedCharPtr)
    {
        {
            Tcl_Obj* res = Tcl_GetObjResult(interp);
            Tcl_AppendToObj(res, badMsg, -1);
        }
        return NULL;
    }
    numScanned = sscanf(codePtr,
                        CMP_SIGNATURE_HEADER " %ld %ld.%ld %ld.%ld%c",
                        &signaturePtr->formatNumber,
                        &signaturePtr->cmpMajorVersion,
                        &signaturePtr->cmpMinorVersion,
                        &signaturePtr->tclMajorVersion,
                        &signaturePtr->tclMinorVersion,
                        &savedChar);
    if ((numScanned != 6) || !isspace(UCHAR(savedChar)))
    {
        {
            Tcl_Obj* res = Tcl_GetObjResult(interp);
            Tcl_AppendToObj(res, badMsg, -1);
        }
        return NULL;
    }
    return (savedCharPtr + 1);
}

/*
 *----------------------------------------------------------------------
 *
 * CheckSignature --
 *
 *  Check the signature struct to see if we support this bytecode version.
 *
 * Results:
 *  Returns TCL_OK if we think that we can handle this bytecode, TCL_ERROR
 *  otherwise.
 *
 * Side effects:
 *  Loads an error message in the result if there was an error.
 *
 *----------------------------------------------------------------------
 */

static int CheckSignature(Tcl_Interp* interp, ImageSignature* signaturePtr)
{
    if (signaturePtr->formatNumber != formatVersion)
    {
        char buf[32];
        sprintf(buf, "%ld", signaturePtr->formatNumber);
        {
            Tcl_Obj* res = Tcl_GetObjResult(interp);
            Tcl_AppendToObj(res, "unsupported bytecode version: ", -1);
            Tcl_AppendToObj(res, buf, -1);
        }
        return TCL_ERROR;
    }

    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ExtractCompiledFile --
 *
 *  Parses the compiled image in the codePtr buffer (of length codeLength),
 *  creates a Tcl_Obj whose internal representation is a ByteCode structure
 *  and whose string representation is invalid.
 *
 * Results:
 *  A Tcl_Obj that holds the generated ByteCode structure. Returns 0 on
 *  failure.
 *
 * Side effects:
 *  May create othe Tcl_Objs, depending on the input. Also will modify the
 *  interprete's result on failure.
 *
 *----------------------------------------------------------------------
 */

static Tcl_Obj* ExtractCompiledFile(Tcl_Interp* interp, char* codePtr, Tcl_Size codeLength)
{
    ExtractionEnv exEnv;
    Tcl_Obj* objPtr;
    char* newCodePtr;

    /*
     * The signature line contains the version of the interpreter that was
     * used to generate the compiled script, and that value should be
     * known to the extraction routines, because formats may change between
     * releases. For the time being, disregard it.
     */

    newCodePtr = ExtractSignature(interp, codePtr, (codePtr + codeLength), &(exEnv.sig));
    if (newCodePtr == NULL)
    {
        return NULL;
    }

    InitExtractEnv(newCodePtr, (codePtr + codeLength), &exEnv);

    /*
     * Do not allow loading newer bytecodes into older interps
     */
    if ((exEnv.sig.tclMajorVersion > tclMajorVersion) ||
        ((exEnv.sig.tclMajorVersion == tclMajorVersion) && (exEnv.sig.tclMinorVersion > tclMinorVersion)))
    {
        char buf[128];

        exEnv.codePtr = NULL;
        CleanupExtractEnv(&exEnv);
        sprintf(buf,
                "unable to load bytecode generated for Tcl %ld.%ld into Tcl %ld.%ld",
                exEnv.sig.tclMajorVersion,
                exEnv.sig.tclMinorVersion,
                tclMajorVersion,
                tclMinorVersion);
        {
            Tcl_Obj* res = Tcl_GetObjResult(interp);
            Tcl_AppendToObj(res, buf, -1);
        }
        return NULL;
    }

    if (ExtractByteCode(interp, &exEnv) != TCL_OK)
    {
        exEnv.codePtr = NULL;
        CleanupExtractEnv(&exEnv);
        return NULL;
    }

    /*
     * See the comment above Tcl_NewStringObj in ExtractObjArray.
     */

    objPtr = Tcl_NewStringObj(noSourceCode, -1);
    Tcl_IncrRefCount(objPtr);
    Tcl_ObjInternalRep ir;
    ir.twoPtrValue.ptr1 = (void*)exEnv.codePtr;
    ir.twoPtrValue.ptr2 = NULL;
    Tcl_StoreInternalRep(objPtr, cmpByteCodeType, &ir);
    exEnv.codePtr->refCount++;
    exEnv.codePtr = NULL;
    CleanupExtractEnv(&exEnv);

    return objPtr;
}

/*
 *----------------------------------------------------------------------
 *
 * InitExtractEnv --
 *
 *  Initializes a ExtractionEnv structure.
 *
 * Results:
 *  None.
 *
 * Side effects:
 *  Populates the ExtractionEnv structure with appropriate initial values.
 *
 *----------------------------------------------------------------------
 */

static void InitExtractEnv(char* codeBase, char* codeEnd, ExtractionEnv* envPtr)
{
    envPtr->imageBase = codeBase;
    envPtr->imageEnd = codeEnd;
    envPtr->curImagePtr = codeBase;

    envPtr->codePtr = NULL;
    envPtr->codeSize = 0;
}

/*
 *----------------------------------------------------------------------
 *
 * CleanupExtractEnv --
 *
 *  Cleans up a ExtractionEnv structure.
 *
 * Results:
 *  None.
 *
 * Side effects:
 *  If owned, frees the ByteCode struct.
 *
 *----------------------------------------------------------------------
 */

static void CleanupExtractEnv(ExtractionEnv* envPtr)
{
    CleanupByteCode(envPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * ExcRangeFromName --
 *
 *  Given a name, place the corresponding ExceptionRangeType enum in *typePtr.
 *  The name is really a one-character type code.
 *
 * Results:
 *  Returns 0 if it was successful at converting the name to an enum value,
 *  -1 otherwise.
 *
 * Side effects:
 *  None.
 *
 *----------------------------------------------------------------------
 */

static int ExcRangeFromName(Tcl_Size name, ExceptionRangeType* typePtr)
{
    const ExcRangeMap* mapPtr;

    for (mapPtr = &excRangeMap[0]; mapPtr->name != 0; mapPtr++)
    {
        if (mapPtr->name == (char)name)
        {
            *typePtr = mapPtr->type;
            return 0;
        }
    }

    return -1;
}

/*
 *----------------------------------------------------------------------
 *
 * InitTypes --
 *
 *  Uses Tcl_GetObjType to load pointers to known object types into static
 *  variables, which can then be used instead of the known objects themselves.
 *
 * Results:
 *  None.
 *
 * Side effects:
 *  None.
 *
 *----------------------------------------------------------------------
 */

static void InitTypes()
{
    if (didLoadTypes == 0)
    {
        cmpTclProProcBodyType = Tcl_GetObjType("procbody");
        if (!cmpTclProProcBodyType)
        {
            Tcl_Panic("InitTypes: failed to find the procbody type");
        }

        cmpByteCodeType = Tcl_GetObjType("bytecode");
        if (!cmpByteCodeType)
        {
            Tcl_Panic("InitTypes: failed to find the bytecode type");
        }

        cmpDoubleType = Tcl_GetObjType("double");
        if (!cmpDoubleType)
        {
            Tcl_Panic("InitTypes: failed to find the double type");
        }

        Tcl_Obj* obj = Tcl_NewIntObj(0);
        cmpIntType = obj->typePtr;
        Tcl_DecrRefCount(obj);

        cmpJumptableInfoType = TclGetAuxDataType("JumptableInfo");
        if (!cmpJumptableInfoType)
        {
            Tcl_Panic("InitTypes: failed to find the JumptableInfo AuxData type");
        }

        cmpDictUpdateInfoType = TclGetAuxDataType("DictUpdateInfo");
        if (!cmpDictUpdateInfoType)
        {
            Tcl_Panic("InitTypes: failed to find the DictUpdateInfo AuxData type");
        }

        cmpNewForeachInfoType = TclGetAuxDataType("NewForeachInfo");
        if (!cmpNewForeachInfoType)
        {
            Tcl_Panic("InitTypes: failed to find the NewForeachInfo AuxData type");
        }

        didLoadTypes += 1;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * InitCompatibilityLayer --
 *
 *  Initializes the internal structures used by the compatibility layer.
 *  Must be called before the public interfaces to the Loader package.
 *
 * Results:
 *  Returns TCL_OK on success, otherwise TCL_ERROR and leaves an error
 *  result in the interpreter.
 *
 * Side effects:
 *  Initializes a number of internal data structures used by the Loader
 *  implementation.
 *
 *----------------------------------------------------------------------
 */

static int InitCompatibilityLayer(Tcl_Interp* interp)
{
    Tcl_CmdInfo info;

    if (compatibilityLayerInit)
    {
        return TCL_OK;
    }

    /*
     * Extract a pointer to the proc object command so we can wrap the API.
     * By extracting the routine, we do not have to add Tcl_ProcObjCmd to
     * the stubs list.
     *
     * Bug #3826: if the proc command is renamed, then this extraction will
     * yeild the new proc def rather than the Tcl_ProcObjCmd.  HACK: To fix
     * this problem in the case of the debugger (bug #3089), we check for the
     * existence of DbgNub_procCmd, which holds the original proc command.
     */

    if ((!Tcl_GetCommandInfo(interp, "DbgNub_procCmd", &info)) || (info.objProc == NULL))
    {
        if ((!Tcl_GetCommandInfo(interp, "proc", &info)) || (info.objProc == NULL))
        {
            {
                Tcl_Obj* res = Tcl_GetObjResult(interp);
                Tcl_AppendToObj(res, "proc command could not be located.", -1);
            }
            return TCL_ERROR;
        }
    }
    bcprocCmdProc = info.objProc;

    /*
     * Determine what interpreter interface to use.
     */

    procBodyFactory = (ProcBodyFactory*)TclNewProcBodyObj;
    procBodyCleanup = TclProcCleanupProc;

    compatibilityLayerInit = 1;

    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * TbcloadInit --
 *
 *  Initializes the internal structures used by the Loader package.
 *  Must be called before the public interfaces to the Loader package.
 *
 * Results:
 *  Returns TCL_OK on success, otherwise TCL_ERROR and leaves an error
 *  result in the interpreter.
 *
 * Side effects:
 *  Initializes a number of internal data structures used by the Loader
 *  implementation.
 *
 *----------------------------------------------------------------------
 */

int TbcloadInit(Tcl_Interp* interp)
{
    if (!Tcl_InitStubs(interp, "9.0", 1))
    {
        return TCL_ERROR;
    }

    /*
     * Initialize the compatibility layer.
     */

    if (InitCompatibilityLayer(interp) != TCL_OK)
    {
        return TCL_ERROR;
    }

    /*
     * Determine the format version of compiled code.  8.0-8.3 is v1,
     * 8.4+ is v2 (new bytecode instructions, different var flags).
     */
    int major, minor;
    Tcl_GetVersion(&major, &minor, NULL, NULL);
    tclMajorVersion = major;
    tclMinorVersion = minor;

    /*
     * Initialize the local copies of pointers to some built-in object types.
     * We need to do it because the built-in types are not exported by the
     * windows Tcl DLL.
     */

    InitTypes();

    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * ExtractJumptableInfo --
 *
 *  Extract a JumptableInfo struct from the extraction environment..
 *
 * Results:
 *  Returns a TCL error code.
 *
 * Side effects:
 *  Creates a ForeachInfo AuxData at *auxPtr.
 *  Sets the TCL result on error.
 *
 *----------------------------------------------------------------------
 */

static int ExtractJumptableInfo(Tcl_Interp* interp, ExtractionEnv* envPtr, AuxData* auxDataPtr)
{
    int result, new;
    Tcl_Size i, numJmp, value, keyLength;
    JumptableInfo* infoPtr = NULL;
    Tcl_HashEntry* hEntry;
    unsigned char* key;

    /*
     * read in the control variables, allocate and initialize the
     * JumptableInfo struct.
     */

    result = ExtractTclSize(interp, envPtr, &numJmp);
    if (result != TCL_OK)
    {
        return result;
    }

    infoPtr = (JumptableInfo*)Tcl_Alloc((unsigned)(sizeof(JumptableInfo)));
    Tcl_InitHashTable(&infoPtr->hashTable, TCL_STRING_KEYS);

    for (i = 0; i < numJmp; i++)
    {
        result = ExtractTclSize(interp, envPtr, &value);
        if (result != TCL_OK)
        {
            goto errorReturn;
        }

        result = AllocAndExtractByteSequence(interp, envPtr, 1, &key, &keyLength);
        if (result != TCL_OK)
        {
            goto errorReturn;
        }
        hEntry = Tcl_CreateHashEntry(&infoPtr->hashTable, key, &new);
        Tcl_Free(key);
        Tcl_SetHashValue(hEntry, (char*)value);
    }

    /*
     * finally! Assign the JumptableInfo to the AuxData.
     */

    auxDataPtr->type = (AuxDataType*)cmpJumptableInfoType;
    auxDataPtr->clientData = (void*)infoPtr;

    return TCL_OK;

errorReturn:

    if (infoPtr)
    {
        /* free hashtable + JumpTable Structure */
        Tcl_HashSearch hSearch;

        hEntry = Tcl_FirstHashEntry(&infoPtr->hashTable, &hSearch);
        while (hEntry)
        {
            Tcl_DeleteHashEntry(hEntry);
            hEntry = Tcl_NextHashEntry(&hSearch);
        }
        Tcl_DeleteHashTable(&infoPtr->hashTable);
        Tcl_Free((char*)infoPtr);
    }

    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * ExtractDictUpdateInfo --
 *
 *  Extract a DictUpdateInfo struct from the extraction environment..
 *
 * Results:
 *  Returns a TCL error code.
 *
 * Side effects:
 *  Creates a DictUpdateInfo AuxData at *auxPtr.
 *  Sets the TCL result on error.
 *
 *----------------------------------------------------------------------
 */

static int ExtractDictUpdateInfo(Tcl_Interp* interp, ExtractionEnv* envPtr, AuxData* auxDataPtr)
{
    int result;
    Tcl_Size i, numVar, value;
    DictUpdateInfo* infoPtr = NULL;

    /*
     * read in the control variables, allocate and initialize the
     * DictUpdateInfo struct.
     */

    result = ExtractTclSize(interp, envPtr, &numVar);
    if (result != TCL_OK)
    {
        return result;
    }

    infoPtr = (DictUpdateInfo*)Tcl_Alloc((unsigned)(sizeof(DictUpdateInfo) + numVar * sizeof(int)));
    infoPtr->length = numVar;

    for (i = 0; i < numVar; i++)
    {
        result = ExtractTclSize(interp, envPtr, &value);
        if (result != TCL_OK)
        {
            goto errorReturn;
        }
        infoPtr->varIndices[i] = value;
    }

    /*
     * finally! Assign the DictUpdateInfo to the AuxData.
     */

    auxDataPtr->type = (AuxDataType*)cmpDictUpdateInfoType;
    auxDataPtr->clientData = (void*)infoPtr;

    return TCL_OK;

errorReturn:

    if (infoPtr)
    {
        Tcl_Free((char*)infoPtr);
    }

    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * ExtractNewForeachInfo --
 *
 *  Extract a ForeachInfo struct from the extraction environment..
 *  For the new foreach bytecodes.
 *
 * Results:
 *  Returns a TCL error code.
 *
 * Side effects:
 *  Creates a ForeachInfo AuxData at *auxPtr.
 *  Sets the TCL result on error.
 *
 *----------------------------------------------------------------------
 */

static int ExtractNewForeachInfo(Tcl_Interp* interp, ExtractionEnv* envPtr, AuxData* auxDataPtr)
{
    int result;
    Tcl_Size i, j;
    Tcl_Size numLists, loopCtTemp, numVars;
    ForeachInfo* infoPtr = NULL;
    ForeachVarList* varListPtr = NULL;
    Tcl_Size* varPtr;

    /*
     * read in the control variables, allocate and initialize the
     * ForeachInfo struct.
     */

    result = ExtractTclSize(interp, envPtr, &numLists);
    if (result != TCL_OK)
    {
        return result;
    }

    /*
     * The new bytecodes handling foreach do not use firstValueTemp.
     * Was dropped from saved bytecode. Fake a nice value, see %% below.
     */

    result = ExtractTclSize(interp, envPtr, &loopCtTemp);
    if (result != TCL_OK)
    {
        return result;
    }

    infoPtr = (ForeachInfo*)Tcl_Alloc((unsigned)(sizeof(ForeachInfo) + (numLists * sizeof(ForeachVarList*))));
    infoPtr->numLists = numLists;
    infoPtr->firstValueTemp = 0; /* %% */
    infoPtr->loopCtTemp = loopCtTemp;
    for (i = 0; i < numLists; i++)
    {
        infoPtr->varLists[i] = (ForeachVarList*)NULL;
    }

    /*
     * now load the ForeachVarList structs
     */

    for (i = 0; i < numLists; i++)
    {
        result = ExtractTclSize(interp, envPtr, &numVars);
        if (result != TCL_OK)
        {
            goto errorReturn;
        }

        varListPtr = (ForeachVarList*)Tcl_Alloc((unsigned)sizeof(ForeachVarList) + numVars * sizeof(int));
        infoPtr->varLists[i] = varListPtr;
        varListPtr->numVars = numVars;

        varPtr = &varListPtr->varIndexes[0];
        for (j = 0; j < numVars; j++)
        {
            result = ExtractTclSize(interp, envPtr, varPtr);
            if (result != TCL_OK)
            {
                goto errorReturn;
            }
            varPtr++;
        }
    }

    /*
     * finally! Assign the ForeachInfo to the AuxData.
     */

    auxDataPtr->type = (AuxDataType*)cmpNewForeachInfoType;
    auxDataPtr->clientData = (void*)infoPtr;

    return TCL_OK;

errorReturn:

    if (infoPtr)
    {
        for (i = 0; i < infoPtr->numLists; i++)
        {
            Tcl_Free((char*)infoPtr->varLists[i]);
        }
        Tcl_Free((char*)infoPtr);
    }

    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * ExtractProcBody --
 *
 *  Extracts the fields in a Proc struct (minus ByteCode, which has already
 *  been extracted), then populate a new Proc with it, create a
 *  "procbody" Tcl_Obj and return it.
 *
 * Results:
 *  Returns a Tcl_Obj of type "procbody", NULL on error.
 *
 * Side effects:
 *  Sets the TCL result on error.
 *
 *----------------------------------------------------------------------
 */

static Tcl_Obj* ExtractProcBody(Tcl_Interp* interp, ByteCode* codePtr, ExtractionEnv* envPtr)
{
    Proc* procPtr;
    Tcl_Obj* bodyPtr;
    Tcl_Size i;
    CompiledLocal* localPtr;

    /*
     * we need a bytecode Tcl_Obj to place in the proc. We bump its reference
     * count because there will be a reference to it in the Proc.
     * We also bump the reference count on the ByteCode because the object
     * contains a reference to it.
     */

    bodyPtr = Tcl_NewStringObj(noSourceCode, -1);
    Tcl_IncrRefCount(bodyPtr);
    Tcl_ObjInternalRep ir;
    ir.twoPtrValue.ptr1 = (void*)codePtr;
    ir.twoPtrValue.ptr2 = NULL;
    Tcl_StoreInternalRep(bodyPtr, cmpByteCodeType, &ir);
    codePtr->refCount++;

    /*
     * allocate the proc struct and start populating it.
     * We initialize the reference count on the Proc to 0 because
     * ProcBodyNewObj will bump it when it creates a TclProProcBody Tcl_Obj.
     */

    procPtr = (Proc*)Tcl_Alloc(sizeof(Proc));
    procPtr->iPtr = NULL;
    procPtr->refCount = 0;
    procPtr->cmdPtr = NULL;
    procPtr->bodyPtr = bodyPtr;
    procPtr->numArgs = 0;
    procPtr->numCompiledLocals = 0;
    procPtr->firstLocalPtr = NULL;
    procPtr->lastLocalPtr = NULL;

    if ((ExtractTclSize(interp, envPtr, &procPtr->numArgs) != TCL_OK) ||
        (ExtractTclSize(interp, envPtr, &procPtr->numCompiledLocals) != TCL_OK))
    {
        goto cleanAndError;
    }
    /*
     * load the compiled locals info
     */

    for (i = 0; i < procPtr->numCompiledLocals; i++)
    {
        localPtr = ExtractCompiledLocal(interp, envPtr);
        if (!localPtr)
        {
            goto cleanAndError;
        }

        if (procPtr->firstLocalPtr == NULL)
        {
            procPtr->firstLocalPtr = procPtr->lastLocalPtr = localPtr;
        }
        else
        {
            procPtr->lastLocalPtr->nextPtr = localPtr;
            procPtr->lastLocalPtr = localPtr;
        }
    }

    return (*procBodyFactory)(procPtr);

cleanAndError:
    (*procBodyCleanup)(procPtr);
    return NULL;
}

/*
 *----------------------------------------------------------------------
 *
 * ExtractCompiledLocal --
 *
 *  Creates a CompiledLocal struct, then populates it with data extracted
 *  from the extraction environment.
 *
 * Results:
 *  Returns a newly allocated CompiledLocal struct, NULL on error.
 *
 * Side effects:
 *  Modified the TCL result on error.
 *
 *----------------------------------------------------------------------
 */

static CompiledLocal* ExtractCompiledLocal(Tcl_Interp* interp, ExtractionEnv* envPtr)
{
    char* curImagePtr;
    Tcl_Size i, nameLength, hasDef;
    CompiledLocal* localPtr;
    unsigned int bit, mask;

    /*
     * read the length of the name from the byte sequence header; we need to
     * do this so that we can allocate the CompiledLocal. Then, let's move
     * the extraction environment back to where it was at the start of the
     * call, so that we can call ExtractByteSequence.
     */

    curImagePtr = envPtr->curImagePtr;
    if (ExtractTclSize(interp, envPtr, &nameLength) != TCL_OK)
    {
        return NULL;
    }
    envPtr->curImagePtr = curImagePtr;

    localPtr = (CompiledLocal*)Tcl_Alloc(offsetof(CompiledLocal, name) + 1U + nameLength);

    localPtr->nextPtr = NULL;
    localPtr->nameLength = nameLength;
    localPtr->defValuePtr = NULL;
    localPtr->flags = 0;
    localPtr->resolveInfo = NULL;

    if (ExtractByteSequence(interp, nameLength, envPtr, (unsigned char*)&localPtr->name[0], nameLength) != TCL_OK)
    {
        Tcl_Free((char*)localPtr);
        return NULL;
    }

    localPtr->name[nameLength] = 0;

    /*
     * extract the fields of the struct, then if necessary extract
     * the default value for the argument
     */

    Tcl_Size aux = 0;
    if ((ExtractTclSize(interp, envPtr, &localPtr->frameIndex) != TCL_OK) ||
        (ExtractTclSize(interp, envPtr, &hasDef) != TCL_OK) || (ExtractTclSize(interp, envPtr, &aux) != TCL_OK))
    {
        Tcl_Free((char*)localPtr);
        return NULL;
    }
    mask = aux;
    bit = 1;
    for (i = 0; i < varFlagsListSize; i++)
    {
        if (mask & bit)
        {
            localPtr->flags |= varFlagsList[i];
        }
        bit <<= 1;
    }

    /*
     * now we get the default value if any
     */

    if (hasDef)
    {
        Tcl_Obj* objPtr = ExtractObject(interp, envPtr);
        if (!objPtr)
        {
            Tcl_Free((char*)localPtr);
            return NULL;
        }
        localPtr->defValuePtr = objPtr;
    }

    return localPtr;
}

/*
 *----------------------------------------------------------------------
 *
 * AppendErrorLocation --
 *
 *  Append a string showing the location where an error ws detected
 *  during extraction of the compiled file.
 *
 * Results:
 *  None.
 *
 * Side effects:
 *  Modifies the TCL result.
 *
 *----------------------------------------------------------------------
 */

static void AppendErrorLocation(Tcl_Interp* interp, ExtractionEnv* envPtr)
{
    char *imagePtr, *imageEnd, *endPtr, *basePtr, *lastPtr, *p;
    char savedChar;

    /*
     * we append about 32 characters, give or take a few to make sure we
     * show a full TCL word at the end
     */

    basePtr = envPtr->curImagePtr;
    imagePtr = basePtr;
    imageEnd = envPtr->imageEnd;
    lastPtr = basePtr + 32;
    if (lastPtr > imageEnd)
    {
        lastPtr = imageEnd;
    }
    endPtr = lastPtr;

    for (;;)
    {
        p = FindEnd(imagePtr, imageEnd);
        if ((p <= imagePtr) || (p > lastPtr))
        {
            break;
        }
        endPtr = p;
        imagePtr = p + 1;
    }

    savedChar = *endPtr;
    *endPtr = '\0';

    {
        Tcl_Obj* res = Tcl_GetObjResult(interp);
        Tcl_AppendToObj(res, " at or near \"", -1);
        Tcl_AppendToObj(res, basePtr, -1);
        Tcl_AppendToObj(res, "\"", -1);
    }
    *endPtr = savedChar;
}

/*
 *----------------------------------------------------------------------
 *
 * A85InitDecodeContext --
 *
 *  Initialize an A85DecodeContext struct.
 *
 * Results:
 *  None.
 *
 * Side effects:
 *  initializes the fields of the A85ContextStruct with appropriate values.
 *
 *----------------------------------------------------------------------
 */

static void A85InitDecodeContext(Tcl_Size numBytes, unsigned char* decodeBuf, A85DecodeContext* ctxPtr)
{
    ctxPtr->bytesToDecode = numBytes;
    ctxPtr->curPtr = decodeBuf;
    ctxPtr->curChar = 0;
}

/*
 *----------------------------------------------------------------------
 *
 * A85DecodeByte --
 *
 *  Decodes a character read in from an ASCII85 encoded string.
 *  This procedure accumulates enough bytes to decode a tuple, then does the
 *  decoding and writes the bytes to the decode buffer pojnted to by the
 *  context.
 *
 * Results:
 *  A standard Tcl result value.
 *
 * Side effects:
 *  Adds to the error result in the interpreter in case of error.
 *
 *----------------------------------------------------------------------
 */

static int A85DecodeByte(Tcl_Interp* interp, int code, A85DecodeContext* ctxPtr)
{
    int i;
    int* decodePtr;
    long int decodeWord;
    unsigned char* curPtr = ctxPtr->curPtr;

    if (code == A85_Z)
    {
        if (ctxPtr->curChar != 0)
        {
            {
                Tcl_Obj* res = Tcl_GetObjResult(interp);
                Tcl_AppendToObj(res, "malformed byte sequence", -1);
            }
            return TCL_ERROR;
        }

        *curPtr = 0;
        curPtr += 1;

        *curPtr = 0;
        curPtr += 1;

        *curPtr = 0;
        curPtr += 1;

        *curPtr = 0;
        curPtr += 1;

        ctxPtr->bytesToDecode -= 4;
    }
    else
    {
        ctxPtr->decodeBuf[ctxPtr->curChar] = code;
        ctxPtr->curChar += 1;

        /*
         * There are two cases here:
         *  - if bytesToDecode > 4, then we expect that a full 5-tuple was
         *    written to the encoded buffer.
         *  - if bytesToDecode < 4, then the encoded buffer contains only
         *    the first (bytesToDecode + 1) characters in the 5-tuple, since
         *    the others can be reconstructed.
         *
         * Also, decoded bytes are stored in reverse order of their packing
         * order, because that's how the encoder did it.
         */

        if (ctxPtr->bytesToDecode >= 4)
        {
            if (ctxPtr->curChar > 4)
            {
                /*
                 * The decode word was stored in base-85, least significant to
                 * most significant char
                 */

                decodePtr = &ctxPtr->decodeBuf[4];
                decodeWord = *decodePtr;
                for (i = 1; i < 5; i++)
                {
                    decodePtr -= 1;
                    decodeWord = times85(decodeWord) + *decodePtr;
                }

                *curPtr = (int)(decodeWord & 0xff);
                curPtr += 1;

                *curPtr = (int)((decodeWord >> 8) & 0xff);
                curPtr += 1;

                *curPtr = (int)((decodeWord >> 16) & 0xff);
                curPtr += 1;

                *curPtr = (int)((decodeWord >> 24) & 0xff);
                curPtr += 1;

                ctxPtr->curChar = 0;
                ctxPtr->bytesToDecode -= 4;
            }
        }
        else
        {
            int i;
            int bytesToDecode = ctxPtr->bytesToDecode;
            if (ctxPtr->curChar > bytesToDecode)
            {
                /*
                 * reconstruct the missing characters, then extract the bytes
                 */

                for (i = bytesToDecode + 1; i < 5; i++)
                {
                    ctxPtr->decodeBuf[i] = 0;
                }

                decodePtr = &ctxPtr->decodeBuf[4];
                decodeWord = *decodePtr;
                for (i = 1; i < 5; i++)
                {
                    decodePtr -= 1;
                    decodeWord = times85(decodeWord) + *decodePtr;
                }

                *curPtr = (int)(decodeWord & 0xff);
                curPtr += 1;

                if (bytesToDecode > 1)
                {
                    *curPtr = (int)((decodeWord >> 8) & 0xff);
                    curPtr += 1;
                }

                if (bytesToDecode > 2)
                {
                    *curPtr = (int)((decodeWord >> 16) & 0xff);
                    curPtr += 1;
                }

                ctxPtr->curChar = 0;
                ctxPtr->bytesToDecode = 0;
            }
        }
    }

    ctxPtr->curPtr = curPtr;
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * FindEnd --
 *
 *  Skips to the end of the current word.
 *
 * Results:
 *  Returns a pointer to the first whitespace or null character
 *  past the end of the word.
 *
 * Side effects:
 *  None.
 *
 *----------------------------------------------------------------------
 */

static char* FindEnd(char* first, char* last)
{
    char* p;
    for (p = first; p != last; p++)
    {
        if (isspace(UCHAR(*p)))
        {
            break;
        }
    }
    return p;
}
