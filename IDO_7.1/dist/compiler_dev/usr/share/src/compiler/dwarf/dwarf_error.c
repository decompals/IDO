/*

	dwarf_error.c



	$Revision: 1.38 $ $Date: 1995/07/09 15:41:31 $

*/

#include <libelf.h>
#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <stdlib.h>

/* Array to hold string representation of errors. Any time a 
   define is added to the list in libdwarf.h, a string should be 
   added to this Array
*/

const char *_dwarf_errmsgs[] = {

    "No error (0)\n",
    "DW_DLE_VMM 1 dwarf format/library version mismatch",
    "DW_DLE_MAP 2 memory map failure",
    "DW_DLE_LEE 3 libelf error",
    "DW_DLE_NDS 4 no debug section",
    "DW_DLE_NLS    5  no line section ",
    "DW_DLE_ID     6  invalid descriptor for query ",
    "DW_DLE_IOF    7  I/O failure ",
    "DW_DLE_MAF    8  memory allocation failure ",
    "DW_DLE_IA     9  invalid argument ",
    "DW_DLE_MDE    10  mangled debugging entry ",
    "DW_DLE_MLE    11  mangled line number entry ",
    "DW_DLE_FNO    12  file not open ",
    "DW_DLE_FNR    13  file not a regular file ",
    "DW_DLE_FWA    14  file open with wrong access ",
    "DW_DLE_NOB    15  not an object file ",
    "DW_DLE_MOF    16  mangled object file header ",
    "DW_DLE_EOLL   17  end of location list entries ",
    "DW_DLE_NOLL   18  no location list section ",
    "DW_DLE_BADOFF 19  Invalid offset ",
    "DW_DLE_EOS    20  end of section  ",
    "DW_DLE_ATRUNC 21  abbreviations section appears truncated",
    "DW_DLE_BADBITC  22  Address size passed to dwarf bad",

	"DW_DLE_DBG_ALLOC 23",
	"DW_DLE_FSTAT_ERROR 24",
	"DW_DLE_FSTAT_MODE_ERROR 25",
	"DW_DLE_INIT_ACCESS_WRONG 26",
	"DW_DLE_ELF_BEGIN_ERROR 27",
	"DW_DLE_ELF_GETEHDR_ERROR 28",
	"DW_DLE_ELF_GETSHDR_ERROR 29",
	"DW_DLE_ELF_STRPTR_ERROR 30",
	"DW_DLE_DEBUG_INFO_DUPLICATE 31",
	"DW_DLE_DEBUG_INFO_NULL 32",
	"DW_DLE_DEBUG_ABBREV_DUPLICATE 33",
	"DW_DLE_DEBUG_ABBREV_NULL 34",
	"DW_DLE_DEBUG_ARANGES_DUPLICATE 35",
	"DW_DLE_DEBUG_ARANGES_NULL 36",
	"DW_DLE_DEBUG_LINE_DUPLICATE 37",
	"DW_DLE_DEBUG_LINE_NULL (38)",
	"DW_DLE_DEBUG_LOC_DUPLICATE (39)",
	"DW_DLE_DEBUG_LOC_NULL (40)",
	"DW_DLE_DEBUG_MACINFO_DUPLICATE (41)",
	"DW_DLE_DEBUG_MACINFO_NULL (42)",
	"DW_DLE_DEBUG_PUBNAMES_DUPLICATE (43)",
	"DW_DLE_DEBUG_PUBNAMES_NULL (44)",
	"DW_DLE_DEBUG_STR_DUPLICATE (45)",
	"DW_DLE_DEBUG_STR_NULL (46)",
	"DW_DLE_CU_LENGTH_ERROR (47)",
	"DW_DLE_VERSION_STAMP_ERROR (48)",
	"DW_DLE_ABBREV_OFFSET_ERROR (49)",
	"DW_DLE_ADDRESS_SIZE_ERROR (50)",
	"DW_DLE_DEBUG_INFO_PTR_NULL (51)",
	"DW_DLE_DIE_NULL (52)",
	"DW_DLE_STRING_OFFSET_BAD (53)",
	"DW_DLE_DEBUG_LINE_LENGTH_BAD (54)",
	"DW_DLE_LINE_PROLOG_LENGTH_BAD (55)",
	"DW_DLE_LINE_NUM_OPERANDS_BAD",
	"DW_DLE_LINE_SET_ADDR_ERROR",
	"DW_DLE_LINE_EXT_OPCODE_BAD",
	"DW_DLE_DWARF_LINE_NULL",
	"DW_DLE_INCL_DIR_NUM_BAD",
	"DW_DLE_LINE_FILE_NUM_BAD",
	"DW_DLE_ALLOC_FAIL",
	"DW_DLE_NO_CALLBACK_FUNC",
	"DW_DLE_SECT_ALLOC",
	"DW_DLE_FILE_ENTRY_ALLOC",
	"DW_DLE_LINE_ALLOC",
	"DW_DLE_FPGM_ALLOC",
	"DW_DLE_INCDIR_ALLOC",
	"DW_DLE_STRING_ALLOC",
	"DW_DLE_CHUNK_ALLOC",
	"DW_DLE_BYTEOFF_ERR",
	"DW_DLE_CIE_ALLOC",
	"DW_DLE_FDE_ALLOC",
	"DW_DLE_REGNO_OVFL",
	"DW_DLE_CIE_OFFS_ALLOC",
	"DW_DLE_WRONG_ADDRESS",
	"DW_DLE_EXTRA_NEIGHBORS",
	"DW_DLE_WRONG_TAG",
	"DW_DLE_DIE_ALLOC",
	"DW_DLE_PARENT_EXISTS",
	"DW_DLE_DBG_NULL",
	"DW_DLE_DEBUGLINE_ERROR",
	"DW_DLE_DEBUGFRAME_ERROR",
	"DW_DLE_DEBUGINFO_ERROR",
	"DW_DLE_ATTR_ALLOC",
	"DW_DLE_ABBREV_ALLOC",
	"DW_DLE_OFFSET_UFLW",
	"DW_DLE_ELF_SECT_ERR",
	"DW_DLE_DEBUG_FRAME_LENGTH_BAD",
	"DW_DLE_FRAME_VERSION_BAD",
	"DW_DLE_CIE_RET_ADDR_REG_ERROR",
	"DW_DLE_FDE_NULL",
	"DW_DLE_FDE_DBG_NULL",
	"DW_DLE_CIE_NULL",
	"DW_DLE_CIE_DBG_NULL",
	"DW_DLE_FRAME_TABLE_COL_BAD",
	"DW_DLE_PC_NOT_IN_FDE_RANGE",
	"DW_DLE_CIE_INSTR_EXEC_ERROR",
	"DW_DLE_FRAME_INSTR_EXEC_ERROR",
	"DW_DLE_FDE_PTR_NULL",
	"DW_DLE_RET_OP_LIST_NULL",
	"DW_DLE_LINE_CONTEXT_NULL",
	"DW_DLE_DBG_NO_CU_CONTEXT",
	"DW_DLE_DIE_NO_CU_CONTEXT",
	"DW_DLE_FIRST_DIE_NOT_CU",
	"DW_DLE_NEXT_DIE_PTR_NULL",
	"DW_DLE_DEBUG_FRAME_DUPLICATE",
	"DW_DLE_DEBUG_FRAME_NULL",
	"DW_DLE_ABBREV_DECODE_ERROR",
	"DW_DLE_DWARF_ABBREV_NULL",
	"DW_DLE_ATTR_NULL",
	"DW_DLE_DIE_BAD",
	"DW_DLE_DIE_ABBREV_BAD",
	"DW_DLE_ATTR_FORM_BAD",
	"DW_DLE_ATTR_NO_CU_CONTEXT",
	"DW_DLE_ATTR_FORM_SIZE_BAD",
	"DW_DLE_ATTR_DBG_NULL",
	"DW_DLE_BAD_REF_FORM",
	"DW_DLE_ATTR_FORM_OFFSET_BAD",
	"DW_DLE_LINE_OFFSET_BAD",
	"DW_DLE_DEBUG_STR_OFFSET_BAD",
	"DW_DLE_STRING_PTR_NULL",
 	"DW_DLE_PUBNAMES_VERSION_ERROR",
	"DW_DLE_PUBNAMES_LENGTH_BAD",
	"DW_DLE_GLOBAL_NULL",
	"DW_DLE_GLOBAL_CONTEXT_NULL",
	"DW_DLE_DIR_INDEX_BAD",
	"DW_DLE_LOC_EXPR_BAD",
	"DW_DLE_DIE_LOC_EXPR_BAD",
	"DW_DLE_ADDR_ALLOC",
	"DW_DLE_OFFSET_BAD",
	"DW_DLE_MAKE_CU_CONTEXT_FAIL",
	"DW_DLE_REL_ALLOC",
	"DW_DLE_ARANGE_OFFSET_BAD",
	"DW_DLE_SEGMENT_SIZE_BAD",
	"DW_DLE_ARANGE_LENGTH_BAD",
	"DW_DLE_ARANGE_DECODE_ERROR",
	"DW_DLE_ARANGES_NULL",
	"DW_DLE_ARANGE_NULL",
	"DW_DLE_NO_FILE_NAME",
	"DW_DLE_NO_COMP_DIR",
	"DW_DLE_CU_ADDRESS_SIZE_BAD",
	"DW_DLE_INPUT_ATTR_BAD",
	"DW_DLE_EXPR_NULL",
	"DW_DLE_BAD_EXPR_OPCODE",
	"DW_DLE_EXPR_LENGTH_BAD",
	"DW_DLE_MULTIPLE_RELOC_IN_EXPR",
	"DW_DLE_ELF_GETIDENT_ERROR",
	"DW_DLE_NO_AT_MIPS_FDE",
	"DW_DLE_NO_CIE_FOR_FDE",
	"DW_DLE_DIE_ABBREV_LIST_NULL",
	"DW_DLE_DEBUG_FUNCNAMES_DUPLICATE",
	"DW_DLE_DEBUG_FUNCNAMES_NULL",
	"DW_DLE_DEBUG_FUNCNAMES_VERSION_ERROR",
	"DW_DLE_DEBUG_FUNCNAMES_LENGTH_BAD",
	"DW_DLE_FUNC_NULL",
	"DW_DLE_FUNC_CONTEXT_NULL",
	"DW_DLE_DEBUG_TYPENAMES_DUPLICATE",
	"DW_DLE_DEBUG_TYPENAMES_NULL",
	"DW_DLE_DEBUG_TYPENAMES_VERSION_ERROR",
	"DW_DLE_DEBUG_TYPENAMES_LENGTH_BAD",
	"DW_DLE_TYPE_NULL",
	"DW_DLE_TYPE_CONTEXT_NULL",
	"DW_DLE_DEBUG_VARNAMES_DUPLICATE",
	"DW_DLE_DEBUG_VARNAMES_NULL",
	"DW_DLE_DEBUG_VARNAMES_VERSION_ERROR",
	"DW_DLE_DEBUG_VARNAMES_LENGTH_BAD",
	"DW_DLE_VAR_NULL",
	"DW_DLE_VAR_CONTEXT_NULL",
	"DW_DLE_DEBUG_WEAKNAMES_DUPLICATE",
	"DW_DLE_DEBUG_WEAKNAMES_NULL",
	"DW_DLE_DEBUG_WEAKNAMES_VERSION_ERROR",
	"DW_DLE_DEBUG_WEAKNAMES_LENGTH_BAD",
	"DW_DLE_WEAK_NULL",
	"DW_DLE_WEAK_CONTEXT_NULL (175)",
	"DW_DLE_LOCDESC_COUNT_WRONG (176)",
	"DW_DLE_MACINFO_STRING_NULL (177)",
	"DW_DLE_MACINFO_STRING_EMPTY (178)",
	"DW_DLE_MACINFO_INTERNAL_ERROR_SPACE (179)",
	"DW_DLE_MACINFO_MALLOC_FAIL (180)",
	"DW_DLE_DEBUGMACINFO_ERROR (181)",
	"DW_DLE_DEBUG_MACRO_LENGTH_BAD (182)",
	"DW_DLE_DEBUG_MACRO_MAX_BAD (183)",
	"DW_DLE_DEBUG_MACRO_INTERNAL_ERR (184)",
	"DW_DLE_DEBUG_MACRO_MALLOC_SPACE (185)",
	"DW_DLE_DEBUG_MACRO_INCONSISTENT (186)",
	"DW_DLE_DF_NO_CIE_AUGMENTATION(187)",
	"DW_DLE_DF_REG_NUM_TOO_HIGH(188)",
	"DW_DLE_DF_MAKE_INSTR_NO_INIT(189)",
	"DW_DLE_DF_NEW_LOC_LESS_OLD_LOC(190)",
	"DW_DLE_DF_POP_EMPTY_STACK(191)",
	"DW_DLE_DF_ALLOC_FAIL(192)",
	"DW_DLE_DF_FRAME_DECODING_ERROR(193)",


	
};
	


#include "dwarf_incl.h"

/* 
    This function performs error handling as described in the 
    libdwarf consumer document section 3.  Dbg is the Dwarf_debug
    structure being processed.  Error is a pointer to the pointer
    to the error descriptor that will be returned.  Errval is an
    error code listed in dwarf_error.h.
*/
void
_dwarf_error (
    Dwarf_Debug     dbg,
    Dwarf_Error     *error,
    Dwarf_Sword     errval
)
{
    Dwarf_Error     			errptr;
    static struct Dwarf_Error_s		alt_err_struct;

    /* 
	Allow NULL dbg on entry, since sometimes that can happen and
        we want to report the upper-level error, not this one.
    */
    if (error != NULL) {

	    /* 
		If dbg is NULL, use the alternate error struct.
		However, this will overwrite the earlier error.
	    */
	if (dbg != NULL) {
            errptr = (Dwarf_Error)_dwarf_get_alloc(dbg, DW_DLA_ERROR, 1);
            if (errptr == NULL) {
                fprintf(stderr,"Could not allocate Dwarf_Error structure, abort() in libdwarf.\n");
                abort();
            }
	}
	else 
	    errptr = &alt_err_struct;

        errptr->er_errval = errval;
        *error = errptr;
        return;
    }

    if (dbg != NULL && dbg->de_errhand != NULL) {
        errptr = (Dwarf_Error)_dwarf_get_alloc(dbg, DW_DLA_ERROR, 1);
        if (errptr == NULL) {
            fprintf(stderr,"Could not allocate Dwarf_Error structure, abort() in libdwarf.\n");
            abort();
        }
        errptr->er_errval = errval;
        dbg->de_errhand(errptr,dbg->de_errarg);
        return;
    }
    fprintf(stderr,"abort() in libdwarf. No error argument, no handler.\n");
    abort();
}


Dwarf_Unsigned
dwarf_errno (
    Dwarf_Error		error
)
{
    if (error == NULL) {
	return(0);
    }

    return(error->er_errval);
}


/* 
*/
char *
dwarf_errmsg (
    Dwarf_Error		error
)
{
    if (error == NULL) {
	return "Dwarf_Error is NULL";
    }

    if (error->er_errval > (sizeof(_dwarf_errmsgs) / sizeof(char *))) {
	return "Dwarf_Error value out of range";
    }

    return ((char *)_dwarf_errmsgs[error->er_errval]);
}
