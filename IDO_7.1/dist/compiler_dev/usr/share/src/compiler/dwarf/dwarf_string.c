#include "dwarf_incl.h"

int
dwarf_get_str (
    Dwarf_Debug		dbg,
    Dwarf_Off		offset,
    char		**string,
    Dwarf_Signed	*returned_str_len,
    Dwarf_Error		*error
)
{

    if (dbg == NULL) {
	_dwarf_error(NULL, error, DW_DLE_DBG_NULL);
	return(DW_DLV_ERROR);
    }

    if (dbg->de_debug_str == NULL) {
	return(DW_DLV_NO_ENTRY);
    }

    if (offset >= dbg->de_debug_str_size) {
	_dwarf_error(dbg, error, DW_DLE_DEBUG_STR_OFFSET_BAD);
	return(DW_DLV_ERROR);
    }

    if (string == NULL) {
	_dwarf_error(dbg, error, DW_DLE_STRING_PTR_NULL);
	return(DW_DLV_ERROR);
    }
    *string = (char *)dbg->de_debug_str + offset;

    *returned_str_len = (strlen(*string));
    return DW_DLV_OK;
}
