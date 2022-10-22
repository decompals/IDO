#include <stdio.h>
#include "dwarf_incl.h"


/*ARGSUSED*/
int
dwarf_get_loclist_entry (
    Dwarf_Debug		dbg,
    Dwarf_Unsigned	offset,
    Dwarf_Addr		*hipc_offset,
    Dwarf_Addr		*lopc_offset,
    Dwarf_Ptr		*data,
    Dwarf_Unsigned	*entry_len,
    Dwarf_Unsigned	*next_entry,
    Dwarf_Error		*error
)
{
    return(DW_DLV_NO_ENTRY);
}


/*ARGSUSED*/
int
dwarf_nextglob (
    Dwarf_Debug		dbg,
    Dwarf_Global	glob,
    Dwarf_Global *      returned_nextglob,
    Dwarf_Error		*error
)
{
    return(DW_DLV_NO_ENTRY);
}
