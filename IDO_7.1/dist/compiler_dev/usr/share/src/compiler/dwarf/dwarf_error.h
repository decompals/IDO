/*
    dwarf_error.h

    $Revision: 1.17 $     $Date: 1993/08/16 23:17:45 $
*/

void _dwarf_error (Dwarf_Debug dbg, Dwarf_Error *error, Dwarf_Sword  errval);

struct Dwarf_Error_s {
    Dwarf_Sword         er_errval;
};
