/*
    dwarf_loc.h

    $Revision: 1.3 $		$Date: 1993/07/19 22:37:18 $
*/

typedef struct Dwarf_Loc_Chain_s	*Dwarf_Loc_Chain;

struct Dwarf_Loc_Chain_s {
    Dwarf_Small		lc_atom;
    Dwarf_Unsigned	lc_number;
    Dwarf_Unsigned	lc_number2;
    Dwarf_Unsigned	lc_offset;
    Dwarf_Loc_Chain	lc_next;
};
