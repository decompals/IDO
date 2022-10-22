/*
    dwarf_arange.h

    $Revision: 1.4 $	$Date: 1994/06/06 20:12:12 $
*/

/* This structure is used to read an arange into. */
struct Dwarf_Arange_s {

	/* Starting address of the arange, ie low-pc. */
    Dwarf_Addr		ar_address;

	/* Length of the arange. */
    Dwarf_Unsigned	ar_length;

	/* 
	    Offset into .debug_info of the start of the 
	    compilation-unit containing this set of aranges.
	*/
    Dwarf_Off		ar_info_offset;

	/* Corresponding Dwarf_Debug. */
    Dwarf_Debug		ar_dbg;
};



int
_dwarf_get_aranges_addr_offsets(
    Dwarf_Debug         dbg,
    Dwarf_Addr          **addrs,
    Dwarf_Off           **offsets,
    Dwarf_Signed        *count,
    Dwarf_Error         *error
);
