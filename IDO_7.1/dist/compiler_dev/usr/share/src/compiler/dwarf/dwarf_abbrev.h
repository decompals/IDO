/*
	dwarf_abbrev.h

	$Revision: 1.5 $              $Date: 1993/08/16 23:16:46 $

	Abbreviations table: used by libdwarf consumer:
	internal to libdwarf.

*/


struct Dwarf_Abbrev_s {
    Dwarf_Word		ab_code;
    Dwarf_Half		ab_tag;
    Dwarf_Small		ab_has_child;
    Dwarf_Byte_Ptr    	ab_abbrev_ptr;
    Dwarf_Debug		ab_dbg;
};
