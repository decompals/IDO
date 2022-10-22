/*
	pro_alloc.h

	allocation routines which are not part of the public
	dwarf interface (but are part of the libdwarf internals)
	are declared here.

	$Revision: 1.2 $    $Date: 1994/12/28 19:02:16 $
*/

Dwarf_Ptr _dwarf_p_get_alloc(Dwarf_P_Debug, Dwarf_Unsigned);

void _dwarf_p_dealloc (Dwarf_P_Debug dbg, Dwarf_Small *ptr);
