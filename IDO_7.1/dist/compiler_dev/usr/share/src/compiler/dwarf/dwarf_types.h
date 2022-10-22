/*
    dwarf_types.h

    $Revision: 1.1 $		$Date: 1993/08/26 01:09:49 $
*/


typedef struct Dwarf_Type_Context_s	*Dwarf_Type_Context;

/* 
    This struct contains header information for a set of type
    names.  Essentially, they contain the context for a set of 
    type names belonging to a compilation-unit.
*/
struct Dwarf_Type_Context_s {

        /* 
	    Length in .debug_typenames of a set of type
	    names for a compilation-unit. 
	*/
    Dwarf_Word			tp_length;

	/* 
	    Offset into .debug_info of the compilation-unit
	    for this set of type names.
	*/
    Dwarf_Off			tp_info_offset;

	/* Size of compilation-unit that these type names are in. */
    Dwarf_Unsigned		tp_info_length;
};


/* This struct contains information for a single type name. */
struct Dwarf_Type_s {

	/* 
	    Offset from the start of the corresponding compilation-unit
	    of the DIE for the given type name.
	*/
    Dwarf_Off			ty_cu_offset;

	/* Points to the given type name. */
    Dwarf_Small			*ty_name;

	/* Context for this type name. */
    Dwarf_Type_Context		ty_context;
};
