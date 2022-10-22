/*
    dwarf_weaks.h

    $Revision: 1.1 $		$Date: 1993/12/22 18:59:56 $
*/


typedef struct Dwarf_Weak_Context_s	*Dwarf_Weak_Context;

/* 
    This struct contains header information for a set of weak
    names.  Essentially, they contain the context for a set of 
    weak names belonging to a compilation-unit.
*/
struct Dwarf_Weak_Context_s {

        /* 
	    Length in .debug_weaknames of a set of weak
	    names for a compilation-unit. 
	*/
    Dwarf_Word			wk_length;

	/* 
	    Offset into .debug_info of the compilation-unit
	    for this set of weak names.
	*/
    Dwarf_Off			wk_info_offset;

	/* Size of compilation-unit that these weak names are in. */
    Dwarf_Unsigned		wk_info_length;
};


/* This struct contains information for a single weak name. */
struct Dwarf_Weak_s {

	/* 
	    Offset from the start of the corresponding compilation-unit
	    of the DIE for the given weak name.
	*/
    Dwarf_Off			we_cu_offset;

	/* Points to the given weak name. */
    Dwarf_Small			*we_name;

	/* Context for this weak name. */
    Dwarf_Weak_Context		we_context;
};
