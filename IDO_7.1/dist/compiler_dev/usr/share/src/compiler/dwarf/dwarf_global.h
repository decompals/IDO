/*
    dwarf_global.h

    $Revision: 1.5 $		$Date: 1993/08/16 23:20:43 $
*/


typedef struct Dwarf_Global_Context_s	*Dwarf_Global_Context;

/* 
    This struct contains header information for a set of pubnames.
    Essentially, they contain the context for a set of pubnames 
    belonging to a compilation-unit.
*/
struct Dwarf_Global_Context_s {

        /* 
	    Length in .debug_pubnames of a set of pubnames 
	    for a compilation-unit. 
	*/
    Dwarf_Word			pu_length;

	/* 
	    Offset into .debug_info of the compilation-unit
	    for this set of pubnames.
	*/
    Dwarf_Off			pu_info_offset;

	/* Size of compilation-unit that these pubnames are in. */
    Dwarf_Unsigned		pu_info_length;
};


/* This struct contains information for a single pubname. */
struct Dwarf_Global_s {

	/* 
	    Offset from the start of the corresponding compilation-unit
	    of the DIE for the given pubname.
	*/
    Dwarf_Off			gl_cu_offset;

	/* Points to the given pubname. */
    Dwarf_Small			*gl_name;

	/* Context for this pubname. */
    Dwarf_Global_Context	gl_context;
};
