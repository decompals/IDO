/*
    dwarf_funcs.h

    $Revision: 1.1 $		$Date: 1993/08/26 01:09:45 $
*/


typedef struct Dwarf_Func_Context_s	*Dwarf_Func_Context;

/* 
    This struct contains header information for a set of function
    names.  Essentially, they contain the context for a set of 
    function names belonging to a compilation-unit.
*/
struct Dwarf_Func_Context_s {

        /* 
	    Length in .debug_funcnames of a set of function
	    names for a compilation-unit. 
	*/
    Dwarf_Word			fu_length;

	/* 
	    Offset into .debug_info of the compilation-unit
	    for this set of function names.
	*/
    Dwarf_Off			fu_info_offset;

	/* Size of compilation-unit that these function names are in. */
    Dwarf_Unsigned		fu_info_length;
};


/* This struct contains information for a single function name. */
struct Dwarf_Func_s {

	/* 
	    Offset from the start of the corresponding compilation-unit
	    of the DIE for the given function name.
	*/
    Dwarf_Off			fl_cu_offset;

	/* Points to the given function name. */
    Dwarf_Small			*fl_name;

	/* Context for this function name. */
    Dwarf_Func_Context		fl_context;
};
