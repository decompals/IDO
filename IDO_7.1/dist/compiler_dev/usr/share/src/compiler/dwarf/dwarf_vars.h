/*
    dwarf_vars.h

    $Revision: 1.1 $		$Date: 1993/08/26 01:10:00 $
*/


typedef struct Dwarf_Var_Context_s	*Dwarf_Var_Context;

/* 
    This struct contains header information for a set of variable
    names.  Essentially, they contain the context for a set of 
    variable names belonging to a compilation-unit.
*/
struct Dwarf_Var_Context_s {

        /* 
	    Length in .debug_varnames of a set of variable
	    names for a compilation-unit. 
	*/
    Dwarf_Word			vr_length;

	/* 
	    Offset into .debug_info of the compilation-unit
	    for this set of variable names.
	*/
    Dwarf_Off			vr_info_offset;

	/* Size of compilation-unit that these variable names are in. */
    Dwarf_Unsigned		vr_info_length;
};


/* This struct contains information for a single variable name. */
struct Dwarf_Var_s {

	/* 
	    Offset from the start of the corresponding compilation-unit
	    of the DIE for the given variable name.
	*/
    Dwarf_Off			va_cu_offset;

	/* Points to the given variable name. */
    Dwarf_Small			*va_name;

	/* Context for this variable name. */
    Dwarf_Var_Context		va_context;
};
