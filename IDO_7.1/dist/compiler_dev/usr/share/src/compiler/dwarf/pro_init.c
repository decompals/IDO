/*
	pro_init.c
	$Revision: 1.7 $    $Date: 1993/09/30 17:04:23 $    
	$Source: /hosts/bonnie/proj/irix6.4-ssg/isms/cmplrs/libdwarf/RCS/pro_init.c,v $

	Has producer initialization routines.
*/

#include <stdio.h>
#include <bstring.h>
#include "pro_incl.h"

/*--------------------------------------------------------------------
	This function sets up a new dwarf producing region. 
	flags: Indicates type of access method, one of DW_DLC* macros
	func(): Used to create a new object file, a call back function
	errhand(): Error Handler provided by user
	errarg: Argument to errhand()
	error: returned error value
--------------------------------------------------------------------*/
Dwarf_P_Debug
dwarf_producer_init(
		    Dwarf_Unsigned flags,
		    Dwarf_Callback_Func func,
		    Dwarf_Handler errhand,
		    Dwarf_Ptr errarg,
		    Dwarf_Error * error)
{

    Dwarf_P_Debug     dbg;

    dbg = (Dwarf_P_Debug) _dwarf_p_get_alloc(NULL, sizeof(struct Dwarf_P_Debug_s));
    if (dbg == NULL) {
        DWARF_P_DBG_ERROR(dbg,DW_DLE_DBG_ALLOC,(Dwarf_P_Debug) DW_DLV_BADADDR);
    }
    bzero((void *) dbg, sizeof(struct Dwarf_P_Debug_s));
    /* For the time being */
    if (func == NULL) {
        DWARF_P_DBG_ERROR(dbg,DW_DLE_NO_CALLBACK_FUNC,(Dwarf_P_Debug) DW_DLV_BADADDR);
    }
    dbg->de_n_debug_sect = 0;
    dbg->de_func = func;
    dbg->de_flags = flags;
    dbg->de_errhand = errhand;
    dbg->de_errarg = errarg;
    return dbg;
}
