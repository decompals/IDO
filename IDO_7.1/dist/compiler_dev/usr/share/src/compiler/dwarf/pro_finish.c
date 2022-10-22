/*
	pro_finish.c 
	$Revision: 1.6 $    $Date: 1996/07/08 21:07:39 $    
	$Source: /hosts/bonnie/proj/irix6.4-ssg/isms/cmplrs/libdwarf/RCS/pro_finish.c,v $

	exit function
*/

#include "pro_incl.h"

/*---------------------------------------------------------------
	This routine deallocates all memory, and does some 
	finishing up
-----------------------------------------------------------------*/
/*ARGSUSED*/
Dwarf_Unsigned
dwarf_producer_finish(Dwarf_P_Debug dbg, Dwarf_Error *error)
{
	dwarf_p_dealloc((void *)dbg, 0);
	return 0;
}
