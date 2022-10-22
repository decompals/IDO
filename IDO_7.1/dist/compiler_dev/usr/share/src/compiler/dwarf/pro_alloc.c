/*

	This is the allocator and deallocator for
	dwarf producer.

	Part of this interface is defined in the public interface.
	Part is hidden in libdwarf.

	Goals: 
		Fast allocation
	 	no leakage on dwarf_finish() kinds of activities
		match libdwarf documented semantics and space
			rules
		For producer, when dwarf_transform_to_disk_form()
			is about finished, wants
		 	all trees to disappear leaving only
			byte streams.

	Producer special case:
		After transform_to_disk_form is called we want
		to have freed all die, attributes etc and just
		leave the transformed byte streams and whatever
		else is needed to support them.
		So we invent a special free operation to do this.

	Issue: What to do if we discover problems in the arena when
		deallocating?  These return nothing....

	We will allocate space in blocks attached to a Dwarf_Debug,
        or, if no such thing present, to a special chain.
        We can clear the special chain when the last Dwarf_Debug
	is freed.

	Initial version is a hack.

	dwarf_alloc.c
	$Revision: 1.3 $   $Date: 1996/07/08 21:07:35 $
	

*/

#include <stdlib.h>
#include "dwarf_incl.h"
#include "bstring.h"

/*
	The allocator wants to know which region
	this is to be in so it can allocate the new space
	with respect to the right region.
*/
/*ARGSUSED*/
Dwarf_Ptr _dwarf_p_get_alloc (
    Dwarf_P_Debug 	dbg,
    Dwarf_Unsigned 	size
)
{
    void *sp;

    sp = malloc(size);
    bzero(sp,(int)size);
    return sp;
}


/*ARGSUSED*/
void dwarf_p_dealloc(void *space, Dwarf_Unsigned typ)
{
	free(space);
	return;
}


/* Essentially a stub for now. */
/*ARGSUSED*/
void
_dwarf_p_dealloc (
    Dwarf_P_Debug       dbg,
    Dwarf_Small         *ptr
)
{
    dwarf_p_dealloc(ptr,DW_DLA_STRING);
}
