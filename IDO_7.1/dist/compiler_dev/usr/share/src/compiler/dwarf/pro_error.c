/*

	pro_error.c



	$Revision: 1.1 $ $Date: 1993/07/19 22:34:15 $

*/

#include <libelf.h>
#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <stdlib.h>
#include "pro_incl.h"

extern char* _dwarf_errmsgs[];

/* 
    This function performs error handling as described in the 
    libdwarf consumer document section 3.  Dbg is the Dwarf_P_debug
    structure being processed.  Error is a pointer to the pointer
    to the error descriptor that will be returned.  Errval is an
    error code listed in dwarf_error.h.
*/
void
_dwarf_p_error (
    Dwarf_P_Debug     	dbg,
    Dwarf_Error     	*error,
    Dwarf_Word      	errval
)
{
    Dwarf_Error     errptr;

    /* Allow NULL dbg on entry, since sometimes that can happen and
       we want to report the upper-level error, not this one.
    */
    if ((Dwarf_Sword)errval < 0)
	printf("ERROR VALUE: %d - %s\n",errval, _dwarf_errmsgs[-errval-1]);
    if (error != NULL) {
        errptr = (Dwarf_Error)
	    _dwarf_p_get_alloc(dbg, sizeof(struct Dwarf_Error_s));
        if (errptr == NULL) {
            fprintf(stderr,"Could not allocate Dwarf_Error structure\n");
            abort();
        }
        errptr->er_errval = errval;
        *error = errptr;
        return;
    }

    if (dbg != NULL && dbg->de_errhand != NULL) {
        errptr = (Dwarf_Error)
	    _dwarf_p_get_alloc(dbg, sizeof(struct Dwarf_Error_s));
        if (errptr == NULL) {
            fprintf(stderr,"Could not allocate Dwarf_Error structure\n");
            abort();
        }
        errptr->er_errval = errval;
        dbg->de_errhand(errptr,dbg->de_errarg);
        return;
    }

    abort();
}

