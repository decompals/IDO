/*

	pro_error.h

	Macros for handling errors.   Made into macros since
	these are often used and may need to change.

	User must supply a trailing ;  as in normal statements
	Made as comma expresions so they can be added to an
	if or else or between them without causing any surprises.

	$Revision: 1.2 $     $Date: 1993/08/16 23:12:16 $
*/


/* Handle error passing in the name of the Dwarf_P_Debug
   User must supply {} around the macro.
   Putting the {} here leads to macro uses that don't look like C.
   The error argument to dwarf_error is hard coded here as 'error'
*/
#define DWARF_P_DBG_ERROR(dbg,errval,retval) \
     _dwarf_p_error(dbg,error,errval); return(retval);

struct Dwarf_Error_s {
    Dwarf_Sword         er_errval;
};

void _dwarf_p_error(Dwarf_P_Debug dbg, Dwarf_Error *error, Dwarf_Word  errval);
