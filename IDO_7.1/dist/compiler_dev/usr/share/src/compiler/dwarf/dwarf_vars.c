#include <stdio.h>
#include "dwarf_incl.h"
#include "dwarf_vars.h"

int
dwarf_get_vars (
    Dwarf_Debug		dbg,
    Dwarf_Var		**vars,
    Dwarf_Signed       * ret_var_count,
    Dwarf_Error		*error
)
{
	/* Sweeps the complete .debug_varnames section. */
    Dwarf_Small			*varnames_ptr;

    Dwarf_Word			length;

	/* 
	    Points to the context for the current set of variable names,
	    and contains information to identify the compilation-unit
	    that the set refers to.
	*/
    Dwarf_Var_Context		varnames_context;

        /* Version number for the current set of variable names. */
    Dwarf_Half			version;

	/* 
	    Offset from the start of compilation-unit 
	    for the current variable .
	*/
    Dwarf_Off			cu_offset;

	/* Counts the number of variables read. */
    Dwarf_Unsigned		var_count = 0;

	/* Points to the current variable read. */
    Dwarf_Var			var;

	/* 
	    Used to chain the Dwarf_Var_s structs for creating
	    contiguous list of pointers to the structs.
	*/
    Dwarf_Chain			curr_chain, prev_chain, head_chain = NULL;

	/* Points to contiguous block of Dwarf_Var's to be returned. */
    Dwarf_Var			*ret_vars;

	/* Temporary counter. */
    Dwarf_Unsigned		i;

    /* ***** BEGIN CODE ***** */


    if (dbg == NULL) 
	{_dwarf_error(NULL, error, DW_DLE_DBG_NULL); return(DW_DLV_ERROR);}

    if (dbg->de_debug_varnames == NULL) {
	return(DW_DLV_NO_ENTRY);
    }

    varnames_ptr = dbg->de_debug_varnames;
    do {
	varnames_context = (Dwarf_Var_Context)
	    _dwarf_get_alloc(dbg, DW_DLA_VAR_CONTEXT, 1);
	if (varnames_context == NULL) {
	    _dwarf_error(dbg, error, DW_DLE_ALLOC_FAIL); 
	    return(DW_DLV_ERROR);
        }

	READ_UNALIGNED(length, varnames_ptr, dbg->de_length_size);
	varnames_ptr += dbg->de_length_size;
	varnames_context->vr_length = length;


	READ_UNALIGNED(version, varnames_ptr, sizeof(Dwarf_Half));
	varnames_ptr += sizeof(Dwarf_Half);
	if (version != CURRENT_VERSION_STAMP) {
	    _dwarf_error(dbg, error, DW_DLE_DEBUG_VARNAMES_VERSION_ERROR);
	    return(DW_DLV_ERROR);
	}

	READ_UNALIGNED(varnames_context->vr_info_offset, varnames_ptr, 
	    dbg->de_length_size);
	varnames_ptr += dbg->de_length_size;
	
	    /* 
		Add the length of the cu_header to point to the
		DW_TAG_compile_unit die.
	    */
	varnames_context->vr_info_offset +=
	    dbg->de_length_size +	/* Size of cu length field. */
	    sizeof(Dwarf_Half) +	/* Size of version stamp field. */
	    dbg->de_length_size +	/* Size of abbrev offset field. */
	    sizeof(Dwarf_Small);	/* Size of address size field. */

	READ_UNALIGNED(varnames_context->vr_info_length, varnames_ptr, 
	    dbg->de_length_size);
	varnames_ptr += dbg->de_length_size;

	if (varnames_ptr > dbg->de_debug_varnames +
	    dbg->de_debug_varnames_size) {
	    _dwarf_error(dbg, error, DW_DLE_DEBUG_VARNAMES_LENGTH_BAD);
	    return(DW_DLV_ERROR);
	}

	READ_UNALIGNED(cu_offset, varnames_ptr, dbg->de_length_size);
	varnames_ptr += dbg->de_length_size;

	while (cu_offset != 0) {

            var = (Dwarf_Var)_dwarf_get_alloc(dbg, DW_DLA_VAR, 1);
	    if (var == NULL) {
		_dwarf_error(dbg, error, DW_DLE_ALLOC_FAIL); 
		return(DW_DLV_ERROR);
	    }
	    var_count++;

	    var->va_context = varnames_context;

		/* Subtract length of CU header to adjust offsets. */
            var->va_cu_offset = cu_offset -
		dbg->de_length_size -	/* Size of CU length field. */
		sizeof(Dwarf_Half) -	/* Size of version stamp field. */
		dbg->de_length_size -	/* Size of abbrev offset field. */
		sizeof(Dwarf_Small);	/* Size of address size field. */

	    var->va_name = varnames_ptr;
	    varnames_ptr = varnames_ptr + strlen((char *)varnames_ptr) + 1;

	    READ_UNALIGNED(cu_offset, varnames_ptr, dbg->de_length_size);
	    varnames_ptr += dbg->de_length_size;

	    if (varnames_ptr > dbg->de_debug_varnames + 
		dbg->de_debug_varnames_size) {
		_dwarf_error(dbg, error, DW_DLE_DEBUG_VARNAMES_LENGTH_BAD);
		return(DW_DLV_ERROR);
	    }

	    curr_chain = (Dwarf_Chain)_dwarf_get_alloc(dbg, DW_DLA_CHAIN, 1);
	    if (curr_chain == NULL) {
		_dwarf_error(dbg, error, DW_DLE_ALLOC_FAIL);
		return(DW_DLV_ERROR);
	    }

		/* Put current variable on singly_linked list. */
	    curr_chain->ch_item = (Dwarf_Var)var;
	    if (head_chain == NULL)
		head_chain = prev_chain = curr_chain;
	    else {
		prev_chain->ch_next = curr_chain;
		prev_chain = curr_chain;
	    }
	}

    } while (varnames_ptr < 
	dbg->de_debug_varnames + dbg->de_debug_varnames_size);
    
	/* Points to contiguous block of Dwarf_Var's. */
    ret_vars = (Dwarf_Var *)
	_dwarf_get_alloc(dbg, DW_DLA_LIST, var_count);
    if (ret_vars == NULL) 
	{_dwarf_error(dbg, error, DW_DLE_ALLOC_FAIL); return(DW_DLV_ERROR);}

	/* 
	    Store pointers to Dwarf_Var_s structs in
	    contiguous block, and deallocate the chain.
	*/
    curr_chain = head_chain;
    for (i = 0; i < var_count; i++) {
	*(ret_vars + i) = curr_chain->ch_item;
	prev_chain = curr_chain;
	curr_chain = curr_chain->ch_next;
	dwarf_dealloc(dbg, prev_chain, DW_DLA_CHAIN);
    }

    *vars = ret_vars;
    *ret_var_count = (var_count);
    return DW_DLV_OK;
}


int
dwarf_varname (
    Dwarf_Var		var,
    char      **        ret_varname,
    Dwarf_Error		*error
)
{
    if (var == NULL)
	{_dwarf_error(NULL, error, DW_DLE_VAR_NULL); return(DW_DLV_ERROR);}

    *ret_varname = (char *)(var->va_name);
    return DW_DLV_OK;
}


int
dwarf_var_die_offset (
    Dwarf_Var		var,
    Dwarf_Off	*       returned_offset,
    Dwarf_Error		*error
)
{
    if (var == NULL) {
 	_dwarf_error(NULL, error, DW_DLE_VAR_NULL); 
	return(DW_DLV_ERROR);
    }

    if (var->va_context == NULL) {
	_dwarf_error(NULL, error, DW_DLE_VAR_CONTEXT_NULL);
	return(DW_DLV_ERROR);
    }

    *returned_offset = (var->va_cu_offset + var->va_context->vr_info_offset);
    return DW_DLV_OK;
}


int
dwarf_var_cu_offset (
    Dwarf_Var		var,
    Dwarf_Off	*       returned_offset,
    Dwarf_Error		*error
)
{
    if (var == NULL) {
	_dwarf_error(NULL, error, DW_DLE_VAR_NULL); 
	return(DW_DLV_ERROR);
    }

    if (var->va_context == NULL) {
	_dwarf_error(NULL, error, DW_DLE_VAR_CONTEXT_NULL);
	return(DW_DLV_ERROR);
    }

    *returned_offset = (var->va_context->vr_info_offset);
    return DW_DLV_OK;
}


int
dwarf_var_name_offsets (
    Dwarf_Var		var,
    char              **returned_name,
    Dwarf_Off		*die_offset,
    Dwarf_Off		*cu_offset,
    Dwarf_Error		*error
)
{
    if (var == NULL) {
	_dwarf_error(NULL, error, DW_DLE_VAR_NULL); 
	return(DW_DLV_ERROR);
    }

    if (var->va_context == NULL) {
	_dwarf_error(NULL, error, DW_DLE_VAR_CONTEXT_NULL); 
	return(DW_DLV_ERROR);
    }

    if (die_offset != NULL)
	*die_offset = var->va_cu_offset + 
	    var->va_context->vr_info_offset;

    if (cu_offset != NULL)
	*cu_offset = var->va_context->vr_info_offset;

    *returned_name = (char *)(var->va_name);
    return DW_DLV_OK;
}
