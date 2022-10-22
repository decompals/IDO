#include <stdio.h>
#include "dwarf_incl.h"
#include "dwarf_funcs.h"

int
dwarf_get_funcs (
    Dwarf_Debug		dbg,
    Dwarf_Func		**funcs,
    Dwarf_Signed	* ret_func_count,
    Dwarf_Error		*error
)
{
	/* Sweeps the complete .debug_funcnames section. */
    Dwarf_Small			*funcnames_ptr;

    Dwarf_Word			length;

	/* 
	    Points to the context for the current set of function names,
	    and contains information to identify the compilation-unit
	    that the set refers to.
	*/
    Dwarf_Func_Context		funcnames_context;

        /* Version number for the current set of funcnames. */
    Dwarf_Half			version;

	/* 
	    Offset from the start of compilation-unit 
	    for the current function.
	*/
    Dwarf_Off			cu_offset;

	/* Counts the number of functions read. */
    Dwarf_Unsigned		func_count = 0;

	/* Points to the current function read. */
    Dwarf_Func			func;

	/* 
	    Used to chain the Dwarf_Func_s structs for creating
	    contiguous list of pointers to the structs.
	*/
    Dwarf_Chain			curr_chain, prev_chain, head_chain = NULL;

	/* Points to contiguous block of Dwarf_Func's to be returned. */
    Dwarf_Func			*ret_funcs;

	/* Temporary counter. */
    Dwarf_Unsigned		i;

    /* ***** BEGIN CODE ***** */

    if (dbg == NULL)  {
	 _dwarf_error(NULL, error, DW_DLE_DBG_NULL); 
	 return(DW_DLV_ERROR);
    }

    if (dbg->de_debug_funcnames == NULL) {
	return(DW_DLV_NO_ENTRY);
    }

    funcnames_ptr = dbg->de_debug_funcnames;
    do {
	funcnames_context = (Dwarf_Func_Context)
	    _dwarf_get_alloc(dbg, DW_DLA_FUNC_CONTEXT, 1);
	if (funcnames_context == NULL) {
	    _dwarf_error(dbg, error, DW_DLE_ALLOC_FAIL); 
	    return(DW_DLV_ERROR);
	}

	READ_UNALIGNED(length, funcnames_ptr, dbg->de_length_size);
	funcnames_ptr += dbg->de_length_size;
	funcnames_context->fu_length = length;


	READ_UNALIGNED(version, funcnames_ptr, sizeof(Dwarf_Half));
	funcnames_ptr += sizeof(Dwarf_Half);
	if (version != CURRENT_VERSION_STAMP) {
	    _dwarf_error(dbg, error, DW_DLE_DEBUG_FUNCNAMES_VERSION_ERROR);
	    return(DW_DLV_ERROR);
	}

	READ_UNALIGNED(funcnames_context->fu_info_offset, funcnames_ptr, 
	    dbg->de_length_size);
	funcnames_ptr += dbg->de_length_size;
	
	    /* 
		Add the length of the cu_header to point to the
		DW_TAG_compile_unit die.
	    */
	funcnames_context->fu_info_offset +=
	    dbg->de_length_size +	/* Size of cu length field. */
	    sizeof(Dwarf_Half) +	/* Size of version stamp field. */
	    dbg->de_length_size +	/* Size of abbrev offset field. */
	    sizeof(Dwarf_Small);	/* Size of address size field. */

	READ_UNALIGNED(funcnames_context->fu_info_length, funcnames_ptr, 
	    dbg->de_length_size);
	funcnames_ptr += dbg->de_length_size;

	if (funcnames_ptr > dbg->de_debug_funcnames +
	    dbg->de_debug_funcnames_size) {
	    _dwarf_error(dbg, error, DW_DLE_DEBUG_FUNCNAMES_LENGTH_BAD);
	    return(DW_DLV_ERROR);
	}

	READ_UNALIGNED(cu_offset, funcnames_ptr, dbg->de_length_size);
	funcnames_ptr += dbg->de_length_size;

	while (cu_offset != 0) {

            func = (Dwarf_Func)_dwarf_get_alloc(dbg, DW_DLA_FUNC, 1);
	    if (func == NULL) {
		_dwarf_error(dbg, error, DW_DLE_ALLOC_FAIL); 
		return(DW_DLV_ERROR);
	    }
	    func_count++;

	    func->fl_context = funcnames_context;

		/* Subtract length of CU header to adjust offsets. */
            func->fl_cu_offset = cu_offset -
		dbg->de_length_size -	/* Size of CU length field. */
		sizeof(Dwarf_Half) -	/* Size of version stamp field. */
		dbg->de_length_size -	/* Size of abbrev offset field. */
		sizeof(Dwarf_Small);	/* Size of address size field. */

	    func->fl_name = funcnames_ptr;
	    funcnames_ptr = funcnames_ptr + strlen((char *)funcnames_ptr) + 1;

	    READ_UNALIGNED(cu_offset, funcnames_ptr, dbg->de_length_size);
	    funcnames_ptr += dbg->de_length_size;

	    if (funcnames_ptr > dbg->de_debug_funcnames + 
		dbg->de_debug_funcnames_size) {
		_dwarf_error(dbg, error, DW_DLE_DEBUG_FUNCNAMES_LENGTH_BAD);
		return(DW_DLV_ERROR);
	    }

	    curr_chain = (Dwarf_Chain)_dwarf_get_alloc(dbg, DW_DLA_CHAIN, 1);
	    if (curr_chain == NULL) {
		_dwarf_error(dbg, error, DW_DLE_ALLOC_FAIL);
		return(DW_DLV_ERROR);
	    }

		/* Put current function on singly_linked list. */
	    curr_chain->ch_item = (Dwarf_Func)func;
	    if (head_chain == NULL)
		head_chain = prev_chain = curr_chain;
	    else {
		prev_chain->ch_next = curr_chain;
		prev_chain = curr_chain;
	    }
	}

    } while (funcnames_ptr < 
	dbg->de_debug_funcnames + dbg->de_debug_funcnames_size);
    
	/* Points to contiguous block of Dwarf_Func's. */
    ret_funcs = (Dwarf_Func *)
	_dwarf_get_alloc(dbg, DW_DLA_LIST, func_count);
    if (ret_funcs == NULL) 
	{_dwarf_error(dbg, error, DW_DLE_ALLOC_FAIL); return(DW_DLV_ERROR);}

	/* 
	    Store pointers to Dwarf_Func_s structs in
	    contiguous block, and deallocate the chain.
	*/
    curr_chain = head_chain;
    for (i = 0; i < func_count; i++) {
	*(ret_funcs + i) = curr_chain->ch_item;
	prev_chain = curr_chain;
	curr_chain = curr_chain->ch_next;
	dwarf_dealloc(dbg, prev_chain, DW_DLA_CHAIN);
    }

    *funcs = ret_funcs;
    *ret_func_count = (func_count);
    return DW_DLV_OK;
}


int
dwarf_funcname (
    Dwarf_Func		func,
    char         **     ret_name,
    Dwarf_Error		*error
)
{
    if (func == NULL)
	{_dwarf_error(NULL, error, DW_DLE_FUNC_NULL); return(DW_DLV_ERROR);}

    *ret_name = (char *)(func->fl_name);
    return DW_DLV_OK;
}

int
dwarf_func_die_offset (
    Dwarf_Func		func,
    Dwarf_Off      *    return_offset,
    Dwarf_Error		*error
)
{
    if (func == NULL) 
	{_dwarf_error(NULL, error, DW_DLE_FUNC_NULL); return(DW_DLV_ERROR);}

    if (func->fl_context == NULL) {
	_dwarf_error(NULL, error, DW_DLE_FUNC_CONTEXT_NULL);
	return(DW_DLV_ERROR);
    }

    *return_offset = (func->fl_cu_offset + func->fl_context->fu_info_offset);
    return DW_DLV_OK;
}


int
dwarf_func_cu_offset (
    Dwarf_Func		func,
    Dwarf_Off      *    return_offset,
    Dwarf_Error		*error
)
{
    if (func == NULL)
	{_dwarf_error(NULL, error, DW_DLE_FUNC_NULL); return(DW_DLV_ERROR);}

    if (func->fl_context == NULL) {
	_dwarf_error(NULL, error, DW_DLE_FUNC_CONTEXT_NULL);
	return(DW_DLV_ERROR);
    }

    *return_offset = (func->fl_context->fu_info_offset);
    return DW_DLV_OK;
}


int
dwarf_func_name_offsets (
    Dwarf_Func		func,
    char **              ret_func_name,
    Dwarf_Off		*die_offset,
    Dwarf_Off		*cu_offset,
    Dwarf_Error		*error
)
{
    if (func == NULL)
	{_dwarf_error(NULL, error, DW_DLE_FUNC_NULL); return(DW_DLV_ERROR);}

    if (func->fl_context == NULL) {
	_dwarf_error(NULL, error, DW_DLE_FUNC_CONTEXT_NULL); 
	return(DW_DLV_ERROR);
    }

    if (die_offset != NULL)
	*die_offset = func->fl_cu_offset + 
	    func->fl_context->fu_info_offset;

    if (cu_offset != NULL)
	*cu_offset = func->fl_context->fu_info_offset;

    *ret_func_name = (char *)(func->fl_name);
    return DW_DLV_OK;
}
