#include <stdio.h>
#include "dwarf_incl.h"
#include "dwarf_global.h"

int
dwarf_get_globals (
    Dwarf_Debug		dbg,
    Dwarf_Global	**globals,
    Dwarf_Signed       *return_count,
    Dwarf_Error		*error
)
{
	/* Sweeps the complete .debug_pubnames section. */
    Dwarf_Small			*pubnames_ptr;

    Dwarf_Unsigned		length;

	/* 
	    Points to the context for the current set of global names,
	    and contains information to identify the compilation-unit
	    that the set refers to.
	*/
    Dwarf_Global_Context	pubnames_context;

        /* Version number for the current set of pubnames. */
    Dwarf_Half			version;

	/* 
	    Offset from the start of compilation-unit 
	    for the current global.
	*/
    Dwarf_Off			cu_offset;

	/* Counts the number of globals read. */
    Dwarf_Unsigned		global_count = 0;

	/* Points to the current global read. */
    Dwarf_Global		global;

	/* 
	    Used to chain the Dwarf_Global_s structs for creating
	    contiguous list of pointers to the structs.
	*/
    Dwarf_Chain			curr_chain, prev_chain, head_chain = NULL;

	/* Points to contiguous block of Dwarf_Global's to be returned. */
    Dwarf_Global		*ret_globals;

	/* Temporary counter. */
    Dwarf_Unsigned		i;

    /* ***** BEGIN CODE ***** */


    if (dbg == NULL) {
	_dwarf_error(NULL, error, DW_DLE_DBG_NULL); 
	return(DW_DLV_ERROR);
    }

    if (dbg->de_debug_pubnames == NULL) {
	return(DW_DLV_NO_ENTRY);
    }

    pubnames_ptr = dbg->de_debug_pubnames;
    do {
	pubnames_context = (Dwarf_Global_Context)
	    _dwarf_get_alloc(dbg, DW_DLA_GLOBAL_CONTEXT, 1);
	if (pubnames_context == NULL) {
	    _dwarf_error(dbg, error, DW_DLE_ALLOC_FAIL); 
	    return(DW_DLV_ERROR);
	}

	READ_UNALIGNED(length, pubnames_ptr, dbg->de_length_size);
	pubnames_ptr += dbg->de_length_size;
	pubnames_context->pu_length = length;


	READ_UNALIGNED(version, pubnames_ptr, sizeof(Dwarf_Half));
	pubnames_ptr += sizeof(Dwarf_Half);
	if (version != CURRENT_VERSION_STAMP) {
	    _dwarf_error(dbg, error, DW_DLE_PUBNAMES_VERSION_ERROR);
	    return(DW_DLV_ERROR);
	}

	READ_UNALIGNED(pubnames_context->pu_info_offset, pubnames_ptr, 
	    dbg->de_length_size);
	pubnames_ptr += dbg->de_length_size;
	
	    /* 
		Add the length of the cu_header to point to the
		DW_TAG_compile_unit die.
	    */
	pubnames_context->pu_info_offset +=
	    dbg->de_length_size +	/* Size of cu length field. */
	    sizeof(Dwarf_Half) +	/* Size of version stamp field. */
	    dbg->de_length_size +	/* Size of abbrev offset field. */
	    sizeof(Dwarf_Small);	/* Size of address size field. */

	READ_UNALIGNED(pubnames_context->pu_info_length, pubnames_ptr, 
	    dbg->de_length_size);
	pubnames_ptr += dbg->de_length_size;

	if (pubnames_ptr > dbg->de_debug_pubnames +
	    dbg->de_debug_pubnames_size) {
	    _dwarf_error(dbg, error, DW_DLE_PUBNAMES_LENGTH_BAD);
	    return(DW_DLV_ERROR);
	}

	READ_UNALIGNED(cu_offset, pubnames_ptr, dbg->de_length_size);
	pubnames_ptr += dbg->de_length_size;

	while (cu_offset != 0) {

            global = (Dwarf_Global)_dwarf_get_alloc(dbg, DW_DLA_GLOBAL, 1);
	    if (global == NULL) {
		_dwarf_error(dbg, error, DW_DLE_ALLOC_FAIL); 
		return(DW_DLV_ERROR);
	    }
	    global_count++;

	    global->gl_context = pubnames_context;

		/* Subtract length of CU header to adjust offsets. */
            global->gl_cu_offset = cu_offset -
		dbg->de_length_size -	/* Size of CU length field. */
		sizeof(Dwarf_Half) -	/* Size of version stamp field. */
		dbg->de_length_size -	/* Size of abbrev offset field. */
		sizeof(Dwarf_Small);	/* Size of address size field. */

	    global->gl_name = pubnames_ptr;
	    pubnames_ptr = pubnames_ptr + strlen((char *)pubnames_ptr) + 1;

	    READ_UNALIGNED(cu_offset, pubnames_ptr, dbg->de_length_size);
	    pubnames_ptr += dbg->de_length_size;

	    if (pubnames_ptr > dbg->de_debug_pubnames + 
		dbg->de_debug_pubnames_size) {
		_dwarf_error(dbg, error, DW_DLE_PUBNAMES_LENGTH_BAD);
		return(DW_DLV_ERROR);
	    }

	    curr_chain = (Dwarf_Chain)_dwarf_get_alloc(dbg, DW_DLA_CHAIN, 1);
	    if (curr_chain == NULL) {
		_dwarf_error(dbg, error, DW_DLE_ALLOC_FAIL);
		return(DW_DLV_ERROR);
	    }

		/* Put current global on singly_linked list. */
	    curr_chain->ch_item = (Dwarf_Global)global;
	    if (head_chain == NULL)
		head_chain = prev_chain = curr_chain;
	    else {
		prev_chain->ch_next = curr_chain;
		prev_chain = curr_chain;
	    }
	}

    } while 
	(pubnames_ptr < dbg->de_debug_pubnames + dbg->de_debug_pubnames_size);
    
	/* Points to contiguous block of Dwarf_Global's. */
    ret_globals = (Dwarf_Global *)
	_dwarf_get_alloc(dbg, DW_DLA_LIST, global_count);
    if (ret_globals == NULL) 
	{_dwarf_error(dbg, error, DW_DLE_ALLOC_FAIL); return(DW_DLV_ERROR);}

	/* 
	    Store pointers to Dwarf_Global_s structs in
	    contiguous block, and deallocate the chain.
	*/
    curr_chain = head_chain;
    for (i = 0; i < global_count; i++) {
	*(ret_globals + i) = curr_chain->ch_item;
	prev_chain = curr_chain;
	curr_chain = curr_chain->ch_next;
	dwarf_dealloc(dbg, prev_chain, DW_DLA_CHAIN);
    }

    *globals = ret_globals;
    *return_count = (global_count);
    return DW_DLV_OK;
}


int
dwarf_globname (
    Dwarf_Global	glob,
    char **             ret_name,
    Dwarf_Error		*error
)
{
    if (glob == NULL)
	{_dwarf_error(NULL, error, DW_DLE_GLOBAL_NULL); return(DW_DLV_ERROR);}

    *ret_name = (char *)(glob->gl_name);
    return DW_DLV_OK;
}


int
dwarf_global_die_offset (
    Dwarf_Global	global,
    Dwarf_Off          *ret_off,
    Dwarf_Error		*error
)
{
    if (global == NULL) {
	_dwarf_error(NULL, error, DW_DLE_GLOBAL_NULL); 
	return(DW_DLV_ERROR);
    }

    if (global->gl_context == NULL) {
	_dwarf_error(NULL, error, DW_DLE_GLOBAL_CONTEXT_NULL);
	return(DW_DLV_ERROR);
    }

    *ret_off = (global->gl_cu_offset + global->gl_context->pu_info_offset);
    return DW_DLV_OK;
}


int
dwarf_global_cu_offset (
    Dwarf_Global	global,
    Dwarf_Off          *ret_off,
    Dwarf_Error		*error
)
{
    if (global == NULL) {
	_dwarf_error(NULL, error, DW_DLE_GLOBAL_NULL); 
	return(DW_DLV_ERROR);
    }

    if (global->gl_context == NULL) {
	_dwarf_error(NULL, error, DW_DLE_GLOBAL_CONTEXT_NULL);
	return(DW_DLV_ERROR);
    }

    *ret_off = (global->gl_context->pu_info_offset);
    return DW_DLV_OK;
}

int
dwarf_global_name_offsets (
    Dwarf_Global	global,
    char               **ret_name,
    Dwarf_Off		*die_offset,
    Dwarf_Off		*cu_offset,
    Dwarf_Error		*error
)
{
    if (global == NULL)
	{_dwarf_error(NULL, error, DW_DLE_GLOBAL_NULL); return(DW_DLV_ERROR);}

    if (global->gl_context == NULL) 
	{_dwarf_error(NULL, error, DW_DLE_GLOBAL_CONTEXT_NULL); 
	return(DW_DLV_ERROR);}

    if (die_offset != NULL)
	*die_offset = global->gl_cu_offset + 
	    global->gl_context->pu_info_offset;

    if (cu_offset != NULL)
	*cu_offset = global->gl_context->pu_info_offset;

    *ret_name = (char *)global->gl_name;
    return DW_DLV_OK;
}

