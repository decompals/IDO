#include <stdio.h>
#include "dwarf_incl.h"
#include "dwarf_weaks.h"

int
dwarf_get_weaks (
    Dwarf_Debug		dbg,
    Dwarf_Weak		**weaks,
    Dwarf_Signed        *ret_weak_count,
    Dwarf_Error		*error
)
{
	/* Sweeps the complete .debug_weaknames section. */
    Dwarf_Small			*weaknames_ptr;

    Dwarf_Word			length;

	/* 
	    Points to the context for the current set of weak names,
	    and contains information to identify the compilation-unit
	    that the set refers to.
	*/
    Dwarf_Weak_Context		weaknames_context;

        /* Version number for the current set of weak names. */
    Dwarf_Half			version;

	/* 
	    Offset from the start of compilation-unit 
	    for the current weak name.
	*/
    Dwarf_Off			cu_offset;

	/* Counts the number of weak names read. */
    Dwarf_Unsigned		weak_count = 0;

	/* Points to the current weak name read. */
    Dwarf_Weak			weak;

	/* 
	    Used to chain the Dwarf_Weak_s structs for creating
	    contiguous list of pointers to the structs.
	*/
    Dwarf_Chain			curr_chain, prev_chain, head_chain = NULL;

	/* Points to contiguous block of Dwarf_Weak's to be returned. */
    Dwarf_Weak			*ret_weaks;

	/* Temporary counter. */
    Dwarf_Unsigned		i;

    /* ***** BEGIN CODE ***** */

    if (dbg == NULL) 
	{_dwarf_error(NULL, error, DW_DLE_DBG_NULL); return(DW_DLV_ERROR);}

    if (dbg->de_debug_weaknames == NULL) {
	return(DW_DLV_NO_ENTRY);
    }

    weaknames_ptr = dbg->de_debug_weaknames;
    do {
	weaknames_context = (Dwarf_Weak_Context)
	    _dwarf_get_alloc(dbg, DW_DLA_WEAK_CONTEXT, 1);
	if (weaknames_context == NULL) {
	    _dwarf_error(dbg, error, DW_DLE_ALLOC_FAIL); 
	    return(DW_DLV_ERROR);
        }

	READ_UNALIGNED(length, weaknames_ptr, dbg->de_length_size);
	weaknames_ptr += dbg->de_length_size;
	weaknames_context->wk_length = length;


	READ_UNALIGNED(version, weaknames_ptr, sizeof(Dwarf_Half));
	weaknames_ptr += sizeof(Dwarf_Half);
	if (version != CURRENT_VERSION_STAMP) {
	    _dwarf_error(dbg, error, DW_DLE_DEBUG_WEAKNAMES_VERSION_ERROR);
	    return(DW_DLV_ERROR);
	}

	READ_UNALIGNED(weaknames_context->wk_info_offset, weaknames_ptr, 
	    dbg->de_length_size);
	weaknames_ptr += dbg->de_length_size;
	
	    /* 
		Add the length of the cu_header to point to the
		DW_TAG_compile_unit die.
	    */
	weaknames_context->wk_info_offset +=
	    dbg->de_length_size +	/* Size of cu length field. */
	    sizeof(Dwarf_Half) +	/* Size of version stamp field. */
	    dbg->de_length_size +	/* Size of abbrev offset field. */
	    sizeof(Dwarf_Small);	/* Size of address size field. */

	READ_UNALIGNED(weaknames_context->wk_info_length, weaknames_ptr, 
	    dbg->de_length_size);
	weaknames_ptr += dbg->de_length_size;

	if (weaknames_ptr > dbg->de_debug_weaknames +
	    dbg->de_debug_weaknames_size) {
	    _dwarf_error(dbg, error, DW_DLE_DEBUG_WEAKNAMES_LENGTH_BAD);
	    return(DW_DLV_ERROR);
	}

	READ_UNALIGNED(cu_offset, weaknames_ptr, dbg->de_length_size);
	weaknames_ptr += dbg->de_length_size;

	while (cu_offset != 0) {

            weak = (Dwarf_Weak)_dwarf_get_alloc(dbg, DW_DLA_WEAK, 1);
	    if (weak == NULL) {
		_dwarf_error(dbg, error, DW_DLE_ALLOC_FAIL); 
		return(DW_DLV_ERROR);
	    }
	    weak_count++;

	    weak->we_context = weaknames_context;

		/* Subtract length of CU header to adjust offsets. */
            weak->we_cu_offset = cu_offset -
		dbg->de_length_size -	/* Size of CU length field. */
		sizeof(Dwarf_Half) -	/* Size of version stamp field. */
		dbg->de_length_size -	/* Size of abbrev offset field. */
		sizeof(Dwarf_Small);	/* Size of address size field. */

	    weak->we_name = weaknames_ptr;
	    weaknames_ptr = weaknames_ptr + strlen((char *)weaknames_ptr) + 1;

	    READ_UNALIGNED(cu_offset, weaknames_ptr, dbg->de_length_size);
	    weaknames_ptr += dbg->de_length_size;

	    if (weaknames_ptr > dbg->de_debug_weaknames + 
		dbg->de_debug_weaknames_size) {
		_dwarf_error(dbg, error, DW_DLE_DEBUG_WEAKNAMES_LENGTH_BAD);
		return(DW_DLV_ERROR);
	    }

	    curr_chain = (Dwarf_Chain)_dwarf_get_alloc(dbg, DW_DLA_CHAIN, 1);
	    if (curr_chain == NULL) {
		_dwarf_error(dbg, error, DW_DLE_ALLOC_FAIL);
		return(DW_DLV_ERROR);
	    }

		/* Put current weak name on singly_linked list. */
	    curr_chain->ch_item = (Dwarf_Weak)weak;
	    if (head_chain == NULL)
		head_chain = prev_chain = curr_chain;
	    else {
		prev_chain->ch_next = curr_chain;
		prev_chain = curr_chain;
	    }
	}

    } while (weaknames_ptr < 
	dbg->de_debug_weaknames + dbg->de_debug_weaknames_size);
    
	/* Points to contiguous block of Dwarf_Weak's. */
    ret_weaks = (Dwarf_Weak *)
	_dwarf_get_alloc(dbg, DW_DLA_LIST, weak_count);
    if (ret_weaks == NULL) {
	_dwarf_error(dbg, error, DW_DLE_ALLOC_FAIL); 
	return(DW_DLV_ERROR);
    }

	/* 
	    Store pointers to Dwarf_Weak_s structs in
	    contiguous block, and deallocate the chain.
	*/
    curr_chain = head_chain;
    for (i = 0; i < weak_count; i++) {
	*(ret_weaks + i) = curr_chain->ch_item;
	prev_chain = curr_chain;
	curr_chain = curr_chain->ch_next;
	dwarf_dealloc(dbg, prev_chain, DW_DLA_CHAIN);
    }

    *weaks = ret_weaks;
    *ret_weak_count = (weak_count);
    return DW_DLV_OK;
}


int
dwarf_weakname (
    Dwarf_Weak		weak,
    char **	     ret_name,
    Dwarf_Error		*error
)
{
    if (weak == NULL) {
	_dwarf_error(NULL, error, DW_DLE_WEAK_NULL); 
	return(DW_DLV_ERROR);
    }

    *ret_name = (char *)(weak->we_name);
    return DW_DLV_OK;
}


int
dwarf_weak_die_offset (
    Dwarf_Weak		weak,
    Dwarf_Off          *weak_off,
    Dwarf_Error		*error
)
{
    if (weak == NULL) 
	{_dwarf_error(NULL, error, DW_DLE_WEAK_NULL); 
	return(DW_DLV_ERROR);}

    if (weak->we_context == NULL) {
	_dwarf_error(NULL, error, DW_DLE_WEAK_CONTEXT_NULL);
	return(DW_DLV_ERROR);
    }

    *weak_off = (weak->we_cu_offset + weak->we_context->wk_info_offset);
    return DW_DLV_OK;
}


int
dwarf_weak_cu_offset (
    Dwarf_Weak		weak,
    Dwarf_Off          *weak_off,
    Dwarf_Error		*error
)
{
    if (weak == NULL)
	{_dwarf_error(NULL, error, DW_DLE_WEAK_NULL); return(DW_DLV_ERROR);}

    if (weak->we_context == NULL) {
	_dwarf_error(NULL, error, DW_DLE_WEAK_CONTEXT_NULL);
	return(DW_DLV_ERROR);
    }

    *weak_off = (weak->we_context->wk_info_offset);
    return DW_DLV_OK;
}


int
dwarf_weak_name_offsets (
    Dwarf_Weak		weak,
    char    **          weak_name,
    Dwarf_Off		*die_offset,
    Dwarf_Off		*cu_offset,
    Dwarf_Error		*error
)
{
    if (weak == NULL)
	{_dwarf_error(NULL, error, DW_DLE_WEAK_NULL); return(DW_DLV_ERROR);}

    if (weak->we_context == NULL) 
	{_dwarf_error(NULL, error, DW_DLE_WEAK_CONTEXT_NULL); 
	return(DW_DLV_ERROR);}

    if (die_offset != NULL)
	*die_offset = weak->we_cu_offset + 
	    weak->we_context->wk_info_offset;

    if (cu_offset != NULL)
	*cu_offset = weak->we_context->wk_info_offset;

    *weak_name = (char *)(weak->we_name);
    return DW_DLV_OK;
}
