#include <stdio.h>
#include "dwarf_incl.h"
#include "dwarf_types.h"

int
dwarf_get_types (
    Dwarf_Debug		dbg,
    Dwarf_Type		**types,
    Dwarf_Signed	*ret_type_count,
    Dwarf_Error		*error
)
{
	/* Sweeps the complete .debug_typenames section. */
    Dwarf_Small			*typenames_ptr;

    Dwarf_Word			length;

	/* 
	    Points to the context for the current set of type names,
	    and contains information to identify the compilation-unit
	    that the set refers to.
	*/
    Dwarf_Type_Context		typenames_context;

        /* Version number for the current set of typenames. */
    Dwarf_Half			version;

	/* 
	    Offset from the start of compilation-unit 
	    for the current type.
	*/
    Dwarf_Off			cu_offset;

	/* Counts the number of types read. */
    Dwarf_Unsigned		type_count = 0;

	/* Points to the current type read. */
    Dwarf_Type			type;

	/* 
	    Used to chain the Dwarf_Type_s structs for creating
	    contiguous list of pointers to the structs.
	*/
    Dwarf_Chain			curr_chain, prev_chain, head_chain = NULL;

	/* Points to contiguous block of Dwarf_Type's to be returned. */
    Dwarf_Type			*ret_types;

	/* Temporary counter. */
    Dwarf_Unsigned		i;

    /* ***** BEGIN CODE ***** */

    if (dbg == NULL)  {
	_dwarf_error(NULL, error, DW_DLE_DBG_NULL); 
	return(DW_DLV_ERROR);
    }

    if (dbg->de_debug_typenames == NULL) {
	return(DW_DLV_NO_ENTRY);
    }

    typenames_ptr = dbg->de_debug_typenames;
    do {
	typenames_context = (Dwarf_Type_Context)
	    _dwarf_get_alloc(dbg, DW_DLA_TYPENAME_CONTEXT, 1);
	if (typenames_context == NULL) {
	    _dwarf_error(dbg, error, DW_DLE_ALLOC_FAIL); 
	    return(DW_DLV_ERROR);
	}

	READ_UNALIGNED(length, typenames_ptr, dbg->de_length_size);
	typenames_ptr += dbg->de_length_size;
	typenames_context->tp_length = length;


	READ_UNALIGNED(version, typenames_ptr, sizeof(Dwarf_Half));
	typenames_ptr += sizeof(Dwarf_Half);
	if (version != CURRENT_VERSION_STAMP) {
	    _dwarf_error(dbg, error, DW_DLE_DEBUG_TYPENAMES_VERSION_ERROR);
	    return(DW_DLV_ERROR);
	}

	READ_UNALIGNED(typenames_context->tp_info_offset, typenames_ptr, 
	    dbg->de_length_size);
	typenames_ptr += dbg->de_length_size;
	
	    /* 
		Add the length of the cu_header to point to the
		DW_TAG_compile_unit die.
	    */
	typenames_context->tp_info_offset +=
	    dbg->de_length_size +	/* Size of cu length field. */
	    sizeof(Dwarf_Half) +	/* Size of version stamp field. */
	    dbg->de_length_size +	/* Size of abbrev offset field. */
	    sizeof(Dwarf_Small);	/* Size of address size field. */

	READ_UNALIGNED(typenames_context->tp_info_length, typenames_ptr, 
	    dbg->de_length_size);
	typenames_ptr += dbg->de_length_size;

	if (typenames_ptr > dbg->de_debug_typenames +
	    dbg->de_debug_typenames_size) {
	    _dwarf_error(dbg, error, DW_DLE_DEBUG_TYPENAMES_LENGTH_BAD);
	    return(DW_DLV_ERROR);
	}

	READ_UNALIGNED(cu_offset, typenames_ptr, dbg->de_length_size);
	typenames_ptr += dbg->de_length_size;

	while (cu_offset != 0) {

            type = (Dwarf_Type)_dwarf_get_alloc(dbg, DW_DLA_TYPENAME, 1);
	    if (type == NULL) {
		_dwarf_error(dbg, error, DW_DLE_ALLOC_FAIL); 
		return(DW_DLV_ERROR);
	    }
	    type_count++;

	    type->ty_context = typenames_context;

		/* Subtract length of CU header to adjust offsets. */
            type->ty_cu_offset = cu_offset -
		dbg->de_length_size -	/* Size of CU length field. */
		sizeof(Dwarf_Half) -	/* Size of version stamp field. */
		dbg->de_length_size -	/* Size of abbrev offset field. */
		sizeof(Dwarf_Small);	/* Size of address size field. */

	    type->ty_name = typenames_ptr;
	    typenames_ptr = typenames_ptr + strlen((char *)typenames_ptr) + 1;

	    READ_UNALIGNED(cu_offset, typenames_ptr, dbg->de_length_size);
	    typenames_ptr += dbg->de_length_size;

	    if (typenames_ptr > dbg->de_debug_typenames + 
		dbg->de_debug_typenames_size) {
		_dwarf_error(dbg, error, DW_DLE_DEBUG_TYPENAMES_LENGTH_BAD);
		return(DW_DLV_ERROR);
	    }

	    curr_chain = (Dwarf_Chain)_dwarf_get_alloc(dbg, DW_DLA_CHAIN, 1);
	    if (curr_chain == NULL) {
		_dwarf_error(dbg, error, DW_DLE_ALLOC_FAIL);
		return(DW_DLV_ERROR);
	    }

		/* Put current type on singly_linked list. */
	    curr_chain->ch_item = (Dwarf_Type)type;
	    if (head_chain == NULL)
		head_chain = prev_chain = curr_chain;
	    else {
		prev_chain->ch_next = curr_chain;
		prev_chain = curr_chain;
	    }
	}

    } while (typenames_ptr < 
	dbg->de_debug_typenames + dbg->de_debug_typenames_size);
    
	/* Points to contiguous block of Dwarf_Type's. */
    ret_types = (Dwarf_Type *)
	_dwarf_get_alloc(dbg, DW_DLA_LIST, type_count);
    if (ret_types == NULL) 
	{_dwarf_error(dbg, error, DW_DLE_ALLOC_FAIL); return(DW_DLV_ERROR);}

	/* 
	    Store pointers to Dwarf_Type_s structs in
	    contiguous block, and deallocate the chain.
	*/
    curr_chain = head_chain;
    for (i = 0; i < type_count; i++) {
	*(ret_types + i) = curr_chain->ch_item;
	prev_chain = curr_chain;
	curr_chain = curr_chain->ch_next;
	dwarf_dealloc(dbg, prev_chain, DW_DLA_CHAIN);
    }

    *types = ret_types;
    *ret_type_count = (type_count);
    return DW_DLV_OK;
}


int
dwarf_typename (
    Dwarf_Type		type,
    char            **  ret_name,
    Dwarf_Error		*error
)
{
    if (type == NULL)
	{_dwarf_error(NULL, error, DW_DLE_TYPE_NULL); return(DW_DLV_ERROR);}

    *ret_name = (char *)(type->ty_name);
    return DW_DLV_OK;
}


int
dwarf_type_die_offset (
    Dwarf_Type		type,
    Dwarf_Off        *  ret_offset,
    Dwarf_Error		*error
)
{
    if (type == NULL) 
	{_dwarf_error(NULL, error, DW_DLE_TYPE_NULL); return(DW_DLV_ERROR);}

    if (type->ty_context == NULL) {
	_dwarf_error(NULL, error, DW_DLE_TYPE_CONTEXT_NULL);
	return(DW_DLV_ERROR);
    }

    *ret_offset = (type->ty_cu_offset + type->ty_context->tp_info_offset);
    return DW_DLV_OK;
}


int
dwarf_type_cu_offset (
    Dwarf_Type		type,
    Dwarf_Off        *  ret_offset,
    Dwarf_Error		*error
)
{
    if (type == NULL)
	{_dwarf_error(NULL, error, DW_DLE_TYPE_NULL); return(DW_DLV_ERROR);}

    if (type->ty_context == NULL) {
	_dwarf_error(NULL, error, DW_DLE_TYPE_CONTEXT_NULL);
	return(DW_DLV_ERROR);
    }

    *ret_offset = (type->ty_context->tp_info_offset);
    return DW_DLV_OK;
    
}


int
dwarf_type_name_offsets (
    Dwarf_Type		type,
    char            **  returned_name,
    Dwarf_Off		*die_offset,
    Dwarf_Off		*cu_offset,
    Dwarf_Error		*error
)
{
    if (type == NULL)
	{_dwarf_error(NULL, error, DW_DLE_TYPE_NULL); return(DW_DLV_ERROR);}

    if (type->ty_context == NULL)  {
	_dwarf_error(NULL, error, DW_DLE_TYPE_CONTEXT_NULL); 
	return(DW_DLV_ERROR);
    }

    if (die_offset != NULL)
	*die_offset = type->ty_cu_offset + 
	    type->ty_context->tp_info_offset;

    if (cu_offset != NULL)
	*cu_offset = type->ty_context->tp_info_offset;

    *returned_name = (char *)(type->ty_name);
    return DW_DLV_OK;
}
