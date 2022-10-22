#include <stdio.h>
#include "dwarf_incl.h"
#include "dwarf_arange.h"


/*
    This function returns the count of the number of
    aranges in the .debug_aranges section.  It sets
    aranges to point to a block of Dwarf_Arange's 
    describing the arange's.  It returns DW_DLV_ERROR
    on error.

    Must be identical in most aspects to
	dwarf_get_aranges_addr_offsets!
*/
int
dwarf_get_aranges (
    Dwarf_Debug		dbg,
    Dwarf_Arange	**aranges,
    Dwarf_Signed       *returned_count,
    Dwarf_Error		*error
)
{
	/* Sweeps the .debug_aranges section. */
    Dwarf_Small		*arange_ptr;

	/* 
	    Start of arange header.  Used for rounding offset of
	    arange_ptr to twice the tuple size.  Libdwarf requirement.
	*/
    Dwarf_Small		*header_ptr;

	/* Length of current set of aranges. */
    Dwarf_Unsigned	length;

	/* Version of .debug_aranges header. */
    Dwarf_Half		version;

        /* Offset of current set of aranges into .debug_info. */
    Dwarf_Off	        info_offset;

	/* Size in bytes of addresses in target. */
    Dwarf_Small		address_size;

	/* Size in bytes of segment offsets in target. */
    Dwarf_Small		segment_size;

    Dwarf_Small		remainder;

	/* Count of total number of aranges. */
    Dwarf_Unsigned	arange_count = 0;

	/* Start address of arange. */
    Dwarf_Addr		range_address;

	/* Length of arange. */
    Dwarf_Unsigned	range_length;

    Dwarf_Arange	arange, *arange_block;

    Dwarf_Unsigned	i;

	/* Used to chain Dwarf_Aranges structs. */
    Dwarf_Chain		curr_chain, prev_chain, head_chain = NULL;

    /* ***** BEGIN CODE ***** */

    if (dbg == NULL)
	{_dwarf_error(NULL, error, DW_DLE_DBG_NULL); return(DW_DLV_ERROR);}

    if (dbg->de_debug_aranges == NULL) {
	return(DW_DLV_NO_ENTRY);
    }

    arange_ptr = dbg->de_debug_aranges;
    do {
	header_ptr = arange_ptr;

	    /* Length of aranges for this compilation-unit. */
	READ_UNALIGNED(length, arange_ptr, dbg->de_length_size);
	arange_ptr += dbg->de_length_size;

	READ_UNALIGNED(version, arange_ptr, sizeof(Dwarf_Half));
	arange_ptr += sizeof(Dwarf_Half);
	length = length - sizeof(Dwarf_Half);
	if (version != CURRENT_VERSION_STAMP) {
	    _dwarf_error(dbg, error, DW_DLE_VERSION_STAMP_ERROR);
	    return(DW_DLV_ERROR);
	}

	READ_UNALIGNED(info_offset, arange_ptr, dbg->de_length_size);
	arange_ptr += dbg->de_length_size;
	length = length - dbg->de_length_size;
	if (info_offset >= dbg->de_debug_info_size) {
	    _dwarf_error(dbg, error, DW_DLE_ARANGE_OFFSET_BAD);
	    return(DW_DLV_ERROR);
	}

        address_size = *(Dwarf_Small *)arange_ptr;
	arange_ptr = arange_ptr + sizeof(Dwarf_Small);
	length = length - sizeof(Dwarf_Small);

	segment_size = *(Dwarf_Small *)arange_ptr;
	arange_ptr = arange_ptr + sizeof(Dwarf_Small);
	length = length - sizeof(Dwarf_Small);
	if (segment_size != 0) {
	    _dwarf_error(dbg, error, DW_DLE_SEGMENT_SIZE_BAD);
	    return(DW_DLV_ERROR);
	}

	    /* Round arange_ptr offset to next multiple of address_size. */
        remainder = (Dwarf_Unsigned)(arange_ptr-header_ptr) % 
	    (2 * address_size);
	if (remainder != 0) {
	    arange_ptr = arange_ptr + (2*address_size) - remainder;
	    length = length - ((2*address_size) - remainder);
	}

	do {
	    READ_UNALIGNED(range_address, arange_ptr, dbg->de_length_size);
	    arange_ptr += dbg->de_length_size;
            length = length - dbg->de_length_size;

	    READ_UNALIGNED(range_length, arange_ptr, dbg->de_length_size);
	    arange_ptr += dbg->de_length_size;
            length = length - dbg->de_length_size;

	    if (range_address != 0 || range_length != 0) {

	        arange = (Dwarf_Arange)
		    _dwarf_get_alloc(dbg, DW_DLA_ARANGE, 1);
	        if (arange == NULL) {
		    _dwarf_error(dbg, error, DW_DLE_ALLOC_FAIL); 
		    return(DW_DLV_ERROR);
	        }

		arange->ar_address = range_address;
		arange->ar_length = range_length;
		arange->ar_info_offset = info_offset;
		arange->ar_dbg = dbg;
		arange_count++;

		curr_chain = (Dwarf_Chain)
		    _dwarf_get_alloc(dbg, DW_DLA_CHAIN, 1);
		if (curr_chain == NULL) {
		    _dwarf_error(dbg, error, DW_DLE_ALLOC_FAIL);
		    return(DW_DLV_ERROR);
		}

		curr_chain->ch_item = arange;
		if (head_chain == NULL) 
		    head_chain = prev_chain = curr_chain;
		else {
		    prev_chain->ch_next = curr_chain;
		    prev_chain = curr_chain;
		}
	    }
	} while (range_address != 0 || range_length != 0);

	if (length != 0) {
	    _dwarf_error(dbg, error, DW_DLE_ARANGE_LENGTH_BAD);
	    return(DW_DLV_ERROR);
	}

    } while (arange_ptr < dbg->de_debug_aranges + dbg->de_debug_aranges_size);

    if (arange_ptr != dbg->de_debug_aranges + dbg->de_debug_aranges_size) {
	_dwarf_error(dbg, error, DW_DLE_ARANGE_DECODE_ERROR);
	return(DW_DLV_ERROR);
    }

    arange_block = (Dwarf_Arange *)
	_dwarf_get_alloc(dbg, DW_DLA_LIST, arange_count);
    if (arange_block == NULL) {
	_dwarf_error(dbg, error, DW_DLE_ALLOC_FAIL);
	return(DW_DLV_ERROR);
    }

    curr_chain = head_chain;
    for (i = 0; i < arange_count; i++) {
	*(arange_block + i) = curr_chain->ch_item;
	prev_chain = curr_chain;
	curr_chain = curr_chain->ch_next;
	dwarf_dealloc(dbg, prev_chain, DW_DLA_CHAIN);
    }

    *aranges = arange_block;
    *returned_count = (arange_count);
    return DW_DLV_OK;
}

/*
    This function returns DW_DLV_OK if it succeeds
    and DW_DLV_ERR or DW_DLV_OK otherwise.
    count is set to the number of addresses in the
    .debug_aranges section. 
    For each address, the corresponding element in
    an array is set to the address itself(aranges) and
    the section offset (offsets).
    Must be identical in most aspects to
	dwarf_get_aranges!
*/
int
_dwarf_get_aranges_addr_offsets(
    Dwarf_Debug		dbg,
    Dwarf_Addr	        **addrs,
    Dwarf_Off           **offsets,
    Dwarf_Signed        *count,
    Dwarf_Error		*error
)
{
	/* Sweeps the .debug_aranges section. */
    Dwarf_Small		*arange_ptr;
    Dwarf_Small		*arange_start_ptr;

	/* 
	    Start of arange header.  Used for rounding offset of
	    arange_ptr to twice the tuple size.  Libdwarf requirement.
	*/
    Dwarf_Small		*header_ptr;

	/* Length of current set of aranges. */
    Dwarf_Unsigned	length;

	/* Version of .debug_aranges header. */
    Dwarf_Half		version;

        /* Offset of current set of aranges into .debug_info. */
    Dwarf_Off	        info_offset;

	/* Size in bytes of addresses in target. */
    Dwarf_Small		address_size;

	/* Size in bytes of segment offsets in target. */
    Dwarf_Small		segment_size;

    Dwarf_Small		remainder;

	/* Count of total number of aranges. */
    Dwarf_Unsigned	arange_count = 0;

	/* Start address of arange. */
    Dwarf_Addr		range_address;

	/* Length of arange. */
    Dwarf_Unsigned	range_length;

    Dwarf_Arange	arange;

    Dwarf_Unsigned	i;

	/* Used to chain Dwarf_Aranges structs. */
    Dwarf_Chain		curr_chain, prev_chain, head_chain = NULL;

    Dwarf_Addr * arange_addrs;
    Dwarf_Off  * arange_offsets;

    /* ***** BEGIN CODE ***** */

    if (error != NULL) *error = NULL;

    if (dbg == NULL)
	{_dwarf_error(NULL, error, DW_DLE_DBG_NULL); return(DW_DLV_ERROR);}

    if (dbg->de_debug_aranges == NULL) {
	return(DW_DLV_NO_ENTRY);
    }

    arange_ptr = dbg->de_debug_aranges;
    do {
	header_ptr = arange_ptr;

	    /* Length of aranges for this compilation-unit. */
	READ_UNALIGNED(length, arange_ptr, dbg->de_length_size);
	arange_ptr += dbg->de_length_size;

	READ_UNALIGNED(version, arange_ptr, sizeof(Dwarf_Half));
	arange_ptr += sizeof(Dwarf_Half);
	length = length - sizeof(Dwarf_Half);
	if (version != CURRENT_VERSION_STAMP) {
	    _dwarf_error(dbg, error, DW_DLE_VERSION_STAMP_ERROR);
	    return(DW_DLV_ERROR);
	}

	READ_UNALIGNED(info_offset, arange_ptr, dbg->de_length_size);
	arange_ptr += dbg->de_length_size;
	length = length - dbg->de_length_size;
	if (info_offset >= dbg->de_debug_info_size) {
	    _dwarf_error(dbg, error, DW_DLE_ARANGE_OFFSET_BAD);
	    return(DW_DLV_ERROR);
	}

        address_size = *(Dwarf_Small *)arange_ptr;
	arange_ptr = arange_ptr + sizeof(Dwarf_Small);
	length = length - sizeof(Dwarf_Small);

	segment_size = *(Dwarf_Small *)arange_ptr;
	arange_ptr = arange_ptr + sizeof(Dwarf_Small);
	length = length - sizeof(Dwarf_Small);
	if (segment_size != 0) {
	    _dwarf_error(dbg, error, DW_DLE_SEGMENT_SIZE_BAD);
	    return(DW_DLV_ERROR);
	}

	    /* Round arange_ptr offset to next multiple of address_size. */
        remainder = (Dwarf_Unsigned)(arange_ptr-header_ptr) % 
	    (2 * address_size);
	if (remainder != 0) {
	    arange_ptr = arange_ptr + (2*address_size) - remainder;
	    length = length - ((2*address_size) - remainder);
	}

	do {
	    arange_start_ptr = arange_ptr;
	    READ_UNALIGNED(range_address, arange_ptr, dbg->de_length_size);
	    arange_ptr += dbg->de_length_size;
            length = length - dbg->de_length_size;

	    READ_UNALIGNED(range_length, arange_ptr, dbg->de_length_size);
	    arange_ptr += dbg->de_length_size;
            length = length - dbg->de_length_size;

	    if (range_address != 0 || range_length != 0) {

	        arange = (Dwarf_Arange)
		    _dwarf_get_alloc(dbg, DW_DLA_ARANGE, 1);
	        if (arange == NULL) {
		    _dwarf_error(dbg, error, DW_DLE_ALLOC_FAIL); 
		    return(DW_DLV_ERROR);
	        }

		arange->ar_address = range_address;
		arange->ar_length = range_length;
		arange->ar_info_offset = arange_start_ptr - dbg->de_debug_aranges;
		arange->ar_dbg = dbg;
		arange_count++;

		curr_chain = (Dwarf_Chain)
		    _dwarf_get_alloc(dbg, DW_DLA_CHAIN, 1);
		if (curr_chain == NULL) {
		    _dwarf_error(dbg, error, DW_DLE_ALLOC_FAIL);
		    return(DW_DLV_ERROR);
		}

		curr_chain->ch_item = arange;
		if (head_chain == NULL) 
		    head_chain = prev_chain = curr_chain;
		else {
		    prev_chain->ch_next = curr_chain;
		    prev_chain = curr_chain;
		}
	    }
	} while (range_address != 0 || range_length != 0);

	if (length != 0) {
	    _dwarf_error(dbg, error, DW_DLE_ARANGE_LENGTH_BAD);
	    return(DW_DLV_ERROR);
	}

    } while (arange_ptr < dbg->de_debug_aranges + dbg->de_debug_aranges_size);

    if (arange_ptr != dbg->de_debug_aranges + dbg->de_debug_aranges_size) {
	_dwarf_error(dbg, error, DW_DLE_ARANGE_DECODE_ERROR);
	return(DW_DLV_ERROR);
    }

    arange_addrs = (Dwarf_Addr *)
	_dwarf_get_alloc(dbg, DW_DLA_ADDR, arange_count);
    if (arange_addrs == NULL) {
	_dwarf_error(dbg, error, DW_DLE_ALLOC_FAIL);
	return(DW_DLV_ERROR);
    }
    arange_offsets = (Dwarf_Off *)
	_dwarf_get_alloc(dbg, DW_DLA_ADDR, arange_count);
    if (arange_offsets == NULL) {
	_dwarf_error(dbg, error, DW_DLE_ALLOC_FAIL);
	return(DW_DLV_ERROR);
    }

    curr_chain = head_chain;
    for (i = 0; i < arange_count; i++) {
	Dwarf_Arange   ar = curr_chain->ch_item;
	arange_addrs[i] = ar->ar_address;
	arange_offsets[i] = ar->ar_info_offset;
	prev_chain = curr_chain;
	curr_chain = curr_chain->ch_next;
	dwarf_dealloc(dbg, ar, DW_DLA_ARANGE);
	dwarf_dealloc(dbg, prev_chain, DW_DLA_CHAIN);
    }
    *count = arange_count;
    *offsets = arange_offsets;
    *addrs = arange_addrs;
    return(DW_DLV_OK);
}


/*
    This function takes a pointer to a block
    of Dwarf_Arange's, and a count of the
    length of the block.  It checks if the
    given address is within the range of an
    address range in the block.  If yes, it
    returns the appropriate Dwarf_Arange.
    Otherwise, it returns DW_DLV_ERROR.
*/
int
dwarf_get_arange (
    Dwarf_Arange	*aranges,
    Dwarf_Unsigned	arange_count,
    Dwarf_Addr	        address,
    Dwarf_Arange       *returned_arange,
    Dwarf_Error		*error
)
{
    Dwarf_Arange	curr_arange;
    Dwarf_Unsigned	i;

    if (aranges == NULL) {
	_dwarf_error(NULL, error, DW_DLE_ARANGES_NULL);
	return(DW_DLV_ERROR);
    }

    for (i = 0; i < arange_count; i++) {
	curr_arange = *(aranges + i);
	if (address >= curr_arange->ar_address &&
	    address < curr_arange->ar_address + curr_arange->ar_length) {
	    *returned_arange = curr_arange;
    	    return(DW_DLV_OK);
        }
    }

    return(DW_DLV_NO_ENTRY);
}


/*
    This function takes an Dwarf_Arange,
    and returns the offset of the first
    die in the compilation-unit that the
    arange belongs to.  Returns DW_DLV_ERROR
    on error.
*/
int
dwarf_get_cu_die_offset (
    Dwarf_Arange	arange,
    Dwarf_Off      *    returned_offset,
    Dwarf_Error		*error
)
{
    if (arange == NULL) {
	_dwarf_error(NULL, error, DW_DLE_ARANGE_NULL); 
	return(DW_DLV_ERROR);
    }

    *returned_offset =
	(arange->ar_info_offset + arange->ar_dbg->de_length_size + 
	CU_VERSION_STAMP_SIZE + arange->ar_dbg->de_length_size + 
	CU_ADDRESS_SIZE_SIZE);
    return DW_DLV_OK;
}


/*
    This function takes a Dwarf_Arange, and returns
    true if it is not NULL.  It also stores the start
    address of the range in *start, the length of the
    range in *length, and the offset of the first die
    in the compilation-unit in *cu_die_offset.  It
    returns false on error.
*/
int
dwarf_get_arange_info (
    Dwarf_Arange	arange,
    Dwarf_Addr		*start,
    Dwarf_Unsigned	*length,
    Dwarf_Off		*cu_die_offset,
    Dwarf_Error		*error
)
{
    if (arange == NULL) {
	_dwarf_error(NULL, error, DW_DLE_ARANGE_NULL); 
	return(DW_DLV_ERROR);
    }

    if (start != NULL) *start = arange->ar_address;
    if (length != NULL) *length = arange->ar_length;
    if (cu_die_offset != NULL)
        *cu_die_offset = arange->ar_info_offset + 
	    arange->ar_dbg->de_length_size + CU_VERSION_STAMP_SIZE + 
	    arange->ar_dbg->de_length_size + CU_ADDRESS_SIZE_SIZE;

    return(DW_DLV_OK);
}

