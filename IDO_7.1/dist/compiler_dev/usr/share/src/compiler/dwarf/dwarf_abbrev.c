/*
    dwarf_abbrev.c

    $Revision: 1.15 $		$Date: 1995/05/01 15:25:10 $
*/

#include <stdio.h>
#include "dwarf_incl.h"
#include "dwarf_abbrev.h"

int
dwarf_get_abbrev (
    Dwarf_Debug		dbg,
    Dwarf_Unsigned	offset,
    Dwarf_Abbrev        *returned_abbrev,
    Dwarf_Unsigned	*length,
    Dwarf_Unsigned	*abbr_count,
    Dwarf_Error		*error
)
{
    Dwarf_Small		*abbrev_ptr;
    Dwarf_Small		*abbrev_section_end;
    Dwarf_Half		attr;
    Dwarf_Half		attr_form;
    Dwarf_Abbrev	ret_abbrev;
    Dwarf_Unsigned     labbr_count = 0;


    if (dbg == NULL) {
	_dwarf_error(NULL, error, DW_DLE_DBG_NULL); 
	return(DW_DLV_ERROR);
    }

    if (offset >= dbg->de_debug_abbrev_size)  {
	return(DW_DLV_NO_ENTRY);
    }

    ret_abbrev = (Dwarf_Abbrev)_dwarf_get_alloc(dbg, DW_DLA_ABBREV, 1);
    if (ret_abbrev == NULL) {
        _dwarf_error(dbg, error, DW_DLE_ALLOC_FAIL); 
        return(DW_DLV_ERROR);
    }
    ret_abbrev->ab_dbg = dbg;
    if(returned_abbrev == 0 || abbr_count == 0 ) {
	_dwarf_error(dbg, error, DW_DLE_DEBUG_ABBREV_NULL);
        return(DW_DLV_ERROR);
    }


    *abbr_count = 0;
    if (length != NULL) *length = 1;

    abbrev_ptr = dbg->de_debug_abbrev + offset;
    abbrev_section_end = dbg->de_debug_abbrev + dbg->de_debug_abbrev_size;

    DECODE_LEB128_UWORD(abbrev_ptr, ret_abbrev->ab_code)
    if (ret_abbrev->ab_code == 0) {
	*returned_abbrev = ret_abbrev;
        *abbr_count = 0;
	return(DW_DLV_OK);
    }

    DECODE_LEB128_UWORD(abbrev_ptr, ret_abbrev->ab_tag)
    ret_abbrev->ab_has_child = *(abbrev_ptr++);
    ret_abbrev->ab_abbrev_ptr = abbrev_ptr;

    do {
        DECODE_LEB128_UWORD(abbrev_ptr, attr)
        DECODE_LEB128_UWORD(abbrev_ptr, attr_form)

	if ( attr != 0) (labbr_count)++;

    } while (abbrev_ptr < abbrev_section_end && (attr != 0 || attr_form != 0));

    if (abbrev_ptr > abbrev_section_end) {
	_dwarf_error(dbg, error, DW_DLE_ABBREV_DECODE_ERROR); 
	return(DW_DLV_ERROR);
    }

    if (length != NULL) 
	*length = abbrev_ptr - dbg->de_debug_abbrev - offset;

    *returned_abbrev = ret_abbrev;
    *abbr_count = labbr_count;
    return(DW_DLV_OK);
}

int
dwarf_get_abbrev_code (
    Dwarf_Abbrev	abbrev,
    Dwarf_Unsigned         *returned_code,
    Dwarf_Error		*error
)
{
    if (abbrev == NULL) {
	_dwarf_error(NULL, error, DW_DLE_DWARF_ABBREV_NULL); 
	return(DW_DLV_ERROR);
    }
    
    *returned_code = abbrev->ab_code;
    return(DW_DLV_OK);
}

int
dwarf_get_abbrev_tag (
    Dwarf_Abbrev	abbrev,
    Dwarf_Half         *returned_tag,
    Dwarf_Error		*error
)
{
    if (abbrev == NULL) {
	_dwarf_error(NULL, error, DW_DLE_DWARF_ABBREV_NULL); 
	return(DW_DLV_ERROR);
    }
    
    *returned_tag = abbrev->ab_tag;
    return(DW_DLV_OK);
}


int
dwarf_get_abbrev_children_flag (
    Dwarf_Abbrev	abbrev,
    Dwarf_Signed       *returned_flag,
    Dwarf_Error		*error
)
{
    if (abbrev == NULL) {
	_dwarf_error(NULL, error, DW_DLE_DWARF_ABBREV_NULL); 
	return(DW_DLV_ERROR);
    }
    
    *returned_flag = abbrev->ab_has_child;
    return(DW_DLV_OK);
}


int
dwarf_get_abbrev_entry (
    Dwarf_Abbrev	abbrev,
    Dwarf_Signed	index,
    Dwarf_Half         *returned_attr_num,
    Dwarf_Signed	*form,
    Dwarf_Off		*offset,
    Dwarf_Error		*error
)
{
    Dwarf_Byte_Ptr	abbrev_ptr;
    Dwarf_Byte_Ptr	abbrev_end;
    Dwarf_Byte_Ptr	mark_abbrev_ptr;
    Dwarf_Half		attr;
    Dwarf_Half		attr_form;

    if (index < 0)
	return(DW_DLV_NO_ENTRY);

    if (abbrev == NULL) {
	_dwarf_error(NULL, error, DW_DLE_DWARF_ABBREV_NULL); 
	return(DW_DLV_ERROR);
    }
    
    if (abbrev->ab_code == 0) {
	return(DW_DLV_NO_ENTRY);
    }

    if (abbrev->ab_dbg == NULL) {
	_dwarf_error(NULL, error, DW_DLE_DBG_NULL);
	return(DW_DLV_ERROR);
    }

    abbrev_ptr = abbrev->ab_abbrev_ptr;
    abbrev_end = 
	abbrev->ab_dbg->de_debug_abbrev + abbrev->ab_dbg->de_debug_abbrev_size;

    for (attr = 1 ,attr_form = 1; 
	index >= 0 && abbrev_ptr < abbrev_end && (attr != 0 || attr_form != 0); 
	index--) {
	mark_abbrev_ptr = abbrev_ptr;
        DECODE_LEB128_UWORD(abbrev_ptr, attr)
        DECODE_LEB128_UWORD(abbrev_ptr, attr_form)
    } 

    if (abbrev_ptr >= abbrev_end) {
	_dwarf_error(abbrev->ab_dbg, error, DW_DLE_ABBREV_DECODE_ERROR);
	return(DW_DLV_ERROR);
    }

    if (index >=  0) {
	return(DW_DLV_NO_ENTRY);
    }

    if (form != NULL) *form = attr_form;
    if (offset != NULL) 
	*offset = mark_abbrev_ptr - abbrev->ab_dbg->de_debug_abbrev;

    *returned_attr_num = (attr);
    return DW_DLV_OK;
}
