/*
    dwarf_util.c
    $Revision: 1.29 $   $Date: 1996/07/23 23:26:42 $

    Dwarf utility functions.
*/


#include <stdio.h>
#include "dwarf_incl.h"
#include "dwarf_die_deliv.h"


/*
    decode ULEB
*/
Dwarf_Unsigned
_dwarf_decode_u_leb128 (
    Dwarf_Small     	*leb128,
    Dwarf_Word      	*leb128_length
)
{
    Dwarf_Small     	byte;
    Dwarf_Word		word_number;
    Dwarf_Unsigned  	number;
    Dwarf_Sword  	shift;
    Dwarf_Sword		byte_length;

    if ((*leb128 & 0x80) == 0) {
	if (leb128_length != NULL) *leb128_length = 1;
	return(*leb128);
    }
    else if ((*(leb128 + 1) & 0x80) == 0) {
	if (leb128_length != NULL) *leb128_length = 2;

	word_number = *leb128 & 0x7f;
	word_number |= (*(leb128 + 1) & 0x7f) << 7;
	return(word_number);
    }
    else if ((*(leb128 + 2) & 0x80) == 0) {
	if (leb128_length != NULL) *leb128_length = 3;

	word_number = *leb128 & 0x7f;
	word_number |= (*(leb128 + 1) & 0x7f) << 7;
	word_number |= (*(leb128 + 2) & 0x7f) << 14;
	return(word_number);
    }
    else if ((*(leb128 + 3) & 0x80) == 0) {
	if (leb128_length != NULL) *leb128_length = 4;

	word_number = *leb128 & 0x7f;
	word_number |= (*(leb128 + 1) & 0x7f) << 7;
	word_number |= (*(leb128 + 2) & 0x7f) << 14;
	word_number |= (*(leb128 + 3) & 0x7f) << 21;
	return(word_number);
    }

    number = 0;
    shift = 0;
    byte_length = 1;
    byte = *(leb128);
    while (true) {
	number |= (byte & 0x7f) << shift;
	shift += 7;

	if ((byte & 0x80) == 0) {
	    if (leb128_length != NULL) *leb128_length = byte_length;
	    return(number);
	}

	byte_length++;
	byte = *(++leb128);
    }
}


/*
    decode SLEB
*/
Dwarf_Signed
_dwarf_decode_s_leb128 (
    Dwarf_Small     	*leb128,
    Dwarf_Word          *leb128_length
)
{
    Dwarf_Small     	byte = *leb128;
    Dwarf_Sword		word_number = 0;
    Dwarf_Signed    	number;
    Dwarf_Bool	    	sign = 0;
    Dwarf_Bool		ndone = true;
    Dwarf_Sword  	shift = 0;
    Dwarf_Sword		byte_length = 0;

    while (byte_length++ < 4) {
	sign = byte & 0x40;
	word_number |= (byte & 0x7f) << shift;
	shift += 7;

	if ((byte & 0x80) == 0) {
	    ndone = false;
	    break;
	}
	byte = *(++leb128);
    }

    number = word_number;
    while (ndone) {
	sign = byte & 0x40;
        number |= (byte & 0x7f) << shift;
        shift += 7;

	if ((byte & 0x80) == 0) {
	    break;
	}

	    /* 
		Increment after byte has been placed in
	        number on account of the increment already
	        done when the first loop terminates.  That
	        is the fourth byte is picked up and byte_length
	        updated in the first loop.  So increment not
	        needed in this loop if break is taken.
	    */
	byte_length++;
        byte = *(++leb128);
    }

    if ((shift < sizeof(Dwarf_Signed)*8) && sign) 
	number |= - (1 << shift);

    if (leb128_length != NULL) *leb128_length = byte_length;
    return(number);
}


/*
    Given a form, and a pointer to the bytes encoding 
    a value of that form, val_ptr, this function returns
    the length, in bytes, of a value of that form.
    When using this function, check for a return of 0
    a recursive DW_FORM_INDIRECT value.
*/
Dwarf_Unsigned
_dwarf_get_size_of_val (
    Dwarf_Debug		dbg,
    Dwarf_Unsigned      form,
    Dwarf_Small     	*val_ptr
)
{
    Dwarf_Unsigned      length;
    Dwarf_Word		leb128_length;
    Dwarf_Unsigned      form_indirect;
    Dwarf_Unsigned	ret_value;

    switch (form) {

	default :			/* Handles form = 0. */
	    return(form);

        case DW_FORM_addr:  
        case DW_FORM_ref_addr:   
            return(dbg->de_length_size);

	case DW_FORM_block1:
	    return(*(Dwarf_Small *)val_ptr + 1);

        case DW_FORM_block2:
	    READ_UNALIGNED(ret_value, val_ptr, sizeof(Dwarf_Half));
	    return(ret_value + sizeof(Dwarf_Half));

        case DW_FORM_block4:
	    READ_UNALIGNED(ret_value, val_ptr, sizeof(Dwarf_ufixed));
	    return(ret_value + sizeof(Dwarf_ufixed));

    
        case DW_FORM_data1:
            return(1);

        case DW_FORM_data2:
            return(2);

        case DW_FORM_data4:
            return(4);

        case DW_FORM_data8:
            return(8);

        case DW_FORM_string:
            return(strlen((char *)val_ptr) + 1);

        case DW_FORM_block:
            length = _dwarf_decode_u_leb128(val_ptr,&leb128_length);
            return(length + leb128_length);

        case DW_FORM_flag:
            return(1);

        case DW_FORM_indirect:
            form_indirect = _dwarf_decode_u_leb128(val_ptr,&leb128_length);
            if (form_indirect == DW_FORM_indirect) return(0);
            return(leb128_length + _dwarf_get_size_of_val(dbg, form_indirect,
	        val_ptr+leb128_length));

        case DW_FORM_ref1:
	    return(1);

	case DW_FORM_ref2:
	    return(2);

	case DW_FORM_ref4:
	    return(4);

	case DW_FORM_ref8:
	    return(8);

        case DW_FORM_sdata: 
            _dwarf_decode_s_leb128(val_ptr,&leb128_length);
            return(leb128_length);

        case DW_FORM_strp:
            return(dbg->de_length_size);

        case DW_FORM_udata: 
            _dwarf_decode_u_leb128(val_ptr,&leb128_length);
            return(leb128_length);
    }
}


/*
    This function returns a pointer to a Dwarf_Abbrev_List_s
    struct for the abbrev with the given code.  It puts the
    struct on the appropriate hash table.  It also adds all
    the abbrev between the last abbrev added and this one to
    the hash table.  In other words, the .debug_abbrev section
    is scanned sequentially from the top for an abbrev with
    the given code.  All intervening abbrevs are also put 
    into the hash table.

    This function hashes the given code, and checks the chain
    at that hash table entry to see if a Dwarf_Abbrev_List_s
    with the given code exists.  If yes, it returns a pointer
    to that struct.  Otherwise, it scans the .debug_abbrev
    section from the last byte scanned for that CU till either
    an abbrev with the given code is found, or an abbrev code
    of 0 is read.  It puts Dwarf_Abbrev_List_s entries for all
    abbrev's read till that point into the hash table.  The
    hash table contains both a head pointer and a tail pointer
    for each entry.

    Returns NULL on error.
*/
Dwarf_Abbrev_List
_dwarf_get_abbrev_for_code (
    Dwarf_CU_Context	cu_context,
    Dwarf_Word		code
)
{
    Dwarf_Debug		dbg = cu_context->cc_dbg;
    Dwarf_Hash_Table	hash_table = cu_context->cc_abbrev_hash_table;
    Dwarf_Word		hash_num;
    Dwarf_Abbrev_List	hash_abbrev_list;
    Dwarf_Abbrev_List	abbrev_list;
    Dwarf_Byte_Ptr	abbrev_ptr;
    Dwarf_Half		abbrev_code, abbrev_tag;
    Dwarf_Half		attr_name, attr_form;

    hash_num = code % ABBREV_HASH_TABLE_SIZE;
    for (hash_abbrev_list = hash_table[hash_num].at_head; 
	hash_abbrev_list != NULL && hash_abbrev_list->ab_code != code; 
        hash_abbrev_list = hash_abbrev_list->ab_next);
    if (hash_abbrev_list != NULL)
        return(hash_abbrev_list);

    abbrev_ptr = cu_context->cc_last_abbrev_ptr != NULL ? 
	cu_context->cc_last_abbrev_ptr : 
	dbg->de_debug_abbrev + cu_context->cc_abbrev_offset;

	/* End of abbrev's for this cu, since abbrev code is 0. */
    if (*abbrev_ptr == 0) return(NULL);

    do {
        DECODE_LEB128_UWORD(abbrev_ptr, abbrev_code)
	DECODE_LEB128_UWORD(abbrev_ptr, abbrev_tag)

        abbrev_list = (Dwarf_Abbrev_List)
	    _dwarf_get_alloc(cu_context->cc_dbg, DW_DLA_ABBREV_LIST, 1);
        if (abbrev_list == NULL) return(NULL);

	hash_num = abbrev_code % ABBREV_HASH_TABLE_SIZE;
	if (hash_table[hash_num].at_head == NULL) {
	    hash_table[hash_num].at_head = 
		hash_table[hash_num].at_tail = abbrev_list;
	}
	else {
	    hash_table[hash_num].at_tail->ab_next = abbrev_list;
	    hash_table[hash_num].at_tail = abbrev_list;
	}

        abbrev_list->ab_code = abbrev_code;
	abbrev_list->ab_tag = abbrev_tag;

	abbrev_list->ab_has_child = *(abbrev_ptr++);
        abbrev_list->ab_abbrev_ptr = abbrev_ptr;

	do {
	    DECODE_LEB128_UWORD(abbrev_ptr, attr_name)
	    DECODE_LEB128_UWORD(abbrev_ptr, attr_form)
        } while (attr_name != 0 && attr_form != 0);

    } while (*abbrev_ptr != 0 && abbrev_code != code);

    cu_context->cc_last_abbrev_ptr = abbrev_ptr;
    return(abbrev_code == code ? abbrev_list : NULL);
}


/* return 1 if string ends before 'endptr' else
** return 0 meaning string is not properly terminated.
** Presumption is the 'endptr' pts to end of some dwarf section data.
*/
int _dwarf_string_valid(void *startptr, void *endptr)
{

	char *start = startptr;
	char *end = endptr;

	while(start < end) {
		if (*start == 0) {
			return 1; /* OK! */
		}
		++start;
		++end;
	}
	return 0; /* FAIL! bad string! */
}

