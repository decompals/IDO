/*
    dwarf_util.h

    $Revision: 1.16 $		$Date: 1994/11/05 01:25:04 $
*/
#include <limits.h>

/*
    Decodes unsigned leb128 encoded numbers that 
    are assumed to be less than 4 bytes long.  
    Make sure ptr is a pointer to a 1-byte type.  
    Returns UINT_MAX on error.

    Limits.h is included to define UINT_MAX.
*/
#define DECODE_LEB128_UWORD(ptr, value) \
    { \
        Dwarf_Small	byte; \
    \
        value = (byte = *(ptr++)) & 0x7f; \
        if ((byte & 0x80) != 0) { \
	    value |= ((byte = *(ptr++)) & 0x7f) << 7; \
	    if ((byte & 0x80) != 0) { \
	        value |= ((byte = *(ptr++)) & 0x7f) << 14; \
	        if ((byte & 0x80) != 0) { \
		    value |= ((byte = *(ptr++)) & 0x7f) << 21; \
		    if ((byte & 0x80) != 0) { \
		        value = UINT_MAX; \
		    } \
	        } \
	    } \
        } \
    }

/*
    Decodes signed leb128 encoded numbers that
    are assumed to be less than 4 bytes long.
    Make sure ptr is a pointer to a 1-byte type.
    Returns INT_MAX on error.

    Make sure value is a 4-byte signed int.
*/
#define DECODE_LEB128_SWORD(ptr, value) \
    { \
	Dwarf_Small	byte; \
    \
	value = (byte = *(ptr++)) & 0x7f; \
	if ((byte & 0x80) == 0) { \
	    if ((byte & 0x40) != 0)  \
	        value |= 0xffffff80; \
	} \
	else { \
	    value |= ((byte = *(ptr++)) & 0x7f) << 7; \
	    if ((byte & 0x80) == 0) { \
		if ((byte & 0x40) != 0) \
		    value |= 0xffffc000; \
	    } \
	    else { \
		value |= ((byte = *(ptr++)) & 0x7f) << 14; \
		if ((byte & 0x80) == 0) { \
		    if ((byte & 0x40) != 0) \
			value |= 0xffe00000; \
		} \
		else { \
		    value |= ((byte = *(ptr++)) & 0x7f) << 21; \
		    if ((byte & 0x80) == 0) { \
			if ((byte & 0x40) != 0) \
			    value |= 0xf0000000; \
		    } \
		    else  \
			value = INT_MAX; \
		} \
	    } \
	} \
    }


/*
    Skips leb128_encoded numbers that are guaranteed 
    to be no more than 4 bytes long.  Same for both
    signed and unsigned numbers.
*/
#define SKIP_LEB128_WORD(ptr) \
    if ((*(ptr++) & 0x80) != 0) { \
        if ((*(ptr++) & 0x80) != 0) { \
            if ((*(ptr++) & 0x80) != 0) { \
	        if ((*(ptr++) & 0x80) != 0) { \
	        } \
	    } \
        } \
    }


#define CHECK_DIE(die, error_ret_value) \
    if (die == NULL) { \
	_dwarf_error(NULL, error, DW_DLE_DIE_NULL); \
	return(error_ret_value); \
    } \
    if (die->di_cu_context == NULL) { \
	_dwarf_error(NULL, error, DW_DLE_DIE_NO_CU_CONTEXT); \
	return(error_ret_value); \
    } \
    if (die->di_cu_context->cc_dbg == NULL) { \
	_dwarf_error(NULL, error, DW_DLE_DBG_NULL); \
	return(error_ret_value); \
    } 


/* 
    This macro copies length number of bytes from source to dest.  
    The actual destination address is adjusted to account for the 
    difference in the number of bytes in dest, and length.  
*/
#define READ_UNALIGNED(dest, source, length) \
    dest = 0; \
    if (length > sizeof(dest)) \
	memcpy(&dest, (char *)source + length - sizeof(dest), sizeof(dest)); \
    else \
        memcpy((char *)&dest + sizeof(dest) - length, source, length) 


/*
    This macro sign-extends a variable depending on the length.
    It fills the bytes between the size of the destination and
    the length with appropriate padding.
*/
#define SIGN_EXTEND(dest, length) \
    if (*(Dwarf_Sbyte *)((char *)&dest + sizeof(dest) - length) < 0) \
	memcpy((char *)&dest, "\xff\xff\xff\xff\xff\xff\xff\xff", \
	    sizeof(dest) - length)


Dwarf_Unsigned
_dwarf_decode_u_leb128 (
    Dwarf_Small         *leb128,
    Dwarf_Word      	*leb128_length
);

Dwarf_Signed 
_dwarf_decode_s_leb128 (
    Dwarf_Small 	*leb128, 
    Dwarf_Word 		*leb128_length
);

Dwarf_Unsigned 
_dwarf_get_size_of_val (
    Dwarf_Debug		dbg,
    Dwarf_Unsigned      form,
    Dwarf_Small         *val_ptr
);

/*
    This struct is used to build a hash table for the
    abbreviation codes for a compile-unit.  
*/
struct Dwarf_Hash_Table_s {
    Dwarf_Abbrev_List	at_head;
    Dwarf_Abbrev_List	at_tail;
};

Dwarf_Abbrev_List
_dwarf_get_abbrev_for_code (
    Dwarf_CU_Context    cu_context,
    Dwarf_Word          code
);


/* return 1 if string ends before 'endptr' else
** return 0 meaning string is not properly terminated.
** Presumption is the 'endptr' pts to end of some dwarf section data.
*/
int _dwarf_string_valid(void *startptr, void *endptr);
