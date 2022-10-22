#include <stdio.h>
#include <string.h>
#include <limits.h>
#include "pro_incl.h"
#include "pro_expr.h"

    /* Indicates no relocation needed. */
#define NO_ELF_SYM_INDEX	0

#define GET_DISP(dbg,val) (IS_64BIT(dbg) ? (char *)&val : ((char *)&val + 4))

/* adds an attribute to a die */
extern void _dwarf_pro_add_at_to_die (Dwarf_P_Die die, Dwarf_P_Attribute attr);

/*
    This function adds an attribute whose value is
    a target address to the given die.  The attribute
    is given the name provided by attr.  The address
    is given in pc_value.
*/
Dwarf_P_Attribute 
dwarf_add_AT_targ_address (
    Dwarf_P_Debug	dbg,
    Dwarf_P_Die 	ownerdie,
    Dwarf_Half		attr,
    Dwarf_Unsigned	pc_value,
    Dwarf_Signed	sym_index,
    Dwarf_Error 	*error
)
{
    Dwarf_P_Attribute 	new_attr;

    if (dbg == NULL) {
        _dwarf_p_error(NULL, error, DW_DLE_DBG_NULL);
        return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }

    if (ownerdie == NULL) {
        _dwarf_p_error(dbg, error, DW_DLE_DIE_NULL);
        return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }

    if (attr != DW_AT_low_pc && attr != DW_AT_high_pc &&
	attr != DW_AT_MIPS_loop_begin && attr != DW_AT_MIPS_tail_loop_begin &&
	attr != DW_AT_MIPS_epilog_begin) {
        _dwarf_p_error(dbg, error, DW_DLE_INPUT_ATTR_BAD);
        return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }

    new_attr = (Dwarf_P_Attribute)
        _dwarf_p_get_alloc(dbg, sizeof(struct Dwarf_P_Attribute_s));
    if (new_attr == NULL) {
        _dwarf_p_error(dbg, error, DW_DLE_ALLOC_FAIL);
        return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }

    new_attr->ar_attribute = attr;
    new_attr->ar_attribute_form = DW_FORM_addr;
    new_attr->ar_nbytes = SIZEOF_UWORD(dbg);
    new_attr->ar_rel_symidx = sym_index;
    if (sym_index != NO_ELF_SYM_INDEX)
        new_attr->ar_rel_type = IS_64BIT(dbg) ? R_MIPS_64 : R_MIPS_32;
    else 
	new_attr->ar_rel_type = R_MIPS_NONE;

    new_attr->ar_data = (char *)
        _dwarf_p_get_alloc(dbg, SIZEOF_UWORD(dbg));
    if (new_attr->ar_data == NULL) {
        _dwarf_p_error(dbg, error, DW_DLE_ALLOC_FAIL);
        return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }
    memcpy(new_attr->ar_data, GET_DISP(dbg, pc_value), SIZEOF_UWORD(dbg));

    /* add attribute to the die */
    _dwarf_pro_add_at_to_die(ownerdie, new_attr);
    return new_attr;
}


/*
    This function adds attributes whose value
    is an unsigned constant.  It determines the 
    size of the value field from the value of 
    the constant.
*/
Dwarf_P_Attribute 
dwarf_add_AT_unsigned_const (
    Dwarf_P_Debug	dbg,
    Dwarf_P_Die 	ownerdie,
    Dwarf_Half		attr,
    Dwarf_Unsigned	value,
    Dwarf_Error 	*error
)
{
    Dwarf_P_Attribute 	new_attr;
    Dwarf_Half		attr_form;
    Dwarf_Small		size;

    if (dbg == NULL) {
        _dwarf_p_error(NULL, error, DW_DLE_DBG_NULL);
        return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }

    if (ownerdie == NULL) {
        _dwarf_p_error(dbg, error, DW_DLE_DIE_NULL);
        return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }

    switch (attr) {
        case DW_AT_ordering :
        case DW_AT_byte_size :
        case DW_AT_bit_offset :
        case DW_AT_bit_size :
	case DW_AT_inline :
        case DW_AT_language :
        case DW_AT_visibility: 
        case DW_AT_virtuality :
	case DW_AT_accessibility :
	case DW_AT_address_class :
	case DW_AT_calling_convention :
	case DW_AT_encoding :
	case DW_AT_identifier_case :
	case DW_AT_MIPS_loop_unroll_factor :
	case DW_AT_MIPS_software_pipeline_depth :
    	    break;

	case DW_AT_decl_column :
	case DW_AT_decl_file :
	case DW_AT_decl_line :
	case DW_AT_const_value :
	case DW_AT_start_scope :
	case DW_AT_stride_size :
	case DW_AT_count :
	    break;

        default : {
    	    _dwarf_p_error(dbg, error, DW_DLE_INPUT_ATTR_BAD);
    	    return((Dwarf_P_Attribute)DW_DLV_BADADDR);
        }
    }

	/* 
	    Compute the number of bytes 
	    needed to hold constant.
	*/
    if (value <= UCHAR_MAX) {attr_form = DW_FORM_data1; size = 1;}
    else if (value <= USHRT_MAX) {attr_form = DW_FORM_data2; size = 2;}
    else if (value <= UINT_MAX) {attr_form = DW_FORM_data4; size = 4;}
    else {attr_form = DW_FORM_data8; size = 8;}

    new_attr = (Dwarf_P_Attribute)
        _dwarf_p_get_alloc(dbg, sizeof(struct Dwarf_P_Attribute_s));
    if (new_attr == NULL) {
        _dwarf_p_error(dbg, error, DW_DLE_ALLOC_FAIL);
        return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }

    new_attr->ar_attribute = attr;
    new_attr->ar_attribute_form = attr_form;
    new_attr->ar_rel_type = R_MIPS_NONE;
    new_attr->ar_nbytes = size;

    new_attr->ar_data = (char *)
        _dwarf_p_get_alloc(dbg, size);
    if (new_attr->ar_data == NULL) {
        _dwarf_p_error(dbg, error, DW_DLE_ALLOC_FAIL);
        return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }
    memcpy(new_attr->ar_data, (char *)&value+sizeof(Dwarf_Unsigned)-size, size);

    /* add attribute to the die */
    _dwarf_pro_add_at_to_die(ownerdie, new_attr);
    return new_attr;
}


/*
    This function adds attributes whose value
    is an signed constant.  It determines the 
    size of the value field from the value of 
    the constant.
*/
Dwarf_P_Attribute 
dwarf_add_AT_signed_const (
    Dwarf_P_Debug		dbg,
    Dwarf_P_Die 		ownerdie,
    Dwarf_Half		attr,
    Dwarf_Signed	value,
    Dwarf_Error 	*error
)
{
    Dwarf_P_Attribute 	new_attr;
    Dwarf_Half		attr_form;
    Dwarf_Small		size;

    if (dbg == NULL) {
        _dwarf_p_error(NULL, error, DW_DLE_DBG_NULL);
        return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }

    if (ownerdie == NULL) {
        _dwarf_p_error(dbg, error, DW_DLE_DIE_NULL);
        return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }

    switch (attr) {
	case DW_AT_upper_bound :
	case DW_AT_lower_bound :
	    break;

        default : {
    	    _dwarf_p_error(dbg, error, DW_DLE_INPUT_ATTR_BAD);
    	    return((Dwarf_P_Attribute)DW_DLV_BADADDR);
        }
    }

	/* 
	    Compute the number of bytes 
	    needed to hold constant.
	*/
    if (value >= SCHAR_MIN && value <= SCHAR_MAX) 
	{attr_form = DW_FORM_data1; size = 1;}
    else if (value >= SHRT_MIN && value <= SHRT_MAX) 
	{attr_form = DW_FORM_data2; size = 2;}
    else if (value >= INT_MIN && value <= INT_MAX) 
	{attr_form = DW_FORM_data4; size = 4;}
    else 
	{attr_form = DW_FORM_data8; size = 8;}

    new_attr = (Dwarf_P_Attribute)
        _dwarf_p_get_alloc(dbg, sizeof(struct Dwarf_P_Attribute_s));
    if (new_attr == NULL) {
        _dwarf_p_error(dbg, error, DW_DLE_ALLOC_FAIL);
        return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }

    new_attr->ar_attribute = attr;
    new_attr->ar_attribute_form = attr_form;
    new_attr->ar_rel_type = R_MIPS_NONE;
    new_attr->ar_nbytes = size;

    new_attr->ar_data = (char *)
        _dwarf_p_get_alloc(dbg, size);
    if (new_attr->ar_data == NULL) {
        _dwarf_p_error(dbg, error, DW_DLE_ALLOC_FAIL);
        return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }
    memcpy(new_attr->ar_data, (char *)&value+sizeof(Dwarf_Signed)-size, size);

    /* add attribute to the die */
    _dwarf_pro_add_at_to_die(ownerdie, new_attr);
    return new_attr;
}


/*
    This function adds attributes whose value
    is a location expression.
*/
Dwarf_P_Attribute 
dwarf_add_AT_location_expr (
    Dwarf_P_Debug	dbg,
    Dwarf_P_Die 	ownerdie,
    Dwarf_Half		attr,
    Dwarf_P_Expr	loc_expr,
    Dwarf_Error 	*error
)
{
    char encode_buffer[ENCODE_SPACE_NEEDED];
    int res;
    Dwarf_P_Attribute 	new_attr;
    Dwarf_Half		attr_form;
    char		*len_str;
    int			len_size;
    int			block_size;
    char		*block_dest_ptr;

    if (dbg == NULL) {
        _dwarf_p_error(NULL, error, DW_DLE_DBG_NULL);
        return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }

    if (ownerdie == NULL) {
        _dwarf_p_error(dbg, error, DW_DLE_DIE_NULL);
        return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }

    if (loc_expr == NULL) {
	_dwarf_p_error(dbg, error, DW_DLE_EXPR_NULL);
	return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }

    if (loc_expr->ex_dbg != dbg) {
	_dwarf_p_error(dbg, error, DW_DLE_LOC_EXPR_BAD);
	return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }
    block_size = loc_expr->ex_next_byte_offset;

    switch (attr) {
	case DW_AT_location :
	case DW_AT_string_length :
	case DW_AT_const_value :
	case DW_AT_use_location :
	case DW_AT_return_addr :
	case DW_AT_data_member_location :
	case DW_AT_frame_base :
	case DW_AT_static_link :
	case DW_AT_vtable_elem_location :
	    break;

        default : {
    	    _dwarf_p_error(dbg, error, DW_DLE_INPUT_ATTR_BAD);
    	    return((Dwarf_P_Attribute)DW_DLV_BADADDR);
        }
    }

	/* 
	    Compute the number of bytes 
	    needed to hold constant.
	*/
    if (block_size <= UCHAR_MAX) {
	attr_form = DW_FORM_block1; 
	len_str = (char *)&block_size + sizeof(block_size) - 1;
	len_size = 1;
    }
    else if (block_size <= USHRT_MAX) {
	attr_form = DW_FORM_block2; 
	len_str = (char *)&block_size + sizeof(block_size) - 2;
	len_size = 2;
    }
    else if (block_size <= UINT_MAX) {
	attr_form = DW_FORM_block4; 
	len_str = (char *)&block_size + sizeof(block_size) - 4;
	len_size = 4;
    }
    else {
	attr_form = DW_FORM_block; 
	res = _dwarf_pro_encode_leb128_nm(block_size, &len_size,
		encode_buffer,sizeof(encode_buffer));
	if (res !=  DW_DLV_OK) {
	    _dwarf_p_error(dbg, error, DW_DLE_ALLOC_FAIL);
	    return((Dwarf_P_Attribute)DW_DLV_BADADDR);
	}
	len_str = (char *)encode_buffer;
    }

    new_attr = (Dwarf_P_Attribute)
        _dwarf_p_get_alloc(dbg, sizeof(struct Dwarf_P_Attribute_s));
    if (new_attr == NULL) {
        _dwarf_p_error(dbg, error, DW_DLE_ALLOC_FAIL);
        return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }

    new_attr->ar_attribute = attr;
    new_attr->ar_attribute_form = attr_form;

    if (loc_expr->ex_reloc_sym_index != NO_ELF_SYM_INDEX)
	new_attr->ar_rel_type = IS_64BIT(dbg) ? R_MIPS_64 : R_MIPS_32;
    else
        new_attr->ar_rel_type = R_MIPS_NONE;
    new_attr->ar_rel_symidx = loc_expr->ex_reloc_sym_index;
    new_attr->ar_rel_offset = loc_expr->ex_reloc_offset + len_size;

    new_attr->ar_nbytes = block_size + len_size;

    new_attr->ar_data = block_dest_ptr = 
	(char *)_dwarf_p_get_alloc(dbg, block_size + len_size);
    if (new_attr->ar_data == NULL) {
        _dwarf_p_error(dbg, error, DW_DLE_ALLOC_FAIL);
        return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }

    memcpy(block_dest_ptr, len_str, len_size);
    block_dest_ptr += len_size;
    memcpy(block_dest_ptr, &(loc_expr->ex_byte_stream[0]), block_size);

    /* add attribute to the die */
    _dwarf_pro_add_at_to_die(ownerdie, new_attr);
    return new_attr;
}


/*
    This function adds attributes of reference class.
    The references here are local CU references, 
    not DW_FORM_ref_addr.
    The offset field is 4 bytes for 32-bit objects,
    and 8-bytes for 64-bit objects.  Otherdie is the
    that is referenced by ownerdie.

    For reference attributes, the ar_data and ar_nbytes
    are not needed.  Instead, the ar_ref_die points to
    the other die, and its di_offset value is used as
    the reference value.
*/
Dwarf_P_Attribute 
dwarf_add_AT_reference (
    Dwarf_P_Debug	dbg,
    Dwarf_P_Die 	ownerdie,
    Dwarf_Half		attr,
    Dwarf_P_Die		otherdie,
    Dwarf_Error 	*error
)
{
    Dwarf_P_Attribute 	new_attr;

    if (dbg == NULL) {
        _dwarf_p_error(NULL, error, DW_DLE_DBG_NULL);
        return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }

    if (ownerdie == NULL) {
        _dwarf_p_error(dbg, error, DW_DLE_DIE_NULL);
        return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }

    if (otherdie == NULL) {
	_dwarf_p_error(dbg, error, DW_DLE_DIE_NULL);
	return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }

    switch (attr) {
        case DW_AT_specification :
        case DW_AT_discr :
        case DW_AT_common_reference :
        case DW_AT_import :
        case DW_AT_containing_type :
        case DW_AT_default_value: 
        case DW_AT_abstract_origin :
        case DW_AT_friend :
        case DW_AT_priority :
        case DW_AT_type : 
	case DW_AT_lower_bound :
	case DW_AT_upper_bound :
	case DW_AT_count :
	case DW_AT_sibling:
	case DW_AT_MIPS_stride:
	case DW_AT_MIPS_clone_origin:
    	    break;

        default : {
    	    _dwarf_p_error(dbg, error, DW_DLE_INPUT_ATTR_BAD);
    	    return((Dwarf_P_Attribute)DW_DLV_BADADDR);
        }
    }

    new_attr = (Dwarf_P_Attribute)
        _dwarf_p_get_alloc(dbg, sizeof(struct Dwarf_P_Attribute_s));
    if (new_attr == NULL) {
        _dwarf_p_error(dbg, error, DW_DLE_ALLOC_FAIL);
        return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }

    new_attr->ar_attribute = attr;
    new_attr->ar_attribute_form = 
        IS_64BIT(dbg) ? DW_FORM_ref8 : DW_FORM_ref4;
    new_attr->ar_nbytes = IS_64BIT(dbg) ? 8 : 4;
    new_attr->ar_ref_die = otherdie;
    new_attr->ar_rel_type = R_MIPS_NONE;

    /* add attribute to the die */
    _dwarf_pro_add_at_to_die(ownerdie, new_attr);
    return new_attr;
}


/*
    This function adds attributes of the flag class.
*/
Dwarf_P_Attribute 
dwarf_add_AT_flag (
    Dwarf_P_Debug		dbg,
    Dwarf_P_Die 		ownerdie,
    Dwarf_Half		attr,
    Dwarf_Small		flag,
    Dwarf_Error 	*error
)
{
    Dwarf_P_Attribute 	new_attr;

    if (dbg == NULL) {
        _dwarf_p_error(NULL, error, DW_DLE_DBG_NULL);
        return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }

    if (ownerdie == NULL) {
        _dwarf_p_error(dbg, error, DW_DLE_DIE_NULL);
        return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }

    switch (attr) {
        case DW_AT_is_optional :
        case DW_AT_artificial :
        case DW_AT_declaration :
        case DW_AT_external :
	case DW_AT_prototyped :
	case DW_AT_variable_parameter :
	case DW_AT_MIPS_has_inlines :
    	    break;

        default : {
    	    _dwarf_p_error(dbg, error, DW_DLE_INPUT_ATTR_BAD);
    	    return((Dwarf_P_Attribute)DW_DLV_BADADDR);
        }
    }

    new_attr = (Dwarf_P_Attribute)
        _dwarf_p_get_alloc(dbg, sizeof(struct Dwarf_P_Attribute_s));
    if (new_attr == NULL) {
        _dwarf_p_error(dbg, error, DW_DLE_ALLOC_FAIL);
        return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }

    new_attr->ar_attribute = attr;
    new_attr->ar_attribute_form = DW_FORM_flag;
    new_attr->ar_nbytes = 1;
    new_attr->ar_rel_type = R_MIPS_NONE;

    new_attr->ar_data = (char *)
        _dwarf_p_get_alloc(dbg, 1);
    if (new_attr->ar_data == NULL) {
        _dwarf_p_error(dbg, error, DW_DLE_ALLOC_FAIL);
        return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }
    memcpy(new_attr->ar_data, &flag, 1);

    /* add attribute to the die */
    _dwarf_pro_add_at_to_die(ownerdie, new_attr);
    return new_attr;
}


/*
    This function adds values of attributes
    belonging to the string class.
*/
Dwarf_P_Attribute 
dwarf_add_AT_string (
    Dwarf_P_Debug	dbg,
    Dwarf_P_Die 	ownerdie,
    Dwarf_Half		attr,
    char 		*string,
    Dwarf_Error 	*error
)
{
    Dwarf_P_Attribute 	new_attr;

    if (dbg == NULL) {
	_dwarf_p_error(NULL, error, DW_DLE_DBG_NULL);
	return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }

    if (ownerdie == NULL) {
	_dwarf_p_error(dbg, error, DW_DLE_DIE_NULL);
	return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }

    new_attr = (Dwarf_P_Attribute)
        _dwarf_p_get_alloc(dbg, sizeof(struct Dwarf_P_Attribute_s));
    if (new_attr == NULL) {
        _dwarf_p_error(dbg, error, DW_DLE_ALLOC_FAIL);
	return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }

    switch (attr) {
	case DW_AT_name :
	case DW_AT_comp_dir :
	case DW_AT_const_value :
	case DW_AT_producer :
	case DW_AT_MIPS_linkage_name :
	case DW_AT_MIPS_abstract_name :
	    break;

	default : {
	    _dwarf_p_error(dbg, error, DW_DLE_INPUT_ATTR_BAD);
	    return((Dwarf_P_Attribute)DW_DLV_BADADDR);
	}
    }

    new_attr->ar_attribute = attr;
    new_attr->ar_attribute_form = DW_FORM_string;
    new_attr->ar_nbytes = strlen(string) + 1;
    new_attr->ar_next = NULL;

    new_attr->ar_data = 
	(char *)_dwarf_p_get_alloc(NULL, strlen(string)+1);
    if (new_attr->ar_data == NULL) {
        _dwarf_p_error(dbg, error, DW_DLE_ALLOC_FAIL);
	return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }

    strcpy(new_attr->ar_data, string);
    new_attr->ar_rel_type = R_MIPS_NONE;

	/* add attribute to the die */
    _dwarf_pro_add_at_to_die(ownerdie, new_attr);
    return new_attr;
}


Dwarf_P_Attribute 
dwarf_add_AT_const_value_string (
    Dwarf_P_Die 		ownerdie,
    char 		*string_value,
    Dwarf_Error 	*error
)
{
    Dwarf_P_Attribute 	new_attr;

    if (ownerdie == NULL) {
	_dwarf_p_error(NULL, error, DW_DLE_DIE_NULL);
	return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }

    new_attr = (Dwarf_P_Attribute)
        _dwarf_p_get_alloc(NULL, sizeof(struct Dwarf_P_Attribute_s));
    if (new_attr == NULL) {
        _dwarf_p_error(NULL, error, DW_DLE_ALLOC_FAIL);
	return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }

    new_attr->ar_attribute = DW_AT_const_value;
    new_attr->ar_attribute_form = DW_FORM_string;
    new_attr->ar_nbytes = strlen(string_value) + 1;
    new_attr->ar_next = NULL;

    new_attr->ar_data = 
	(char *)_dwarf_p_get_alloc(NULL, strlen(string_value)+1);
    if (new_attr->ar_data == NULL) {
        _dwarf_p_error(NULL, error, DW_DLE_ALLOC_FAIL);
	return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }

    strcpy(new_attr->ar_data, string_value);
    new_attr->ar_rel_type = R_MIPS_NONE;

	/* add attribute to the die */
    _dwarf_pro_add_at_to_die(ownerdie, new_attr);
    return new_attr;
}


Dwarf_P_Attribute 
dwarf_add_AT_producer (
    Dwarf_P_Die 		ownerdie,
    char 		*producer_string,
    Dwarf_Error 	*error
)
{
    Dwarf_P_Attribute 	new_attr;

    if (ownerdie == NULL) {
        _dwarf_p_error(NULL, error, DW_DLE_DIE_NULL);
	return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }

    new_attr = (Dwarf_P_Attribute)
        _dwarf_p_get_alloc(NULL, sizeof(struct Dwarf_P_Attribute_s));
    if (new_attr == NULL) {
        _dwarf_p_error(NULL, error, DW_DLE_ALLOC_FAIL);
	return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }

    new_attr->ar_attribute = DW_AT_producer;
    new_attr->ar_attribute_form = DW_FORM_string;
    new_attr->ar_nbytes = strlen(producer_string) + 1;
    new_attr->ar_next = NULL;

    new_attr->ar_data = 
	(char *)_dwarf_p_get_alloc(NULL, strlen(producer_string)+1);
    if (new_attr->ar_data == NULL) {
	_dwarf_p_error(NULL, error, DW_DLE_ALLOC_FAIL);
	return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }

    strcpy(new_attr->ar_data, producer_string);
    new_attr->ar_rel_type = R_MIPS_NONE;

	/* add attribute to the die */
    _dwarf_pro_add_at_to_die(ownerdie,new_attr);
    return new_attr;
}


Dwarf_P_Attribute
dwarf_add_AT_const_value_signedint (
    Dwarf_P_Die 	ownerdie,
    Dwarf_Signed 	signed_value,
    Dwarf_Error 	*error
)
{
    Dwarf_P_Attribute 	new_attr;
    int			leb_size;
    char encode_buffer[ENCODE_SPACE_NEEDED];
    int res;

    if (ownerdie == NULL) {
        _dwarf_p_error(NULL, error, DW_DLE_DIE_NULL);
	return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }

    new_attr = (Dwarf_P_Attribute)
        _dwarf_p_get_alloc(NULL, sizeof(struct Dwarf_P_Attribute_s));
    if (new_attr == NULL) {
        _dwarf_p_error(NULL, error, DW_DLE_ALLOC_FAIL);
	return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }

    new_attr->ar_attribute = DW_AT_const_value;
    new_attr->ar_attribute_form = DW_FORM_sdata;
    new_attr->ar_rel_type = R_MIPS_NONE;
    new_attr->ar_next = NULL;

    res =       _dwarf_pro_encode_signed_leb128_nm(signed_value, &leb_size,
          encode_buffer,sizeof(encode_buffer));
    if (res != DW_DLV_OK)  {
	_dwarf_p_error(NULL, error, DW_DLE_ALLOC_FAIL);
	return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }
    new_attr->ar_data =  (char *)
        _dwarf_p_get_alloc(NULL, leb_size);
    if (new_attr->ar_data == NULL) {
	_dwarf_p_error(NULL, error, DW_DLE_ALLOC_FAIL);
	return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }
    memcpy(new_attr->ar_data,encode_buffer,leb_size);
    new_attr->ar_nbytes = leb_size;
    new_attr->ar_nbytes = leb_size;

	/* add attribute to the die */
    _dwarf_pro_add_at_to_die(ownerdie,new_attr);
    return new_attr;
}


Dwarf_P_Attribute 
dwarf_add_AT_const_value_unsignedint (
    Dwarf_P_Die 		ownerdie,
    Dwarf_Unsigned 	unsigned_value,
    Dwarf_Error 	*error
)
{
    Dwarf_P_Attribute 	new_attr;
    int			leb_size;
    char encode_buffer[ENCODE_SPACE_NEEDED];
    int res;

    if (ownerdie == NULL) {
        _dwarf_p_error(NULL, error, DW_DLE_DIE_NULL);
	return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }

    new_attr = (Dwarf_P_Attribute)
        _dwarf_p_get_alloc(NULL, sizeof(struct Dwarf_P_Attribute_s));
    if (new_attr == NULL) {
        _dwarf_p_error(NULL, error, DW_DLE_ALLOC_FAIL);
	return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }

    new_attr->ar_attribute = DW_AT_const_value;
    new_attr->ar_attribute_form = DW_FORM_udata;
    new_attr->ar_rel_type = R_MIPS_NONE;
    new_attr->ar_next = NULL;

    res = 	_dwarf_pro_encode_leb128_nm(unsigned_value, &leb_size,
	  encode_buffer,sizeof(encode_buffer));
    if (res != DW_DLV_OK) {
	_dwarf_p_error(NULL, error, DW_DLE_ALLOC_FAIL);
	return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }
    new_attr->ar_data = (char *)
		_dwarf_p_get_alloc(NULL, leb_size);
    if (new_attr->ar_data == NULL) {
	_dwarf_p_error(NULL, error, DW_DLE_ALLOC_FAIL);
	return((Dwarf_P_Attribute)DW_DLV_BADADDR);
    }
    memcpy(new_attr->ar_data,encode_buffer,leb_size);
    new_attr->ar_nbytes = leb_size;

	/* add attribute to the die */
    _dwarf_pro_add_at_to_die(ownerdie,new_attr);
    return new_attr;
}
