#include <stdio.h>
#include <string.h>
#include <elfaccess.h>
#include "pro_incl.h"
#include "pro_arange.h"
#include "pro_section.h"

#define GET_DISP(dbg,val) (IS_64BIT(dbg) ? (char *)&val : ((char *)&val + 4))

extern int elf_sects[NUM_DEBUG_SECTIONS];
extern int sect_name_idx[NUM_DEBUG_SECTIONS];
extern int reloc_sects[NUM_DEBUG_SECTIONS];

/*
    This function adds another address range 
    to the list of address ranges for the
    given Dwarf_P_Debug.  It returns 0 on error,
    and 1 otherwise.
*/
Dwarf_Unsigned 
dwarf_add_arange (
    Dwarf_P_Debug	dbg,
    Dwarf_Addr		begin_address,
    Dwarf_Unsigned	length,
    Dwarf_Signed	symbol_index,
    Dwarf_Error		*error
)
{
    Dwarf_P_Arange	arange;

    if (dbg == NULL) {
	_dwarf_p_error(NULL, error, DW_DLE_DBG_NULL);
	return(0);
    }

    arange = (Dwarf_P_Arange)
	_dwarf_p_get_alloc(dbg, sizeof(struct Dwarf_P_Arange_s));
    if (arange == NULL) {
	_dwarf_p_error(dbg, error, DW_DLE_ALLOC_FAIL);
	return(0);
    }

    arange->ag_begin_address = begin_address;
    arange->ag_length = length;
    arange->ag_symbol_index = symbol_index;

    if (dbg->de_arange == NULL)
	dbg->de_arange = dbg->de_last_arange = arange;
    else {
	dbg->de_last_arange->ag_next = arange;
	dbg->de_last_arange = arange;
    }
    dbg->de_arange_count++;

    return(1);
}


int
_dwarf_transform_arange_to_disk (
    Dwarf_P_Debug		dbg,
    Dwarf_Error		*error
)
{
	/* Total num of bytes in .debug_aranges section. */
    Dwarf_Unsigned	arange_num_bytes;

	/* 
	    Adjustment to align the start of the actual
	    address ranges on a boundary aligned with
	    twice the address size.
	*/
    Dwarf_Small		remainder;

	/* Total number of bytes excluding the length field. */
    Dwarf_Unsigned	adjusted_length;

	/* Total num of bytes in .rel.debug_aranges section. */
    Dwarf_Unsigned	arange_reloc_num_bytes;

	/* Points to first byte of .debug_aranges buffer. */
    Dwarf_Small		*arange;

	/* Fills in the .debug_aranges buffer. */
    Dwarf_Small		*arange_ptr;

	/* Points to first byte of .rel.debug_aranges section. */
    Dwarf_Small		*arange_reloc;

	/* Fills in the .rel.debug_aranges section. */
    Dwarf_Small		*arange_reloc_ptr;

	/* Scans the list of address ranges provided by user. */
    Dwarf_P_Arange	given_arange;

	/* Used to fill in 0. */
    const Dwarf_Signed	big_zero = 0;

	/* Points to a Elf64_Rel record. */
    Elf64_Rel		*elf64_reloc;

	/* Points to a Elf32_Rel record. */
    Elf32_Rel		*elf32_reloc;

    int			name_idx;
    int			err;

    /* ***** BEGIN CODE ***** */

	/* Size of the .debug_aranges section header. */
    arange_num_bytes = SIZEOF_UWORD(dbg) +  /* Size of length field. */
	sizeof(Dwarf_Half) + 		    /* Size of version field. */
	SIZEOF_UWORD(dbg) + 		    /* Size of .debug_info offset. */
	sizeof(Dwarf_Small) + 		    /* Size of address size field. */
	sizeof(Dwarf_Small); 		    /* Size of segment size field. */

	/* 
	    Adjust the size so that the set of aranges begins on
	    a boundary that aligned with twice the address size.  This
	    is a Libdwarf requirement.
	*/
    remainder = arange_num_bytes % (2 * SIZEOF_UWORD(dbg));
    if (remainder != 0)
        arange_num_bytes += (2*SIZEOF_UWORD(dbg)) - remainder;


	/* Add the bytes for the actual address ranges. */
    arange_num_bytes += SIZEOF_UWORD(dbg) * 2 * (dbg->de_arange_count + 1);

    GET_NEW_CHUNK(dbg, elf_sects[DEBUG_ARANGES], arange, arange_num_bytes, 
	error);
    arange_ptr = arange;
    if (arange == NULL)
	{_dwarf_p_error(dbg, error, DW_DLE_ALLOC_FAIL); return(0);}

	/* Write the total length of .debug_aranges section. */
    adjusted_length = arange_num_bytes - SIZEOF_UWORD(dbg);
    memcpy(arange_ptr, GET_DISP(dbg, adjusted_length), SIZEOF_UWORD(dbg));
    arange_ptr += SIZEOF_UWORD(dbg);

	/* Write the version as 2 bytes. */
    *arange_ptr = 0;
    arange_ptr++;
    *arange_ptr = CURRENT_VERSION_STAMP;
    arange_ptr++;

	/* Write the .debug_info offset.  This is always 0. */
    memcpy(arange_ptr, GET_DISP(dbg, big_zero), SIZEOF_UWORD(dbg));
    arange_ptr += SIZEOF_UWORD(dbg);

	/* Write the size of addresses. */
    *arange_ptr = IS_64BIT(dbg) ? 8 : 4;
    arange_ptr++;

	/* 
	    Write the size of segment addresses.  
	    This is zero for MIPS architectures.
	*/
    *arange_ptr = 0;
    arange_ptr++;

	/* 
	    Skip over the padding to align the start of the 
	    actual address ranges to twice the address size.
	*/
    if (remainder != 0)
        arange_ptr += (2*SIZEOF_UWORD(dbg)) - remainder;

    arange_reloc_num_bytes = 
	(IS_64BIT(dbg) ? sizeof(Elf64_Rel) : sizeof(Elf32_Rel)) * 
	(dbg->de_arange_count + 1);

    reloc_sects[DEBUG_ARANGES] = dbg->de_func(".rel.debug_aranges",
	IS_64BIT(dbg), SHT_REL, 0, SHN_UNDEF,elf_sects[DEBUG_ARANGES], 
	&name_idx, &err);
    if (reloc_sects[DEBUG_ARANGES] == -1) {
        DWARF_P_DBG_ERROR(dbg,DW_DLE_ELF_SECT_ERR,DW_DLV_NOCOUNT);
    }

    GET_NEW_CHUNK (dbg, reloc_sects[DEBUG_ARANGES], arange_reloc, 
	arange_reloc_num_bytes, error);
    arange_reloc_ptr = arange_reloc;
    if (arange_reloc == NULL) 
	{_dwarf_p_error(dbg, error, DW_DLE_ALLOC_FAIL); return(0);}

	/* Write relocation record for .debug_info offset. */
    if (IS_64BIT(dbg)) {
	elf64_reloc = (Elf64_Rel *)arange_reloc_ptr;
	elf64_reloc->r_offset = SIZEOF_UWORD(dbg) + sizeof(Dwarf_Half);
	Set_REL64_info(*elf64_reloc, sect_name_idx[DEBUG_INFO], R_MIPS_64);
	arange_reloc_ptr += sizeof(Elf64_Rel);
    }
    else {
	elf32_reloc = (Elf32_Rel *)arange_reloc_ptr;
	elf32_reloc->r_offset = SIZEOF_UWORD(dbg) + sizeof(Dwarf_Half);
	Set_REL32_info(*elf32_reloc, sect_name_idx[DEBUG_INFO], R_MIPS_32);
	arange_reloc_ptr += sizeof(Elf32_Rel);
    }


    for (given_arange = dbg->de_arange; given_arange != NULL;
	given_arange = given_arange->ag_next) {

	    /* Write relocation record for beginning of address range. */
	if (IS_64BIT(dbg)) {
	    elf64_reloc = (Elf64_Rel *)arange_reloc_ptr;
	    elf64_reloc->r_offset = arange_ptr - arange;
	    Set_REL64_info
		(*elf64_reloc, given_arange->ag_symbol_index, R_MIPS_64);
	    arange_reloc_ptr += sizeof(Elf64_Rel);
	}
	else {
	    elf32_reloc = (Elf32_Rel *)arange_reloc_ptr;
	    elf32_reloc->r_offset = arange_ptr - arange;
	    Set_REL32_info
		(*elf32_reloc, given_arange->ag_symbol_index, R_MIPS_32);
	    arange_reloc_ptr += sizeof(Elf32_Rel);
	}

	    /* Copy beginning address of range. */
	memcpy(arange_ptr, GET_DISP(dbg, given_arange->ag_begin_address),
	    SIZEOF_UWORD(dbg));
	arange_ptr += SIZEOF_UWORD(dbg);

	    /* Copy length of range. */
	memcpy(arange_ptr, GET_DISP(dbg, given_arange->ag_length), 
	    SIZEOF_UWORD(dbg));
	arange_ptr += SIZEOF_UWORD(dbg);
    }

    memcpy(arange_ptr, GET_DISP(dbg, big_zero), SIZEOF_UWORD(dbg));
    arange_ptr += SIZEOF_UWORD(dbg);
    memcpy(arange_ptr, GET_DISP(dbg, big_zero), SIZEOF_UWORD(dbg));

    return dbg->de_n_debug_sect;
}
