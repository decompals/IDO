#include <stdio.h>
#include <string.h>
#include <elfaccess.h>
#include "pro_incl.h"
#include "pro_pubnames.h"
#include "pro_section.h"

#define GET_DISP(dbg,val) (IS_64BIT(dbg) ? (char *)&val : ((char *)&val + 4))

extern int elf_sects[NUM_DEBUG_SECTIONS];
extern int sect_name_idx[NUM_DEBUG_SECTIONS];
extern int reloc_sects[NUM_DEBUG_SECTIONS];

/*
    This function adds another public name to the 
    list of public names for the given Dwarf_P_Debug.  
    It returns 0 on error, and 1 otherwise.
*/
Dwarf_Unsigned 
dwarf_add_pubname (
    Dwarf_P_Debug	dbg,
    Dwarf_P_Die		die,
    char		*pubname_name,
    Dwarf_Error		*error
)
{
    Dwarf_P_Pubname		pubname;
    char			*name;

    if (dbg == NULL) {
	_dwarf_p_error(NULL, error, DW_DLE_DBG_NULL);
	return(0);
    }

    if (die == NULL) {
	_dwarf_p_error(NULL, error, DW_DLE_DIE_NULL);
	return(0);
    }

    pubname = (Dwarf_P_Pubname)
	_dwarf_p_get_alloc(dbg, sizeof(struct Dwarf_P_Pubname_s));
    if (pubname == NULL) {
	_dwarf_p_error(dbg, error, DW_DLE_ALLOC_FAIL);
	return(0);
    }

    name = _dwarf_p_get_alloc(dbg, strlen(pubname_name)+1);
    if (name == NULL) {
	_dwarf_p_error(dbg, error, DW_DLE_ALLOC_FAIL);
	return(0);
    }
    strcpy(name, pubname_name);

    pubname->pu_die = die;
    pubname->pu_name = name;

    if (dbg->de_pubname == NULL)
	dbg->de_pubname = dbg->de_last_pubname = pubname;
    else {
	dbg->de_last_pubname->pu_next = pubname;
	dbg->de_last_pubname = pubname;
    }
    dbg->de_pubname_count++;

    return(1);
}


int
_dwarf_transform_pubname_to_disk (
    Dwarf_P_Debug		dbg,
    Dwarf_Error		*error
)
{
	/* Total num of bytes in .debug_pubnames section. */
    Dwarf_Unsigned	pubname_num_bytes;

	/* Total number of bytes excluding the length field. */
    Dwarf_Unsigned	adjusted_length;

	/* Points to first byte of .debug_pubnames buffer. */
    Dwarf_Small		*pubname;

	/* Scans the list of pub names provided by user. */
    Dwarf_P_Pubname	given_pubname;

	/* Fills in the .debug_pubnames buffer. */
    Dwarf_Small		*pubname_ptr;

	/* Points to first byte of .rel.debug_pubnames section. */
    Dwarf_Small		*pubname_reloc;

	/* Used to fill in 0. */
    const Dwarf_Signed	big_zero = 0;

        /* Used to scan the section data buffers. */
    Dwarf_P_Section_Data	debug_sect;

	/* Size of the .debug_info section. */
    Dwarf_Signed	debug_info_size;

	/* Points to a Elf64_Rel record. */
    Elf64_Rel		*elf64_reloc;

	/* Points to a Elf32_Rel record. */
    Elf32_Rel		*elf32_reloc;

    int			name_idx;
    int			err;

    /* ***** BEGIN CODE ***** */

	/* Get the size of the .debug_info section. */
    debug_info_size = 0;
    for (debug_sect = dbg->de_debug_sects; debug_sect != NULL;
	debug_sect = debug_sect->ds_next) 
	if (debug_sect->ds_elf_sect_no == elf_sects[DEBUG_INFO])
	    debug_info_size += debug_sect->ds_nbytes;

	/* Size of the .debug_pubnames section header. */
    pubname_num_bytes = 
	SIZEOF_UWORD(dbg) +  	/* Size of length field. */
	sizeof(Dwarf_Half) +    /* Size of version field. */
	SIZEOF_UWORD(dbg) +     /* Size of .debug_info offset. */
	SIZEOF_UWORD(dbg);      /* Size of .debug_info. */

	/* Add the size of the names portion. */
    for (given_pubname = dbg->de_pubname; given_pubname != NULL; 
	given_pubname = given_pubname->pu_next)
	pubname_num_bytes += 
	    SIZEOF_UWORD(dbg) + 		/* size of die offset. */
	    strlen(given_pubname->pu_name) + 1;	/* size of pub name. */

	/* Size of the last 0 offset. */
    pubname_num_bytes += SIZEOF_UWORD(dbg);

    GET_NEW_CHUNK(dbg, elf_sects[DEBUG_PUBNAMES], pubname, pubname_num_bytes, 
	error);
    if (pubname == NULL)
	{_dwarf_p_error(dbg, error, DW_DLE_ALLOC_FAIL); return(0);}
    pubname_ptr = pubname;

	/* Write the adjusted length of .debug_pubnames section. */
    adjusted_length = pubname_num_bytes - SIZEOF_UWORD(dbg);
    memcpy(pubname_ptr, GET_DISP(dbg, adjusted_length), SIZEOF_UWORD(dbg));
    pubname_ptr += SIZEOF_UWORD(dbg);

	/* Write the version as 2 bytes. */
    *pubname_ptr = 0;
    pubname_ptr++;
    *pubname_ptr = CURRENT_VERSION_STAMP;
    pubname_ptr++;

	/* Write the offset of the compile-unit. */
    memcpy(pubname_ptr, GET_DISP(dbg, big_zero), SIZEOF_UWORD(dbg));
    pubname_ptr += SIZEOF_UWORD(dbg);

	/* Write the size of .debug_info. */
    memcpy(pubname_ptr, GET_DISP(dbg, debug_info_size), SIZEOF_UWORD(dbg));
    pubname_ptr += SIZEOF_UWORD(dbg);

    reloc_sects[DEBUG_PUBNAMES] = dbg->de_func(".rel.debug_pubnames", 
	IS_64BIT(dbg), SHT_REL, 0, SHN_UNDEF, elf_sects[DEBUG_PUBNAMES], 
	&name_idx, &err);
    if (reloc_sects[DEBUG_PUBNAMES] == -1) {
        DWARF_P_DBG_ERROR(dbg, DW_DLE_ELF_SECT_ERR, DW_DLV_NOCOUNT);
    }

    GET_NEW_CHUNK(dbg, reloc_sects[DEBUG_PUBNAMES], pubname_reloc, 
	IS_64BIT(dbg)?sizeof(Elf64_Rel):sizeof(Elf32_Rel), error);
    if (pubname_reloc == NULL) 
	{_dwarf_p_error(dbg, error, DW_DLE_ALLOC_FAIL); return(0);}

	/* Write relocation record for .debug_info offset. */
    if (IS_64BIT(dbg)) {
	elf64_reloc = (Elf64_Rel *)pubname_reloc;
	elf64_reloc->r_offset = SIZEOF_UWORD(dbg) + sizeof(Dwarf_Half);
	Set_REL64_info
	    (*elf64_reloc, sect_name_idx[DEBUG_INFO], R_MIPS_64);
    }
    else {
	elf32_reloc = (Elf32_Rel *)pubname_reloc;
	elf32_reloc->r_offset = SIZEOF_UWORD(dbg) + sizeof(Dwarf_Half);
	Set_REL32_info
	    (*elf32_reloc, sect_name_idx[DEBUG_INFO], R_MIPS_32);
    }

    for (given_pubname = dbg->de_pubname; given_pubname != NULL;
	given_pubname = given_pubname->pu_next) {

	    /* Copy offset of die from start of compile-unit. */
	memcpy(pubname_ptr, GET_DISP(dbg, given_pubname->pu_die->di_offset),
	    SIZEOF_UWORD(dbg));
	pubname_ptr += SIZEOF_UWORD(dbg);

	    /* Copy the pub name. */
	strcpy((char *)pubname_ptr, given_pubname->pu_name);
	pubname_ptr += strlen(given_pubname->pu_name) + 1;
    }

    memcpy(pubname_ptr, GET_DISP(dbg, big_zero), SIZEOF_UWORD(dbg));

    return dbg->de_n_debug_sect;
}
