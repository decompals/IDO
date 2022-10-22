#include <stdio.h>
#include <string.h>
#include <elfaccess.h>
#include "pro_incl.h"
#include "pro_vars.h"
#include "pro_section.h"

#define GET_DISP(dbg,val) (IS_64BIT(dbg) ? (char *)&val : ((char *)&val + 4))

extern int elf_sects[NUM_DEBUG_SECTIONS];
extern int sect_name_idx[NUM_DEBUG_SECTIONS];
extern int reloc_sects[NUM_DEBUG_SECTIONS];

/*
    This function adds another variable name to the 
    list of variable names for the given Dwarf_P_Debug.  
    It returns 0 on error, and 1 otherwise.
*/
Dwarf_Unsigned 
dwarf_add_varname (
    Dwarf_P_Debug	dbg,
    Dwarf_P_Die		die,
    char		*var_name,
    Dwarf_Error		*error
)
{
    Dwarf_P_Varname		varname;
    char			*name;

    if (dbg == NULL) {
	_dwarf_p_error(NULL, error, DW_DLE_DBG_NULL);
	return(0);
    }

    if (die == NULL) {
	_dwarf_p_error(NULL, error, DW_DLE_DIE_NULL);
	return(0);
    }

    varname = (Dwarf_P_Varname)
	_dwarf_p_get_alloc(dbg, sizeof(struct Dwarf_P_Varname_s));
    if (varname == NULL) {
	_dwarf_p_error(dbg, error, DW_DLE_ALLOC_FAIL);
	return(0);
    }

    name = _dwarf_p_get_alloc(dbg, strlen(var_name)+1);
    if (name == NULL) {
	_dwarf_p_error(dbg, error, DW_DLE_ALLOC_FAIL);
	return(0);
    }
    strcpy(name, var_name);

    varname->vu_die = die;
    varname->vu_name = name;

    if (dbg->de_varname == NULL)
	dbg->de_varname = dbg->de_last_varname = varname;
    else {
	dbg->de_last_varname->vu_next = varname;
	dbg->de_last_varname = varname;
    }
    dbg->de_varname_count++;

    return(1);
}


int
_dwarf_transform_varname_to_disk (
    Dwarf_P_Debug	dbg,
    Dwarf_Error		*error
)
{
	/* Total num of bytes in .debug_varnames section. */
    Dwarf_Unsigned	varname_num_bytes;

	/* Total number of bytes excluding the length field. */
    Dwarf_Unsigned	adjusted_length;

	/* Points to first byte of .debug_varnames buffer. */
    Dwarf_Small		*varname;

	/* Scans the list of variable names provided by user. */
    Dwarf_P_Varname	given_varname;

	/* Fills in the .debug_varnames buffer. */
    Dwarf_Small		*varname_ptr;

	/* Points to first byte of .rel.debug_varnames section. */
    Dwarf_Small		*varname_reloc;

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

	/* Size of the .debug_varnames section header. */
    varname_num_bytes = 
	SIZEOF_UWORD(dbg) +  	/* Size of length field. */
	sizeof(Dwarf_Half) +    /* Size of version field. */
	SIZEOF_UWORD(dbg) +     /* Size of .debug_info offset. */
	SIZEOF_UWORD(dbg);      /* Size of .debug_info. */

	/* Add the size of the names portion. */
    for (given_varname = dbg->de_varname; given_varname != NULL; 
	given_varname = given_varname->vu_next)
	varname_num_bytes += 
	    SIZEOF_UWORD(dbg) + 	    	    /* size of die offset. */
	    strlen(given_varname->vu_name) + 1;	    /* size of pub name. */

	/* Size of the last 0 offset. */
    varname_num_bytes += SIZEOF_UWORD(dbg);

    GET_NEW_CHUNK(dbg, elf_sects[DEBUG_VARNAMES], varname, 
	varname_num_bytes, error);
    if (varname == NULL)
	{_dwarf_p_error(dbg, error, DW_DLE_ALLOC_FAIL); return(0);}
    varname_ptr = varname;

	/* Write the adjusted length of .debug_varnames section. */
    adjusted_length = varname_num_bytes - SIZEOF_UWORD(dbg);
    memcpy(varname_ptr, GET_DISP(dbg, adjusted_length), SIZEOF_UWORD(dbg));
    varname_ptr += SIZEOF_UWORD(dbg);

	/* Write the version as 2 bytes. */
    *varname_ptr = 0;
    varname_ptr++;
    *varname_ptr = CURRENT_VERSION_STAMP;
    varname_ptr++;

	/* Write the offset of the compile-unit. */
    memcpy(varname_ptr, GET_DISP(dbg, big_zero), SIZEOF_UWORD(dbg));
    varname_ptr += SIZEOF_UWORD(dbg);

	/* Write the size of .debug_info. */
    memcpy(varname_ptr, GET_DISP(dbg, debug_info_size), SIZEOF_UWORD(dbg));
    varname_ptr += SIZEOF_UWORD(dbg);

    reloc_sects[DEBUG_VARNAMES] = dbg->de_func(".rel.debug_varnames", 
	IS_64BIT(dbg), SHT_REL, 0, SHN_UNDEF, elf_sects[DEBUG_VARNAMES], 
	&name_idx, &err);
    if (reloc_sects[DEBUG_VARNAMES] == -1) {
        DWARF_P_DBG_ERROR(dbg, DW_DLE_ELF_SECT_ERR, DW_DLV_NOCOUNT);
    }

    GET_NEW_CHUNK(dbg, reloc_sects[DEBUG_VARNAMES], varname_reloc, 
	IS_64BIT(dbg)?sizeof(Elf64_Rel):sizeof(Elf32_Rel), error);
    if (varname_reloc == NULL) 
	{_dwarf_p_error(dbg, error, DW_DLE_ALLOC_FAIL); return(0);}

	/* Write relocation record for .debug_info offset. */
    if (IS_64BIT(dbg)) {
	elf64_reloc = (Elf64_Rel *)varname_reloc;
	elf64_reloc->r_offset = SIZEOF_UWORD(dbg) + sizeof(Dwarf_Half);
	Set_REL64_info
	    (*elf64_reloc, sect_name_idx[DEBUG_INFO], R_MIPS_64);
    }
    else {
	elf32_reloc = (Elf32_Rel *)varname_reloc;
	elf32_reloc->r_offset = SIZEOF_UWORD(dbg) + sizeof(Dwarf_Half);
	Set_REL32_info
	    (*elf32_reloc, sect_name_idx[DEBUG_INFO], R_MIPS_32);
    }

    for (given_varname = dbg->de_varname; given_varname != NULL;
	given_varname = given_varname->vu_next) {

	    /* Copy offset of die from start of compile-unit. */
	memcpy(varname_ptr, GET_DISP(dbg, given_varname->vu_die->di_offset),
	    SIZEOF_UWORD(dbg));
	varname_ptr += SIZEOF_UWORD(dbg);

	    /* Copy the variable name. */
	strcpy((char *)varname_ptr, given_varname->vu_name);
	varname_ptr += strlen(given_varname->vu_name) + 1;
    }

    memcpy(varname_ptr, GET_DISP(dbg, big_zero), SIZEOF_UWORD(dbg));

    return dbg->de_n_debug_sect;
}
