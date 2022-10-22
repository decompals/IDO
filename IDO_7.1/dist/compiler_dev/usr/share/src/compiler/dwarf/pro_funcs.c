#include <stdio.h>
#include <string.h>
#include <elfaccess.h>
#include "pro_incl.h"
#include "pro_funcs.h"
#include "pro_section.h"

#define GET_DISP(dbg,val) (IS_64BIT(dbg) ? (char *)&val : ((char *)&val + 4))

extern int elf_sects[NUM_DEBUG_SECTIONS];
extern int sect_name_idx[NUM_DEBUG_SECTIONS];
extern int reloc_sects[NUM_DEBUG_SECTIONS];

/*
    This function adds another function name to the 
    list of function names for the given Dwarf_P_Debug.  
    It returns 0 on error, and 1 otherwise.
*/
Dwarf_Unsigned 
dwarf_add_funcname (
    Dwarf_P_Debug	dbg,
    Dwarf_P_Die		die,
    char		*function_name,
    Dwarf_Error		*error
)
{
    Dwarf_P_Funcname		funcname;
    char			*name;

    if (dbg == NULL) {
	_dwarf_p_error(NULL, error, DW_DLE_DBG_NULL);
	return(0);
    }

    if (die == NULL) {
	_dwarf_p_error(NULL, error, DW_DLE_DIE_NULL);
	return(0);
    }

    funcname = (Dwarf_P_Funcname)
	_dwarf_p_get_alloc(dbg, sizeof(struct Dwarf_P_Funcname_s));
    if (funcname == NULL) {
	_dwarf_p_error(dbg, error, DW_DLE_ALLOC_FAIL);
	return(0);
    }

    name = _dwarf_p_get_alloc(dbg, strlen(function_name)+1);
    if (name == NULL) {
	_dwarf_p_error(dbg, error, DW_DLE_ALLOC_FAIL);
	return(0);
    }
    strcpy(name, function_name);

    funcname->fu_die = die;
    funcname->fu_name = name;

    if (dbg->de_funcname == NULL)
	dbg->de_funcname = dbg->de_last_funcname = funcname;
    else {
	dbg->de_last_funcname->fu_next = funcname;
	dbg->de_last_funcname = funcname;
    }
    dbg->de_funcname_count++;

    return(1);
}


int
_dwarf_transform_funcname_to_disk (
    Dwarf_P_Debug	dbg,
    Dwarf_Error		*error
)
{
	/* Total num of bytes in .debug_funcnames section. */
    Dwarf_Unsigned	funcname_num_bytes;

	/* Total number of bytes excluding the length field. */
    Dwarf_Unsigned	adjusted_length;

	/* Points to first byte of .debug_funcnames buffer. */
    Dwarf_Small		*funcname;

	/* Scans the list of funcnames provided by user. */
    Dwarf_P_Funcname	given_funcname;

	/* Fills in the .debug_funcnames buffer. */
    Dwarf_Small		*funcname_ptr;

	/* Points to first byte of .rel.debug_funcnames section. */
    Dwarf_Small		*funcname_reloc;

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

	/* Size of the .debug_funcnames section header. */
    funcname_num_bytes = 
	SIZEOF_UWORD(dbg) +  	/* Size of length field. */
	sizeof(Dwarf_Half) +    /* Size of version field. */
	SIZEOF_UWORD(dbg) +     /* Size of .debug_info offset. */
	SIZEOF_UWORD(dbg);      /* Size of .debug_info. */

	/* Add the size of the names portion. */
    for (given_funcname = dbg->de_funcname; given_funcname != NULL; 
	given_funcname = given_funcname->fu_next)
	funcname_num_bytes += 
	    SIZEOF_UWORD(dbg) + 		    /* size of die offset. */
	    strlen(given_funcname->fu_name) + 1;    /* size of pub name. */

	/* Size of the last 0 offset. */
    funcname_num_bytes += SIZEOF_UWORD(dbg);

    GET_NEW_CHUNK(dbg, elf_sects[DEBUG_FUNCNAMES], funcname, 
	funcname_num_bytes, error);
    if (funcname == NULL)
	{_dwarf_p_error(dbg, error, DW_DLE_ALLOC_FAIL); return(0);}
    funcname_ptr = funcname;

	/* Write the adjusted length of .debug_funcnames section. */
    adjusted_length = funcname_num_bytes - SIZEOF_UWORD(dbg);
    memcpy(funcname_ptr, GET_DISP(dbg, adjusted_length), SIZEOF_UWORD(dbg));
    funcname_ptr += SIZEOF_UWORD(dbg);

	/* Write the version as 2 bytes. */
    *funcname_ptr = 0;
    funcname_ptr++;
    *funcname_ptr = CURRENT_VERSION_STAMP;
    funcname_ptr++;

	/* Write the offset of the compile-unit. */
    memcpy(funcname_ptr, GET_DISP(dbg, big_zero), SIZEOF_UWORD(dbg));
    funcname_ptr += SIZEOF_UWORD(dbg);

	/* Write the size of .debug_info. */
    memcpy(funcname_ptr, GET_DISP(dbg, debug_info_size), SIZEOF_UWORD(dbg));
    funcname_ptr += SIZEOF_UWORD(dbg);

    reloc_sects[DEBUG_FUNCNAMES] = dbg->de_func(".rel.debug_funcnames", 
	IS_64BIT(dbg), SHT_REL, 0, SHN_UNDEF, elf_sects[DEBUG_FUNCNAMES], 
	&name_idx, &err);
    if (reloc_sects[DEBUG_FUNCNAMES] == -1) {
        DWARF_P_DBG_ERROR(dbg, DW_DLE_ELF_SECT_ERR, DW_DLV_NOCOUNT);
    }

    GET_NEW_CHUNK(dbg, reloc_sects[DEBUG_FUNCNAMES], funcname_reloc, 
	IS_64BIT(dbg)?sizeof(Elf64_Rel):sizeof(Elf32_Rel), error);
    if (funcname_reloc == NULL) 
	{_dwarf_p_error(dbg, error, DW_DLE_ALLOC_FAIL); return(0);}

	/* Write relocation record for .debug_info offset. */
    if (IS_64BIT(dbg)) {
	elf64_reloc = (Elf64_Rel *)funcname_reloc;
	elf64_reloc->r_offset = SIZEOF_UWORD(dbg) + sizeof(Dwarf_Half);
	Set_REL64_info
	    (*elf64_reloc, sect_name_idx[DEBUG_INFO], R_MIPS_64);
    }
    else {
	elf32_reloc = (Elf32_Rel *)funcname_reloc;
	elf32_reloc->r_offset = SIZEOF_UWORD(dbg) + sizeof(Dwarf_Half);
	Set_REL32_info
	    (*elf32_reloc, sect_name_idx[DEBUG_INFO], R_MIPS_32);
    }

    for (given_funcname = dbg->de_funcname; given_funcname != NULL;
	given_funcname = given_funcname->fu_next) {

	    /* Copy offset of die from start of compile-unit. */
	memcpy(funcname_ptr, GET_DISP(dbg, given_funcname->fu_die->di_offset),
	    SIZEOF_UWORD(dbg));
	funcname_ptr += SIZEOF_UWORD(dbg);

	    /* Copy the function name. */
	strcpy((char *)funcname_ptr, given_funcname->fu_name);
	funcname_ptr += strlen(given_funcname->fu_name) + 1;
    }

    memcpy(funcname_ptr, GET_DISP(dbg, big_zero), SIZEOF_UWORD(dbg));

    return dbg->de_n_debug_sect;
}
