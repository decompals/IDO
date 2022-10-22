/*


	dwarf_init_finish.c
	$Revision: 1.38 $   $Date: 1996/07/08 21:07:29 $

*/

#include <libelf.h>
#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <string.h>
#include <stdlib.h>

#include "dwarf_incl.h"

#define DWARF_DBG_ERROR(dbg,errval,retval) \
     _dwarf_error(dbg, error, errval); return(retval);

#define FALSE	0
#define TRUE	1

extern Elf64_Ehdr *elf64_getehdr (Elf *);
extern Elf64_Shdr *elf64_getshdr (Elf_Scn *);


/* This static is copied to the dbg on dbg init
   so that the static need not be referenced at
   run time, preserving better locality of
   reference.
   Value is 0 means do the string check.
   Value non-zero means do not do the check.
*/
static Dwarf_Small  _dwarf_assume_string_bad;


int dwarf_set_stringcheck(int newval)
{
	int oldval = _dwarf_assume_string_bad;
	_dwarf_assume_string_bad = newval;
	return oldval;
}

/*
    Given an Elf ptr, set up dbg with pointers
    to all the Dwarf data sections.
    Return NULL on error.

    This function is also responsible for determining
    whether the given object contains Dwarf information
    or not.  The test currently used is that it contains
    either a .debug_info or a .debug_frame section.  If 
    not, it returns DW_DLV_NO_ENTRY causing dwarf_init() also to 
    return DW_DLV_NO_ENTRY.  Earlier, we had thought of using only 
    the presence/absence of .debug_info to test, but we 
    added .debug_frame since there could be stripped objects 
    that have only a .debug_frame section for exception 
    processing.
    DW_DLV_NO_ENTRY or DW_DLV_OK or DW_DLV_ERROR
*/
static int
_dwarf_setup (
    Dwarf_Debug     dbg,
    Elf             *elf,
    Dwarf_Error     *error
)
{
    Elf32_Ehdr      *ehdr32;
    Elf64_Ehdr	    *ehdr64;
    Elf32_Shdr      *shdr32;
    Elf64_Shdr      *shdr64;
    Elf_Scn         *scn;
    Elf_Data        *data;
    char            *scn_name;
    char	    *ehdr_ident;
    int		    is_64bit;
    int		    foundDwarf;

    foundDwarf = FALSE;
    dbg->de_elf = elf;

    dbg->de_assume_string_in_bounds = _dwarf_assume_string_bad;

    if ((ehdr_ident=elf_getident(elf,NULL)) == NULL)
	{DWARF_DBG_ERROR(dbg,DW_DLE_ELF_GETIDENT_ERROR,DW_DLV_ERROR);}

    is_64bit = (ehdr_ident[EI_CLASS] == ELFCLASS64);
    dbg->de_length_size = is_64bit ? 8 : 4;

    if (is_64bit) {
	if ((ehdr64=elf64_getehdr(elf)) == NULL)
	    {DWARF_DBG_ERROR(dbg,DW_DLE_ELF_GETEHDR_ERROR,DW_DLV_ERROR);}
    }
    else {
	if ((ehdr32=elf32_getehdr(elf)) == NULL)
	    {DWARF_DBG_ERROR(dbg,DW_DLE_ELF_GETEHDR_ERROR,DW_DLV_ERROR);}
    }

    scn = NULL;
    while ((scn=elf_nextscn(elf,scn)) != NULL) {

	if (is_64bit) {
            if ((shdr64=elf64_getshdr(scn)) == NULL)
                {DWARF_DBG_ERROR(dbg,DW_DLE_ELF_GETSHDR_ERROR,DW_DLV_ERROR);}
            if ((scn_name=elf_strptr(elf,ehdr64->e_shstrndx,shdr64->sh_name)) 
		== NULL) {DWARF_DBG_ERROR(dbg,DW_DLE_ELF_STRPTR_ERROR,DW_DLV_ERROR);}
	}
	else {
            if ((shdr32=elf32_getshdr(scn)) == NULL)
                {DWARF_DBG_ERROR(dbg,DW_DLE_ELF_GETSHDR_ERROR,NULL);}
            if ((scn_name=elf_strptr(elf,ehdr32->e_shstrndx,shdr32->sh_name)) 
		== NULL) {DWARF_DBG_ERROR(dbg,DW_DLE_ELF_STRPTR_ERROR,DW_DLV_ERROR);}
	}

        if (strncmp(scn_name,".debug_",7)) continue;

        else if (strcmp(scn_name,".debug_info") == 0) {
            if (dbg->de_debug_info != NULL)
                {DWARF_DBG_ERROR(dbg,DW_DLE_DEBUG_INFO_DUPLICATE,DW_DLV_ERROR);}
            if ((data=elf_getdata(scn,0)) == NULL || data->d_size == 0)
                {DWARF_DBG_ERROR(dbg,DW_DLE_DEBUG_INFO_NULL,DW_DLV_ERROR);}
            dbg->de_debug_info = data->d_buf;
            dbg->de_debug_info_size = data->d_size;
	    foundDwarf = TRUE;
        }

        else if (strcmp(scn_name,".debug_abbrev") == 0) {
            if (dbg->de_debug_abbrev != NULL)
                {DWARF_DBG_ERROR(dbg,DW_DLE_DEBUG_ABBREV_DUPLICATE,DW_DLV_ERROR);}
            if ((data=elf_getdata(scn,0)) == NULL || data->d_size == 0)
                {DWARF_DBG_ERROR(dbg,DW_DLE_DEBUG_ABBREV_NULL,DW_DLV_ERROR);}
            dbg->de_debug_abbrev = data->d_buf;
            dbg->de_debug_abbrev_size = data->d_size;
        }

        else if (strcmp(scn_name,".debug_aranges") == 0) {
            if (dbg->de_debug_aranges != NULL)
                {DWARF_DBG_ERROR(dbg,DW_DLE_DEBUG_ARANGES_DUPLICATE,DW_DLV_ERROR);}
            if ((data=elf_getdata(scn,0)) == NULL || data->d_size == 0)
                {DWARF_DBG_ERROR(dbg,DW_DLE_DEBUG_ARANGES_NULL,DW_DLV_ERROR);}
            dbg->de_debug_aranges = data->d_buf;
            dbg->de_debug_aranges_size = data->d_size;
        }

        else if (strcmp(scn_name,".debug_line") == 0) {
            if (dbg->de_debug_line != NULL)
                {DWARF_DBG_ERROR(dbg,DW_DLE_DEBUG_LINE_DUPLICATE,DW_DLV_ERROR);}
            if ((data=elf_getdata(scn,0)) == NULL || data->d_size == 0)
                {DWARF_DBG_ERROR(dbg,DW_DLE_DEBUG_LINE_NULL,DW_DLV_ERROR);}
            dbg->de_debug_line = data->d_buf;
            dbg->de_debug_line_size = data->d_size;
        }

        else if (strcmp(scn_name,".debug_frame") == 0) {
            if (dbg->de_debug_frame != NULL)
                {DWARF_DBG_ERROR(dbg,DW_DLE_DEBUG_FRAME_DUPLICATE,DW_DLV_ERROR);}
            if ((data=elf_getdata(scn,0)) == NULL || data->d_size == 0)
                {DWARF_DBG_ERROR(dbg,DW_DLE_DEBUG_FRAME_NULL,DW_DLV_ERROR);}
            dbg->de_debug_frame = data->d_buf;
            dbg->de_debug_frame_size = data->d_size;
	    foundDwarf = TRUE;
        }

        else if (strcmp(scn_name,".debug_loc") == 0) {
            if (dbg->de_debug_loc != NULL)
                {DWARF_DBG_ERROR(dbg,DW_DLE_DEBUG_LOC_DUPLICATE,DW_DLV_ERROR);}
            if ((data=elf_getdata(scn,0)) == NULL || data->d_size == 0)
                {DWARF_DBG_ERROR(dbg,DW_DLE_DEBUG_LOC_NULL,DW_DLV_ERROR);}
            dbg->de_debug_loc = data->d_buf;
            dbg->de_debug_loc_size = data->d_size;
        }

        else if (strcmp(scn_name,".debug_macinfo") == 0) {
            if (dbg->de_debug_macinfo != NULL)
                {DWARF_DBG_ERROR(dbg,DW_DLE_DEBUG_MACINFO_DUPLICATE,DW_DLV_ERROR);}
            if ((data=elf_getdata(scn,0)) == NULL || data->d_size == 0)
                {DWARF_DBG_ERROR(dbg,DW_DLE_DEBUG_MACINFO_NULL,DW_DLV_ERROR);}
            dbg->de_debug_macinfo = data->d_buf;
            dbg->de_debug_macinfo_size = data->d_size;
        }

        else if (strcmp(scn_name,".debug_pubnames") == 0) {
            if (dbg->de_debug_pubnames != NULL)
                {DWARF_DBG_ERROR(dbg,DW_DLE_DEBUG_PUBNAMES_DUPLICATE,DW_DLV_ERROR);}
            if ((data=elf_getdata(scn,0)) == NULL || data->d_size == 0)
                {DWARF_DBG_ERROR(dbg,DW_DLE_DEBUG_PUBNAMES_DUPLICATE,DW_DLV_ERROR);}
            dbg->de_debug_pubnames = data->d_buf;
            dbg->de_debug_pubnames_size = data->d_size;
        }

        else if (strcmp(scn_name,".debug_str") == 0) {
            if (dbg->de_debug_str != NULL)
                {DWARF_DBG_ERROR(dbg,DW_DLE_DEBUG_STR_DUPLICATE,DW_DLV_ERROR);}
            if ((data=elf_getdata(scn,0)) == NULL || data->d_size == 0)
                {DWARF_DBG_ERROR(dbg,DW_DLE_DEBUG_STR_NULL,DW_DLV_ERROR);}
            dbg->de_debug_str = data->d_buf;
            dbg->de_debug_str_size = data->d_size;
        }

        else if (strcmp(scn_name,".debug_funcnames") == 0) {
            if (dbg->de_debug_funcnames != NULL)
                {DWARF_DBG_ERROR(dbg,DW_DLE_DEBUG_FUNCNAMES_DUPLICATE,DW_DLV_ERROR);}
            if ((data=elf_getdata(scn,0)) == NULL || data->d_size == 0)
                {DWARF_DBG_ERROR(dbg,DW_DLE_DEBUG_FUNCNAMES_NULL,DW_DLV_ERROR);}
            dbg->de_debug_funcnames = data->d_buf;
            dbg->de_debug_funcnames_size = data->d_size;
        }

        else if (strcmp(scn_name,".debug_typenames") == 0) {
            if (dbg->de_debug_typenames != NULL)
                {DWARF_DBG_ERROR(dbg,DW_DLE_DEBUG_TYPENAMES_DUPLICATE,DW_DLV_ERROR);}
            if ((data=elf_getdata(scn,0)) == NULL || data->d_size == 0)
                {DWARF_DBG_ERROR(dbg,DW_DLE_DEBUG_TYPENAMES_NULL,DW_DLV_ERROR);}
            dbg->de_debug_typenames = data->d_buf;
            dbg->de_debug_typenames_size = data->d_size;
        }

        else if (strcmp(scn_name,".debug_varnames") == 0) {
            if (dbg->de_debug_varnames != NULL)
                {DWARF_DBG_ERROR(dbg,DW_DLE_DEBUG_VARNAMES_DUPLICATE,DW_DLV_ERROR);}
            if ((data=elf_getdata(scn,0)) == NULL || data->d_size == 0)
                {DWARF_DBG_ERROR(dbg,DW_DLE_DEBUG_VARNAMES_NULL,DW_DLV_ERROR);}
            dbg->de_debug_varnames = data->d_buf;
            dbg->de_debug_varnames_size = data->d_size;
        }

        else if (strcmp(scn_name,".debug_weaknames") == 0) {
            if (dbg->de_debug_weaknames != NULL)
                {DWARF_DBG_ERROR(dbg,DW_DLE_DEBUG_WEAKNAMES_DUPLICATE,DW_DLV_ERROR);}
            if ((data=elf_getdata(scn,0)) == NULL || data->d_size == 0)
                {DWARF_DBG_ERROR(dbg,DW_DLE_DEBUG_WEAKNAMES_NULL,DW_DLV_ERROR);}
            dbg->de_debug_weaknames = data->d_buf;
            dbg->de_debug_weaknames_size = data->d_size;
        }
        else if (strcmp(scn_name,".debug_macinfo") == 0) {
            if (dbg->de_debug_macinfo != NULL)
                {DWARF_DBG_ERROR(dbg,DW_DLE_DEBUG_MACINFO_DUPLICATE,DW_DLV_ERROR);}
            if ((data=elf_getdata(scn,0)) == NULL || data->d_size == 0)
                {DWARF_DBG_ERROR(dbg,DW_DLE_DEBUG_MACINFO_NULL,DW_DLV_ERROR);}
            dbg->de_debug_macinfo = data->d_buf;
            dbg->de_debug_macinfo_size = data->d_size;
        }
    }
    if(foundDwarf) {
	return DW_DLV_OK;
    }

    return(DW_DLV_NO_ENTRY);
}


/*
    The basic dwarf initializer function for consumers.
    Return NULL on error.
*/
int
dwarf_init (
    int             fd,
    Dwarf_Unsigned  access,
    Dwarf_Handler   errhand,
    Dwarf_Ptr       errarg,
    Dwarf_Debug     * ret_dbg,
    Dwarf_Error     *error
)
{
    Dwarf_Debug     dbg;
    struct stat     fstat_buf;
    Elf             *elf;
    int res;

    dbg = _dwarf_get_debug();
    if (dbg == NULL)
        {DWARF_DBG_ERROR(dbg,DW_DLE_DBG_ALLOC,DW_DLV_ERROR);}
    dbg->de_errhand = errhand;
    dbg->de_errarg = errarg;

    if (fstat(fd,&fstat_buf) != 0)
        {DWARF_DBG_ERROR(dbg,DW_DLE_FSTAT_ERROR,DW_DLV_ERROR);}
    if (!S_ISREG(fstat_buf.st_mode))
        {DWARF_DBG_ERROR(dbg,DW_DLE_FSTAT_MODE_ERROR,DW_DLV_ERROR);}

    if (access != DW_DLC_READ)
        {DWARF_DBG_ERROR(dbg,DW_DLE_INIT_ACCESS_WRONG,DW_DLV_ERROR);}
    dbg->de_access = access;

    elf_version(EV_CURRENT); 
    /* changed to mmap request per bug 281217. 6/95 */
    if ((elf=elf_begin(fd,ELF_C_READ_MMAP,0)) == NULL)
        {DWARF_DBG_ERROR(dbg,DW_DLE_ELF_BEGIN_ERROR,DW_DLV_ERROR);}

    if ((res =_dwarf_setup(dbg, elf,  error)) != DW_DLV_OK) {
	free(dbg);
	return(res);
    }

    /* call cannot fail: no malloc or free involved */
    _dwarf_setup_debug(dbg);

    *ret_dbg = dbg;
    return(DW_DLV_OK);
}


/*
    The alternate dwarf setup call for consumers
*/
int
dwarf_elf_init (
    Elf             *elf_file_pointer,
    Dwarf_Unsigned  access,
    Dwarf_Handler   errhand,
    Dwarf_Ptr       errarg,
    Dwarf_Debug     * ret_dbg,
    Dwarf_Error     *error
)
{
    Dwarf_Debug     dbg;
    int res;

    dbg = _dwarf_get_debug();
    if (dbg == NULL)
        {DWARF_DBG_ERROR(dbg,DW_DLE_DBG_ALLOC,DW_DLV_ERROR);}
    dbg->de_errhand = errhand;
    dbg->de_errarg = errarg;

    if (access != DW_DLC_READ)
        {DWARF_DBG_ERROR(dbg,DW_DLE_INIT_ACCESS_WRONG,DW_DLV_ERROR);}
    dbg->de_access = access;

    if((res =_dwarf_setup(dbg, elf_file_pointer,  error)) !=DW_DLV_OK){
	free(dbg); 
	return(res);
    }

    /* this call cannot fail: allocates nothing, releases nothing */
    _dwarf_setup_debug(dbg);

    *ret_dbg = dbg;
    return(DW_DLV_OK);
}


/*
	Frees all memory that was not previously freed
	by dwarf_dealloc.
	Aside from certain categories.
*/ 
int
dwarf_finish (
    Dwarf_Debug		dbg,
    Dwarf_Error		*error
)
{
    int res = _dwarf_free_all_of_one_debug(dbg);
    if(res == DW_DLV_ERROR) {
        {DWARF_DBG_ERROR(dbg,DW_DLE_DBG_ALLOC,DW_DLV_ERROR);}
    }

    return res;

	
}


/*
    This function returns the Elf * pointer
    associated with a Dwarf_Debug.
*/
int
dwarf_get_elf (
    Dwarf_Debug		dbg,
    Elf   **		elf,
    Dwarf_Error		*error
)
{
    if (dbg == NULL) {
	_dwarf_error(NULL, error, DW_DLE_DBG_NULL);
	return(DW_DLV_ERROR);
    }

    *elf = dbg->de_elf;
    return(DW_DLV_OK);
}
