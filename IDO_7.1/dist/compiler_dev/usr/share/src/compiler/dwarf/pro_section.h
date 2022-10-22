/*
	pro_section.h 
	$Revision: 1.13 $    $Date: 1994/12/28 19:02:57 $    
	$Source: /hosts/bonnie/proj/irix6.4-ssg/isms/cmplrs/libdwarf/RCS/pro_section.h,v $

	This struct is used to hold information about all
	debug* sections. On creating a new section, section
	names and indices are added to this struct
*/


/* defined used to get at the elf section numbers and section name 
   indices in symtab for the dwarf sections */
#define 	DEBUG_INFO	0
#define		DEBUG_LINE	1
#define		DEBUG_ABBREV	2
#define		DEBUG_FRAME	3
#define		DEBUG_ARANGES	4
#define		DEBUG_PUBNAMES	5
#define		DEBUG_STR	6
#define		DEBUG_FUNCNAMES 7
#define		DEBUG_TYPENAMES 8
#define		DEBUG_VARNAMES  9
#define		DEBUG_WEAKNAMES 10
#define		DEBUG_MACINFO   11

    /* number of debug_* sections not including the relocations */
#define		NUM_DEBUG_SECTIONS	DEBUG_MACINFO + 1

/* struct to hold relocation entries. Its mantained as a linked
   list of relocation structs, and will then be written at as a 
   whole into the relocation section. Whether its 32 bit or
   64 bit will be obtained from Dwarf_Debug pointer.
*/

typedef struct Dwarf_P_Rel_s 	*Dwarf_P_Rel;

struct Dwarf_P_Rel_s {
    union {
    	Elf32_Rel	*rel32;
	Elf64_Rel	*rel64;
    } dr_relrec;
    Dwarf_P_Rel		dr_next;
};

/* macros to access the above structure */
#define dr_rel32(dr_reloc) (dr_reloc->dr_relrec.rel32)
#define dr_rel64(dr_reloc) (dr_reloc->dr_relrec.rel64)


/*
	struct stores a chunk of data pertaining to a section 
*/
struct Dwarf_P_Section_Data_s {
    int 			ds_elf_sect_no;	/* elf section number */
    char 			*ds_data;	/* data contained in section */
    int				ds_nbytes;	/* bytes of data */
    Dwarf_P_Section_Data	ds_next;
};

#define 	CHUNK_SIZE 		4096
		/* size of chunk of data allocated in one alloc */

/*
	chunk alloc routine - 
	if chunk->data is nil, it will alloc CHUNK_SIZE bytes, 
	and return pointer to the beginning. If chunk is not nil, 
	it will see if there's enoungh space for nbytes in current 
	chunk, if not, add new chunk to linked list, and return 
	a char * pointer to it. Return null if unsuccessful.
*/
Dwarf_Small *
    _dwarf_pro_buffer(Dwarf_P_Debug dbg,int sectno, int nbytes, int new_chunk);

#define GET_CHUNK(dbg,sectno,ptr,nbytes,error) \
	{ \
	    (ptr) = _dwarf_pro_buffer((dbg),(sectno),(nbytes),0); \
	    if ((ptr) == NULL) { \
		DWARF_P_DBG_ERROR(dbg,DW_DLE_CHUNK_ALLOC,-1); \
	    } \
	}

#define GET_NEW_CHUNK(dbg,sectno,ptr,nbytes,error) \
	{ \
	    (ptr) = _dwarf_pro_buffer((dbg),(sectno),(nbytes),1); \
	    if ((ptr) == NULL) { \
		DWARF_P_DBG_ERROR(dbg,DW_DLE_CHUNK_ALLOC,-1); \
	    } \
	}

/* 
	Get pointer for the nth byte in the section data. Uses the
	linked list, but is transparent to user of function
*/
char * _dwarf_pro_nth_byteoff(Dwarf_P_Debug dbg, int byteoff);

#define NTH_BYTEOFF(dbg,ptr,n,error) \
	{ \
	    (ptr) = _dwarf_pro_nth_byteoff((dbg),(n)); \
	    if ((ptr) == NULL) { \
		DWARF_P_DBG_ERROR(dbg,DW_DLE_BYTEOFF_ERR,-1); \
	    } \
	}

int
_dwarf_transform_arange_to_disk (
    Dwarf_P_Debug       dbg,
    Dwarf_Error         *error
);

int
_dwarf_transform_pubname_to_disk (
    Dwarf_P_Debug       dbg,
    Dwarf_Error         *error
);

int
_dwarf_transform_funcname_to_disk (
    Dwarf_P_Debug	dbg,
    Dwarf_Error		*error
);

int
_dwarf_transform_typename_to_disk (
    Dwarf_P_Debug	dbg,
    Dwarf_Error		*error
);

int
_dwarf_transform_varname_to_disk (
    Dwarf_P_Debug	dbg,
    Dwarf_Error		*error
);

int
_dwarf_transform_weakname_to_disk (
    Dwarf_P_Debug	dbg,
    Dwarf_Error		*error
);
