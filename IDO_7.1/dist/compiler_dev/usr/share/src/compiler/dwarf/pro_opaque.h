/* 
   pro_opaque.h

   This file defines all the opaque structures for which pointers
   are provided in libdwarf.h.  In addition, it defines private
   structures that are used internally by libdwarf.

    $Revision: 1.6 $ $Date: 1996/07/23 23:26:47 $
*/

/* 
    Sgidefs included to define __uint32_t, 
    a guaranteed 4-byte quantity.            
*/
#include <sgidefs.h>

#define true                    1
#define false                   0

/* to identify a cie */
#define DW_CIE_ID 		~(0x0)
#define DW_CIE_VERSION		1

/*Dwarf_Word  is unsigned word usable for index, count in memory */
/*Dwarf_Sword is   signed word usable for index, count in memory */
/* The are 32 or 64 bits depending if 64 bit longs or not, which
** fits the  ILP32 and LP64 models
** These work equally well with ILP64.
*/

typedef unsigned long               Dwarf_Word;
typedef long		    	    Dwarf_Sword;


typedef signed char                 Dwarf_Sbyte;
typedef unsigned char               Dwarf_Ubyte;
typedef signed short		    Dwarf_Shalf;


/* these 2 are fixed sizes which must not vary with the
** ILP32/LP64 model. Between these two, stay at 32 bit.
*/
typedef __uint32_t                  Dwarf_ufixed;
typedef __int32_t                   Dwarf_sfixed;

/*
	In various places the code mistakenly associates
	forms 8 bytes long with Dwarf_Signed or Dwarf_Unsigned
	This is not a very portable assumption.
        The following should be used instead for 64 bit integers.
*/
typedef __uint32_t                  Dwarf_ufixed64;
typedef __int32_t                   Dwarf_sfixed64;


/* 
	producer:
	This struct is used to hold information about all
	debug* sections. On creating a new section, section
	names and indices are added to this struct
	definition in pro_section.h
*/
typedef struct Dwarf_P_Section_Data_s 	*Dwarf_P_Section_Data;

/*
	producer:
	This struct is used to hold entries in the include directories
	part of statement prologue. Definition in pro_line.h
*/
typedef struct Dwarf_P_Inc_Dir_s 	*Dwarf_P_Inc_Dir;

/*
	producer:
	This struct holds file entries for the statement prologue. 
	Defined in pro_line.h
*/
typedef struct Dwarf_P_F_Entry_s	 *Dwarf_P_F_Entry;

/*
	producer:
	This struct holds information for each cie. Defn in pro_frame.h
*/
typedef struct Dwarf_P_Cie_s 		*Dwarf_P_Cie;

/*
	producer:
	Struct to hold line number information, different from 
	Dwarf_Line opaque type.
*/
typedef struct Dwarf_P_Line_s	 	*Dwarf_P_Line;

/*
	producer:
	Struct to hold information about address ranges.
*/
typedef struct Dwarf_P_Arange_s		*Dwarf_P_Arange;
typedef struct Dwarf_P_Pubname_s	*Dwarf_P_Pubname;
typedef struct Dwarf_P_Funcname_s	*Dwarf_P_Funcname;
typedef struct Dwarf_P_Typename_s	*Dwarf_P_Typename;
typedef struct Dwarf_P_Varname_s	*Dwarf_P_Varname;
typedef struct Dwarf_P_Weakname_s	*Dwarf_P_Weakname;

/* fields used by producer */
struct Dwarf_P_Die_s {
    Dwarf_Unsigned	 di_offset;		/* offset in debug info */
    char *	 	 di_abbrev;		/* abbreviation */
    Dwarf_Word		 di_abbrev_nbytes;	/* # of bytes in abbrev */
    Dwarf_Tag		 di_tag;
    Dwarf_P_Die		 di_parent;		/* parent of current die */
    Dwarf_P_Die		 di_child;		/* first child */
    Dwarf_P_Die		 di_left;		/* left sibling */
    Dwarf_P_Die		 di_right;		/* right sibling */
    Dwarf_P_Attribute 	 di_attrs;		/* list of attributes */
    Dwarf_P_Attribute 	 di_last_attr;		/* last attribute */
    Dwarf_Word		 di_n_attr;		/* number of attributes */
};


/* producer fields */
struct Dwarf_P_Attribute_s {
    Dwarf_Half          ar_attribute;		/* Attribute Value. */
    Dwarf_Half          ar_attribute_form;	/* Attribute Form. */
    Dwarf_P_Die		ar_ref_die;	/* die pointer if form ref */
    char 		*ar_data;	/* data, format given by form */
    Dwarf_Unsigned	ar_nbytes;	/* no. of bytes of data */
    Dwarf_Word		ar_rel_symidx;	/* when attribute has a relocatable
					   value, holds index of symbol in 
					   SYMTAB */
    Dwarf_Ubyte		ar_rel_type;	/* relocation type */
    Dwarf_Word		ar_rel_offset;	/* Offset of relocation within block */
    Dwarf_P_Attribute	ar_next;
};

/* A block of .debug_macinfo data: this forms a series of blocks.
** Each macinfo input is compressed immediately and put into
** the current block if room, else a newblock allocated.
** The space allocation is such that the block and the macinfo
** data are one malloc block: free with a pointer to this and the
** mb_data is freed automatically.
** Like the struct hack, but legal ANSI C.
*/
struct dw_macinfo_block_s {
   struct dw_macinfo_block_s *mb_next;
   unsigned long	mb_avail_len;
   unsigned long	mb_used_len;
   unsigned long	mb_macinfo_data_space_len;
   char *		mb_data; /* original malloc ptr. */
};


/* Fields used by producer */
struct Dwarf_P_Debug_s {
    Elf                 *de_elf;

    Dwarf_Unsigned      de_access;
    Dwarf_Handler       de_errhand;
    Dwarf_Ptr           de_errarg;

        /* 
	    Call back function, used to create .debug* sections.  
	    Provided by user 
	*/ 
    Dwarf_Callback_Func		de_func;     

        /* Flags indication 64/32, got from producer_init */
    Dwarf_Unsigned		de_flags;    

        /* This holds information about each debug section */
    Dwarf_P_Section_Data	de_debug_sects; 

        /* Pointer to the last section */
    Dwarf_P_Section_Data	de_last_debug_sect;

        /* Number of debug data sections */
    Dwarf_Word 			de_n_debug_sect;

	/* Holds an array of file entry information, null terminated */
    Dwarf_P_F_Entry		de_file_entries;

        /* last file entry */
    Dwarf_P_F_Entry		de_last_file_entry;

        /* Number of file entries, needed to return index of file */
    Dwarf_Unsigned		de_n_file_entries;

        /* Has the directories used to search for source files */
    Dwarf_P_Inc_Dir		de_inc_dirs;

        /* Last include directory */
    Dwarf_P_Inc_Dir		de_last_inc_dir;

        /* Number of include directories, needed to return dir index */
    Dwarf_Unsigned 		de_n_inc_dirs;

        /* Has all the line number info for the stmt program */
    Dwarf_P_Line		de_lines;

        /* Handle to the last line number entry. */
    Dwarf_P_Line		de_last_line;

        /* List of cie's for the debug unit */
    Dwarf_P_Cie			de_frame_cies;

        /* Number of cie entries */
    Dwarf_Unsigned		de_n_cie;

        /* Pointer to the last entry */
    Dwarf_P_Cie			de_last_cie;

        /* List of fde's for the debug unit */
    Dwarf_P_Fde			de_frame_fdes;

	/* Number of fde's. */
    Dwarf_Unsigned		de_n_fde;

        /* Pointer to the last entry */
    Dwarf_P_Fde			de_last_fde;

        /* First die, leads to all others */
    Dwarf_P_Die 		de_dies;   

        /* Pointer to list of strings */
    char			*de_strings;

	/* Pointer to chain of aranges */
    Dwarf_P_Arange		de_arange;

        /* Pointer to last arange */
    Dwarf_P_Arange		de_last_arange;

	/* Number of aranges */
    Dwarf_Sword			de_arange_count;

	/* Pointer to chain of pubnames. */
    Dwarf_P_Pubname		de_pubname;

	/* Pointer to last pubname. */
    Dwarf_P_Pubname		de_last_pubname;

	/* Number of pubnames. */
    Dwarf_Sword			de_pubname_count;

	/* Pointer to chain of function names. */
    Dwarf_P_Funcname		de_funcname;

	/* Pointer to last function name. */
    Dwarf_P_Funcname		de_last_funcname;

        /* Number of function names. */
    Dwarf_Sword			de_funcname_count;

	/* Pointer to chain of type names. */
    Dwarf_P_Typename		de_typename;

	/* Pointer to last type name. */
    Dwarf_P_Typename		de_last_typename;

        /* Number of type names. */
    Dwarf_Sword			de_typename_count;

	/* Pointer to chain of variable names. */
    Dwarf_P_Varname		de_varname;

	/* Pointer to last variable name. */
    Dwarf_P_Varname		de_last_varname;

        /* Number of variable names. */
    Dwarf_Sword			de_varname_count;

	/* Pointer to chain of weak names. */
    Dwarf_P_Weakname		de_weakname;

	/* Pointer to last weak name. */
    Dwarf_P_Weakname		de_last_weakname;

        /* Number of weak names. */
    Dwarf_Sword			de_weakname_count;

    /* macinfo controls. */
    /* first points to beginning of the list during creation */
    struct dw_macinfo_block_s *de_first_macinfo;

    /* current points to the current, unfilled, block */
    struct dw_macinfo_block_s *de_current_macinfo;

};

#define CURRENT_VERSION_STAMP		2
