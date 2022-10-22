/*
	pro_frame.h
	$Revision: 1.9 $    $Date: 1995/10/20 23:01:06 $    
	$Source : $

	pro_frame.h contains definitions used to create and store
	debug_frame information.
*/

/*
    Largest register value that can be coded into
    the opcode since there are only 6 bits in the
    register field.
*/
#define MAX_6_BIT_VALUE		0x3f

/*
	This struct holds debug_frame instructions
*/
typedef struct Dwarf_P_Frame_Pgm_s 	*Dwarf_P_Frame_Pgm;

struct Dwarf_P_Frame_Pgm_s {
    Dwarf_Ubyte		dfp_opcode;	/* opcode - includes reg # */
    char		*dfp_args;	/* operands */
    int			dfp_nbytes;	/* number of bytes in args */
    Dwarf_P_Frame_Pgm	dfp_next;
};


/*
	This struct has cie related information. Used to gather data 
	from user program, and later to transform to disk form
*/
struct Dwarf_P_Cie_s {
	Dwarf_Ubyte		cie_version;
	char 			*cie_aug;	/* augmentation */
	Dwarf_Ubyte		cie_code_align;	/* alignment of code */
	Dwarf_Sbyte		cie_data_align;	
	Dwarf_Ubyte		cie_ret_reg;	/* return register # */
	char			*cie_inst;	/* initial instruction */
	long			cie_inst_bytes;
						/* no of init_inst */
	Dwarf_P_Cie		cie_next;
};


/* producer fields */
struct Dwarf_P_Fde_s {
	    /* number of bytes */
	Dwarf_Unsigned		fde_length;	

	    /* function/subr die for this fde */
	Dwarf_P_Die		fde_die;	

	    /* index to asso. cie */
	Dwarf_Word 		fde_cie;	

	    /* address of first location */
	Dwarf_Addr		fde_initloc;	

	    /* relocation section symbol ptr */
	Dwarf_Unsigned		fde_r_symidx;	

	    /* bytes of instr for this fde */
	Dwarf_Addr		fde_addr_range;	

	    /* instructions */
	Dwarf_P_Frame_Pgm	fde_inst;	

	    /* number of instructions */ 
	long			fde_n_inst;	

	    /* number of bytes of inst */
	long			fde_n_bytes;	

	    /* offset into exception table for this function. */
	Dwarf_Signed		fde_offset_into_exception_tables;

	    /* The symbol for the exception table elf section. */
	Dwarf_Unsigned		fde_exception_table_symbol;

	    /* pointer to last inst */
	Dwarf_P_Frame_Pgm	fde_last_inst;	

	Dwarf_P_Fde		fde_next;
};
