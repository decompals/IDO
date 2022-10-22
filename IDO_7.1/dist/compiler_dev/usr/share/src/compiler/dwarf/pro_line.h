/*
	pro_line.h 
	$Revision: 1.6 $    $Date: 1996/07/08 21:07:42 $    

	Contains header file descriptions for line number information
*/

#define VERSION				2
#define MIN_INST_LENGTH			4
#define DEFAULT_IS_STMT			false
			/* line base and range are temporarily defines.
			   They need to be calculated later */
#define LINE_BASE			-1
#define LINE_RANGE			4

#define OPCODE_BASE			10
#define MAX_OPCODE			255


/*
	This struct is used to hold entries in the include directories
	part of statement prologue.
*/
struct Dwarf_P_Inc_Dir_s {
    char 		*did_name;		/* name of directory */
    Dwarf_P_Inc_Dir	did_next;
};


/*
	This struct holds file entries for the statement prologue. 
	Defined in pro_line.h
*/
struct Dwarf_P_F_Entry_s {
    char 		*dfe_name;
    char		*dfe_args;	/* has dir index, time of modification,
					   length in bytes. Encodes as leb128 */
    int			dfe_nbytes;	/* number of bytes in args */
    Dwarf_P_F_Entry	dfe_next;
};


/*
	Struct holding line number information for each of the producer 
	line entries 
*/
struct Dwarf_P_Line_s {
        /* code address */
    Dwarf_Addr              dpl_address;	

        /* file index, index into file entry */
    Dwarf_Word              dpl_file;		

        /* line number */
    Dwarf_Word              dpl_line;		

        /* column number */
    Dwarf_Word              dpl_column;		

        /* whether its a beginning of a stmt */
    Dwarf_Ubyte		    dpl_is_stmt;	

        /* whether its a beginning of basic blk */
    Dwarf_Ubyte		    dpl_basic_block;	

        /* used to store opcodes set_address, and end_seq */
    Dwarf_Ubyte		    dpl_opc;	

        /* 
	    Used only for relocations.  Has index of symbol 
	    relative to which relocation has to be done 
	    (the S part in S + A) 
        */
    Dwarf_Unsigned	    dpl_r_symidx;	

    Dwarf_P_Line	    dpl_next;
};

/* 
	to initialize state machine registers, definition in 
	pro_line.c
*/
void _dwarf_pro_reg_init (Dwarf_P_Line);
