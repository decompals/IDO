#ifndef _LIBXLATE_H
#define _LIBXLATE_H
#ifdef __cplusplus
extern "C" {
#endif

/*
    libXlate.h

    $Revision: 1.6 $				$Date: 1994/06/16 21:24:52 $
*/


#include <libelf.h>
#include <dwarf.h>
#include <libdwarf.h>

/*
    The following enumerates the different kinds of tables possible.  
    We assume that the new address range is contiguous, and ranges
    are merged as much as possible. 
    
    TK_general tables have all 3 numbers for every entry, and will 
    be used for cord2 and composed tables.  
    
    TK_preserve_size will be used for cord and transformations that 
    basically move blocks of code around without adding or deleting 
    any instructions.  For these tables, there will be only 2 numbers 
    for each entry.  The 3rd number will be dropped due to being 0 
    always.  
    
    TK_preserve_order will be used for pixie.  Again, there will be 
    2 numbers per entry, but this time the 2nd number will be dropped 
    because the next old address of successive entries will be one 
    instruction greater than the last instruction of the preceding
    range.
*/
typedef enum {	
		TK_general,
		TK_preserve_size,
		TK_preserve_order
} Xlate_TableKind;


/*
    We use different structs as handles for the Producer and Consumer.
*/
typedef struct Xlate_Table_Pro_s		*Xlate_Table_Pro;
typedef struct Xlate_Table_Con_s		*Xlate_Table_Con;

/*
    This struct is used to return pieces of the fully expanded
    table to the user.  It is used to return the ranges that
    correspond to a given contiguous range.
*/
typedef struct Xlate_Block_s {
    Elf64_Addr		xe_newAddress;
    Elf32_Sword		xe_newRange;
    Elf64_Addr		xe_oldAddress;
    Elf32_Sword		xe_oldRange;
} Xlate_Block;


/*
    Version number that also serves as the magic word
    for our tables.
*/
#define TB_TABLE_VERSION_MAIN	0x1
#define TB_TABLE_VERSION_COPY	0x2


/*
    I intend to adopt Dave's idea of making every interface routine
    uniformly return an error code.  Actual information will be passed
    through parameters.
*/
#define TB_STATUS_NO_ERROR		0
#define TB_STATUS_ALLOC_FAIL		-1
#define TB_STATUS_NULL_TABLE		-2
#define TB_STATUS_BAD_TABLEKIND		-3
#define TB_STATUS_BAD_ADD_ADDR		-4
#define TB_STATUS_RET_ADDR_NULL		-6
#define TB_STATUS_NO_MORE_BLOCKS	-7
#define TB_STATUS_NOT_YET_IMPLEMENT	-8
#define TB_STATUS_NO_DEBUG		-9
#define TB_STATUS_ALREADY_DONE		-10
#define TB_STATUS_ADDR_UNALIGNED	-11
#define TB_STATUS_RANGE_BAD		-12

#define TB_STATUS_FSTAT_ERROR		-13
#define TB_STATUS_MMAP_ERROR		-14
#define TB_STATUS_BAD_VERSION		-15
#define TB_STATUS_NULL_HEADER		-16
#define TB_STATUS_NO_HEADER		-17
#define TB_STATUS_BAD_FILE_SIZE		-18
#define TB_STATUS_NEW_ADDR_ERROR	-19
#define TB_STATUS_DECODE_ERROR		-20
#define TB_STATUS_BAD_BLOCK_INDEX	-21
#define TB_STATUS_UPPER_ADDR_BAD	-22
#define TB_STATUS_TABLE_NOT_PO		-23
#define TB_STATUS_MUNMAP_ERROR		-24
#define TB_STATUS_ELF_IDENT_BAD		-25
#define TB_STATUS_ELF_SHDR_BAD		-26
#define TB_STATUS_NO_XLATE		-27
#define TB_STATUS_NO_XLATE_DATA		-28
#define TB_STATUS_XLATE_BAD		-29
#define TB_STATUS_XLATE_DEBUG_BAD	-30
#define TB_STATUS_ELF_VERSION_BAD	-31
#define TB_STATUS_ELF_BEGIN_BAD		-32
#define TB_STATUS_NOT_ELF		-33
#define TB_STATUS_OLD_ADDR_ERROR	-34
#define TB_STATUS_ADD_TOO_LATE		-35
#define TB_STATUS_BAD_REG_VAL		-36
#define TB_STATUS_BAD_REG_OP		-37
#define TB_STATUS_BAD_FRAME_OP		-38
#define TB_STATUS_NO_REG_INFO		-39



/***************** Producer Interface *******************/

/*
    Creating a table begins with this function.  It returns a 
    handle in *retTable that should be used for all operations
    on this table.  The user specifies the tableKind.  Whether
    the table is for a 64-bit object, in is64Bit.  If so, the 
    upper 32 bits of all new addresses is specified in upper32BitsNew,
    and the upper 32 bits of all old addresses is specified in 
    upper32BitsOld.  Ragnarok guarantees that no text segment 
    will cross a 256MB boundary.  This means that all addresses 
    handled by the producer interface can be 32-bits.

    oldTable is a pointer to the old table.  makeDebug is a flag 
    that indicates whether a debugging translation table should 
    be created or not.  
*/
Elf32_Sword
xlate_pro_init (
    Xlate_Table_Pro *	/*retTable*/,
    Xlate_TableKind     /*tableKind*/,
    void *		/*oldTable*/,
    Elf32_Word		/*makeDebug*/,
    Elf32_Word         	/*is64Bit*/,
    Elf32_Word        	/*upper32BitsNew*/,
    Elf32_Word		/*upper32BitsOld*/
);


/*
    This routine is used to add miscellaneous
    information about the translation to the
    appropriate table.  The information must
    be added before xlate_pro_disk_header() or
    its debugging table counterpart has been
    called.
*/
Elf32_Sword
xlate_pro_add_info (
    Xlate_Table_Pro     /*table*/,
    Elf32_Sword         /*dataMoved*/,
    Elf32_Word          /*startupFwa*/,
    Elf32_Word          /*startupLwa*/,
    Elf32_Word          /*oldTextExists*/,
    Elf32_Word          /*oldTextAlloc*/
);


/*
    This routine is used to add information about the translation
    of register in the transformation process.  This routine if
    called, should be called before xlate_pro_disk_header() or its
    debugging counterpart.
*/
Elf32_Sword
xlate_pro_add_reg_info (
    Xlate_Table_Pro     /*table*/,
    Dwarf_Small         /*op*/,
    Dwarf_Unsigned      /*val1*/,
    Dwarf_Unsigned      /*val2*/
);


/*
    This function is used to add entries to a table of 
    TK_general kind.  The function checks that the table 
    specified has this tableKind.  The values of newAddress 
    provided must be non-decreasing.  The function checks 
    that the range of old addresses specified by oldRange 
    is not greater than the range of the corresponding new 
    addresses.
*/
Elf32_Sword
xlate_pro_add_range_GE (
    Xlate_Table_Pro     /*table*/,
    Elf32_Word        	/*newAddress*/,
    Elf32_Sword		/*newRange*/,
    Elf32_Word        	/*oldAddress*/,
    Elf32_Sword        	/*oldRange*/
);


/*
    This function is used to add entries to a table of 
    TK_preserve_size kind.  It checks that the table 
    specified has the right tableKind.  It also checks 
    that values of newAddress given is non-decreasing.
*/
Elf32_Sword
xlate_pro_add_range_PS (
    Xlate_Table_Pro     /*table*/,
    Elf32_Word        	/*newAddress*/,
    Elf32_Word        	/*oldAddress*/
);


/*
    This function is used to add entries to a table of 
    TK_preserve_order kind.  It checks that the given 
    table has the right tableKind.  It also checks that 
    the values of newAddress as well as the values of 
    oldAddress specified are non-decreasing.
*/
Elf32_Sword
xlate_pro_add_range_PO (
    Xlate_Table_Pro     /*table*/,
    Elf32_Word        	/*newAddress*/,
    Elf32_Word        	/*oldAddress*/
);


/*
    This function is called after the user has added all the entries 
    they want to add, and is used to prepare the user to receive the 
    data blocks for the table.  It is the user's responsibility to do 
    the actual writing of the table data.  The library returns the 
    table in blocks.  It is the user's responsibility to free the 
    blocks returned.  The user gets the blocks by iteratively calling 
    xlate_pro_disk_next_block().  This routine tells the user how much 
    memory is needed to store the entire table in *totalMemoryReq.  It 
    tells them how many blocks there are, and thus how many times 
    xlate_pro_disk_next_block() needs to be called in *numBlocks.  
    
    However, the user need not actually allocate *totalMemoryReq amount 
    of memory if they don't want to construct a contiguous table.  They 
    can deal with each block as they get it, if they wish.

    This call gets information for the main table.  The same process 
    must be repeated with a similar pair of calls to get the debugging
    table.  The user ofcourse has to remember whether they asked for
    a debugging table or not in xlate_pro_table_init().
*/
Elf32_Sword
xlate_pro_disk_header (
    Xlate_Table_Pro     /*table*/,
    Elf32_Sword *	/*totalMemoryReq*/,
    Elf32_Sword *	/*numBlocks*/
);


/*
    This function returns the next block of the main table to the 
    user.  It returns a pointer to the block in *data, and the size 
    of the block in *dataSize.  It is the user's responsibility to 
    free the memory pointed to by *data.
*/
Elf32_Sword
xlate_pro_disk_next_block (
    Xlate_Table_Pro	/*table*/,
    void **		/*data*/,
    Elf32_Sword *	/*dataSize*/
);


/*
    Conterpart of xlate_pro_disk_header() for the debugging table.
*/
Elf32_Sword
xlate_pro_disk_header_debug (
    Xlate_Table_Pro     /*table*/,
    Elf32_Sword *	/*totalMemoryReq*/,
    Elf32_Sword *	/*numBlocks*/
);


/*
    Counterpart of xlate_pro_next_block() for the debugging table.
*/
Elf32_Sword
xlate_pro_disk_next_block_debug (
    Xlate_Table_Pro	/*table*/,
    void **		/*data*/,
    Elf32_Sword *	/*dataSize*/
);


/*
    This function is used to close the table handle, and deallocate
    all the memory used by this table, that was not given to the user
    by the xlate_pro_disk_next_block() and xlate_pro_disk_next_block_debug() 
    functions.  The handle becomes invalid after this call.
*/
Elf32_Sword
xlate_pro_finish (
    Xlate_Table_Pro     /*table*/
);


/******************** Consumer Interface *************************/


/*
    This struct is used to return the exact
    register (frame) instruction recorded in
    the register information portion.  This
    is expected to be identical to the calls
    made by the producer.  This is also why
    we are not using Dwarf_Frame_Op for this
    purpose.
*/
typedef struct {
    Dwarf_Small		sr_op;
    Dwarf_Unsigned	sr_val1;
    Dwarf_Unsigned	sr_val2;
} Xlate_Reg_Instr;


/*
    This routine initializes a table handle to perform address
    translations for an object.  Given an open file descriptor,
    this routine looks for sections of type SHT_MIPS_XLATE and
    SHT_MIPS_XLATE_DEBUG.  The first one is assumed to contain
    the main translation table, the second one the debugging
    table.  It is also assumed that there is only one section
    of each of these types.  Libelf routines are used for the 
    lookup.

    This routine will return an error if the given file turns
    out to be not an Elf file.

    *debugFound is TRUE if a debugging translation table was found.  
    It returns an error code if the main translation table was not 
    found.
*/
Elf32_Sword
xlate_init_fd (
    int			/*fd*/,
    Xlate_Table_Con *	/*table*/,
    Elf32_Word *	/*debugFound*/
);


/*
    Identical to xlate_init_fd() except that an open Elf
    descriptor is given instead of an open file descriptor.
*/
Elf32_Sword
xlate_init_elf (
    Elf	*		/*elf*/,
    Xlate_Table_Con *	/*table*/,
    Elf32_Word *	/*debugFound*/
);


/*
    This routine returns miscellaneous information
    about the translation table.
*/
Elf32_Sword
xlate_get_info (
    Xlate_Table_Con     /*table*/,
    Elf32_Word          /*useDebug*/,
    Elf64_Addr *	/*newAddrLow*/,
    Elf64_Addr *	/*newAddrHigh*/,
    Elf64_Addr *	/*oldAddrLow*/,
    Elf64_Addr *	/*oldAddrHigh*/,
    Elf64_Addr *	/*startupFwa*/,
    Elf64_Addr *	/*startupLwa*/,
    Elf32_Word *	/*oldTextExists*/,
    Elf32_Word *	/*oldTextAlloc*/,
    Elf32_Word *	/*debugDuplicate*/
);


Elf32_Sword
xlate_make_jumptable (
    Xlate_Table_Con     /*table*/,
    Elf32_Word         	/*useDebug*/,
    Elf32_Word **	/*jumpTable*/,
    Elf32_Sword *	/*jumpTableEntries*/,
    Elf32_Word *	/*upper32BitsNew*/,
    Elf32_Word *	/*upper32BitsOld*/
);


/*
    This routine takes a table descriptor, table, and an address,
    addressIn, and returns a corresponding address, *addressOut.  
    The caller also specifies whether the given address is an old 
    or a new address, and the routine returns the corresponding 
    new or old address respectively.  The given address is a new 
    address if isNewAddress is TRUE, and is an old address otherwise.
    The caller also specifies whether the translation should be 
    done with respect to the debugging table.  The debugging table 
    is used if useDebug is TRUE, and the main table is used otherwise.  

    The routine also returns the number of consecutive addresses 
    starting with the given address that belong to the same entry 
    in the appropriate table is returned in *addressInRange.  The 
    number of addresses beginning with the returned address that 
    belong the same entry is returned in *addressOutRange.  The 
    ranges are returned so that users can optimize translations 
    where they are sequentially scanning an address range.
*/
Elf32_Sword
xlate_address (
    Xlate_Table_Con     /*table*/,
    Elf32_Word		/*useDebug*/,
    Elf32_Word		/*isNewAddress*/,
    Elf64_Addr        	/*addressIn*/,
    Elf64_Addr *	/*addressOut*/,
    Xlate_Block *	/*range*/
);


/*
    This function takes a table descriptor, table, and an
    address, addressIn, and a range of addresses, rangeIn,
    that begins with the given address, and returns the
    ranges that correspond to it.  The caller also specifies
    whether the debugging table should be used for the 
    translation.  The debugging table is used if useDebug 
    is TRUE, and the main table otherwise.  The caller also 
    specifies whether the given address is a new address or 
    an old address.  It is a new address if isNewAddress is 
    TRUE, and an old address otherwise.

    The function returns the number of ranges corresponding
    to the given range in *numRanges.  A pointer to a block
    of structs that specify the returned ranges is returned
    in *blocks.  It is the caller's responsibility to free
    the memory returned when done with the information.
*/
Elf32_Sword
xlate_range (
    Xlate_Table_Con	/*table*/,
    Elf32_Word		/*useDebug*/,
    Elf32_Word		/*isNewAddress*/,
    Elf64_Addr		/*addressIn*/,
    Elf32_Sword		/*rangeIn*/,
    Elf32_Sword *	/*numRanges*/,
    Xlate_Block **	/*blocks*/
);


/*
    This function takes a table descriptor, table and scans
    every entry in either the main table or the debugging 
    table.  It scans the debugging table if useDebug table 
    is TRUE, and the main table otherwise.  It also takes a 
    pointer to a function which it calls for every entry.  
    The function is given a pointer to the range of the entry 
    in the form of a Xlate_Block struct.  It is also passed 
    a pointer to memory which is also passed on to the function 
    called.

    This routine will be used to perform some kind of global
    operation on the table.
*/
Elf32_Sword
xlate_apply_function (
    Xlate_Table_Con	/*table*/,
    Elf32_Word		/*useDebug*/,
    void		(*/*func*/)(Xlate_Block *, void *),
    void *		/*ptr*/
);


/*
    This function gets the rule to obtain the value
    of the given reg at the given pc.  See the Dwarf
    documentation for explanations.
*/
Elf32_Sword
xlate_get_reg_rule (
    Xlate_Table_Con     /*table*/,
    Elf32_Word          /*useDebug*/,
    Elf64_Addr          /*pc*/,
    Elf32_Word          /*reg*/,
    Elf32_Word *	/*ruleReg*/,
    Elf32_Word *	/*ruleOffset*/,
    Elf32_Word *	/*ruleIsOffset*/
);


/*
    This function expands the register information in
    the given table.  It returns exactly the instructions
    stored in the table by the producer.
*/
Elf32_Sword
xlate_expand_reg_info (
    Xlate_Table_Con     /*table*/,
    Elf32_Word          /*useDebug*/,
    Elf32_Sword *	/*numInstrs*/,
    Xlate_Reg_Instr **	/*instrs*/
);


/*
    This function deallocates all the memory allocated for the
    given table, and invalidates the given handle.
*/
Elf32_Sword
xlate_finish (
    Xlate_Table_Con	/*table*/
);

#ifdef __cplusplus
}
#endif
#endif /* _LIBXLATE_H */
