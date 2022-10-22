/*
	pro_util.h 
	$Revision: 1.9 $    $Date: 1996/07/23 23:26:51 $    
	$Source: /hosts/bonnie/proj/irix6.4-ssg/isms/cmplrs/libdwarf/RCS/pro_util.h,v $

	Utility routines 

*/


#define IS_64BIT(dbg) 	((dbg)->de_flags & DW_DLC_SIZE_64 ? 1 : 0)

/* definition of sizes of types, given target machine */
#define sizeof_sbyte(dbg) 	sizeof(Dwarf_Sbyte)
#define sizeof_ubyte(dbg)	sizeof(Dwarf_Ubyte)
#define sizeof_uhalf(dbg)	sizeof(Dwarf_Half)
#define sizeof_uword(dbg) 	\
    (IS_64BIT(dbg) ? sizeof(Dwarf_Unsigned) : sizeof(Dwarf_ufixed))
#define SIZEOF_UWORD(dbg) 	\
    (IS_64BIT(dbg) ? sizeof(Dwarf_Unsigned) : sizeof(Dwarf_ufixed))
#define sizeof_sword(dbg) 	sizeof_uword(dbg)

/* Computes amount of padding necessary to align n to a k-boundary. */
/* Important: Assumes n, k both GREATER than zero. */
#define PADDING(n, k) ( (k)-1 - ((n)-1)%(k) )
