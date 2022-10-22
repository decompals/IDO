/*
	pro_die.h   
	$Revision: 1.7 $    $Date: 1996/07/23 23:26:44 $    

	Header file for producer die related information. This file is included 
	in pro_section.c. 

*/


/* 
	This struct holds the abbreviation table, before they are written 
	on disk. Holds a linked list of abbreviations, each consisting of
	a bitmap for attributes and a bitmap for forms
*/
typedef struct Dwarf_P_Abbrev_s 	*Dwarf_P_Abbrev;

struct Dwarf_P_Abbrev_s {
	Dwarf_Unsigned 		abb_idx;	/* index of abbreviation */
	Dwarf_Tag 		abb_tag;	/* tag of die */
	Dwarf_Ubyte 		abb_children;	/* if children are present */
	Dwarf_ufixed 		*abb_attrs;	/* holds names of attrs */
	Dwarf_ufixed 		*abb_forms; 	/* forms of attributes */
	Dwarf_Word 		abb_n_attr;	/* num of attrs = # of forms */
	Dwarf_P_Abbrev 		abb_next;
};

/* used in pro_section.c */

int _dwarf_pro_add_AT_fde (Dwarf_P_Debug dbg, Dwarf_P_Die die, 
    Dwarf_Unsigned offset, Dwarf_Error *error);

int _dwarf_pro_add_AT_stmt_list (Dwarf_P_Debug dbg, Dwarf_P_Die first_die, 
    Dwarf_Error *error);

int _dwarf_pro_add_AT_macro_info(Dwarf_P_Debug dbg, Dwarf_P_Die first_die,
    Dwarf_Unsigned offset, Dwarf_Error *error);
