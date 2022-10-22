/*
    dwarf_die_deliv.h

    $Revision: 1.9 $      $Date: 1993/08/16 23:17:36 $
*/


/*
    This struct holds information about a abbreviation.
    It is put in the hash table for abbreviations for
    a compile-unit.
*/
struct Dwarf_Abbrev_List_s {

    Dwarf_Word          		ab_code;
    Dwarf_Half				ab_tag;
    Dwarf_Half				ab_has_child;

	/* 
	    Points to start of attribute and form pairs in
	    the .debug_abbrev section for the abbrev.
	*/
    Dwarf_Byte_Ptr     	    		ab_abbrev_ptr;

    struct Dwarf_Abbrev_List_s		*ab_next;
};
