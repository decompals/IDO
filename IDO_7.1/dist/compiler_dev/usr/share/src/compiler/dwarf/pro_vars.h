/*
    pro_vars.h
    
    $Revision: 1.1 $		$Date: 1993/08/19 01:07:35 $
*/

struct Dwarf_P_Varname_s {
    Dwarf_P_Die			vu_die;
    char			*vu_name;
    Dwarf_P_Varname		vu_next;
};
