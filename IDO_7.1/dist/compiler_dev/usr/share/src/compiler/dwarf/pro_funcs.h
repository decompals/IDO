/*
    pro_funcs.h
    
    $Revision: 1.1 $		$Date: 1993/08/19 01:06:30 $
*/

struct Dwarf_P_Funcname_s {
    Dwarf_P_Die			fu_die;
    char			*fu_name;
    Dwarf_P_Funcname		fu_next;
};
