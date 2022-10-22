/*
    pro_types.h
    
    $Revision: 1.1 $		$Date: 1993/08/19 01:07:04 $
*/

struct Dwarf_P_Typename_s {
    Dwarf_P_Die			tu_die;
    char			*tu_name;
    Dwarf_P_Typename		tu_next;
};
