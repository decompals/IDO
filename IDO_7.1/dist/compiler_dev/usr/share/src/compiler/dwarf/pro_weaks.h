/*
    pro_weaks.h
    
    $Revision: 1.1 $		$Date: 1993/12/22 19:00:10 $
*/

struct Dwarf_P_Weakname_s {
    Dwarf_P_Die			wk_die;
    char			*wk_name;
    Dwarf_P_Weakname		wk_next;
};
