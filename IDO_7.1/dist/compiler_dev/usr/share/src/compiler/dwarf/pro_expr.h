#define	MAXIMUM_LOC_EXPR_LENGTH		20

struct Dwarf_P_Expr_s {
    Dwarf_Small		ex_byte_stream[MAXIMUM_LOC_EXPR_LENGTH];
    Dwarf_P_Debug	ex_dbg;
    Dwarf_Unsigned	ex_next_byte_offset;
    Dwarf_Signed	ex_reloc_sym_index;
    Dwarf_Unsigned	ex_reloc_offset;
};
