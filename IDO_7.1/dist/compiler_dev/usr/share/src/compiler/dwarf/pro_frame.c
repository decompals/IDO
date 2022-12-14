/*
	pro_frame.c
	$Revision: 1.20 $    $Date: 1996/07/08 21:07:40 $    
	$Source: /hosts/bonnie/proj/irix6.4-ssg/isms/cmplrs/libdwarf/RCS/pro_frame.c,v $

	Frame information routines
*/

#include <stdio.h>
#include <string.h>
#include <limits.h>
#include "pro_incl.h"
#include "pro_frame.h"

static void _dwarf_pro_add_to_fde(Dwarf_P_Fde fde, Dwarf_P_Frame_Pgm inst);

/*-------------------------------------------------------------------------
	This functions adds a cie struct to the debug pointer. Its in the
	form of a linked list.
	augmenter: string reps augmentation (implementation defined)
	code_align: alignment of code
	data_align: alignment of data
	init_bytes: byts having initial instructions
	init_n_bytes: number of bytes of initial instructions
--------------------------------------------------------------------------*/
Dwarf_Unsigned
dwarf_add_frame_cie(
	Dwarf_P_Debug dbg,
	char *augmenter,
	Dwarf_Small code_align,
	Dwarf_Small data_align,
	Dwarf_Small return_reg,
	Dwarf_Ptr init_bytes,
	Dwarf_Unsigned init_n_bytes,
	Dwarf_Error *error)
{
	Dwarf_P_Cie curcie;

	if (dbg->de_frame_cies == NULL) {
	    dbg->de_frame_cies = (Dwarf_P_Cie) 
		_dwarf_p_get_alloc(dbg, sizeof(struct Dwarf_P_Cie_s));
	    if (dbg->de_frame_cies == NULL) {
		DWARF_P_DBG_ERROR(dbg,DW_DLE_CIE_ALLOC,DW_DLV_NOCOUNT);
	    }
	    curcie = dbg->de_frame_cies;
	    dbg->de_n_cie = 1;
 	    dbg->de_last_cie = curcie;
	} 
	else {
	    curcie = dbg->de_last_cie;
	    curcie->cie_next = (Dwarf_P_Cie) 
		_dwarf_p_get_alloc(dbg, sizeof(struct Dwarf_P_Cie_s));
	    if (curcie->cie_next == NULL) {
		DWARF_P_DBG_ERROR(dbg,DW_DLE_CIE_ALLOC,DW_DLV_NOCOUNT);
	    }
	    curcie = curcie->cie_next;
	    dbg->de_n_cie++;
	    dbg->de_last_cie = curcie;    
	}
	curcie->cie_version = DW_CIE_VERSION;
	curcie->cie_aug = augmenter;
	curcie->cie_code_align = code_align;
	curcie->cie_data_align = data_align;
	curcie->cie_ret_reg = return_reg;
	curcie->cie_inst = (char *) init_bytes;
	curcie->cie_inst_bytes = init_n_bytes;
	curcie->cie_next = NULL;
	return dbg->de_n_cie;
}


/*-------------------------------------------------------------------------
	This functions adds a fde struct to the debug pointer. Its in the
	form of a linked list.
	die: subprogram/function die corresponding to this fde
	cie: cie referred to by this fde, obtained from call to 
	    add_frame_cie() routine.
	virt_addr: beginning address
	code_len: length of code reps by the fde
--------------------------------------------------------------------------*/
/*ARGSUSED*/ /* pretend all args used */
Dwarf_Unsigned
dwarf_add_frame_fde(
	Dwarf_P_Debug dbg,
	Dwarf_P_Fde fde,
	Dwarf_P_Die die,
	Dwarf_Unsigned cie,
	Dwarf_Unsigned virt_addr,
	Dwarf_Unsigned code_len,
	Dwarf_Unsigned  symidx,
	Dwarf_Error *error)
{
	Dwarf_P_Fde curfde;
	
	fde->fde_die = die;
	fde->fde_cie = cie;
	fde->fde_initloc = virt_addr;
	fde->fde_r_symidx = symidx;
	fde->fde_addr_range = code_len;
	fde->fde_offset_into_exception_tables = DW_DLX_NO_EH_OFFSET;
	fde->fde_exception_table_symbol = 0;
	
	curfde = dbg->de_last_fde;
	if (curfde == NULL) {
	    dbg->de_frame_fdes = fde;
	    dbg->de_last_fde = fde;
	    dbg->de_n_fde = 1;
	}
	else {
	    curfde->fde_next = fde;
	    dbg->de_last_fde = fde;
	    dbg->de_n_fde++;
	}
	return dbg->de_n_fde;
}

/*-------------------------------------------------------------------------
	This functions adds information to an fde. The fde is
	linked into the linked list of fde's maintained in the Dwarf_P_Debug
	structure.
	dbg: The debug descriptor.
	fde: The fde to be added.
	die: subprogram/function die corresponding to this fde
	cie: cie referred to by this fde, obtained from call to 
	    add_frame_cie() routine.
	virt_addr: beginning address
	code_len: length of code reps by the fde
	symidx: The symbol id of the symbol wrt to which relocation needs
		to be performed for 'virt_addr'.
	offset_into_exception_tables: The start of exception tables for
		this function (indicated as an offset into the exception
		tables). A value of -1 indicates that there is no exception
		table entries associated with this function.
	exception_table_symbol: The symbol id of the section for exception
		tables wrt to which the offset_into_exception_tables will
		be relocated.
--------------------------------------------------------------------------*/
/*ARGSUSED*/ /* pretend all args used */
Dwarf_Unsigned
dwarf_add_frame_info(
	Dwarf_P_Debug dbg,
	Dwarf_P_Fde fde,
	Dwarf_P_Die die,
	Dwarf_Unsigned cie,
	Dwarf_Unsigned virt_addr,
	Dwarf_Unsigned code_len,
	Dwarf_Unsigned symidx,
	Dwarf_Signed   offset_into_exception_tables,
	Dwarf_Unsigned exception_table_symbol,
	Dwarf_Error *error)
{
	Dwarf_P_Fde curfde;
	
	fde->fde_die = die;
	fde->fde_cie = cie;
	fde->fde_initloc = virt_addr;
	fde->fde_r_symidx = symidx;
	fde->fde_addr_range = code_len;
	fde->fde_offset_into_exception_tables = offset_into_exception_tables;
	fde->fde_exception_table_symbol = exception_table_symbol;
	
	curfde = dbg->de_last_fde;
	if (curfde == NULL) {
	    dbg->de_frame_fdes = fde;
	    dbg->de_last_fde = fde;
	    dbg->de_n_fde = 1;
	}
	else {
	    curfde->fde_next = fde;
	    dbg->de_last_fde = fde;
	    dbg->de_n_fde++;
	}
	return dbg->de_n_fde;
}

/*-------------------------------------------------------------------
	Create a new fde 
---------------------------------------------------------------------*/
Dwarf_P_Fde 
dwarf_new_fde (Dwarf_P_Debug dbg, Dwarf_Error *error)
{
	Dwarf_P_Fde fde;

	fde = (Dwarf_P_Fde)
	    _dwarf_p_get_alloc(dbg, sizeof(struct Dwarf_P_Fde_s));
	if (fde == NULL) {
	    DWARF_P_DBG_ERROR(dbg,DW_DLE_FDE_ALLOC,(Dwarf_P_Fde) DW_DLV_BADADDR);
	}
	fde->fde_next = NULL;
	fde->fde_inst = NULL;
	fde->fde_n_inst = 0;
	fde->fde_n_bytes = 0;
	fde->fde_last_inst = NULL;
	return fde;
}

/*------------------------------------------------------------------------
	Add cfe_offset instruction to fde
-------------------------------------------------------------------------*/
Dwarf_P_Fde
dwarf_fde_cfa_offset(
	Dwarf_P_Fde fde, 
	Dwarf_Unsigned reg,
	Dwarf_Signed offset,
	Dwarf_Error *error)
{
	Dwarf_Ubyte opc, regno;
	char *ptr;
	Dwarf_P_Frame_Pgm curinst;
	int nbytes;
	int res;
	char buff1[ENCODE_SPACE_NEEDED];

	curinst = (Dwarf_P_Frame_Pgm)
	   _dwarf_p_get_alloc(NULL,sizeof(struct Dwarf_P_Frame_Pgm_s));
	if (curinst == NULL) {
	    DWARF_P_DBG_ERROR(NULL,DW_DLE_FPGM_ALLOC,(Dwarf_P_Fde) DW_DLV_BADADDR);
	}
	opc = DW_CFA_offset;
	regno = reg;
	if (regno & 0xc0) {
	    DWARF_P_DBG_ERROR(NULL,DW_DLE_REGNO_OVFL,(Dwarf_P_Fde) DW_DLV_BADADDR);
	}
	opc = opc | regno;	/* lower 6 bits are register number */
	curinst->dfp_opcode = opc;
        res = _dwarf_pro_encode_leb128_nm(offset, &nbytes,
                        buff1,sizeof(buff1));
        if (res != DW_DLV_OK) {
                    _dwarf_p_error(NULL, error, DW_DLE_STRING_ALLOC);
                    return((Dwarf_P_Fde)DW_DLV_BADADDR);
        }
        ptr =  (char *) _dwarf_p_get_alloc(NULL, nbytes);
        if (ptr == NULL) {
                    _dwarf_p_error(NULL, error, DW_DLE_STRING_ALLOC);
                    return((Dwarf_P_Fde)DW_DLV_BADADDR);
        }
        memcpy(ptr, buff1, nbytes);

	curinst->dfp_args = ptr;
	curinst->dfp_nbytes = nbytes;
	curinst->dfp_next = NULL;

	_dwarf_pro_add_to_fde(fde,curinst);
	return fde;
}

/*
    Generic routine to add opcode to fde instructions. val1 and
    val2 are parameters whose interpretation depends on the 'op'.
*/
Dwarf_P_Fde
dwarf_add_fde_inst(
    Dwarf_P_Fde 	fde,
    Dwarf_Small 	op,
    Dwarf_Unsigned 	val1,
    Dwarf_Unsigned 	val2,
    Dwarf_Error 	*error
)
{
    Dwarf_P_Frame_Pgm 	curinst;
    int 		nbytes, nbytes1, nbytes2;
    Dwarf_Ubyte 	db;
    Dwarf_Half 		dh;
    Dwarf_Word 		dw;
    Dwarf_Unsigned 	du;
    char 		*ptr;
    int res;
    char buff1[ENCODE_SPACE_NEEDED];
    char buff2[ENCODE_SPACE_NEEDED];

    /* debug trace. uncomment if needed. 
    printf ("add_fde: op = %d, val1 = 0x%llx, val2 = 0x%llx \n", 
		    op, val1, val2);
    */

    nbytes = 0;
    ptr = NULL;
    curinst = (Dwarf_P_Frame_Pgm)
	_dwarf_p_get_alloc(NULL,sizeof(struct Dwarf_P_Frame_Pgm_s));
    if (curinst == NULL) {
	_dwarf_p_error(NULL, error, DW_DLE_FPGM_ALLOC);
	return((Dwarf_P_Fde)DW_DLV_BADADDR);
    }

    switch (op) {

        case DW_CFA_advance_loc:
	    if (val1 <= 0x3f) {
	        db = val1;
	        op |= db;
	    } 
	    else if (val1 <= UCHAR_MAX) {
	        op = DW_CFA_advance_loc1;
	        db = val1;
	        ptr = (char *) _dwarf_p_get_alloc(NULL, 1);
	        if (ptr == NULL) {
	            _dwarf_p_error(NULL, error, DW_DLE_STRING_ALLOC);
		    return((Dwarf_P_Fde)DW_DLV_BADADDR);
	        }
	        memcpy((void *)ptr, (const void *)&db,1);
	        nbytes = 1;
	    }
	    else if (val1 <= USHRT_MAX) {
	        op = DW_CFA_advance_loc2;
	        dh = val1;
	        ptr = (char *) _dwarf_p_get_alloc(NULL, 2);
		if (ptr == NULL) {
	            _dwarf_p_error(NULL, error, DW_DLE_STRING_ALLOC);
		    return((Dwarf_P_Fde)DW_DLV_BADADDR);
	        }
	        memcpy((void *)ptr, (const void *)&dh,2);
	        nbytes = 2;
	    }
	    else if (val1 <= ULONG_MAX) {
	        op = DW_CFA_advance_loc4;
	        dw = val1;
	        ptr = (char *) _dwarf_p_get_alloc(NULL, 4);
	        if (ptr == NULL) {
	            _dwarf_p_error(NULL, error, DW_DLE_STRING_ALLOC);
		    return((Dwarf_P_Fde)DW_DLV_BADADDR);
	        }
	        memcpy((void *)ptr, (const void *)&dw,4);
	        nbytes = 4;
	    }
	    else {
	        op = DW_CFA_MIPS_advance_loc8;
	        du = val1;
	        ptr = (char *)_dwarf_p_get_alloc(NULL, sizeof(Dwarf_Unsigned));
	        if (ptr == NULL) {
	            _dwarf_p_error(NULL, error, DW_DLE_STRING_ALLOC);
		    return((Dwarf_P_Fde)DW_DLV_BADADDR);
	        }
	        memcpy((void *)ptr, (const void *)&du,8);
	        nbytes = 8;
	    }
            break;

        case DW_CFA_offset:
	    if (val1 <= MAX_6_BIT_VALUE) {
	        db = val1;
	        op |= db;
	        res = _dwarf_pro_encode_leb128_nm(val2, &nbytes,
			buff1,sizeof(buff1));
                if (res != DW_DLV_OK) {
                    _dwarf_p_error(NULL, error, DW_DLE_STRING_ALLOC);
                    return((Dwarf_P_Fde)DW_DLV_BADADDR);
                }
	        ptr =  (char *) _dwarf_p_get_alloc(NULL, nbytes);
                if (ptr == NULL) {
                    _dwarf_p_error(NULL, error, DW_DLE_STRING_ALLOC);
                    return((Dwarf_P_Fde)DW_DLV_BADADDR);
                }
		memcpy(ptr,buff1,nbytes);

	    }
	    else {
		op = DW_CFA_offset_extended;

	        res =  _dwarf_pro_encode_leb128_nm(val1, &nbytes1,
			buff1,sizeof(buff1));
	        if (res != DW_DLV_OK) {
	            _dwarf_p_error(NULL, error, DW_DLE_STRING_ALLOC);
		    return((Dwarf_P_Fde)DW_DLV_BADADDR);
	        }
	        res =  _dwarf_pro_encode_leb128_nm(val2, &nbytes2,
			buff2,sizeof(buff2));
	        if (res != DW_DLV_OK) {
	            _dwarf_p_error(NULL, error, DW_DLE_STRING_ALLOC);
		    return((Dwarf_P_Fde)DW_DLV_BADADDR);
	        }
	        ptr =  (char *) _dwarf_p_get_alloc(NULL, nbytes1+nbytes2);
                if (ptr == NULL) {
                    _dwarf_p_error(NULL, error, DW_DLE_STRING_ALLOC);
                    return((Dwarf_P_Fde)DW_DLV_BADADDR);
                }
	        memcpy(ptr, buff1, nbytes1);
	        memcpy(ptr+nbytes1, buff2, nbytes2);
	        nbytes = nbytes1 + nbytes2;
	    }
	    break;

	case DW_CFA_undefined:
	case DW_CFA_same_value:
	    res = _dwarf_pro_encode_leb128_nm(val1, &nbytes,
			buff1,sizeof(buff1));
                if (res != DW_DLV_OK) {
                    _dwarf_p_error(NULL, error, DW_DLE_STRING_ALLOC);
                    return((Dwarf_P_Fde)DW_DLV_BADADDR);
                }
                ptr =  (char *) _dwarf_p_get_alloc(NULL, nbytes);
                if (ptr == NULL) {
                    _dwarf_p_error(NULL, error, DW_DLE_STRING_ALLOC);
                    return((Dwarf_P_Fde)DW_DLV_BADADDR);
                }
	        memcpy(ptr, buff1, nbytes);
	    break;

	case DW_CFA_register:
        case DW_CFA_def_cfa:
	    res =  _dwarf_pro_encode_leb128_nm(val1, &nbytes1,
			buff1,sizeof(buff1));
                if (res != DW_DLV_OK) {
                    _dwarf_p_error(NULL, error, DW_DLE_STRING_ALLOC);
                    return((Dwarf_P_Fde)DW_DLV_BADADDR);
                }

	    res =  _dwarf_pro_encode_leb128_nm(val2, &nbytes2,
			buff2,sizeof(buff2));
                if (res != DW_DLV_OK) {
                    _dwarf_p_error(NULL, error, DW_DLE_STRING_ALLOC);
                    return((Dwarf_P_Fde)DW_DLV_BADADDR);
                }

	    ptr =  (char *) _dwarf_p_get_alloc(NULL, nbytes1+nbytes2);
                if (ptr == NULL) {
                    _dwarf_p_error(NULL, error, DW_DLE_STRING_ALLOC);
                    return((Dwarf_P_Fde)DW_DLV_BADADDR);
                }
	    memcpy(ptr, buff1, nbytes1);
	    memcpy(ptr+nbytes1, buff2, nbytes2);
	    nbytes = nbytes1 + nbytes2;
	    break;
		
	case DW_CFA_def_cfa_register:
	case DW_CFA_def_cfa_offset:
            res = _dwarf_pro_encode_leb128_nm(val1, &nbytes,
                        buff1,sizeof(buff1));
                if (res != DW_DLV_OK) {
                    _dwarf_p_error(NULL, error, DW_DLE_STRING_ALLOC);
                    return((Dwarf_P_Fde)DW_DLV_BADADDR);
                }
                ptr =  (char *) _dwarf_p_get_alloc(NULL, nbytes);
                if (ptr == NULL) {
                    _dwarf_p_error(NULL, error, DW_DLE_STRING_ALLOC);
                    return((Dwarf_P_Fde)DW_DLV_BADADDR);
                }
                memcpy(ptr, buff1, nbytes);
	    break;

        default:
	    break;
    }

    curinst->dfp_opcode = op;
    curinst->dfp_args = ptr;
    curinst->dfp_nbytes = nbytes;
    curinst->dfp_next = NULL;

    _dwarf_pro_add_to_fde(fde, curinst);
    return fde;
}


/*------------------------------------------------------------------------
	instructions are added to fde in the form of a linked
	list. This functions manages the linked list
-------------------------------------------------------------------------*/
void 
_dwarf_pro_add_to_fde(Dwarf_P_Fde fde, Dwarf_P_Frame_Pgm curinst)
{
	if (fde->fde_last_inst) {
		fde->fde_last_inst->dfp_next = curinst;
		fde->fde_last_inst = curinst;
		fde->fde_n_inst++;
		fde->fde_n_bytes += curinst->dfp_nbytes + sizeof(Dwarf_Ubyte) ;
	} 
	else {
		fde->fde_last_inst = fde->fde_inst = curinst;
		fde->fde_n_inst = 1;
		fde->fde_n_bytes = curinst->dfp_nbytes + sizeof(Dwarf_Ubyte);
	}
}
