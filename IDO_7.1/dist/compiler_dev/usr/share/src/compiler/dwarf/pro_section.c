/*
	pro_section.c 
	$Revision: 1.54 $    $Date: 1996/07/23 23:26:49 $    
	$Source: /hosts/bonnie/proj/irix6.4-ssg/isms/cmplrs/libdwarf/RCS/pro_section.c,v $

	This files contains routines for converting dwarf information
	to disk form, and giving out bytes to be written to disk
*/

#include <stdio.h>
#include <string.h>
#include <sgidefs.h>
#include <elfaccess.h>
#include "pro_incl.h"
#include "pro_section.h"
#include "pro_line.h"
#include "pro_frame.h"
#include "pro_die.h"
#include "pro_macinfo.h"


static Dwarf_Ubyte std_opcode_len[] =
        { 0,    /* DW_LNS_copy */
          1,    /* DW_LNS_advance_pc */
          1,    /* DW_LNS_advance_line */
          1,    /* DW_LNS_set_file */
          1,    /* DW_LNS_set_column */
          0,    /* DW_LNS_negate_stmt */
          0,    /* DW_LNS_set_basic_block */
          0,    /* DW_LNS_const_add_pc */
          1,    /* DW_LNS_fixed_advance_pc */
        };


static int _dwarf_pro_generate_debugline (Dwarf_P_Debug dbg, Dwarf_Error *error);
static int _dwarf_pro_generate_debugframe (Dwarf_P_Debug dbg, Dwarf_Error *error);
static int _dwarf_pro_generate_debuginfo (Dwarf_P_Debug dbg, Dwarf_Error *error);
static Dwarf_P_Abbrev _dwarf_pro_getabbrev (Dwarf_P_Die, Dwarf_P_Abbrev);
static int _dwarf_pro_match_attr
    (Dwarf_P_Attribute, Dwarf_P_Abbrev, int no_attr);
static Dwarf_P_Rel _dwarf_pro_get_new_relrec (Dwarf_P_Debug dbg);
static int _dwarf_pro_rel_info (Dwarf_P_Debug dbg, Dwarf_P_Rel relrec,
    Dwarf_Unsigned offset, Dwarf_Word symidx, Dwarf_Ubyte type);
static int _dwarf_pro_write_reloc_section (Dwarf_P_Debug dbg, Dwarf_P_Rel rel_head,
    int relsectno, int rel_nbytes, Dwarf_Error *error);

/* these macros used as return value for below functions */
#define		OPC_INCS_ZERO		-1
#define		OPC_OUT_OF_RANGE	-2
#define		LINE_OUT_OF_RANGE	-3
static int _dwarf_pro_get_opc(Dwarf_Unsigned addr_adv, int line_adv);

/* names of sections. Ensure that it matches the defines 
   in pro_section.h, in the same order */
char *sectnames[] = {
	".debug_info",
	".debug_line",
	".debug_abbrev",
	".debug_frame",
	".debug_aranges",
	".debug_pubnames",
	".debug_str",
	".debug_funcnames",
	".debug_typenames",
	".debug_varnames",
	".debug_weaknames",
	".debug_macinfo"
};

/* arrays to hold elfsection indices, and indices into SYMTAB section
   for the dwarf section names. The SYMTAB indices are used in generating
   relocation records for dwarf sections. The relocation section numbers 
   are held in reloc_sects[]
*/
int elf_sects[NUM_DEBUG_SECTIONS];
int sect_name_idx[NUM_DEBUG_SECTIONS];
int reloc_sects[NUM_DEBUG_SECTIONS];

/*
    Convert debug information to format so that 
    it can be written on disk
*/
Dwarf_Signed
dwarf_transform_to_disk_form (
    Dwarf_P_Debug 	dbg,
    Dwarf_Error 	*error
)
{
	/* 
	    Section data in written out in a number of 
	    buffers. Each _generate_*() function returns
	    a cumulative count of buffers for all the
	    sections. get_section_bytes() returns these
	    buffers one at a time 
	*/
    int 	nbufs;	

	/* Loop for all sections, and create section headers */
    int 	sect;	

        /* 
	    Index of section name into SYMTAB 
            to be passed to call back routine 
	*/
    int 	name_idx;	

    int 	err;	

	/* Create dwarf section headers */
    for (sect = 0; sect < NUM_DEBUG_SECTIONS; sect++) {
	long flags = 0;
        switch (sect) {

            case DEBUG_INFO : 
                if (dbg->de_dies == NULL) continue;
                break;

            case DEBUG_LINE :
                if (dbg->de_lines == NULL) continue;
                break;

            case DEBUG_ABBREV :
                if (dbg->de_dies == NULL) continue;
                break;

            case DEBUG_FRAME :
                if (dbg->de_frame_cies == NULL) continue;
		flags = SHF_MIPS_NOSTRIP;
                break;

            case DEBUG_ARANGES :
                if (dbg->de_arange == NULL) continue;
                break;

            case DEBUG_PUBNAMES :
                if (dbg->de_pubname == NULL) continue;
		break;

	    case DEBUG_STR :
		if (dbg->de_strings == NULL) continue;
		break;

	    case DEBUG_FUNCNAMES :
		if (dbg->de_funcname == NULL) continue;
		break;

	    case DEBUG_TYPENAMES :
		if (dbg->de_typename == NULL) continue;
		break;

	    case DEBUG_VARNAMES :
		if (dbg->de_varname == NULL) continue;
		break;

	    case DEBUG_WEAKNAMES :
		if (dbg->de_weakname == NULL) continue;
		break;

	    case DEBUG_MACINFO :
		if (dbg->de_first_macinfo == NULL) continue;
		break;
	}

        elf_sects[sect] = dbg->de_func(sectnames[sect], IS_64BIT(dbg), 
	    SHT_MIPS_DWARF, flags,
	    SHN_UNDEF, 0, &name_idx, &err);
	if (elf_sects[sect] == -1) {
    	    DWARF_P_DBG_ERROR(dbg, DW_DLE_ELF_SECT_ERR, DW_DLV_NOCOUNT);
	}
        sect_name_idx[sect] = name_idx;
    }

    nbufs = 0; 

	/* 
	    Changing the order in which the sections are generated 
	    may cause problems because of relocations. 
	*/

    if (dbg->de_lines) {
	nbufs = _dwarf_pro_generate_debugline(dbg, error);
	if (nbufs < 0) {
	    DWARF_P_DBG_ERROR(dbg, DW_DLE_DEBUGLINE_ERROR, DW_DLV_NOCOUNT);
	}
    }

    if (dbg->de_frame_cies) {
	nbufs = _dwarf_pro_generate_debugframe(dbg,error);
	if (nbufs < 0) {
	    DWARF_P_DBG_ERROR(dbg, DW_DLE_DEBUGFRAME_ERROR, DW_DLV_NOCOUNT);
	}
    }
    if (dbg->de_first_macinfo) {
	nbufs = _dwarf_pro_transform_macro_info_to_disk(dbg,error);
	if (nbufs < 0) {
	    DWARF_P_DBG_ERROR(dbg, DW_DLE_DEBUGMACINFO_ERROR, DW_DLV_NOCOUNT);
	}
    }

    if (dbg->de_dies) {
	nbufs = _dwarf_pro_generate_debuginfo(dbg, error); 
	if (nbufs < 0) {
	    DWARF_P_DBG_ERROR(dbg, DW_DLE_DEBUGINFO_ERROR, DW_DLV_NOCOUNT);
	}
    }

    if (dbg->de_arange) {
	nbufs = _dwarf_transform_arange_to_disk(dbg, error);
	if (nbufs < 0) {
	    DWARF_P_DBG_ERROR(dbg, DW_DLE_DEBUGINFO_ERROR, DW_DLV_NOCOUNT);
	}
    }

    if (dbg->de_pubname) {
	nbufs = _dwarf_transform_pubname_to_disk(dbg, error);
	if (nbufs < 0) {
	    DWARF_P_DBG_ERROR(dbg, DW_DLE_DEBUGINFO_ERROR, DW_DLV_NOCOUNT);
	}
    }

    if (dbg->de_funcname) {
	nbufs = _dwarf_transform_funcname_to_disk(dbg, error);
	if (nbufs < 0) {
	    DWARF_P_DBG_ERROR(dbg, DW_DLE_DEBUGINFO_ERROR, DW_DLV_NOCOUNT);
	}
    }

    if (dbg->de_typename) {
	nbufs = _dwarf_transform_typename_to_disk(dbg, error);
	if (nbufs < 0) {
	    DWARF_P_DBG_ERROR(dbg, DW_DLE_DEBUGINFO_ERROR, DW_DLV_NOCOUNT);
	}
    }

    if (dbg->de_varname) {
	nbufs = _dwarf_transform_varname_to_disk(dbg, error);
	if (nbufs < 0) {
	    DWARF_P_DBG_ERROR(dbg, DW_DLE_DEBUGINFO_ERROR, DW_DLV_NOCOUNT);
	}
    }

    if (dbg->de_weakname) {
	nbufs = _dwarf_transform_weakname_to_disk(dbg, error);
	if (nbufs < 0) {
	    DWARF_P_DBG_ERROR(dbg, DW_DLE_DEBUGINFO_ERROR, DW_DLV_NOCOUNT);
	}
    }

    return nbufs;
}


/*---------------------------------------------------------------
	Generate debug_line section 
---------------------------------------------------------------*/
int
_dwarf_pro_generate_debugline(Dwarf_P_Debug dbg, Dwarf_Error *error)
{
	Dwarf_P_Inc_Dir curdir;
	Dwarf_P_F_Entry curentry;
	Dwarf_P_Line curline, prevline;
			/* all data named cur* are used to loop thru 
			   linked lists */
	Dwarf_P_Rel rel_head, rel_tail;
			/* pointer to head & tail of list of relocation 
			   record */
	int rel_nbytes;	/* number of bytes of relocation record */
	int sum_bytes;
	int prolog_size;
	unsigned char *data; 	/* holds disk form data */
	int elfsectno;
    	int name_idx;	/* index of section name into SYMTAB */
    	int err;	/* to be passed to call back routine */
	int i;
	unsigned char *start_sec;		/* pointer to the buffer at 
					   section start */
	/* temps for memcpy */
	Dwarf_Unsigned du;
	Dwarf_Word dw;
	Dwarf_Ubyte db;
	Dwarf_Half dh;
	int res;
	char buff1[ENCODE_SPACE_NEEDED];

	sum_bytes = 0;
	rel_nbytes = 0;
	rel_head = NULL;
	rel_tail = NULL;

	elfsectno = elf_sects[DEBUG_LINE];

	/* statement prologue information */
	prolog_size = 0;
	/* include directories */
	curdir = dbg->de_inc_dirs;
	while (curdir) {
		prolog_size += strlen(curdir->did_name)+1;
		curdir = curdir->did_next;
	}
	prolog_size++; 	/* last null following last directory entry. */

	/* file entries */
	curentry = dbg->de_file_entries;
	while (curentry) {
		prolog_size += strlen(curentry->dfe_name)+1+curentry->dfe_nbytes;
		curentry = curentry->dfe_next;
	}
	prolog_size++;	/* last null byte */


	prolog_size +=	sizeof_uhalf(dbg) + 	/* version # */
			sizeof_uword(dbg) + 	/* prologue length */
			sizeof_ubyte(dbg) +	/* min_instr length */
			sizeof_ubyte(dbg) +   /* default is_stmt */
			sizeof_ubyte(dbg) +	/* linebase */
			sizeof_ubyte(dbg) +	/* linerange */
			sizeof_ubyte(dbg);	/* opcode base */

	/* length of table specifying # of opnds */
	prolog_size += sizeof(std_opcode_len);
	prolog_size += sizeof_uword(dbg) ;	/* for total length field */

	GET_NEW_CHUNK(dbg,elfsectno,data,prolog_size,error);
	start_sec = data;

	/* copy over the data */
	/* total_length */
	if (IS_64BIT(dbg)) {
	   du = 0;			/* will be overwritten when total length is found */
	   memcpy((void *)data, (const void *)&du, sizeof(Dwarf_Unsigned));
	   data += sizeof(Dwarf_Unsigned);
	} else {
	   dw = 0;			/* will be overwritten when total length is found */
	   memcpy((void *)data, (const void *)&dw, sizeof(Dwarf_ufixed));
	   data += sizeof(Dwarf_ufixed);
	}

	dh = VERSION;
	memcpy((void *)data,(const void *)&dh, sizeof(Dwarf_Half));
	data += sizeof(Dwarf_Half);
	/*prologue length */
	du = prolog_size - (sizeof_uword(dbg)+sizeof(Dwarf_Half)+sizeof_uword(dbg));
	if (IS_64BIT(dbg)) {
	    memcpy((void *)data,(const void *)&du, sizeof(Dwarf_Unsigned));
	    data += sizeof(Dwarf_Unsigned);
	} else {
	    dw = du;
	    memcpy((void *)data,(const void *)&dw, sizeof(Dwarf_ufixed));
	    data += sizeof(Dwarf_ufixed);
	}
	db = MIN_INST_LENGTH;
	memcpy((void *)data,(const void *)&db, sizeof(Dwarf_Ubyte));
	data += sizeof(Dwarf_Ubyte);
	db = DEFAULT_IS_STMT;
	memcpy((void *)data,(const void *)&db, sizeof(Dwarf_Ubyte));
	data += sizeof(Dwarf_Ubyte);
	db = (Dwarf_Ubyte) LINE_BASE;
	memcpy((void *)data,(const void *)&db, sizeof(Dwarf_Ubyte));
	data += sizeof(Dwarf_Ubyte);
	db = LINE_RANGE;
	memcpy((void *)data,(const void *)&db, sizeof(Dwarf_Ubyte));
	data += sizeof(Dwarf_Ubyte);
	db = OPCODE_BASE;
	memcpy((void *)data,(const void *)&db, sizeof(Dwarf_Ubyte));
	data += sizeof(Dwarf_Ubyte);
	memcpy((void *)data,(const void *)std_opcode_len, sizeof(std_opcode_len));
	data += sizeof(std_opcode_len);

	/* copy over include directories */
	curdir = dbg->de_inc_dirs;
	while (curdir) {
		strcpy((char *)data,curdir->did_name);
		data += strlen(curdir->did_name)+1;
		curdir = curdir->did_next;
	}
	*data = '\0';	/* last null */
	data++;

	/* copy file entries */
	curentry = dbg->de_file_entries;
	while (curentry) {
		strcpy((char *)data,curentry->dfe_name);
		data += strlen(curentry->dfe_name)+1;
		memcpy((void *)data,(const void *)curentry->dfe_args,curentry->dfe_nbytes);
		data += curentry->dfe_nbytes;
		curentry = curentry->dfe_next;
	}
	*data = '\0';
	data++;

	sum_bytes += prolog_size;

	curline = dbg->de_lines;
	prevline = (Dwarf_P_Line)
	    _dwarf_p_get_alloc(dbg, sizeof(struct Dwarf_P_Line_s));
	if (prevline == NULL) {
	    DWARF_P_DBG_ERROR(dbg,DW_DLE_LINE_ALLOC,-1);
	}
	_dwarf_pro_reg_init(prevline);
	/* generate opcodes for line numbers */
	while(curline) {
	  int nbytes;
	  char *arg;
	  int opc;
	  int no_lns_copy;	/* if lns copy opcode doesnt need to be 
				   generated, if special opcode or 
				   end sequence */
	  Dwarf_Unsigned addr_adv; 
	  int line_adv;

	  no_lns_copy = 0;
	  if (curline->dpl_opc != 0) {
		int inst_bytes;	/* no of bytes in extended opcode */
		char *str;	/* hold leb encoded inst_bytes */
		int str_nbytes;	/* no of bytes in str */
		switch (curline->dpl_opc) {
		case DW_LNE_end_sequence:

		    /* Advance pc to end of text section. */
	    	    addr_adv = curline->dpl_address - prevline->dpl_address;
		    if (addr_adv > 0) {
		        db = DW_LNS_advance_pc;
		        res = _dwarf_pro_encode_leb128_nm(
			    addr_adv/MIN_INST_LENGTH, &nbytes,
				buff1,sizeof(buff1));
			if(res != DW_DLV_OK) {
                	  DWARF_P_DBG_ERROR(dbg,DW_DLE_CHUNK_ALLOC,-1); 
            		}
		        GET_CHUNK(dbg,elfsectno,data,
			    nbytes+sizeof(Dwarf_Ubyte),error);
		        memcpy((void *)data, (const void *)&db, 
			    sizeof(Dwarf_Ubyte));
		        data += sizeof(Dwarf_Ubyte);
		        memcpy((void *)data,(const void *)buff1, nbytes);
		        data += nbytes + sizeof(Dwarf_Ubyte);
		        sum_bytes += nbytes + sizeof(Dwarf_Ubyte);
		        prevline->dpl_address = curline->dpl_address;
		    }

		    /*first null byte */
		    db = 0;
		    GET_CHUNK(dbg,elfsectno,data,sizeof(Dwarf_Ubyte),error);
		    memcpy((void *)data,(const void *)&db, sizeof(Dwarf_Ubyte));
		    data += sizeof(Dwarf_Ubyte);
		    sum_bytes += sizeof(Dwarf_Ubyte);

		    /* write length of extended opcode */
		    inst_bytes = sizeof(Dwarf_Ubyte);
		    res = _dwarf_pro_encode_leb128_nm(inst_bytes,&str_nbytes,
			buff1,sizeof(buff1));
		    if(res != DW_DLV_OK) {
                	  DWARF_P_DBG_ERROR(dbg,DW_DLE_CHUNK_ALLOC,-1); 
            	    }
		    GET_CHUNK(dbg,elfsectno,data,str_nbytes,error);
		    memcpy((void *)data,(const void *)buff1,str_nbytes);
		    data += str_nbytes;
		    sum_bytes += str_nbytes;

		    /* write extended opcode */
		    db = DW_LNE_end_sequence;
		    GET_CHUNK(dbg,elfsectno,data,sizeof(Dwarf_Ubyte),error);
		    memcpy((void *)data,(const void *)&db, sizeof(Dwarf_Ubyte));
		    data += sizeof(Dwarf_Ubyte);
		    sum_bytes += sizeof(Dwarf_Ubyte);
		    /* reset value to original values */
		    _dwarf_pro_reg_init(prevline);
		    no_lns_copy = 1;
			/* this is set only for end_sequence, so that a 
			   dw_lns_copy is not generated */
		    break;

		case DW_LNE_set_address:

		    /*first null byte */
		    db = 0;
		    GET_CHUNK(dbg,elfsectno,data,sizeof(Dwarf_Ubyte),error);
		    memcpy((void *)data,(const void *)&db, sizeof(Dwarf_Ubyte));
		    data += sizeof(Dwarf_Ubyte);
		    sum_bytes += sizeof(Dwarf_Ubyte);

		    /* write length of extended opcode */
		    inst_bytes = sizeof(Dwarf_Ubyte)+sizeof_uword(dbg);
		    res = _dwarf_pro_encode_leb128_nm(inst_bytes,&str_nbytes,
			buff1,sizeof(buff1));
		    if(res != DW_DLV_OK) {
                	  DWARF_P_DBG_ERROR(dbg,DW_DLE_CHUNK_ALLOC,-1); 
            	    }
		    GET_CHUNK(dbg,elfsectno,data,str_nbytes,error);
		    str = buff1;
		    memcpy((void *)data,(const void *)str,str_nbytes);
		    data += str_nbytes;
		    sum_bytes += str_nbytes;

		    /* write extended opcode */
		    db = DW_LNE_set_address;
		    GET_CHUNK(dbg,elfsectno,data,sizeof_uword(dbg) + 
					sizeof(Dwarf_Ubyte),error);
		    memcpy((void *)data,(const void *)&db, sizeof(Dwarf_Ubyte));
		    data += sizeof(Dwarf_Ubyte);
		    sum_bytes += sizeof(Dwarf_Ubyte);

		    /* store relocation record in linked list */
		    if (rel_head == NULL) {
			rel_head = _dwarf_pro_get_new_relrec(dbg);
			if (rel_head == NULL) {
			    DWARF_P_DBG_ERROR(dbg,DW_DLE_REL_ALLOC, -1);
			}
			rel_tail = rel_head;
		    } else {
			rel_tail->dr_next = _dwarf_pro_get_new_relrec(dbg);
			if (rel_tail->dr_next == NULL) {
			    DWARF_P_DBG_ERROR(dbg,DW_DLE_REL_ALLOC, -1);
			}
			rel_tail = rel_tail->dr_next;
			rel_tail->dr_next = NULL;
		    }
		    rel_nbytes += _dwarf_pro_rel_info(dbg,rel_tail,sum_bytes,
			curline->dpl_r_symidx,(IS_64BIT(dbg) ? R_MIPS_64 : R_MIPS_32));

		    /* write offset */
		    if (IS_64BIT(dbg)) {
			du = curline->dpl_address;
		    	memcpy((void *)data,(const void *)&du, sizeof(Dwarf_Unsigned));
		    	data += sizeof(Dwarf_Unsigned);
		    	sum_bytes += sizeof(Dwarf_Unsigned);
		    }
		    else {
			dw = curline->dpl_address;
		    	memcpy((void *)data,(const void *)&dw, sizeof(Dwarf_ufixed));
		    	data += sizeof(Dwarf_ufixed);
		    	sum_bytes += sizeof(Dwarf_ufixed);
		    }
		    prevline->dpl_address = curline->dpl_address;
		    no_lns_copy = 1;
		    break;
		}
	  } 
	  else {
	    if (curline->dpl_file != prevline->dpl_file) {
		db = DW_LNS_set_file;
		res = _dwarf_pro_encode_leb128_nm(curline->dpl_file,&nbytes,
			buff1,sizeof(buff1));
		if(res != DW_DLV_OK) {
                	  DWARF_P_DBG_ERROR(dbg,DW_DLE_CHUNK_ALLOC,-1); 
            	}
		arg = buff1;
		GET_CHUNK(dbg,elfsectno,data,nbytes+sizeof(Dwarf_Ubyte),error);
		memcpy((void *)data,(const void *)&db, sizeof(Dwarf_Ubyte));
		data += sizeof(Dwarf_Ubyte);
		memcpy((void *)data,(const void *)arg, nbytes);
		data += nbytes;
		sum_bytes += nbytes + sizeof(Dwarf_Ubyte);
		prevline->dpl_file = curline->dpl_file;
	    } 
	    if (curline->dpl_column != prevline->dpl_column) {
		db = DW_LNS_set_column;
		res = _dwarf_pro_encode_leb128_nm(curline->dpl_column,&nbytes,
			buff1,sizeof(buff1));
                if(res != DW_DLV_OK) {
                          DWARF_P_DBG_ERROR(dbg,DW_DLE_CHUNK_ALLOC,-1);
                }

		arg = buff1;
		GET_CHUNK(dbg,elfsectno,data,nbytes+sizeof(Dwarf_Ubyte),error);
		memcpy((void *)data,(const void *)&db, sizeof(Dwarf_Ubyte));
		data += sizeof(Dwarf_Ubyte);
		memcpy((void *)data,(const void *)arg, nbytes);
		data += nbytes;
		sum_bytes += nbytes + sizeof(Dwarf_Ubyte);
		prevline->dpl_column = curline->dpl_column;
	    }
	    if (curline->dpl_is_stmt != prevline->dpl_is_stmt) {
		db = DW_LNS_negate_stmt;
		GET_CHUNK(dbg,elfsectno,data,sizeof(Dwarf_Ubyte),error);
		memcpy((void *)data,(const void *)&db, sizeof(Dwarf_Ubyte));
		data += sizeof(Dwarf_Ubyte);
		sum_bytes += sizeof(Dwarf_Ubyte);
		prevline->dpl_is_stmt = curline->dpl_is_stmt;
	    }
	    if (curline->dpl_basic_block == true && prevline->dpl_basic_block == false) {
		db = DW_LNS_set_basic_block;
		GET_CHUNK(dbg,elfsectno,data,sizeof(Dwarf_Ubyte),error);
		memcpy((void *)data,(const void *)&db, sizeof(Dwarf_Ubyte));
		data += sizeof(Dwarf_Ubyte);
		sum_bytes += sizeof(Dwarf_Ubyte);
		prevline->dpl_basic_block = curline->dpl_basic_block;
	    }
	    addr_adv = curline->dpl_address - prevline->dpl_address;
	    line_adv = curline->dpl_line - prevline->dpl_line;
	    if ((addr_adv % MIN_INST_LENGTH) != 0) {
		DWARF_P_DBG_ERROR(dbg,DW_DLE_WRONG_ADDRESS,-1);
	    }
	    if ((opc = _dwarf_pro_get_opc(addr_adv,line_adv)) > 0) {
		no_lns_copy = 1;
		db = opc;
	    	GET_CHUNK(dbg,elfsectno,data,sizeof(Dwarf_Ubyte),error);
	    	memcpy((void *)data,(const void *)&db, sizeof(Dwarf_Ubyte));
	    	data += sizeof(Dwarf_Ubyte);
	    	sum_bytes += sizeof(Dwarf_Ubyte);
		prevline->dpl_basic_block = false;
		prevline->dpl_address = curline->dpl_address;
		prevline->dpl_line = curline->dpl_line;
	    }
	    else {
		if (addr_adv > 0) {
		    db = DW_LNS_advance_pc;
		    res = _dwarf_pro_encode_leb128_nm(addr_adv/MIN_INST_LENGTH, 
			&nbytes,
			buff1,sizeof(buff1));
                    if(res != DW_DLV_OK) {
                          DWARF_P_DBG_ERROR(dbg,DW_DLE_CHUNK_ALLOC,-1);
                    }

		    arg = buff1;
		    GET_CHUNK(dbg,elfsectno,data,nbytes+sizeof(Dwarf_Ubyte),error);
		    memcpy((void *)data, (const void *)&db, sizeof(Dwarf_Ubyte));
		    data += sizeof(Dwarf_Ubyte);
		    memcpy((void *)data,(const void *)arg, nbytes);
		    data += nbytes + sizeof(Dwarf_Ubyte);
		    sum_bytes += nbytes + sizeof(Dwarf_Ubyte);
		    prevline->dpl_basic_block = false;
		    prevline->dpl_address = curline->dpl_address;
		}
		if (line_adv != 0) {
		    db = DW_LNS_advance_line;
		    res = _dwarf_pro_encode_signed_leb128_nm(line_adv, &nbytes,
				buff1,sizeof(buff1));
                    if(res != DW_DLV_OK) {
                          DWARF_P_DBG_ERROR(dbg,DW_DLE_CHUNK_ALLOC,-1);
                    }

		    arg = buff1;
		    GET_CHUNK(dbg,elfsectno,data,nbytes+sizeof(Dwarf_Ubyte),error);
		    memcpy((void *)data, (const void *)&db, sizeof(Dwarf_Ubyte));
		    data += sizeof(Dwarf_Ubyte);
		    memcpy((void *)data,(const void *)arg, nbytes);
		    data += nbytes + sizeof(Dwarf_Ubyte);
		    sum_bytes += nbytes + sizeof(Dwarf_Ubyte);
		    prevline->dpl_basic_block = false;
		    prevline->dpl_line = curline->dpl_line;
		}
	    }
	  }		/* ends else for opc != 0 */
	  if (no_lns_copy == 0) {	/* if not a special or dw_lne_end_seq
						generate a matrix line */
		db = DW_LNS_copy;
	    	GET_CHUNK(dbg,elfsectno,data,sizeof(Dwarf_Ubyte),error);
	    	memcpy((void *)data,(const void *)&db, sizeof(Dwarf_Ubyte));
	    	data += sizeof(Dwarf_Ubyte);
	    	sum_bytes += sizeof(Dwarf_Ubyte);
		prevline->dpl_basic_block = false;
	  }
	  curline = curline->dpl_next;
	}

	/* write total length field */
	du = sum_bytes - sizeof_uword(dbg);	/* subtract length field */
	if (IS_64BIT(dbg))
	    memcpy((void *)start_sec, (const void *)&du, sizeof(Dwarf_Unsigned));
	else {
	    dw = du;
	    memcpy((void *)start_sec, (const void *)&dw, sizeof(Dwarf_ufixed));
	}

    	if (rel_nbytes > 0) {
            reloc_sects[DEBUG_LINE] = dbg->de_func(".rel.debug_line",
	        IS_64BIT(dbg), SHT_REL, 0, SHN_UNDEF,elf_sects[DEBUG_LINE], 
		&name_idx, &err);
            if (reloc_sects[DEBUG_LINE] == -1) {
    	        DWARF_P_DBG_ERROR(dbg,DW_DLE_ELF_SECT_ERR,DW_DLV_NOCOUNT);
            }

            /* write out relocation records */
            elfsectno = reloc_sects[DEBUG_LINE];
            i = _dwarf_pro_write_reloc_section(dbg,rel_head,elfsectno,rel_nbytes,error);
            if (i < 0) return i;
        }

	return dbg->de_n_debug_sect;
}

/*---------------------------------------------------------------
	Generate debug_frame section 
---------------------------------------------------------------*/
int 
_dwarf_pro_generate_debugframe(Dwarf_P_Debug dbg, Dwarf_Error *error)
{
	int elfsectno;
	int i;
	int firsttime, pad;	/* pad for padding to align cies and fdes */
	Dwarf_P_Cie curcie;
	Dwarf_P_Fde curfde;
	unsigned char *data;
	Dwarf_Word dw;
	Dwarf_Sword dsw;
	Dwarf_Unsigned du;
	Dwarf_Ubyte db;
	long *cie_offs;		/* holds byte offsets for links to fde's */
	long cie_length;
	int cie_no;
	Dwarf_P_Rel rel_head, rel_tail;	/* relocation pointers */
	int rel_nbytes;			/* bytes in relocation section */
	int name_idx;			/* holds index for rel section */
	int err;
	Dwarf_Unsigned cur_off;		/* current offset of written 
					   data, held for relocation info */

	elfsectno = elf_sects[DEBUG_FRAME];

	curcie = dbg->de_frame_cies;
	cie_length = 0;
	rel_head = NULL;
	rel_tail = NULL;
	rel_nbytes = 0;
	cur_off = 0;
	cie_offs = (long *)
	    _dwarf_p_get_alloc(dbg, sizeof(long)*dbg->de_n_cie);
	if (cie_offs == NULL) {
	    DWARF_P_DBG_ERROR(dbg,DW_DLE_CIE_OFFS_ALLOC,-1);
	}
	firsttime = 1;
	/* generate cie number as we go along */
	cie_no = 1;
	while (curcie) {
	    char *code_al;
	    int c_bytes;
	    char *data_al;
	    int d_bytes;
	    int res;
	    char buff1[ENCODE_SPACE_NEEDED];
	    char buff2[ENCODE_SPACE_NEEDED];
	    char buff3[ENCODE_SPACE_NEEDED];
	    char *augmentation;
	    char *augmented_al;
	    long augmented_fields_length;
	    int  a_bytes;

	    res = _dwarf_pro_encode_leb128_nm(curcie->cie_code_align, 
		&c_bytes,
			buff1,sizeof(buff1));
	    if(res != DW_DLV_OK) {
		           DWARF_P_DBG_ERROR(dbg,DW_DLE_CIE_OFFS_ALLOC,-1);
	    }
	    res = _dwarf_pro_encode_leb128_nm(curcie->cie_data_align,
		 &d_bytes,
			buff2,sizeof(buff2));
	    if(res != DW_DLV_OK) {
		           DWARF_P_DBG_ERROR(dbg,DW_DLE_CIE_OFFS_ALLOC,-1);
	    }
	    code_al = buff1;
	    data_al = buff2;

	    /* get the correct offset */
	    if (firsttime)
		cie_offs[cie_no-1] = 0;
	    else
		cie_offs[cie_no-1] = cie_offs[cie_no-2]+cie_length+sizeof_uword(dbg);
	    cie_no++;	
	    augmentation = curcie->cie_aug;
	    if (strcmp(augmentation, DW_CIE_AUGMENTER_STRING_V0) == 0) {
		augmented_fields_length = 0;
		res = _dwarf_pro_encode_leb128_nm(augmented_fields_length,
		     				   &a_bytes, buff3,
						   sizeof(buff3));
		augmented_al = buff3;
		if (res != DW_DLV_OK) {
		    DWARF_P_DBG_ERROR(dbg,DW_DLE_CIE_OFFS_ALLOC,-1);
		}
		cie_length = sizeof(Dwarf_Unsigned) +   /* cie_id */
			 sizeof(Dwarf_Ubyte) +		/* cie version */
			 strlen(curcie->cie_aug)+1  + 	/* augmentation */
			 c_bytes +	/* code alignment factor */
			 d_bytes +      /* data alignment factor */
			 sizeof(Dwarf_Ubyte) +	  /* return reg address */
			 a_bytes +      /* augmentation length   */
			 curcie->cie_inst_bytes;
	    }
	    else {
	        cie_length = sizeof(Dwarf_Unsigned) + 	/* cie_id */
			 sizeof(Dwarf_Ubyte) +		/* cie version */
			 strlen(curcie->cie_aug)+1  + 	/* augmentation */
			 c_bytes +
			 d_bytes +
			 sizeof(Dwarf_Ubyte) +	/* return reg address */
			 curcie->cie_inst_bytes;
	    }
	    pad = PADDING(cie_length, sizeof_uword(dbg));
	    cie_length += pad;
	    if (firsttime) {
		GET_NEW_CHUNK(dbg,elfsectno,data,cie_length+sizeof_uword(dbg), error);
		firsttime = 0;
	    }
	    else
		GET_CHUNK(dbg,elfsectno,data,cie_length+sizeof_uword(dbg), error);
	    du = cie_length;
	    if (IS_64BIT(dbg)) {
		memcpy((void *)data, (const void *)&du, sizeof(Dwarf_Unsigned));
		data += sizeof(Dwarf_Unsigned);
		du = DW_CIE_ID;
		memcpy((void *)data, (const void *)&du, sizeof(Dwarf_Unsigned));
		data += sizeof(Dwarf_Unsigned);
	    }
	    else {
		dw = du;
		memcpy((void *)data, (const void *)&dw, sizeof(Dwarf_ufixed));
		data += sizeof(Dwarf_ufixed);
		dw = DW_CIE_ID;
		memcpy((void *)data, (const void *)&dw, sizeof(Dwarf_ufixed));
		data += sizeof(Dwarf_ufixed);
	    }
	    db = curcie->cie_version;
	    memcpy((void *)data, (const void *)&db, sizeof(Dwarf_Ubyte));
	    data += sizeof(Dwarf_Ubyte);
	    strcpy((char *)data, curcie->cie_aug);
	    data += strlen(curcie->cie_aug)+1;
	    memcpy((void *)data, (const void *)code_al, c_bytes);
	    data += c_bytes;
	    memcpy((void *)data, (const void *)data_al, d_bytes);
	    data += d_bytes;
	    db = curcie->cie_ret_reg;
	    memcpy((void *)data, (const void *)&db, sizeof(Dwarf_Ubyte));
	    data += sizeof(Dwarf_Ubyte);

	    if (strcmp(augmentation, DW_CIE_AUGMENTER_STRING_V0) == 0) {
		memcpy((void *)data, (const void *) augmented_al, a_bytes);
		data += a_bytes;
	    }
	    memcpy((void *)data, (const void *)curcie->cie_inst, curcie->cie_inst_bytes);
	    data += curcie->cie_inst_bytes;
	    for (i = 0 ; i < pad ; i++) {
		*data = DW_CFA_nop;
		data++;
	    }
	    curcie = curcie->cie_next;
	}
	/* calculate current offset */
	cur_off = cie_offs[cie_no-2] + cie_length + sizeof_uword(dbg);

	/* write out fde's */
	curfde = dbg->de_frame_fdes;
	while (curfde) {
	    Dwarf_P_Frame_Pgm curinst;
	    long fde_length;
	    int pad;
	    Dwarf_P_Cie cie_ptr;
	    Dwarf_Word  cie_index, index;
	    int oet_length, afl_length, res;
	    int v0_augmentation = 0;
	    char afl_buff[ENCODE_SPACE_NEEDED];

	    /* Find the CIE associated with this fde. */
	    cie_ptr = dbg->de_frame_cies;
	    cie_index = curfde->fde_cie;
	    index = 1; /* The cie_index of the first cie is 1, not 0. */
	    while (cie_ptr && index < cie_index) {
		cie_ptr = cie_ptr->cie_next;
		index++;
	    }
	    if (cie_ptr == NULL) {
		DWARF_P_DBG_ERROR(dbg,DW_DLE_CIE_NULL,-1);
	    }

	    if (strcmp(cie_ptr->cie_aug, DW_CIE_AUGMENTER_STRING_V0) == 0) {
		v0_augmentation = 1;
		oet_length = sizeof(Dwarf_sfixed);
		/* encode the length of augmented fields. */
		res = _dwarf_pro_encode_leb128_nm(
				oet_length,
		     	        &afl_length, afl_buff,
			        sizeof(afl_buff));
		if (res != DW_DLV_OK) {
		    DWARF_P_DBG_ERROR(dbg,DW_DLE_CIE_OFFS_ALLOC,-1);
		}

	        fde_length = curfde->fde_n_bytes + 
			 sizeof_uword(dbg) +		/* cie pointer */
			 sizeof_uword(dbg) + 		/* initial loc */
			 sizeof_uword(dbg) +		/* address range */
			 afl_length        +   /* augmented field length */
			 oet_length;   /* exception_table offset */
	    }
	    else {
	        fde_length = curfde->fde_n_bytes + 
			 sizeof_uword(dbg) +		/* cie pointer */
			 sizeof_uword(dbg) + 		/* initial loc */
			 sizeof_uword(dbg);		/* address range */
	    }

	    /* using fde offset, generate DW_AT_MIPS_fde attribute 
	       for the die corresponding to this fde */
	    if (_dwarf_pro_add_AT_fde(dbg, curfde->fde_die, cur_off, error) < 0)
		return -1;

	    /* store relocation for cie pointer */
	    if (rel_head == NULL) {
		rel_head = _dwarf_pro_get_new_relrec(dbg);
		if (rel_head == NULL) {
		    DWARF_P_DBG_ERROR(dbg,DW_DLE_REL_ALLOC, -1);
		}
		rel_tail = rel_head;
	    } else {
		rel_tail->dr_next = _dwarf_pro_get_new_relrec(dbg);
		if (rel_tail->dr_next == NULL) {
		    DWARF_P_DBG_ERROR(dbg,DW_DLE_REL_ALLOC, -1);
		}
		rel_tail = rel_tail->dr_next;
		rel_tail->dr_next = NULL;
	    }
	    rel_nbytes += _dwarf_pro_rel_info(
			    dbg,
			    rel_tail,
			    /*  + for length field */
			    cur_off+sizeof_uword(dbg),
			    sect_name_idx[DEBUG_FRAME],
			    (IS_64BIT(dbg) ? R_MIPS_64:R_MIPS_32));

	    /* store relocation information for initial location */
	    rel_tail->dr_next = _dwarf_pro_get_new_relrec(dbg);
	    if (rel_tail->dr_next == NULL) {
		 DWARF_P_DBG_ERROR(dbg,DW_DLE_REL_ALLOC, -1);
	    }
	    rel_tail = rel_tail->dr_next;
	    rel_tail->dr_next = NULL;
	    rel_nbytes += _dwarf_pro_rel_info(
			    dbg,
			    rel_tail,
			    /* 2* for length and cie_ptr fields */
			    cur_off+2*sizeof_uword(dbg),
			    curfde->fde_r_symidx,
			    (IS_64BIT(dbg) ? R_MIPS_64:R_MIPS_32));

	    /* Store the relocation information for the 
	       offset_into_exception_info field, if the offset is 
	       valid (0 is a valid offset). */
	    if (v0_augmentation && 
	        curfde->fde_offset_into_exception_tables >= 0) {
		rel_tail->dr_next = _dwarf_pro_get_new_relrec(dbg);
		if (rel_tail->dr_next == NULL) {
		     DWARF_P_DBG_ERROR(dbg,DW_DLE_REL_ALLOC, -1);
		}
		rel_tail = rel_tail->dr_next;
		rel_tail->dr_next = NULL;
		rel_nbytes += _dwarf_pro_rel_info(
				dbg,
				rel_tail,
				/* offset into cie where this field starts.*/
				cur_off+4*sizeof_uword(dbg)+ afl_length,
				curfde->fde_exception_table_symbol,
				R_MIPS_SCN_DISP);
	    }

	    /* adjust for padding */
	    pad = PADDING(fde_length, sizeof_uword(dbg));
	    fde_length += pad;

	    cur_off += fde_length + sizeof_uword(dbg);

	    /* write out fde */
	    GET_CHUNK(dbg,elfsectno,data,fde_length+sizeof_uword(dbg), error);
	    du = fde_length;
	    if (IS_64BIT(dbg)) {
		memcpy((void *)data, (const void *)&du, sizeof(Dwarf_Unsigned));
		data += sizeof(Dwarf_Unsigned);
	        du = cie_offs[curfde->fde_cie-1];
		memcpy((void *)data, (const void *)&du, sizeof(Dwarf_Unsigned));
		data += sizeof(Dwarf_Unsigned);
		du = curfde->fde_initloc;
		memcpy((void *)data, (const void *)&du, sizeof(Dwarf_Unsigned));
		data += sizeof(Dwarf_Unsigned);
		du = curfde->fde_addr_range;
		memcpy((void *)data, (const void *)&du, sizeof(Dwarf_Unsigned));
		data += sizeof(Dwarf_Unsigned);
	    } 
	    else {
		dw = du;
		memcpy((void *)data, (const void *)&dw, sizeof(Dwarf_ufixed));
		data += sizeof(Dwarf_ufixed);
		dw = cie_offs[curfde->fde_cie-1];
		memcpy((void *)data, (const void *)&dw, sizeof(Dwarf_ufixed));
		data += sizeof(Dwarf_ufixed);
		dw = curfde->fde_initloc;
		memcpy((void *)data, (const void *)&dw, sizeof(Dwarf_ufixed));
		data += sizeof(Dwarf_ufixed);
		dw = curfde->fde_addr_range;
		memcpy((void *)data, (const void *)&dw, sizeof(Dwarf_ufixed));
		data += sizeof(Dwarf_ufixed);
	    }
	    if (v0_augmentation) {
		/* write the encoded augmented field length. */
		memcpy((void *)data, (const void *)afl_buff, afl_length);
		data += afl_length;
		/* write the offset_into_exception_tables field. */
		dsw = curfde->fde_offset_into_exception_tables;
		memcpy((void *)data, (const void *)&dsw, 
					    sizeof(Dwarf_sfixed));
		data += sizeof(Dwarf_sfixed);
	    }
		
	    curinst = curfde->fde_inst;
	    while (curinst) {
		db = curinst->dfp_opcode;
		memcpy((void *)data, (const void *)&db, sizeof(Dwarf_Ubyte));
		data += sizeof(Dwarf_Ubyte);
		memcpy((void *)data, (const void *)curinst->dfp_args,curinst->dfp_nbytes);
		data += curinst->dfp_nbytes;
		curinst = curinst->dfp_next;
	    }
	    /* padding */
	    for (i = 0 ; i < pad ; i++) {
		*data = DW_CFA_nop;
		data++;
	    }
	    curfde = curfde->fde_next;
	}

    	if (rel_nbytes > 0) {
            reloc_sects[DEBUG_FRAME] = dbg->de_func(".rel.debug_frame",
		IS_64BIT(dbg), SHT_REL, 0, SHN_UNDEF, elf_sects[DEBUG_FRAME], 
		&name_idx, &err);
            if (reloc_sects[DEBUG_FRAME] == -1) {
    	        DWARF_P_DBG_ERROR(dbg,DW_DLE_ELF_SECT_ERR,DW_DLV_NOCOUNT);
            }

            /* write out relocation records */
            elfsectno = reloc_sects[DEBUG_FRAME];
            i = _dwarf_pro_write_reloc_section(dbg,rel_head,elfsectno,rel_nbytes,error);
            if (i < 0) return i;
        }

	return dbg->de_n_debug_sect;
}


/*---------------------------------------------------------------
	Generate debug_info and debug_abbrev sections
---------------------------------------------------------------*/
int
_dwarf_pro_generate_debuginfo(Dwarf_P_Debug dbg,Dwarf_Error *error)
{
	int elfsectno;
	unsigned char *data;
	int cu_header_size;
	Dwarf_P_Abbrev curabbrev, abbrev_head, abbrev_tail;
	Dwarf_P_Die curdie;
	Dwarf_P_Die first_child;
	Dwarf_Word dw;
	Dwarf_Unsigned du;
	Dwarf_Half dh;
	Dwarf_Ubyte db;
	Dwarf_Half version;			/* need 2 byte quantity */
	Dwarf_Unsigned die_off;			/* offset of die in debug_info */
	int n_abbrevs;

    	int name_idx;	/* index of section name into SYMTAB */
    	int err;	/* to be passed to call back routine */
	int firsttime;
	Dwarf_Small *start_sec;
	Dwarf_P_Rel rel_head, rel_tail;	/* linked list of relocation records */
	int rel_nbytes;			/* number of bytes of relocation info */
	int retval;		/* return value from functions called */

	abbrev_head = abbrev_tail = NULL;
	rel_head = rel_tail = NULL;
	rel_nbytes = 0;
	elfsectno = elf_sects[DEBUG_INFO];
	
	/* write cu header */
	cu_header_size = sizeof_uword(dbg) +		/* length of info section */
			 sizeof(Dwarf_Half) +		/* version stamp */
			 sizeof_uword(dbg) +		/* offset into abbrev table */
			 sizeof(Dwarf_Ubyte);		/* size of target address */
	GET_NEW_CHUNK(dbg,elfsectno,data,cu_header_size,error);
	start_sec = data;
	if (IS_64BIT(dbg)) {
	    du = 0;		/* debug info size, now zero */
	    memcpy((void *)data, (const void *)&du, sizeof(Dwarf_Unsigned));
	    data += sizeof(Dwarf_Unsigned);
	}
	else {
	    dw = 0;
	    memcpy((void *)data, (const void *)&dw, sizeof(Dwarf_ufixed));
	    data += sizeof(Dwarf_ufixed);
	}
	version = CURRENT_VERSION_STAMP;	/* assume this length will not change */
	memcpy((void *)data,(const void *)&version,sizeof(Dwarf_Half));
	data += sizeof(Dwarf_Half);
	if (IS_64BIT(dbg)) {
	    du = 0;		/* offset into abbrev table */
	    memcpy((void *)data, (const void *)&du, sizeof(Dwarf_Unsigned));
	    data += sizeof(Dwarf_Unsigned);
	}
	else {
	    dw = 0;
	    memcpy((void *)data, (const void *)&dw, sizeof(Dwarf_ufixed));
	    data += sizeof(Dwarf_ufixed);
	}
	if (IS_64BIT(dbg))
	    db = sizeof(Dwarf_Addr);
	else
	    db = sizeof(Dwarf_ufixed);
	memcpy((void *)data, (const void *)&db,1);
	data++;

	curdie = dbg->de_dies;

	/* create AT_macro_info if appropriate */
	if(dbg->de_first_macinfo != NULL) {
	    if (_dwarf_pro_add_AT_macro_info(dbg,curdie,0,error) < 0)
		return -1;
	}

	/* create AT_stmt_list attribute if necessary */
	if (dbg->de_lines != NULL) 
	    if (_dwarf_pro_add_AT_stmt_list(dbg,curdie,error) < 0)
		return -1;

	die_off = cu_header_size;

	/* 
	    Relocation for abbrev offset in cu header
            store relocation record in linked list 
	*/
	rel_head = rel_tail = _dwarf_pro_get_new_relrec(dbg);
	if (rel_head == NULL) {
	    DWARF_P_DBG_ERROR(dbg,DW_DLE_REL_ALLOC, -1);
	}
	rel_nbytes += _dwarf_pro_rel_info(dbg, rel_tail,
	    SIZEOF_UWORD(dbg)+sizeof(Dwarf_Half), sect_name_idx[DEBUG_ABBREV],
	    IS_64BIT(dbg)?R_MIPS_64:R_MIPS_32);

	/* pass 0: only top level dies, add at_sibling attribute to 
		   those dies with children */
	first_child = curdie->di_child;
	while (first_child && first_child->di_right) {
	    if (first_child->di_child)
	    	dwarf_add_AT_reference(dbg,
				       first_child, 
				       DW_AT_sibling,
				       first_child->di_right,
				       error);
	    first_child = first_child->di_right;
	}

	/* pass 1: create abbrev info, get die offsets, calc relocations */
	while (curdie != NULL) {
	    int nbytes;
	    Dwarf_P_Attribute curattr;
  	    char *space;
            int res;
            char buff1[ENCODE_SPACE_NEEDED];

	    curdie->di_offset = die_off;
	    curabbrev = _dwarf_pro_getabbrev(curdie, abbrev_head);
	    if (curabbrev == NULL) {
		DWARF_P_DBG_ERROR(dbg,DW_DLE_ABBREV_ALLOC, -1);
	    }
	    if (abbrev_head == NULL) {
		n_abbrevs = 1;
		curabbrev->abb_idx = n_abbrevs;
		abbrev_tail = abbrev_head = curabbrev;
	    } else {
		/* check if its a new abbreviation, if yes, add to tail */
		if (curabbrev->abb_idx == 0) {
		    n_abbrevs++;
		    curabbrev->abb_idx = n_abbrevs;
		    abbrev_tail->abb_next = curabbrev;
		    abbrev_tail = curabbrev;
		}
	    }
	    res  = _dwarf_pro_encode_leb128_nm(curabbrev->abb_idx,
		&nbytes,
		buff1,sizeof(buff1));
	    if (res != DW_DLV_OK) {
		DWARF_P_DBG_ERROR(dbg,DW_DLE_ABBREV_ALLOC, -1);
	    }
            space = _dwarf_p_get_alloc(dbg, nbytes);
	    if (space == NULL) {
		DWARF_P_DBG_ERROR(dbg,DW_DLE_ABBREV_ALLOC, -1);
	    }
	    memcpy(space,buff1,nbytes);
	    curdie->di_abbrev = space;
	    curdie->di_abbrev_nbytes = nbytes;
	    die_off += nbytes;
	    curattr = curdie->di_attrs;
	    while (curattr) {
		if (curattr->ar_rel_type != R_MIPS_NONE) {
		    /* store relocation record in linked list */
		    if (rel_head == NULL) {
			rel_head = _dwarf_pro_get_new_relrec(dbg);
			if (rel_head == NULL) {
			    DWARF_P_DBG_ERROR(dbg,DW_DLE_REL_ALLOC, -1);
			}
			rel_tail = rel_head;
		    } else {
			rel_tail->dr_next = _dwarf_pro_get_new_relrec(dbg);
			if (rel_tail->dr_next == NULL) {
			    DWARF_P_DBG_ERROR(dbg,DW_DLE_REL_ALLOC, -1);
			}
			rel_tail = rel_tail->dr_next;
			rel_tail->dr_next = NULL;
		    }
		    switch (curattr->ar_attribute) {
			case DW_AT_stmt_list:
			    curattr->ar_rel_symidx = sect_name_idx[DEBUG_LINE];
			    break;
			case DW_AT_MIPS_fde:
			    curattr->ar_rel_symidx = sect_name_idx[DEBUG_FRAME];
			    break;
			case DW_AT_macro_info:
			    curattr->ar_rel_symidx = sect_name_idx[DEBUG_MACINFO];
			    break;
			default:
			    break;
		    }
		    rel_nbytes += _dwarf_pro_rel_info(dbg,rel_tail,
			die_off+curattr->ar_rel_offset,curattr->ar_rel_symidx,
			curattr->ar_rel_type);
		}
		die_off += curattr->ar_nbytes;
		curattr = curattr->ar_next;
	    }
	    /* depth first search */
	    if (curdie->di_child)
		curdie = curdie->di_child;
	    else {
		while (curdie != NULL && curdie->di_right == NULL) {
		    curdie = curdie->di_parent;
		    die_off++; /* since we are writing a null die at the 
				  end of each sibling chain */
		}
		if (curdie != NULL) 
		    curdie = curdie->di_right;
	    }
	}

	/* pass 2: Write out the die information */
	curdie = dbg->de_dies;
	while (curdie != NULL) {
	    Dwarf_P_Attribute curattr;

	    /* index to abbreviation table */
	    GET_CHUNK(dbg,elfsectno,data,curdie->di_abbrev_nbytes,error);
	    memcpy((void *)data, (const void *)curdie->di_abbrev,curdie->di_abbrev_nbytes);

	    /* Attribute values - need to fill in all form attributes */
	    curattr = curdie->di_attrs;
	    while (curattr) {
		GET_CHUNK(dbg,elfsectno,data,curattr->ar_nbytes,error);
		switch (curattr->ar_attribute_form) {
		    case DW_FORM_ref1:
			{ if (curattr->ar_ref_die->di_offset > (unsigned) 0xff) {
				   DWARF_P_DBG_ERROR(dbg,DW_DLE_OFFSET_UFLW,-1);
			     }
			     db = curattr->ar_ref_die->di_offset;
			     memcpy((void *)data,(const void *)&db,sizeof(Dwarf_Ubyte));
			     data += sizeof(Dwarf_Ubyte);
			     break;
			}
		    case DW_FORM_ref2:
			{ if (curattr->ar_ref_die->di_offset > (unsigned) 0xffff) {
				   DWARF_P_DBG_ERROR(dbg,DW_DLE_OFFSET_UFLW,-1);
			     }
			     dh = curattr->ar_ref_die->di_offset;
			     memcpy((void *)data,(const void *)&dh,sizeof(Dwarf_Half));
			     data += sizeof(Dwarf_Half);
			     break;
			}
		    case DW_FORM_ref_addr:
		    case DW_FORM_ref4:
			{ if (curattr->ar_ref_die->di_offset > (unsigned) 0xffffffff) {
				   DWARF_P_DBG_ERROR(dbg,DW_DLE_OFFSET_UFLW,-1);
			     }
			     dw = curattr->ar_ref_die->di_offset;
			     memcpy((void *)data,(const void *)&dw,sizeof(Dwarf_ufixed));
			     data += sizeof(Dwarf_ufixed);
			     break;
			}
		    case DW_FORM_ref8:
		    case DW_FORM_ref_udata:
			 du = curattr->ar_ref_die->di_offset;
			 memcpy((void *)data,(const void *)&du,sizeof(Dwarf_Unsigned));
			 data += sizeof(Dwarf_Unsigned);
			 break;
		    default:
		      memcpy((void *)data,(const void *)curattr->ar_data,curattr->ar_nbytes);
		      break;
		}
		curattr = curattr->ar_next;
	    }

	    /* depth first search */
	    if (curdie->di_child)
		curdie = curdie->di_child;
	    else {
		while (curdie != NULL && curdie->di_right == NULL) {
		    GET_CHUNK(dbg,elfsectno,data,1,error);
		    *data = '\0';
		    data++;
		    curdie = curdie->di_parent;
		}
		if (curdie != NULL) 
		    curdie = curdie->di_right;
	    }
	}

	/* write out debug_info size */
	if (IS_64BIT(dbg)) {
	    du = die_off - sizeof(Dwarf_Unsigned); /* dont include length field */
	    memcpy((void *)start_sec, (const void *)&du, sizeof(Dwarf_Unsigned));
	} else {
	    dw = die_off - sizeof(Dwarf_ufixed);	/* dont include length field */
	    memcpy((void *)start_sec, (const void *)&dw, sizeof(Dwarf_ufixed));
	}


	/* write out debug_abbrev section */
	elfsectno = elf_sects[DEBUG_ABBREV];

	firsttime = 1;
	curabbrev = abbrev_head;
	while (curabbrev) {
	    char *val;
	    int nbytes;
	    int idx;
	    int res;
	    char buff1[ENCODE_SPACE_NEEDED];

	    res = _dwarf_pro_encode_leb128_nm(curabbrev->abb_idx,&nbytes,
			buff1,sizeof(buff1));
	    if (res != DW_DLV_OK) {
                DWARF_P_DBG_ERROR(dbg,DW_DLE_ABBREV_ALLOC, -1);
            }

	    if (firsttime) {
		GET_NEW_CHUNK(dbg,elfsectno,data,nbytes,error);
		firsttime = 0;
	    }
	    else
		GET_CHUNK(dbg,elfsectno,data,nbytes,error);
	    val = buff1;
	    memcpy((void *)data,(const void *)val, nbytes);
	    res = _dwarf_pro_encode_leb128_nm(curabbrev->abb_tag,&nbytes,
		buff1,sizeof(buff1));
	    if (res != DW_DLV_OK) {
                DWARF_P_DBG_ERROR(dbg,DW_DLE_ABBREV_ALLOC, -1);
            }
	    val = buff1;
	    GET_CHUNK(dbg,elfsectno,data,nbytes,error);
	    memcpy((void *)data,(const void *)val, nbytes);
	    db = curabbrev->abb_children;
	    GET_CHUNK(dbg,elfsectno,data,sizeof(Dwarf_Ubyte),error);
	    memcpy((void *)data,(const void *)&db,sizeof(Dwarf_Ubyte));
	
	    /* add attributes and forms */
	    for (idx = 0; idx < curabbrev->abb_n_attr; idx++) {
		res = _dwarf_pro_encode_leb128_nm(curabbrev->abb_attrs[idx],
			&nbytes,
			buff1,sizeof(buff1));
                if (res != DW_DLV_OK) {
                    DWARF_P_DBG_ERROR(dbg,DW_DLE_ABBREV_ALLOC, -1);
                }
                val = buff1;
		GET_CHUNK(dbg,elfsectno,data,nbytes,error);
		memcpy((void *)data,(const void *)val, nbytes);
		res = _dwarf_pro_encode_leb128_nm(curabbrev->abb_forms[idx],
			&nbytes,
			buff1,sizeof(buff1));
                if (res != DW_DLV_OK) {
                    DWARF_P_DBG_ERROR(dbg,DW_DLE_ABBREV_ALLOC, -1);
                }
                val = buff1;
		GET_CHUNK(dbg,elfsectno,data,nbytes,error);
		memcpy((void *)data,(const void *)val, nbytes);
	    }
	    GET_CHUNK(dbg,elfsectno,data,2,error);	/* two zeros, for last entry */
	    *data = 0;
	    data++;
	    *data = 0;

	    curabbrev = curabbrev->abb_next;
	}

    if (rel_nbytes > 0) {
            reloc_sects[DEBUG_INFO] = dbg->de_func(".rel.debug_info",
		IS_64BIT(dbg), SHT_REL, 0, SHN_UNDEF,elf_sects[DEBUG_INFO], 
		&name_idx, &err);
            if (reloc_sects[DEBUG_INFO] == -1) {
    	        DWARF_P_DBG_ERROR(dbg,DW_DLE_ELF_SECT_ERR,DW_DLV_NOCOUNT);
        }
        /* write out relocation records */
        elfsectno = reloc_sects[DEBUG_INFO];
        retval = _dwarf_pro_write_reloc_section(dbg,rel_head,
    		elfsectno,rel_nbytes,error);
        if (retval < 0) return retval;
    }

    return dbg->de_n_debug_sect;
}


/*---------------------------------------------------------------------
	Get a buffer of section data. 
	section_idx is the elf-section number that this data applies to. 
	length shows length of returned data 
----------------------------------------------------------------------*/
/*ARGSUSED*/ /* pretend all arts used */
Dwarf_Ptr
dwarf_get_section_bytes(
	Dwarf_P_Debug dbg,
	Dwarf_Signed dwarf_section,
	Dwarf_Signed *section_idx, 
	Dwarf_Unsigned *length,
	Dwarf_Error *error)
{
	Dwarf_Ptr buf;

	if(dbg->de_debug_sects == 0) {
	  /* no more data !! */
	  return NULL;
	}
	*section_idx = dbg->de_debug_sects->ds_elf_sect_no;
	*length = dbg->de_debug_sects->ds_nbytes;
	buf = (Dwarf_Ptr *)dbg->de_debug_sects->ds_data;
	dbg->de_debug_sects = dbg->de_debug_sects->ds_next;
	dbg->de_n_debug_sect--;

	return buf;
}

/*
    Storage handler. Gets either a new chunk of memory, or
    a pointer in existing memory, from the linked list attached
    to dbg at de_debug_sects, depending on size of nbytes

    Assume dbg not null, checked in top level routine 
*/
Dwarf_Small *
_dwarf_pro_buffer (
    Dwarf_P_Debug 	dbg,
    int 		elfsectno,
    int 		nbytes,
    int 		new_chunk
)
{
    Dwarf_P_Section_Data 	cursect;

    cursect = dbg->de_last_debug_sect;
    if (new_chunk || cursect == NULL || 
	cursect->ds_nbytes + nbytes > CHUNK_SIZE) {

	cursect = (Dwarf_P_Section_Data)
	    _dwarf_p_get_alloc(dbg, sizeof(struct Dwarf_P_Section_Data_s));
	if (cursect == NULL) return(NULL);

        if (dbg->de_debug_sects == NULL) {
	    dbg->de_debug_sects = dbg->de_last_debug_sect = cursect;
        }
        else {
    	    dbg->de_last_debug_sect->ds_next = cursect;
	    dbg->de_last_debug_sect = cursect;
        }
	dbg->de_n_debug_sect++;

	cursect->ds_elf_sect_no = elfsectno;
    }
    else 
	cursect = dbg->de_last_debug_sect;

    if (cursect->ds_data == NULL) { 
        cursect->ds_data = (char *)
	    _dwarf_p_get_alloc(dbg, nbytes > CHUNK_SIZE ? nbytes : CHUNK_SIZE); 
	if (cursect->ds_data == NULL) return(NULL);

        cursect->ds_nbytes = nbytes; 
        return((Dwarf_Small *)cursect->ds_data);
    }
    else {			
            /* There is enough space in the current buffer */
        cursect->ds_nbytes += nbytes;
        return((Dwarf_Small *)(cursect->ds_data + cursect->ds_nbytes - nbytes));
    }
}


/*--------------------------------------------------------------------
	Get a pointer at byteoff offset in the buffer 
----------------------------------------------------------------------*/
char *
_dwarf_pro_nth_byteoff(
	Dwarf_P_Debug dbg,
	int byteoff)
{
	/* functions incomplete, may also need a elf section number */
	Dwarf_P_Section_Data cursect;

	cursect = dbg->de_debug_sects;

	if (cursect == NULL) 
		return NULL;
	
	for (; cursect != NULL; cursect = cursect->ds_next) {
	    if (byteoff >= cursect->ds_nbytes) {
		byteoff -= cursect->ds_nbytes;
		continue;
	    }
	    return (char *)(cursect->ds_data + byteoff);
	}
	return NULL;
}

/*------------------------------------------------------------
	Given address advance and line advance, it gives 
	either special opcode, or a number < 0
------------------------------------------------------------*/
static int
_dwarf_pro_get_opc(Dwarf_Unsigned addr_adv, int line_adv)
{
	int opc ;

	addr_adv = addr_adv/MIN_INST_LENGTH;
	if (line_adv == 0 && addr_adv == 0)
	    return OPC_INCS_ZERO;
	if (line_adv >= LINE_BASE && line_adv < LINE_BASE + LINE_RANGE) {
	    opc = (line_adv - LINE_BASE) + (addr_adv*LINE_RANGE) + OPCODE_BASE;
	    if (opc > 255) return OPC_OUT_OF_RANGE;
	    return opc;
	}
	else 
	    return LINE_OUT_OF_RANGE;
}

/*-----------------------------------------------------------------------
	Handles abbreviations. It takes a die, searches through 
	current list of abbreviations for matching one. If it
	finds one, it returns a pointer to it, and if it doesnt, 
	it returns a new one. Upto the user of this function to 
	link it up to the abbreviation head. If its a new one,
	abb_idx has 0.
-----------------------------------------------------------------------*/
static Dwarf_P_Abbrev 
_dwarf_pro_getabbrev(Dwarf_P_Die die, Dwarf_P_Abbrev head)
{
	Dwarf_P_Abbrev curabbrev;
	Dwarf_P_Attribute  curattr;
	int res1;
	int nattrs;
	int match;
	Dwarf_ufixed *forms, *attrs;	/* holds attr and form names */

	curabbrev = head;
	while (curabbrev) {
	    if ((die->di_tag == curabbrev->abb_tag) &&
	       ((die->di_child != NULL && curabbrev->abb_children == DW_CHILDREN_yes) || 
		   (die->di_child == NULL && curabbrev->abb_children == DW_CHILDREN_no)) &&
	       (die->di_n_attr == curabbrev->abb_n_attr)) {	/* chance of a match */
		curattr = die->di_attrs;
		match = 1;	/* assume match found */
		while (match && curattr) {
		    res1 = _dwarf_pro_match_attr(curattr, curabbrev, curabbrev->abb_n_attr);
		    if (res1 == 0) 
			match = 0;
		    curattr = curattr->ar_next;
		}
		if (match == 1) return curabbrev;
	    }
	    curabbrev = curabbrev->abb_next;
	}

	/* no match, create new abbreviation */
	if (die->di_n_attr != 0) {
	    forms = (Dwarf_ufixed *)
	    	_dwarf_p_get_alloc(NULL,sizeof(Dwarf_ufixed)*die->di_n_attr);
	    if (forms == NULL) return NULL;
	    attrs = (Dwarf_ufixed *)
	    	_dwarf_p_get_alloc(NULL,sizeof(Dwarf_ufixed)*die->di_n_attr);
	    if (attrs == NULL) return NULL;
	}
	nattrs = 0;
	curattr = die->di_attrs;
	while (curattr) {
	    attrs[nattrs] = curattr->ar_attribute;
	    forms[nattrs] = curattr->ar_attribute_form;
	    nattrs++;
	    curattr = curattr->ar_next;
	}

	curabbrev = (Dwarf_P_Abbrev)
	    _dwarf_p_get_alloc(NULL,sizeof(struct Dwarf_P_Abbrev_s));
	if (curabbrev == NULL) return NULL;

	if (die->di_child == NULL) 
	    curabbrev->abb_children = DW_CHILDREN_no;
	else
	    curabbrev->abb_children = DW_CHILDREN_yes;
	curabbrev->abb_tag = die->di_tag;
	curabbrev->abb_attrs = attrs;
	curabbrev->abb_forms = forms;
	curabbrev->abb_n_attr = die->di_n_attr;
	curabbrev->abb_idx = 0;
	curabbrev->abb_next = NULL;

	return curabbrev;
}

/*------------------------------------------------------------------
	Tries to see if given attribute and form combination 
	exists in the given abbreviation
-------------------------------------------------------------------*/
static int 
_dwarf_pro_match_attr(
	Dwarf_P_Attribute attr,
	Dwarf_P_Abbrev abbrev, 
	int no_attr)

{
	int i;
	int found = 0;

	for (i = 0; i < no_attr; i++) {
		if (attr->ar_attribute == abbrev->abb_attrs[i] &&
		    attr->ar_attribute_form == abbrev->abb_forms[i]) {
		    found = 1;
		    break;
		}
	}
	return found;
}

/*--------------------------------------------------------------------
	allocate a new relocation linked list element, alongwith an
	appropriate relocation record pointer
----------------------------------------------------------------------*/
static Dwarf_P_Rel 
_dwarf_pro_get_new_relrec(Dwarf_P_Debug dbg)
{
	Dwarf_P_Rel rel;

	rel = _dwarf_p_get_alloc(dbg, sizeof(struct Dwarf_P_Rel_s));
	if (rel == NULL) return NULL;

	if (IS_64BIT(dbg)) {
	    dr_rel64(rel) = (Elf64_Rel *)
			     _dwarf_p_get_alloc(dbg,sizeof(Elf64_Rel));
	    if (dr_rel64(rel) == NULL) return NULL;
	}
	else {
	    dr_rel32(rel) = (Elf32_Rel *)
			     _dwarf_p_get_alloc(dbg, sizeof(Elf32_Rel));
	    if (dr_rel32(rel) == NULL) return NULL;
	}
	return rel;
}

static int
_dwarf_pro_rel_info(
	Dwarf_P_Debug dbg,
	Dwarf_P_Rel relrec,
	Dwarf_Unsigned offset,
	Dwarf_Word symidx,
	Dwarf_Ubyte type) 
{
	int nbytes = 0;
	if (IS_64BIT(dbg)) {
	    dr_rel64(relrec)->r_offset = offset;
	    Set_REL64_info (*dr_rel64(relrec), symidx, type);
	    nbytes += sizeof(Elf64_Rel);
	}
	else {
	    dr_rel32(relrec)->r_offset = offset;
	    Set_REL32_info ( *dr_rel32(relrec), symidx, type );
	    nbytes += sizeof(Elf32_Rel);
	}

	return nbytes;
}

static int 
_dwarf_pro_write_reloc_section(
	Dwarf_P_Debug dbg,
	Dwarf_P_Rel rel_head,
	int relsectno,
	int rel_nbytes,
	Dwarf_Error *error)
{
	unsigned char *data;

	if (rel_nbytes != 0) {
	    GET_NEW_CHUNK(dbg,relsectno,data,rel_nbytes,&error);
	    while (rel_head) {
		if (IS_64BIT(dbg)) {
		    memcpy((void *)data, (const void *)dr_rel64(rel_head), 
			sizeof(Elf64_Rel));
		    data += sizeof(Elf64_Rel);
		}
		else {
		    memcpy((void *)data, (const void *)dr_rel32(rel_head), 
			sizeof(Elf32_Rel));
		    data += sizeof(Elf32_Rel);
		}
		rel_head = rel_head->dr_next;
	    }
	}
	return 0;
}
