/*
	dwarf_addr_finder.c
	$Source: /hosts/bonnie/proj/irix6.4-ssg/isms/cmplrs/libdwarf/RCS/dwarf_addr_finder.c,v $
	$Date: 1996/06/28 15:04:31 $

	This function make it possible to fix up addresses in
	dwarf, such as is needed by rqs.   The method: for each
	address in dwarf, it calls a callback-function,
	passing a section number and the offset-in-the-section
	of the address and the present value of the addr at that
	offset and the size, in bytes, of an address.
	The size will be identical for all callbacks, of course!

	This only finds addresses actually marked as such. addresses
	hidden in constants are NOT noticed.

	This is presumed to be running against an a.out or dso and
	it does not rely on relocation information (no such information
	exists for the dwarf sections in an a.out or dso).

	The implementor of the callback-function then
	does whatever is needed with that address.
	  rqs, for example, will modify the section data to update
	  the address.
	

	User Interface:

	int dwarf_addr_finder(Elf * elf_file_ptr,
		Dwarf_addr_callback_func cb_func,
		int *dwerr);
	It returns
		DW_DLV_OK if it succeeds(all callbacks have been done).
		DW_DLV_ERROR if there was an error (and sets the
			error pointer (see the dwarf Consumer doc
			for information on handling that).
		DW_DLV_NO_ENTRY if there is no dwarf data present.
	*dwerr may be set to a dwarf error code if there is one known.
	Or it may be set to -1 (some other error).

 	The caller must pass a valid open Elf * elf file pointer.
	If there is an error, the dwerr parameter is used to put the libdwarf
	error code in the int pointed at.


	The callback Function (whose address is
	passed in the 'func' arg above) is implemented by the user as:

	int cb_func( int section, Dwarf_Off secoff, Dwarf_Addr existingAddr,
		int addrsize) { ...do whatever... return DW_DLV_OK;}

	The callback function should return one of the following to
	its caller:
		DW_DLV_OK meaning all is ok, continue.
	 	DW_DLV_ERROR meaning that the called-back function
		detected something wrong: abort the entire process
		and return immediately from dwarf_addr_finder, returning,
		from there, DW_DLV_ERROR.


	Obviously you may choose any name: there is nothing
	special about cb_func!
	
			

*/
#include <libelf.h>
#include <dwarf.h>
#include <libdwarf.h>
#include "dwarf_base_types.h"
#include "dwarf_alloc.h"
#include "dwarf_opaque.h"
#include "dwarf_arange.h"
#include "dwarf_line.h"
#include "dwarf_frame.h"
#include <cmplrs/dwarf_addr_finder.h>
#include "dwarf_error.h"

typedef unsigned long long ull;

static int do_this_die_and_dealloc(Dwarf_Debug dbg,Dwarf_Die die,int *errval);
static int 
handle_debug_info(Dwarf_Debug dbg,int *errval);
static int 
handle_debug_frame(Dwarf_Debug dbg,Dwarf_addr_callback_func cb_func,int *errval);
static int 
handle_debug_aranges(Dwarf_Debug dbg,Dwarf_addr_callback_func cb_func,int *errval);
static int 
handle_debug_line(Dwarf_Debug dbg,Dwarf_Die cu_die,Dwarf_addr_callback_func cb_func,int *errval);
static int 
handle_debug_loc(void);


static Dwarf_addr_callback_func send_addr_note;

int
_dwarf_addr_finder(Elf * elf_file_ptr,
                Dwarf_addr_callback_func cb_func,
                int *dwerr)
{

	Dwarf_Error err = 0;
	Dwarf_Debug dbg = 0;
	int res = 0;
	int errval = 0;
	int sections_found = 0;

	res  = dwarf_elf_init(elf_file_ptr,DW_DLC_READ,/*errhand*/0,
		/*errarg*/0,&dbg,&err);
	if(res == DW_DLV_ERROR){
		int errv = (int)dwarf_errno(err);
		return  errv;
	}
        if(res == DW_DLV_NO_ENTRY) {
	 return res;
	}
	
	send_addr_note = cb_func;
    
	res = handle_debug_info(dbg,&errval);
	switch(res) {
	case DW_DLV_OK:
		++sections_found;
		break;
	case DW_DLV_NO_ENTRY:
		
		break;
	default:
	case DW_DLV_ERROR:
	  dwarf_finish(dbg,&err);
	  *dwerr = errval;
	  return res; 
	}

	res = handle_debug_aranges(dbg,cb_func,&errval);
	switch(res) {
	case DW_DLV_OK:
		++sections_found;
		break;
	case DW_DLV_NO_ENTRY:
		break;
	default:
	case DW_DLV_ERROR:
	  dwarf_finish(dbg,&err);
	  *dwerr = errval;
	  return res; 
	}
	res = handle_debug_frame(dbg,cb_func,&errval);
	switch(res) {
	case DW_DLV_OK:
		++sections_found;
		break;
	case DW_DLV_NO_ENTRY:
		break;
	default:
	case DW_DLV_ERROR:
	  dwarf_finish(dbg,&err);
	  *dwerr = errval;
	  return res; 
	}

	res = handle_debug_loc(); /* does nothing */
	switch(res) {
	case DW_DLV_OK:
		++sections_found;
		break;
	case DW_DLV_NO_ENTRY:
		break;
	default:
	case DW_DLV_ERROR:
	  /* IMPOSSIBLE  : handle_debug_loc cannot return this */
	  dwarf_finish(dbg,&err);
	  *dwerr = errval;
	  return res; 
	}

	

	*dwerr = 0;
	res =dwarf_finish(dbg,&err);
	if(res == DW_DLV_ERROR) {
		*dwerr = (int)dwarf_errno(err);
		return DW_DLV_ERROR;
	}
	if(sections_found == 0) {
		return DW_DLV_NO_ENTRY;
	}
	return  DW_DLV_OK;

}

/*
	Return DW_DLV_OK, ERROR, or NO_ENTRY.
*/
static int 
handle_debug_info(Dwarf_Debug dbg,int *errval)
{
  Dwarf_Unsigned nxtoff = 1;
  Dwarf_Unsigned hdr_length;
  Dwarf_Half     version_stamp;
  Dwarf_Unsigned  abbrev_offset;
  Dwarf_Half     addr_size;
  Dwarf_Error err;
  int terminate_now = 0;
  int res = 0;
  Dwarf_Die sibdie;
  int sibres;
  int nres = DW_DLV_OK;


  for(
      nres = dwarf_next_cu_header(dbg,&hdr_length,&version_stamp,
		&abbrev_offset,
		&addr_size,&nxtoff,&err)
    ; terminate_now == 0 && nres == DW_DLV_OK  ;
        nres  = dwarf_next_cu_header(dbg,&hdr_length,&version_stamp,
		&abbrev_offset,
		&addr_size, &nxtoff, &err) 
	                             ) {

	Dwarf_Die curdie = 0;
	/* try to get the compilation unit die */
	sibres = dwarf_siblingof(dbg,curdie,&sibdie,&err);
        if(sibres == DW_DLV_OK) {
	  res = do_this_die_and_dealloc(dbg, sibdie, errval);
	  switch(res) {
	  case DW_DLV_OK:
		break;
	  case DW_DLV_NO_ENTRY:
		break;
	  default:
	  case DW_DLV_ERROR:
		return DW_DLV_ERROR;
	  }
	}else  if(sibres == DW_DLV_ERROR) {
		*errval = (int)dwarf_errno(err);
		return DW_DLV_ERROR;
	}else {
		/* NO ENTRY! */
		/* impossible? */
	}
     
  }
  if(nres == DW_DLV_ERROR) {
	int  localerr = (int)dwarf_errno(err);
	*errval = localerr;
	return DW_DLV_ERROR;
  }
  return DW_DLV_OK;
}

static int
might_have_addr[] = {
DW_AT_high_pc,
DW_AT_low_pc,
};
static int
might_have_locdesc[] = {
DW_AT_segment,
DW_AT_return_addr,
DW_AT_frame_base,
DW_AT_static_link,
DW_AT_data_member_location,
DW_AT_string_length,
DW_AT_location,
DW_AT_use_location,
DW_AT_vtable_elem_location,
};

/*
	Return DW_DLV_OK if handling this went ok.
*/
static int
handle_attr_addr(Dwarf_Debug dbg,Dwarf_Die die,Dwarf_Half attrnum,
	Dwarf_Error*perr)
{
	int res = DW_DLV_OK;
	Dwarf_Off offset;
	Dwarf_Addr addr;
        Dwarf_Half form;
        int ares;

	Dwarf_Attribute attr;
	    ares = dwarf_attr(die,attrnum,&attr,perr);
	    if(ares == DW_DLV_OK) {
	      int formres = dwarf_whatform(attr,&form,perr);
	      switch(formres) {
	      case DW_DLV_OK:
			break;
	      case DW_DLV_ERROR:
	      case DW_DLV_NO_ENTRY: /* impossible. */
		  return formres;
	      
	      }
	   
	      switch(form) {
	      case DW_FORM_ref_addr:
	      case DW_FORM_addr:
		  res = dwarf_attr_offset(die,attr,&offset,perr);
		  if(res == DW_DLV_OK) {
		    ares = dwarf_formaddr(attr,&addr,perr);
		    if(ares == DW_DLV_OK) {
		      send_addr_note(DW_SECTION_INFO,offset,addr);
		    } else if( ares == DW_DLV_ERROR) {
			return ares;
		    } /* no entry: ok. */
		  } else {
		    res = DW_DLV_ERROR; /* NO_ENTRY is impossible.*/
		  }
		  break;
  
	      default:
		/* surprising! An error? */

		  ; /* do nothing */
	      }
	      dwarf_dealloc(dbg,attr, DW_DLA_ATTR);
  	
	    } else {
	      res = ares;
	    }
   return res;
}

/*
	Return DW_DLV_OK if handling this went ok.
*/
static int
handle_attr_locdesc(Dwarf_Debug dbg,Dwarf_Die die,Dwarf_Half attrnum,
	Dwarf_Error*perr)
{
	int retval = DW_DLV_OK;
	Dwarf_Attribute attr;
	Dwarf_Locdesc *llbuf;
	Dwarf_Signed i;
	Dwarf_Off offset;
	Dwarf_Loc *locp;
	unsigned int entindx;
	int res;
	int ares;


	    ares = dwarf_attr(die,attrnum,&attr,perr);
	    if(ares == DW_DLV_OK) {
	      Dwarf_Half form ;
	      int fres =  dwarf_whatform(attr,&form,perr);
	      if(fres == DW_DLV_OK) {
	        switch(form) {
	        case DW_FORM_block1:
	        case DW_FORM_block2:
	        case DW_FORM_block4:
		  /* must be location description */
		  res = dwarf_attr_offset(die,attr,&offset,perr);
		  llbuf = 0;
		  if(res == DW_DLV_OK) {
		    Dwarf_Signed count;
		    int lres =  dwarf_loclist(attr,&llbuf,&count,perr);
		    if(lres != DW_DLV_OK) {
			return lres;
		    }
		    if(count != 1) {
			/* this cannot happen! */
			/* perr? */
			_dwarf_error(dbg,perr,DW_DLE_LOCDESC_COUNT_WRONG);
			retval = DW_DLV_ERROR;
			return retval;
		    }
		    for(i = 0; i < count; ++i) {
			unsigned int ents = llbuf[i].ld_cents;
			locp = llbuf[i].ld_s;
			for(entindx = 0; entindx < ents; entindx++) {
			  Dwarf_Loc *llocp;
			  llocp = locp + entindx;
			  if (llocp->lr_atom == DW_OP_addr) {
		               send_addr_note(DW_SECTION_INFO,offset+
				llocp->lr_offset+1
				/* The offset is the offset of the  atom,
				** and we know the addr is 1 past it.
				*/
					,llocp->lr_number);
			  }
			}
		    }


		    if(count > 0) {
		        for(i = 0; i < count; ++i) {
			    dwarf_dealloc(dbg,llbuf[i].ld_s,DW_DLA_LOC_BLOCK);
	                }
			dwarf_dealloc(dbg,llbuf,DW_DLA_LOCDESC);
		    }
		  } else {
		    retval =  res;
	          }
		  break;
                  
	        default:
		  /* must be a const offset in debug_loc */
		  ; /* do nothing */
	        }
	        dwarf_dealloc(dbg,attr, DW_DLA_ATTR);
	     } /* else error or no entry */
	     retval = fres;
	    } else {
	      retval = ares;
	    }
   return retval;
}

/*
  Return DW_DLV_OK, or DW_DLV_ERROR

  Handle the addrs in a single die.
*/
static int
process_this_die_attrs(Dwarf_Debug dbg,Dwarf_Die newdie,int *errval)
{
	Dwarf_Error err;
	Dwarf_Half i;
	Dwarf_Half newattrnum;
	int res;
	int tres;
	Dwarf_Half ltag;

	Dwarf_Off doff;
	int doffres = dwarf_dieoffset(newdie,&doff,&err);
	if(doffres != DW_DLV_OK) {
	   if(doffres ==DW_DLV_ERROR) {
		*errval = (int)dwarf_errno(err);
	   }
	   return doffres;
	}
        tres = dwarf_tag(newdie,&ltag,&err);
	if(tres != DW_DLV_OK) {
		return tres;
	}
	if(DW_TAG_compile_unit == ltag) {
	   /* because of the way the dwarf_line code works, we
	   ** do lines only per compile unit.
	   ** This may turn out to be wrong if we have lines
	   ** left unconnected to a CU.
	   ** of course such lines will not, at present, be
	   ** used by gnome
	   ** This is not ideal as coded due to the dwarf_line.c issue.
	   */
	   int lres;
           lres = handle_debug_line(dbg, newdie,
                 send_addr_note,errval);
	   if(lres == DW_DLV_ERROR) {
		return lres;
	   }
	}

	for(i=0;i < sizeof(might_have_addr)/sizeof(int);i++){
	  int resattr;
	  Dwarf_Bool hasattr;
	  newattrnum = might_have_addr[i];
	  err = 0;
	  resattr = dwarf_hasattr(newdie,newattrnum,&hasattr,&err);
	  if(DW_DLV_OK == resattr) {
	    if(hasattr) {
	      res = handle_attr_addr(dbg,newdie,newattrnum,&err);
	      if(res != DW_DLV_OK) {
	        *errval = (int)dwarf_errno(err);
	        return DW_DLV_ERROR;
	      }
	    }
	  } else {
	    if(resattr == DW_DLV_ERROR) {
	        *errval = (int)dwarf_errno(err);
		return resattr;
	    }
	  }
	}
	for(i=0;i < sizeof(might_have_locdesc)/sizeof(int);i++){
	  int resattr;
	  Dwarf_Bool hasattr;
	  newattrnum = might_have_locdesc[i];
	  err = 0;
	  resattr = dwarf_hasattr(newdie,newattrnum,&hasattr,&err);
	  if(DW_DLV_OK == resattr) {
	    if(hasattr) {
	      res = handle_attr_locdesc(dbg,newdie,newattrnum,&err);
	      if(res != DW_DLV_OK) {
	        *errval = (int)dwarf_errno(err);
	        return DW_DLV_ERROR;
	      }
	    }
	  } else {
	    if(resattr == DW_DLV_ERROR) {
	        *errval = (int)dwarf_errno(err);
		return resattr;
	    }
	  }
	}

	return DW_DLV_OK;
}

/*
	Handle siblings as a list,
	Do children by recursing.
	Effectively this is walking the tree preorder.

	This dealloc's any die passed to it, so the
	caller should not do that dealloc.
	It seems more logical to have the one causing
	the alloc to do the dealloc, but that way this
	routine became a mess.

*/
static int 
do_this_die_and_dealloc(Dwarf_Debug dbg,Dwarf_Die die,int *errval)
{

	Dwarf_Die prevdie = 0;
	Dwarf_Die newdie = die;
	Dwarf_Error err = 0;
	int res = 0;
	int sibres = DW_DLV_OK;
	int tres = DW_DLV_OK;
	Dwarf_Die sibdie;

	while (sibres == DW_DLV_OK) {
	    Dwarf_Die ch_die;


	    res = process_this_die_attrs(dbg,newdie,errval);
	    switch(res) {
		case DW_DLV_OK:
			break;
		case DW_DLV_NO_ENTRY:
			break;
		default:
		case DW_DLV_ERROR:
	                if(prevdie) {
	                   dwarf_dealloc(dbg,prevdie,DW_DLA_DIE);
	                   prevdie = 0;
	                }
			return DW_DLV_ERROR;
	    }

	    tres  = dwarf_child(newdie,&ch_die,&err);
	   
	    if(tres == DW_DLV_OK ) {
		   res = do_this_die_and_dealloc(dbg,ch_die,errval);
	           switch(res) {
		       case DW_DLV_OK:
			break;
		       case DW_DLV_NO_ENTRY:
			break;
		       default:
		       case DW_DLV_ERROR:
	                if(prevdie) {
	                   dwarf_dealloc(dbg,prevdie,DW_DLA_DIE);
	                   prevdie = 0;
	                }
			return DW_DLV_ERROR;
	           }
	    } else if (tres  == DW_DLV_ERROR) {
		/* An error!
		*/
	        *errval = (int)dwarf_errno(err);
	        if(prevdie) {
	              dwarf_dealloc(dbg,prevdie,DW_DLA_DIE);
	              prevdie = 0;
	        }
		dwarf_dealloc(dbg,err,DW_DLA_ERROR);
		return DW_DLV_ERROR;
	    } /* else was NO ENTRY */
	    prevdie = newdie;
	    sibdie = 0;
	    sibres = dwarf_siblingof(dbg,newdie,&sibdie,&err);
	    if(prevdie) {
	      dwarf_dealloc(dbg,prevdie,DW_DLA_DIE);
	      prevdie = 0;
	    }
	    newdie = sibdie;
		
	}
	if(sibres  == DW_DLV_NO_ENTRY) {
	   return DW_DLV_OK;
  	}
	/*error. */
	*errval = (int)dwarf_errno(err);
	if(prevdie) {
	      dwarf_dealloc(dbg,prevdie,DW_DLA_DIE);
	      prevdie = 0;
	}
	dwarf_dealloc(dbg,err,DW_DLA_ERROR);
	return DW_DLV_ERROR;

}


static int 
handle_debug_frame(Dwarf_Debug dbg,Dwarf_addr_callback_func cb_func,int *errval)
{
  int retval = DW_DLV_OK;
  int res;
  Dwarf_Error err;
  Dwarf_Addr  *addrlist;
  Dwarf_Off  *offsetlist;
  Dwarf_Signed count;
  int i;

  res = _dwarf_frame_address_offsets(dbg,&addrlist,&offsetlist,&count,&err);
  if(res == DW_DLV_OK) {
     for(i = 0; i < count; i++) {
        cb_func(DW_SECTION_FRAME,offsetlist[i],addrlist[i]);
     }
     dwarf_dealloc(dbg,offsetlist,DW_DLA_ADDR);
     dwarf_dealloc(dbg,addrlist,DW_DLA_ADDR);
  } else if (res == DW_DLV_NO_ENTRY) {
        retval = res;
  } else {
        *errval = (int)dwarf_errno(err);
        retval = DW_DLV_ERROR;
  }
  return retval;

}
static int 
handle_debug_aranges(Dwarf_Debug dbg,Dwarf_addr_callback_func cb_func,int *errval)
{
  int retval = DW_DLV_OK;
  Dwarf_Error err;
  Dwarf_Addr  *aranges;
  Dwarf_Signed count;
  int indx;
  Dwarf_Off *offsets;

  retval = _dwarf_get_aranges_addr_offsets(dbg,&aranges,&offsets,&count,&err);
  if(retval == DW_DLV_OK) {
	if(count == 0) {
	  retval = DW_DLV_NO_ENTRY;
	} else {
	 for(indx = 0; indx < count; indx++) {
		cb_func(DW_SECTION_ARANGES,offsets[indx],aranges[indx]);
	 }
	}
	dwarf_dealloc(dbg,aranges,DW_DLA_ADDR);
	dwarf_dealloc(dbg,offsets,DW_DLA_ADDR);
  } else if(retval == DW_DLV_NO_ENTRY) {
	; /* do nothing */
  } else {
	*errval = (int)dwarf_errno(err);
	retval =  DW_DLV_ERROR;
  }
  return retval;
}
static int 
handle_debug_line(Dwarf_Debug dbg, Dwarf_Die cu_die,
   Dwarf_addr_callback_func cb_func,int *errval)
{
  int retval = DW_DLV_OK;
  int res;
  Dwarf_Error err;
  Dwarf_Addr  *addrlist;
  Dwarf_Off  *offsetlist;
  Dwarf_Unsigned count;
  Dwarf_Unsigned i;
 
  res = _dwarf_line_address_offsets(dbg,cu_die,&addrlist,&offsetlist,&count,&err);
  if(res == DW_DLV_OK) {
     for(i = 0; i < count; i++) {
	cb_func(DW_SECTION_LINE,offsetlist[i],addrlist[i]);

     }
     dwarf_dealloc(dbg,offsetlist,DW_DLA_ADDR);
     dwarf_dealloc(dbg,addrlist,DW_DLA_ADDR);
  } else if (res == DW_DLV_NO_ENTRY) {
	retval = res;
  } else {
	*errval = (int)dwarf_errno(err);
	retval = DW_DLV_ERROR;
  }
  return retval;
}

/*
	We need to add support for this. Currently we do not
	generate this section.
	FIX!
*/
static int 
handle_debug_loc(void)
{
  int retval = DW_DLV_NO_ENTRY;
  return retval;
}
