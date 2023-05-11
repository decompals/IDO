/* 
 * (c) Copyright 1989, 1990, 1991, 1992, 1993 OPEN SOFTWARE FOUNDATION, INC. 
 * ALL RIGHTS RESERVED 
*/ 
/* 
 * Motif Release 1.2.2
*/ 
/*   $RCSfile: DropTransP.h,v $ $Revision: 1.6 $ $Date: 1993/05/25 03:56:54 $ */
/*
*  (c) Copyright 1990, 1991, 1992 HEWLETT-PACKARD COMPANY */

#ifndef _XmDropTransferP_h
#define _XmDropTransferP_h

#include <Xm/DropTrans.h>
#include <Xm/XmP.h>


#ifdef __cplusplus
extern "C" {
#endif

/*  DropTransfer class structure  */

#ifdef _NO_PROTO
typedef Widget (*XmDropTransferStartTransferProc)();
typedef void (*XmDropTransferAddTransferProc)();
#else
typedef Widget (*XmDropTransferStartTransferProc)(Widget,
	ArgList, Cardinal);
typedef void (*XmDropTransferAddTransferProc)(Widget,
	XmDropTransferEntry, Cardinal);
#endif

typedef struct _XmDropTransferClassPart
{
	XmDropTransferStartTransferProc	start_drop_transfer;
	XmDropTransferAddTransferProc	add_drop_transfer;
	XtPointer extension;
#ifdef __sgi /* Allow future extensions without breaking shared library compatibility */
	_SgClassExtension _SG_vendorExtension;
#endif /* __sgi */
} XmDropTransferClassPart;

/*  Full class record declaration */

typedef struct _XmDropTransferClassRec
{
   ObjectClassPart        object_class;
   XmDropTransferClassPart dropTransfer_class;
} XmDropTransferClassRec;

extern XmDropTransferClassRec xmDropTransferClassRec;


typedef struct _XmDropTransferListRec {
	XmDropTransferEntry	transfer_list;
	Cardinal		num_transfers;
} XmDropTransferListRec, * XmDropTransferList;

#ifdef __sgi	/* SGI drag and drop extension*/
  /* this extension includes variables for suspending a drop */
  typedef struct __SG_XmDropTransferExtPart
  {
    Boolean			suspended;
    Atom			save_selection;
  } _SG_XmDropTransferExtPart;

  typedef struct __SG_XmDropTransferExt
  {
    _SgInstanceExtensionRec	common;   /* Stuff all instance rec's have */
    _SG_XmDropTransferExtPart	rsrc;	  /* Resources & instance var's */
  } _SG_XmDropTransferExtRec, *_SG_XmDropTransferExt;

#define _SG_DropTransferPtr(w) \
((_SG_XmDropTransferExt)(((XmDropTransferObject)(w))->dropTransfer._SG_vendorExtension))

#endif /* __sgi */

/*  The DropTransfer instance record  */

typedef struct _XmDropTransferPart
{
    XmDropTransferEntry		drop_transfers;
    Cardinal			num_drop_transfers;
    Atom			selection;
    Widget			dragContext;
    Time			timestamp;
    Boolean			incremental;
    Window			source_window;
    unsigned int		tag;
    XtSelectionCallbackProc 	transfer_callback;
    unsigned char		transfer_status;

    Atom 			motif_drop_atom;
    
    XmDropTransferList		drop_transfer_lists;
    Cardinal			num_drop_transfer_lists;
    Cardinal			cur_drop_transfer_list;
    Cardinal			cur_xfer;
    Atom *			cur_targets;
    XtPointer *			cur_client_data;
#ifdef __sgi /* Allow future extensions without breaking shared library compatibility */
    _SG_XmDropTransferExt	 _SG_vendorExtension;
#endif /* __sgi */
} XmDropTransferPart;

/*  Full instance record declaration  */

typedef struct _XmDropTransferRec
{
	ObjectPart	object;
	XmDropTransferPart dropTransfer;
} XmDropTransferRec;


/********    Private Function Declarations    ********/
#ifdef _NO_PROTO


#else


#endif /* _NO_PROTO */
/********    End Private Function Declarations    ********/


#if defined(__cplusplus) || defined(c_plusplus)
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* _XmDropTransferP_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */
