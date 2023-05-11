/* 
 * (c) Copyright 1989, 1990, 1991, 1992 OPEN SOFTWARE FOUNDATION, INC. 
 * ALL RIGHTS RESERVED 
*/ 
/* 
 * Motif Release 1.2
*/ 
/*   $RCSfile: DropTrans.h,v $ $Revision: 1.4 $ $Date: 1993/01/12 21:16:45 $ */
/*
*  (c) Copyright 1990, 1991, 1992 HEWLETT-PACKARD COMPANY */

#ifndef _XmDropTrans_h
#define _XmDropTrans_h

#include <Xm/Xm.h>

#ifdef __cplusplus
extern "C" {
#endif

#define XmTRANSFER_FAILURE 0
#define XmTRANSFER_SUCCESS 1

externalref WidgetClass xmDropTransferObjectClass;

typedef struct _XmDropTransferClassRec * XmDropTransferObjectClass;
typedef struct _XmDropTransferRec      * XmDropTransferObject;

#ifndef XmIsDropTransfer
#define XmIsDropTransfer(w) \
	XtIsSubclass((w), xmDropTransferObjectClass)
#endif /* XmIsDropTransfer */

typedef struct _XmDropTransferEntryRec {
	XtPointer	client_data;
	Atom		target;
} XmDropTransferEntryRec, * XmDropTransferEntry;

/********    Public Function Declarations    ********/
#ifdef _NO_PROTO

extern Widget XmDropTransferStart() ;
extern void XmDropTransferAdd() ;

#else

extern Widget XmDropTransferStart( 
                        Widget refWidget,
                        ArgList args,
                        Cardinal argCount) ;
extern void XmDropTransferAdd( 
                        Widget widget,
                        XmDropTransferEntry transfers,
                        Cardinal num_transfers) ;

#endif /* _NO_PROTO */
#ifdef __sgi	/* Silicon Graphics added functions for suspending a drop */
#ifdef _NO_PROTO
extern void SG_DropTransferSuspend();
extern void SG_DropTransferContinue();
#else /* _NO_PROTO */
extern void SG_DropTransferSuspend(Widget widget);
extern void SG_DropTransferContinue(Widget widget);
#endif /* _NO_PROTO */
#endif /* __sgi */
/********    End Public Function Declarations    ********/


#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* _XmDropTrans_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */
