/* 
 * (c) Copyright 1989, 1990, 1991, 1992 OPEN SOFTWARE FOUNDATION, INC. 
 * ALL RIGHTS RESERVED 
*/ 
/* 
 * Motif Release 1.2
*/ 
/*   $RCSfile: textE.h,v $ $Revision: 1.1 $ $Date: 1992/05/08 18:13:58 $ */

#if ( defined text_h )
#define extern 
#endif

#ifdef _NO_PROTO
extern void FileOKCallback();
extern void NewPaneCallback();
extern void KillPaneCallback();
extern void FindCallback();
#else
extern void FileOKCallback(Widget fsb, ViewPtr this,
			   XmFileSelectionBoxCallbackStruct *call_data);

extern void FindCallback(Widget button, ViewPtr this,
			  XmPushButtonCallbackStruct *call_data);

extern void NewPaneCallback(Widget fsb, ViewPtr this,
			    XmPushButtonCallbackStruct *call_data);

extern void KillPaneCallback(Widget button, ViewPtr this,
			    XmPushButtonCallbackStruct *call_data);

#endif

#if ( defined extern )
#undef extern 
#endif
