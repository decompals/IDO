/* 
 * (c) Copyright 1989, 1990, 1991, 1992 OPEN SOFTWARE FOUNDATION, INC. 
 * ALL RIGHTS RESERVED 
*/ 
/* 
 * Motif Release 1.2
*/ 
/*   $RCSfile: Screen.h,v $ $Revision: 1.1 $ $Date: 1992/05/08 18:15:25 $ */
/*
*  (c) Copyright 1990, 1991, 1992 HEWLETT-PACKARD COMPANY */

#ifndef _XmScreen_h
#define _XmScreen_h

#include <Xm/Xm.h>
#ifdef __cplusplus
extern "C" {
#endif

#ifndef XmIsScreen
#define XmIsScreen(w) (XtIsSubclass(w, xmScreenClass))
#endif /* XmIsScreen */

/* Class record constants */

typedef struct _XmScreenRec *XmScreen;
typedef struct _XmScreenClassRec *XmScreenClass;
extern 	WidgetClass xmScreenClass;


/********    Public Function Declarations    ********/
#ifdef _NO_PROTO

extern Widget XmGetXmScreen() ;

#else

extern Widget XmGetXmScreen( 
                        Screen *screen) ;
#endif /* _NO_PROTO */
/********    End Public Function Declarations    ********/


#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* _XmScreen_h */

