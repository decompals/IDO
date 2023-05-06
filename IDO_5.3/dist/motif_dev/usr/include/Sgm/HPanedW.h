/*******************************************************************************
///////   Copyright 1992, Silicon Graphics, Inc.  All Rights Reserved.   ///////
//                                                                            //
// This is UNPUBLISHED PROPRIETARY SOURCE CODE of Silicon Graphics, Inc.;     //
// the contents of this file may not be disclosed to third parties, copied    //
// or duplicated in any form, in whole or in part, without the prior written  //
// permission of Silicon Graphics, Inc.                                       //
//                                                                            //
// RESTRICTED RIGHTS LEGEND:                                                  //
// Use,duplication or disclosure by the Government is subject to restrictions //
// as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data     //
// and Computer Software clause at DFARS 252.227-7013, and/or in similar or   //
// successor clauses in the FAR, DOD or NASA FAR Supplement. Unpublished -    //
// rights reserved under the Copyright Laws of the United States.             //
//                                                                            //
*******************************************************************************/
#ifndef _XmHPanedW_h
#define _XmHPanedW_h

#include <Xm/Xm.h>

/* Class record constant */
externalref WidgetClass sgHorzPanedWindowWidgetClass;

#define XmNsashPixmask "sashPixmask"
#define XmCSashPixmask "SashPixmask"

#ifndef SgIsHorzPanedWindow
#define SgIsHorzPanedWindow(w)	XtIsSubclass(w, sgHorzPanedWindowWidgetClass)
#endif /* SgIsHorzPanedWindow */

typedef struct _SgHorzPanedWindowClassRec  *SgHorzPanedWindowWidgetClass;
typedef struct _SgHorzPanedWindowRec	*SgHorzPanedWindowWidget;


#ifdef _NO_PROTO
extern Widget SgCreateHorzPanedWindow ();
#else /* _NO_PROTO */

#if defined(__cplusplus) || defined(c_plusplus)
extern "C" {
#endif

extern Widget SgCreateHorzPanedWindow (Widget parent, char *name, ArgList args, Cardinal argCount);
#endif /* _NO_PROTO */

#if defined(__cplusplus) || defined(c_plusplus)
}
#endif

#endif /* _SgHorzPanedWindow_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */
