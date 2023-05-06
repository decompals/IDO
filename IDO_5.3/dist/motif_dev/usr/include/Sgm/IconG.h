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
#ifndef _SgIconG_h
#define _SgIconG_h

#include <Xm/Xm.h>

#ifdef __cplusplus
extern "C" {
#endif

/*  Widget class and record definitions  */

externalref WidgetClass sgIconGadgetClass;

typedef struct _SgIconGadgetClassRec * SgIconGadgetClass;
typedef struct _SgIconGadgetRec      * SgIconGadget;
typedef struct _SgIconGCacheObjRec   * SgIconGCacheObject;

/*fast subclass define */
#ifndef SgIsIconGadget
#define SgIsIconGadget(w)     XtIsSubclass(w, sgIconGadgetClass)
#endif /* SgIsIconGadget */


/********    Public Function Declarations    ********/

#ifdef _NO_PROTO

extern Widget SgCreateIconGadget() ;

#else

extern Widget SgCreateIconGadget(Widget   parent,
				 char    *name,
				 Arg     *arglist,
				 Cardinal argCount) ;

#endif /* _NO_PROTO */
/********    End Public Function Declarations    ********/

#define XmICON_TOP          0
#define XmICON_BOTTOM       1
#define XmICON_LEFT         2
#define XmICON_RIGHT        3
#define XmNiconBackground  "iconBackground"
#define XmNiconForeground  "iconForeground"
#define XmNiconPlacement   "iconPlacement"
#define XmCIconPlacement   "IconPlacement"
#define XmRIconPlacement   "IconPlacement"
#define XmNshowArrow       "showArrow"
#define XmCShowArrow       "ShowArrow"
#define XmNlabelBitmap     "labelBitmap"
#define XmCLabelBitmap     "LabelBitmap"
#define XmNmarginMiddle      "marginMiddle"
#define XmCMarginMiddle      "MarginMiddle"
#define XmCLabelInsensitiveBitmap     "LabelInsensitiveBitmap"
#define XmNlabelInsensitiveBitmap     "labelInsensitiveBitmap"
#define XmSHADOW_DROP      11

#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif


#endif /* _SgIconG_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */

