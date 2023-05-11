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

#ifndef _SgSpringBox_h
#define _SgSpringBox_h

#include <Xm/Xm.h>

#ifdef __cplusplus
extern "C" {
#endif

/*  SpringBox Widget  */

externalref WidgetClass sgSpringBoxWidgetClass;

typedef struct _SgSpringBoxClassRec * SgSpringBoxWidgetClass;
typedef struct _SgSpringBoxRec      * SgSpringBoxWidget;

/* ifndef for Fast Subclassing  */

#ifndef SgIsSpringBox
#define SgIsSpringBox(w)	XtIsSubclass(w, sgSpringBoxWidgetClass)
#endif  /* SgIsSpringBox */

/********    Public Function Declarations    ********/
#ifdef _NO_PROTO

extern Widget SgCreateSpringBox() ;
extern Widget SgCreateSpringBoxDialog() ;

#else

extern Widget SgCreateSpringBox( 
                        Widget parent,
                        char *name,
                        ArgList arglist,
                        Cardinal argcount) ;
extern Widget SgCreateSpringBoxDialog( 
                        Widget parent,
                        char *name,
                        ArgList arglist,
                        Cardinal argcount) ;

#endif /* _NO_PROTO */
/********    End Public Function Declarations    ********/


#define XmNtopSpring         "topSpring"
#define XmNbottomSpring      "bottomSpring"
#define XmNleftSpring        "leftSpring"
#define XmNrightSpring       "rightSpring"

#define XmNverticalSpring    "verticalSpring"
#define XmNhorizontalSpring  "horizontalSpring"
#define XmCSpring            "Spring"

#define XmNminSpacing         "minSpacing"

#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif


#endif /* _SgSpringBox_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */
