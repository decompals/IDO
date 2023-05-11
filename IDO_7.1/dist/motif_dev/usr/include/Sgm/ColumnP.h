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

#ifndef _SgColumnP_h
#define _SgColumnP_h

#include <Xm/Xm.h>
#include <Xm/BulletinBP.h>
#include "Column.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _SgColumnConstraintPart
{
    int         row;
    XmString    label;
    Pixmap      pixmap;
    

#ifdef sgi
/* Allow future extensions without breaking shared library compatibility */
   caddr_t _SG_vendorExtension;
#endif /* sgi */
} SgColumnConstraintPart, *SgColumnConstraint;

typedef struct _SgColumnConstraintRec
{
   XmManagerConstraintPart       manager;
   SgColumnConstraintPart       column;
} SgColumnConstraintRec, *SgColumnConstraintPtr;


/*  Column class structure  */

typedef struct _SgColumnClassPart
{
   int foo;	/*  No new fields needed  */
#ifdef sgi
/* Allow future extensions without breaking shared library compatibility */
   caddr_t _SG_vendorExtension;
#endif /* sgi */
} SgColumnClassPart;


/*  Full class record declaration for form class  */

typedef struct _SgColumnClassRec
{
   CoreClassPart             core_class;
   CompositeClassPart        composite_class;
   ConstraintClassPart       constraint_class;
   XmManagerClassPart        manager_class;
   XmBulletinBoardClassPart  bulletin_board_class;
   SgColumnClassPart         column_class;
} SgColumnClassRec;

externalref SgColumnClassRec sgColumnClassRec;

/*  The Column instance record  */

typedef struct _SgColumnPart
{
    Dimension      horizontal_spacing;
    Dimension      vertical_spacing;
    unsigned char  caption_vertical_alignment;
    unsigned char  caption_horizontal_alignment;
    unsigned char  child_horizontal_alignment;        
    XmFontList     font_list;
    GC             gc;
    Boolean        resize;

#ifdef sgi
/* Allow future extensions without breaking shared library compatibility */
   caddr_t _SG_vendorExtension;
#endif /* sgi */
} SgColumnPart;

/*  Full instance record declaration  */

typedef struct _SgColumnRec
{
   CorePart	        core;
   CompositePart        composite;
   ConstraintPart       constraint;
   XmManagerPart        manager;
   XmBulletinBoardPart  bulletin_board;
   SgColumnPart         column;
} SgColumnRec;

/********    Private Function Declarations    ********/
#ifdef _NO_PROTO

#else

#endif /* _NO_PROTO */
/********    End Private Function Declarations    ********/

#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* _SgColumnP_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */
