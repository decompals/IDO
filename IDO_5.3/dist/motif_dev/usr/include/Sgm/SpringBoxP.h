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
#ifndef _SgSpringBoxP_h
#define _SgSpringBoxP_h


#include "SpringBox.h"
#include <Xm/BulletinBP.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _SgSpringBoxConstraintPart
{
    int  top, bottom, left, right;
    int  vert, horiz;
    Dimension  prefH, prefW;

#ifdef sgi
/* Allow future extensions without breaking shared library compatibility */
   caddr_t _SG_vendorExtension;
#endif /* sgi */
} SgSpringBoxConstraintPart, * SgSpringBoxConstraint;

typedef struct _SgSpringBoxConstraintRec
{
   XmManagerConstraintPart     manager;
   SgSpringBoxConstraintPart   spring_box;
} SgSpringBoxConstraintRec, * SgSpringBoxConstraintPtr;


/*  SpringBox class structure  */

typedef struct _SgSpringBoxClassPart
{
   int foo;	/*  No new fields needed  */
#ifdef sgi
/* Allow future extensions without breaking shared library compatibility */
   caddr_t _SG_vendorExtension;
#endif /* sgi */
} SgSpringBoxClassPart;


/*  Full class record declaration for form class  */

typedef struct _SgSpringBoxClassRec
{
   CoreClassPart             core_class;
   CompositeClassPart        composite_class;
   ConstraintClassPart       constraint_class;
   XmManagerClassPart        manager_class;
   XmBulletinBoardClassPart  bulletin_board_class;
   SgSpringBoxClassPart      spring_box_class;
} SgSpringBoxClassRec;

externalref SgSpringBoxClassRec sgSpringBoxClassRec;

/*  The SpringBox instance record  */

typedef struct _SgSpringBoxPart
{
    unsigned char orientation;
    Dimension  min_spacing;

#ifdef sgi
/* Allow future extensions without breaking shared library compatibility */
   caddr_t _SG_vendorExtension;
#endif /* sgi */
} SgSpringBoxPart;

/*  Full instance record declaration  */

typedef struct _SgSpringBoxRec
{
   CorePart	        core;
   CompositePart        composite;
   ConstraintPart       constraint;
   XmManagerPart        manager;
   XmBulletinBoardPart  bulletin_board;
   SgSpringBoxPart      spring_box;
} SgSpringBoxRec;

/********    Private Function Declarations    ********/
#ifdef _NO_PROTO

#else

#endif /* _NO_PROTO */
/********    End Private Function Declarations    ********/

#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* _SgSpringBoxP_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */
