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
#ifndef _SgRubberBoardP_h
#define _SgRubberBoardP_h


#include "RubberBoard.h"
#include <Xm/BulletinBP.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _SgRubberBoardConstraintPart
{
    Position initialX, initialY;
    Position finalX, finalY;

    Dimension initialWidth, initialHeight;
    Dimension finalWidth, finalHeight;
#ifdef sgi
/* Allow future extensions without breaking shared library compatibility */
   caddr_t _SG_vendorExtension;
#endif /* sgi */
} SgRubberBoardConstraintPart, * SgRubberBoardConstraint;

typedef struct _SgRubberBoardConstraintRec
{
   XmManagerConstraintPart     manager;
   SgRubberBoardConstraintPart   rubber;
} SgRubberBoardConstraintRec, * SgRubberBoardConstraintPtr;


/*  RubberBoard class structure  */

typedef struct _SgRubberBoardClassPart
{
   int foo;	/*  No new fields needed  */
#ifdef sgi
/* Allow future extensions without breaking shared library compatibility */
   caddr_t _SG_vendorExtension;
#endif /* sgi */
} SgRubberBoardClassPart;


/*  Full class record declaration for form class  */

typedef struct _SgRubberBoardClassRec
{
   CoreClassPart             core_class;
   CompositeClassPart        composite_class;
   ConstraintClassPart       constraint_class;
   XmManagerClassPart        manager_class;
   XmBulletinBoardClassPart  bulletin_board_class;
   SgRubberBoardClassPart      rubber_class;
} SgRubberBoardClassRec;

externalref SgRubberBoardClassRec sgRubberBoardClassRec;

/*  The RubberBoard instance record  */

typedef struct _SgRubberBoardPart
{
    Dimension initialWidth, initialHeight;
    Dimension finalWidth, finalHeight;

    Boolean setInitial, setFinal;

#ifdef sgi
/* Allow future extensions without breaking shared library compatibility */
   caddr_t _SG_vendorExtension;
#endif /* sgi */
} SgRubberBoardPart;

/*  Full instance record declaration  */

typedef struct _SgRubberBoardRec
{
   CorePart	        core;
   CompositePart        composite;
   ConstraintPart       constraint;
   XmManagerPart        manager;
   XmBulletinBoardPart  bulletin_board;
   SgRubberBoardPart      rubber;
} SgRubberBoardRec;

/********    Private Function Declarations    ********/
#ifdef _NO_PROTO

#else

#endif /* _NO_PROTO */
/********    End Private Function Declarations    ********/

#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* _SgRubberBoardP_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */
