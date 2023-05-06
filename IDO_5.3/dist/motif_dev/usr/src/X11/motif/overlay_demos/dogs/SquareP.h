/* 
 * (c) Copyright 1989, 1990, 1991, 1992, 1993 OPEN SOFTWARE FOUNDATION, INC. 
 * ALL RIGHTS RESERVED 
*/ 
/* 
 * Motif Release 1.2.3
*/ 
/*   $RCSfile: SquareP.h,v $ $Revision: 1.3 $ $Date: 1993/10/23 21:06:16 $ */


#ifndef _SquareP_h
#define _SquareP_h

#include "Square.h"
#include <Xm/BulletinBP.h>

#define SquareIndex (XmBulletinBIndex + 1)

typedef struct _SquareClassPart
{
   XtPointer reserved;
} SquareClassPart;


typedef struct _SquareClassRec
{
   CoreClassPart       core_class;
   CompositeClassPart  composite_class;
   ConstraintClassPart constraint_class;
   XmManagerClassPart  manager_class;
   XmBulletinBoardClassPart  bulletin_board_class;
   SquareClassPart     square_class;
} SquareClassRec;

externalref SquareClassRec squareClassRec;

typedef struct _SquarePart
{
    int major_dimension;
} SquarePart;


/*  Full instance record declaration  */

typedef struct _SquareRec
{
   CorePart	  core;
   CompositePart  composite;
   ConstraintPart constraint;
   XmManagerPart  manager;
   XmBulletinBoardPart  bulletin_board;
   SquarePart     square;
} SquareRec;

typedef struct _SquareConstraintPart
{
   Boolean make_square;
} SquareConstraintPart;

typedef struct _SquareConstraintRec
{
   XmManagerConstraintPart manager;
   SquareConstraintPart    square;
} SquareConstraintRec, *SquareConstraint;


#endif /* _SquareP_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */
