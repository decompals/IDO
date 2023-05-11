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

#ifndef _SgGridP_h
#define _SgGridP_h

#include "Grid.h"
#include <Xm/BulletinBP.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _SgGridConstraintPart
{
    int         gravity;
    Boolean     resizable_horiz, resizable_vert;
    int         row, column;
    Dimension   prefH, prefW;
    int         cursorX, cursorY;
    Boolean     moveOnly;
    Boolean     builderAction;

#ifdef sgi
/* Allow future extensions without breaking shared library compatibility */
   caddr_t _SG_vendorExtension;
#endif /* sgi */
} SgGridConstraintPart, *SgGridConstraint;

typedef struct _SgGridConstraintRec
{
   XmManagerConstraintPart       manager;
   SgGridConstraintPart          grid;
} SgGridConstraintRec, *SgGridConstraintPtr;


/*  Grid class structure  */

typedef struct _SgGridClassPart
{
   int foo;	/*  No new fields needed  */
#ifdef sgi
/* Allow future extensions without breaking shared library compatibility */
   caddr_t _SG_vendorExtension;
#endif /* sgi */
} SgGridClassPart;


/*  Full class record declaration for form class  */

typedef struct _SgGridClassRec
{
   CoreClassPart             core_class;
   CompositeClassPart        composite_class;
   ConstraintClassPart       constraint_class;
   XmManagerClassPart        manager_class;
   XmBulletinBoardClassPart  bulletin_board_class;
   SgGridClassPart    grid_class;
} SgGridClassRec;

externalref SgGridClassRec sgGridClassRec;

typedef struct _RowColumnInfo {
    int        spring_constant;
    Dimension  margin;
    Dimension  size;
} RowColumnInfo;

/*  The Grid instance record  */

typedef struct _SgGridPart
{
    Boolean        buildMode;
    Boolean        automatic;
    Dimension      default_spacing;
    int            num_rows;
    int            num_columns;
    RowColumnInfo *row_info;
    RowColumnInfo *col_info;
    WidgetList     matrix;
    Boolean        show_grid;
    String         rowString;
    String         colString;  
    String         rowResizeString;  
    String         colResizeString;  

#ifdef sgi
/* Allow future extensions without breaking shared library compatibility */
   caddr_t _SG_vendorExtension;
#endif /* sgi */
} SgGridPart;

/*  Full instance record declaration  */

typedef struct _SgGridRec
{
   CorePart	        core;
   CompositePart        composite;
   ConstraintPart       constraint;
   XmManagerPart        manager;
   XmBulletinBoardPart  bulletin_board;
   SgGridPart           grid;
} SgGridRec;

/********    Private Function Declarations    ********/
#ifdef _NO_PROTO

#else

#endif /* _NO_PROTO */
/********    End Private Function Declarations    ********/

#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* _SgGridP_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */
