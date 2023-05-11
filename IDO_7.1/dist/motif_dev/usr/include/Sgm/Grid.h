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

#ifndef _SgGrid_h
#define _SgGrid_h


#include <Xm/Xm.h>

#ifdef __cplusplus
extern "C" {
#endif

/*  Grid Widget  */

externalref WidgetClass sgGridWidgetClass;

typedef struct _SgGridClassRec * SgGridWidgetClass;
typedef struct _SgGridRec      * SgGridWidget;

/* ifndef for Fast Subclassing  */

#ifndef SgIsGrid
#define SgIsGrid(w)	XtIsSubclass(w, sgGridWidgetClass)
#endif  /* SgIsGrid */

/********    Public Function Declarations    ********/
#ifdef _NO_PROTO

extern Widget SgCreateGrid() ;
extern Widget SgCreateGridDialog() ;
extern void SgGridSetColumnResizability();
extern void SgGridSetRowResizability();
extern void SgGridSetColumnMargin();
extern void SgGridSetRowMargin();
#else
extern void SgGridSetColumnResizability(
				  Widget widget,
				  int col,
				  int resizability);
extern void SgGridSetRowResizability(
			       Widget widget,
			       int row,
			       int resizability);
extern void SgGridSetColumnMargin(Widget widget,
				  int col,
				  Dimension margin);
extern void SgGridSetRowMargin(
			       Widget widget,
			       int row,
			       Dimension margin);
extern Widget SgCreateGrid( 
                        Widget parent,
                        char *name,
                        ArgList arglist,
                        Cardinal argcount) ;
extern Widget SgCreateGridDialog( 
                        Widget parent,
                        char *name,
                        ArgList arglist,
                        Cardinal argcount) ;

#endif /* _NO_PROTO */
/********    End Public Function Declarations    ********/

#define XmNnumRows             "numRows"
#define XmCNumRows             "NumRows"
#define XmNrow                 "row"
#define XmCRow                 "Row"
#define XmNcolumn              "column"
#define XmCColumn              "Column"

#define XmNresizeVertical     "resizeVertical"
#define XmNresizeHorizontal   "resizeHorizontal"
#define XmCResizeVertical     "ResizeVertical"
#define XmCResizeHorizontal   "ResizeHorizontal"

#define XmNshowGrid     "showGrid"
#define XmCShowGrid     "ShowGrid"

#define XmCGravity   "Gravity"
#define XmNgravity   "gravity"
#define XmRGravity   "Gravity"

#define XmNautoLayout   "autoLayout"
#define XmCAutoLayout   "AutoLayout"

#define XmNdefaultSpacing   "defaultSpacing"
#define XmCDefaultSpacing   "DefaultSpacing"
#define XmNrowResizable   "rowResizable"
#define XmCRowResizable   "RowResizable"
#define XmNcolumnResizable   "columnResizable"
#define XmCColumnResizable   "ColumnResizable"
#define XmNrowInfo   "rowInfo"
#define XmNcolInfo   "colInfo"

#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif


#endif /* _SgGrid_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */
