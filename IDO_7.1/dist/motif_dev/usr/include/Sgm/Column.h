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

#ifndef _SgColumn_h
#define _SgColumn_h


#ifdef __cplusplus
extern "C" {
#endif

/*  Column Widget  */

externalref WidgetClass sgColumnWidgetClass;

typedef struct _SgColumnClassRec * SgColumnWidgetClass;
typedef struct _SgColumnRec      * SgColumnWidget;

/* ifndef for Fast Subclassing  */

#ifndef SgIsColumn
#define SgIsColumn(w)	XtIsSubclass(w, sgColumnWidgetClass)
#endif  /* SgIsColumn */

/********    Public Function Declarations    ********/
#ifdef _NO_PROTO

extern Widget SgCreateColumn() ;
#else
extern Widget SgCreateColumn( 
                        Widget parent,
                        char *name,
                        ArgList arglist,
                        Cardinal argcount) ;

#endif /* _NO_PROTO */
/********    End Public Function Declarations    ********/


#define SgNcaptionLabelString                      "captionLabelString"

#define SgNcaptionHorizAlignment      "captionHorizAlignment"
#define SgNcaptionVertAlignment       "captionVertAlignment"
#define SgNcolumnHorizAlignment       "columnHorizAlignment"
#define SgRColumnAlignment            "ColumnAlignment"
#define SgRCaptionAlignment           "CaptionAlignment"

enum{	SgALIGNMENT_JUSTIFY=3
	} ;

enum{	SgALIGNMENT_TOP, 
	SgALIGNMENT_BOTTOM=2
	} ;

#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif



#endif /* _SgColumn_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */
