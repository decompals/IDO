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
#ifdef REV_INFO
#ifndef lint
static char SCCSID[] = "OSF/Motif: @(#)ScrollBar.h	4.8 92/01/03";
#endif /* lint */
#endif /* REV_INFO */
/******************************************************************************
*******************************************************************************
*
*  (c) Copyright 1989, 1990, 1991, 1992 OPEN SOFTWARE FOUNDATION, INC.
*  (c) Copyright 1989, DIGITAL EQUIPMENT CORPORATION, MAYNARD, MASS.
*  (c) Copyright 1987, 1988, 1989, 1990, 1991, 1992 HEWLETT-PACKARD COMPANY
*  ALL RIGHTS RESERVED
*  
*  	THIS SOFTWARE IS FURNISHED UNDER A LICENSE AND MAY BE USED
*  AND COPIED ONLY IN ACCORDANCE WITH THE TERMS OF SUCH LICENSE AND
*  WITH THE INCLUSION OF THE ABOVE COPYRIGHT NOTICE.  THIS SOFTWARE OR
*  ANY OTHER COPIES THEREOF MAY NOT BE PROVIDED OR OTHERWISE MADE
*  AVAILABLE TO ANY OTHER PERSON.  NO TITLE TO AND OWNERSHIP OF THE
*  SOFTWARE IS HEREBY TRANSFERRED.
*  
*  	THE INFORMATION IN THIS SOFTWARE IS SUBJECT TO CHANGE WITHOUT
*  NOTICE AND SHOULD NOT BE CONSTRUED AS A COMMITMENT BY OPEN SOFTWARE
*  FOUNDATION, INC. OR ITS THIRD PARTY SUPPLIERS  
*  
*  	OPEN SOFTWARE FOUNDATION, INC. AND ITS THIRD PARTY SUPPLIERS,
*  ASSUME NO RESPONSIBILITY FOR THE USE OR INABILITY TO USE ANY OF ITS
*  SOFTWARE .   OSF SOFTWARE IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY
*  KIND, AND OSF EXPRESSLY DISCLAIMS ALL IMPLIED WARRANTIES, INCLUDING
*  BUT NOT LIMITED TO THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
*  FITNESS FOR A PARTICULAR PURPOSE.
*  
*  Notice:  Notwithstanding any other lease or license that may pertain to,
*  or accompany the delivery of, this computer software, the rights of the
*  Government regarding its use, reproduction and disclosure are as set
*  forth in Section 52.227-19 of the FARS Computer Software-Restricted
*  Rights clause.
*  
*  (c) Copyright 1989, 1990, 1991, 1992 Open Software Foundation, Inc.  Unpublished - all
*  rights reserved under the Copyright laws of the United States.
*  
*  RESTRICTED RIGHTS NOTICE:  Use, duplication, or disclosure by the
*  Government is subject to the restrictions as set forth in subparagraph
*  (c)(1)(ii) of the Rights in Technical Data and Computer Software clause
*  at DFARS 52.227-7013.
*  
*  Open Software Foundation, Inc.
*  11 Cambridge Center
*  Cambridge, MA   02142
*  (617)621-8700
*  
*  RESTRICTED RIGHTS LEGEND:  This computer software is submitted with
*  "restricted rights."  Use, duplication or disclosure is subject to the
*  restrictions as set forth in NASA FAR SUP 18-52.227-79 (April 1985)
*  "Commercial Computer Software- Restricted Rights (April 1985)."  Open
*  Software Foundation, Inc., 11 Cambridge Center, Cambridge, MA  02142.  If
*  the contract contains the Clause at 18-52.227-74 "Rights in Data General"
*  then the "Alternate III" clause applies.
*  
*  (c) Copyright 1989, 1990, 1991, 1992 Open Software Foundation, Inc.
*  ALL RIGHTS RESERVED 
*  
*  
* Open Software Foundation is a trademark of The Open Software Foundation, Inc.
* OSF is a trademark of Open Software Foundation, Inc.
* OSF/Motif is a trademark of Open Software Foundation, Inc.
* Motif is a trademark of Open Software Foundation, Inc.
* DEC is a registered trademark of Digital Equipment Corporation
* DIGITAL is a registered trademark of Digital Equipment Corporation
* X Window System is a trademark of the Massachusetts Institute of Technology
*
*******************************************************************************
******************************************************************************/
#ifndef _SgScrollBar_h
#define _SgScrollBar_h

#include <Xm/Xm.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct
{
   int reason;
   XEvent * event;
   int value;
   int pixel;
} SgScrollBarCallbackStruct;



/*  ScrollBar Widget  */

externalref WidgetClass sgScrollBarWidgetClass;

typedef struct _SgScrollBarClassRec * SgScrollBarWidgetClass;
typedef struct _SgScrollBarRec      * SgScrollBarWidget;

/* ifndef for Fast Subclassing  */

#ifndef SgIsScrollBar
#define SgIsScrollBar(w)	XtIsSubclass(w, sgScrollBarWidgetClass)
#endif  /* XmIsScrollBar */
#define ASBCTroughPixmapCallback "TroughPixmapCallback"
#define ASBNtroughPixmapCallback "troughPixmapCallback"
#define ASBCR_NEED_TROUGH_PIXMAP 99

typedef struct
{
  int reason;
  XEvent * event;
  int width, height, depth, maximum;
  Pixmap pixmap;
} ASBCallbackStruct;


/********    Public Function Declarations    ********/
#ifdef _NO_PROTO

extern Widget SgCreateScrollBar() ;
extern void SgScrollBarGetValues() ;
extern void SgScrollBarSetValues() ;
extern void SgScrollBarRedisplay ();
#else
extern void SgScrollBarRedisplay (Widget w);
extern Widget SgCreateScrollBar( 
                        Widget parent,
                        char *name,
                        ArgList arglist,
                        Cardinal argcount) ;
extern void SgScrollBarGetValues( 
                        Widget w,
                        int *value,
                        int *slider_size,
                        int *increment,
                        int *page_increment) ;
extern void SgScrollBarSetValues( 
                        Widget w,
                        int value,
                        int slider_size,
                        int increment,
                        int page_increment,
#if NeedWidePrototypes
                        int notify) ;
#else
                        Boolean notify) ;
#endif /* NeedWidePrototypes */

#endif /* _NO_PROTO */
/********    End Public Function Declarations    ********/


#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* _SgScrollBar_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */
