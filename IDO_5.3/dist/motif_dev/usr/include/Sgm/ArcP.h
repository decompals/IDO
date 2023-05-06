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
#ifndef ARCP_H
#define ARCP_H
    
#include <Xm/Xm.h>
    
typedef struct arclist {
    WidgetList  arcs;
    int         n_arcs;
    int         n_slots;
} ArcList;

typedef void (*DispatchProc) (  struct _SgArcRec *arc, XEvent     *event,   Mask        event_mask);

typedef struct _SgArcClassPart{
    DispatchProc         input_dispatch;
#ifdef __sgi /* Allow future extensions without breaking shared library compatibility */
	caddr_t _SG_vendorExtension;
#endif /* __sgi */
} SgArcClassPart;


typedef struct _SgArcClassRec {
    CoreClassPart	core_class;
    SgArcClassPart	arc_class;
} SgArcClassRec;

externalref SgArcClassRec sgArcClassRec;

typedef struct _SgArcPart {
    int          width, dashes, dash_offset;
    unsigned char direction, style, cap_style;
    Atom          attribute;

    Widget       to;
    Widget       from;

    short   to_side, from_side;
    short    to_pos, from_pos;

    Pixel	 foreground;
    Boolean      highlight;

    Boolean      visible;

    XmFontList	 font;  
    _XmString    label;
    Dimension    labelwidth, labelheight;
    Boolean      map_name;

    ArcList      *siblings;
    GC           gc;
    GC           cleargc;
    XtCallbackList arm_callback;
    XtCallbackList activate_callback;
    XtCallbackList disarm_callback;

    XtPointer      user_data;
    Boolean        armed;
    int            rank;
#ifdef __sgi /* Allow future extensions without breaking shared library compatibility */
	caddr_t _SG_vendorExtension;
#endif /* __sgi */
} SgArcPart;

typedef struct _SgArcRec {
    CorePart    core;
    SgArcPart	arc;
} SgArcRec;

#endif



