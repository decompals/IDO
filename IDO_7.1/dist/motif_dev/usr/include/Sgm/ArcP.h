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
#include <Sgm/Arc.h>
    
typedef struct arclist {
    WidgetList  arcs;
    int         n_arcs;
    int         n_slots;
} ArcList;

#ifdef NEVER
I get this when compiling Graph.o:
"./ArcP.h", line 28: warning(1204): declaration is not visible outside of
          function
  typedef void (*DispatchProc) (  struct _SgArcRec *arc, XEvent     *event,
 Mask        event_mask);
                                         ^
#endif /* NEVER */
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


/* externs, mainly used in Graph.c.  Definitely not public. */
void _SgArcGetPoints      (Widget arc,
		           int f_x, int f_y, int f_width, int f_height,
		           int t_x, int t_y, int t_width, int t_height,
		           int *X1, int *Y1, int *X2, int *Y2);
Region _SgArcAddRegionToRegion(Region region1, Region region2);
int _SgArc_sibling_rank   (Widget arc);
void _SgArcEraseArc       (Widget arc);
void _SgArcUnhighlightArc (Widget w);
void _SgArcHighlightArc   (Widget w);
Region SgArcGetRegion     (Widget w);

#endif /* ARCP_H */
