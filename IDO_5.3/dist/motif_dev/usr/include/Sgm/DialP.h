/*
 * Modified from -- Dial Widget By Doug Young: 
 * XWindow Programming and Applications with Xt
 *
 * DialP.h : private header file.
 */
#ifndef DIAL_PH
#define DIAL_PH

#include <Xm/XmP.h>
#include <Xm/PrimitiveP.h>
#include <Sgm/Dial.h>

typedef struct _SgDialClassPart{
   int ignore;
} SgDialClassPart;

typedef struct _SgDialClassRec {
   CoreClassPart	core_class;
   XmPrimitiveClassPart primitive_class;
   SgDialClassPart	dial_class;
} SgDialClassRec;

extern SgDialClassRec  SgdialClassRec;

#define MAXDIALSEGMENTS 360

/* Private defines */
#define HLPTS 6
static XPoint ACCENT[] = {
	{-11,0}, {-10,-1}, {-9,-1}, {-9,1}, {-10,1}, {-10,0}
};
/* End private defines */

/* SgDial instance record */
typedef struct _SgDialPart {
   Pixel	indicator_color;
   Pixel	foreground;
   int		minimum;
   int		maximum;
   int		start;
   int		end;
   int		markers;
   Dimension	marker_length;
   Position	position;
   Position	indicator_x;
   Position	indicator_y;
   Position	center_x;
   Position	center_y;
   Position	inner_diam;
   Position	outer_diam;
   GC		dial_GC;
   GC		indicator_GC;
   GC		inverse_GC;
   XPoint	segments[MAXDIALSEGMENTS];
   XtCallbackList	select;
   XtCallbackList	value_changed;
   Position     pre_drag_position;
   Position     Ilength;
   double       Gangle;
/* Resource for "knob" visual added. */
   SgDialVisual dialVisual;
   GC           knob_GC1;
   GC           knob_GC2;
   GC           knob_GC3;
   shaderptr    shader;
   XPoint	knob_pts[5];
   XPoint	old_knob_pts[5];
   XPoint	knob_accent_pts[HLPTS];
   Boolean      located;
   Boolean      dragging;
} SgDialPart;


typedef struct _SgDialRec {
   CorePart		core;
   XmPrimitivePart  	prim;
   SgDialPart		dial;
} SgDialRec;

#endif /* DIAL_PH */
