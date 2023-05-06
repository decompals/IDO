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
#ifndef _XmHPanedWP_h
#define _XmHPanedWP_h

#include "HPanedW.h"
#include <Xm/XmP.h>

/* New fields for the HorzPanedWindow widget class record */

typedef struct _SgHorzPanedWindowClassPart
{
    caddr_t extension;
#ifdef sgi
/* Allow future extensions without breaking shared library compatibility */
    caddr_t _SG_vendorExtension;
#endif /* sgi */
} SgHorzPanedWindowClassPart;


/* Full Class record declaration */

typedef struct _SgHorzPanedWindowClassRec {
    CoreClassPart       core_class;
    CompositeClassPart  composite_class;
    ConstraintClassPart constraint_class;
    XmManagerClassPart  manager_class;
    SgHorzPanedWindowClassPart     paned_window_class;
} SgHorzPanedWindowClassRec;

externalref SgHorzPanedWindowClassRec sgHorzPanedWindowClassRec;


/* HorzPanedWindow constraint record */

typedef struct _SgHorzPanedWindowConstraintPart {
    int		position;	/* position location in HorzPanedWindow */
    int		dwidth;		/* Desired width */
    Position	dx;		/* Desired Location */
    Position	olddx;		/* The last value of dx. */
    Dimension	min;		/* Minimum width */
    Dimension	max;		/* Maximum width */
    Boolean     isPane;         /* true if constraint of pane, false if
				   constraint of sash */
    Boolean	allow_resize;	/* TRUE iff child resize requests are ok */
    Boolean	skip_adjust;	/* TRUE iff child's height should not be */
				/* changed without explicit user action. */
    Widget	sash;		/* The sash for this child */
    Widget	separator;	/* The separator for this child */
    short       position_index; /* new 1.2 positionIndex resource */
#ifdef sgi
/* Allow future extensions without breaking shared library compatibility */
    caddr_t	_SG_vendorExtension;
#endif /* sgi */
} SgHorzPanedWindowConstraintPart;

typedef struct _SgHorzPanedWindowConstraintRec 
{
    XmManagerConstraintPart manager;
    SgHorzPanedWindowConstraintPart  panedw;
} SgHorzPanedWindowConstraintRec, * SgHorzPanedWindowConstraintPtr;


/* New Fields for the HorzPanedWindow widget record */

typedef struct {
    /* resources */
    Boolean     refiguremode;        /* Whether to refigure changes right now */
    Boolean	separator_on;	     /* make separator visible */

    Dimension  	margin_width;	     /* space between right and left edges of
					HorzPanedWindow window and it's children */
    Dimension  	margin_height;	     /* space between top and bottom edges of
					HorzPanedWindow window and it's children */
    Dimension   spacing;             /* whitespace between panes
				        around window, else leave none */
    /* sash modifying resources */
    Dimension	sash_width;	       /* Modify sash width */
    Dimension	sash_height;	       /* Modify sash height */
    Dimension   sash_shadow_thickness; /* Modify sash shadow_thickness */

    Position    sash_indent;           /* Location of sashs (offset
                                          from bottom margin)	*/
    Position	sash_offset;		/* offset of sashes from sep */
    /* private */
    int         startx;               /* mouse origin when adjusting */

    short	increment_count;      /* Sash increment count */
    short       pane_count;           /* number of managed panes */
    short       num_slots;	      /* number of avail. slots for children*/
    short       num_managed_children; /* holds number of managed children */

    Boolean     recursively_called;   /* For changed managed */

    Boolean     resize_at_realize;    /* For realize if GeometryNo condition */

    SgHorzPanedWindowConstraintPtr top_pane;    /* pane closest to 0 index */
    SgHorzPanedWindowConstraintPtr bottom_pane; /* pane farthest away from 0 index*/

    GC          flipgc;               /* GC to use when animating borders */
    WidgetList  managed_children;     /* keep track of managed children */
#ifdef sgi
/* Allow future extensions without breaking shared library compatibility */
     struct _SG_PanedWExtension *_SG_vendorExtension;
#endif /* sgi */
} SgHorzPanedWindowPart;

#ifdef sgi
/* Cabletron bug fix -- segv if timer goes off after widget destroyed */
    struct _SG_PanedWExtension {
        XtIntervalId   ztimer;          /* Save so it can be destroyed later */
    };
#endif /* sgi */

/**************************************************************************
 *
 * Full instance record declaration
 *
 **************************************************************************/

typedef struct _SgHorzPanedWindowRec {
    CorePart       core;
    CompositePart  composite;
    ConstraintPart constraint;
    XmManagerPart  manager;
    SgHorzPanedWindowPart   paned_window;
} SgHorzPanedWindowRec;

#endif /* _XmHPanedWP_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */
