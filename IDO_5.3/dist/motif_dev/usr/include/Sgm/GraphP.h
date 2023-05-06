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
/* -*-C-*-
*******************************************************************************
*
*  File:         GraphP.h
*  Description:  Private header file for GraphWidget.
*  Author:       Luis Miguel (luis@postgres.berkeley.edu)
*  
*  $Author: dyoung $
*  $Source: /proj/irix5.3/isms/motif/src/lib/Sgm/RCS/GraphP.h,v $
*  $Revision: 1.3 $
*  $Date: 1993/09/23 20:04:27 $
*
*******************************************************************************
*/

#ifndef _SgGraphP_h
#define _SgGraphP_h

#include "Graph.h"
#include "ArcP.h"
#include "Arc.h"


/* useful macros */
#ifndef MAX
#define MAX(a,b)        ((a) > (b) ? (a) : (b))
#endif

#ifndef MIN
#define MIN(a,b)        ((a) < (b) ? (a) : (b))
#endif

/* some constants */

#define  LARGE_NUM           987654321

typedef enum  {NORMAL, 
		   NODE_INDICATED, 
		   NODE_INDICATED_PENDING_CANCEL, 
		   NODES_INDICATED, 
		   NODES_INDICATED_PENDING_CANCEL, 
		   ARC_INDICATED, 
		   ARC_INDICATED_PENDING_CANCEL, 
		   ARCS_INDICATED, 
		   ARCS_INDICATED_PENDING_CANCEL, 
		   NODE_SELECTED_FOR_MOTION, 
		   ARC_SELECTED_FOR_MOTION,
		   MULTIPLE_NODES_SELECTED_FOR_MOTION, 
		   NODE_SELECTED,
		   NODES_SELECTED, 
		   ARC_SELECTED, 
		   ARCS_SELECTED,
		   POSSIBLE_DOUBLE_CLICK,
		   SUBGRAPH_INDICATED,
		   SUBGRAPH_INDICATED_PENDING_CANCEL,
		   MOVING_ARC, 
		   MOVING_ARCS, 
		   MOVING_NODE, 
		   MOVING_NODES, 
		   ADDING_NODE, 
		   ADDING_ARC,
		   ADDING_ARC_IN_PARENT,
		   REGION_INDICATED} ActionMode;

/* The node structure "surrounding" widgets */

typedef struct _node *NodePtr;
typedef NodePtr *NList;

typedef struct nodelist {
    NList      nodes;
    int        n_nodes;
    int        n_slots;
} NodeList;


typedef struct _node {
    /* all node and arc widgets */
    Widget      widget;
    ArcList     from_arcs;
    ArcList     to_arcs;
    
    /* the structure of the graph */
    NodeList    parents;
    NodeList    kids;
    int         visited;  /* the visited flag can contain a Boolean or count */
    
    /* tree processing needs these */
    int         level;
    NodePtr     tree_parent;
    NodeList    tree_kids;
    
    
} Node;

/* New fields for the GraphWidget widget class record */

typedef struct _SgGraphClassPart {
    caddr_t extension;
#ifdef __sgi /* Allow future extensions without breaking shared library compatibility */
	caddr_t _SG_vendorExtension;
#endif /* __sgi */
} SgGraphClassPart;

/* Full Class record declaration */

typedef struct _SgGraphClassRec {
    CoreClassPart	core_class;
    CompositeClassPart  composite_class;
    ConstraintClassPart	constraint_class;
    XmManagerClassPart  manager_class;
    SgGraphClassPart	graph_class;
} SgGraphClassRec;

externalref SgGraphClassRec sgGraphClassRec;

/* New Fields for the GraphWidget  widget record */


typedef struct {

    Window         bufferWindow;
    Pixmap         bufferPixmap;
    int            bufferWindowCount;
    ActionMode     current_action;
    Time           last_button_time;
    int            double_click_interval;
    int            scroll_increment;
    Cursor         indicate_child_cursor;
    Cursor         indicate_parent_cursor;
    Cursor         ptr_cursor;
    Cursor         motion_cursor;
    Cursor         indicate_cursor;
    Cursor         high_right_cursor;
    Cursor         high_left_cursor;
    Cursor         low_right_cursor;
    Cursor         low_left_cursor;
    Pixel          foreground;
    int            start_x, start_y, end_x, end_y;
    int            delta_x, delta_y;
    int            pick_delta;
    int            child_spacing;
    int            sibling_spacing;
    int            original_child_spacing;
    int            original_sibling_spacing;
    GC             gc;
    GC             xorgc;
    GC             highlightgc;
    GC             overview_top_gc;
    GC             overview_bottom_gc;
    GC             overview_markgc;
    GC             overview_arc_gc;
    GC             overview_node_gc;
    Boolean        show_overview_arcs;
    Boolean        edit_mode;
    Boolean        add_mode;
    Boolean        nodes_movable;
    Boolean        arcs_selectable;
    Boolean        arcs_editable;
    autoLayoutType auto_layout_mode;
    Atom           layoutKey;
    Boolean        siblings_visible;
    Boolean        re_layout;
    Boolean	   re_orient;
    Boolean	   layed_out;
    Boolean        batch_drawing_mode;
    unsigned char  layoutStyle;
    unsigned char  direction;
    unsigned char  arc_draw_mode;
    WidgetList     arcs;
    ArcList        selected_arcs;
    NodeList       selected_nodes;
    NodeList       user_roots;
    int            n_arcs;
    int            n_arc_slots;
    NodePtr        root;
    NodePtr        current_node;
    NodePtr        current_subgraph;
    Widget         current_arc;
    Widget         highlighted_arc;
    Widget         indicated_widget;
    Widget         overview_widget;
    Boolean        overview_in_motion;
    Pixel          arc_highlight_color;
    Pixel          overview_arc_color;
    Pixel          overview_node_color;
    Pixel          overview_viewport_top_color;
    Pixel          overview_viewport_bottom_color;
    XRectangle     overview_rect;
    float          x_scale_factor;
    float          y_scale_factor;
    Boolean        skew_overview;
    Boolean        allow_grandchild_input;
    int            default_widget_class;
    Boolean allow_multiple_selections;
    void  (*layout_proc)();
    unsigned char  arc_style;

    Widget butterfly_widget;
    XtCallbackList graph_too_big_callback;      
    XtCallbackList user_button_callback;      
    XtCallbackList graph_changed;      
    XtCallbackList new_arc;  
    XtCallbackList new_node;  
    XtCallbackList node_moved;
    XtCallbackList arc_moved;
    XtCallbackList double_click;  
    XtCallbackList select_node;  
    XtCallbackList deselect;  
    XtCallbackList select_arc;  
    XtCallbackList select_subgraph;  
/*    XtCallbackList constraint_free;  
    XtCallbackList constraint_init;  
    XtCallbackList arm_arc_callback;
    XtCallbackList activate_arc_callback;
    XtCallbackList disarm_arc_callback;
*/
    
    Boolean         is_scrolled;
    Boolean         show_crossing_arcs;
    Dimension       clip_width;
    Dimension       clip_height;
    Dimension       scrollbar_width;
    int         adjust_size;

#ifdef __sgi /* Allow future extensions without breaking shared library compatibility */
	caddr_t _SG_vendorExtension;
#endif /* __sgi */
} SgGraphPart;


/**************************************************************************
 *
 * Full instance record declaration
 *
 **************************************************************************/

typedef struct _SgGraphRec {
    CorePart	    core;
    CompositePart   composite;
    ConstraintPart  constraint;
    XmManagerPart   manager;
    SgGraphPart     graph;
}  SgGraphRec;

/* Graph constraint record */

typedef struct _SgGraphConstraintsRec {
    XmManagerConstraintPart manager;
    int           old_mask;
    NodePtr       node;    
    int           delta_x;
    int           delta_y;
    int           old_x;
    int           old_y;
    Pixel         overview_highlight_color;
    caddr_t       extension;
} SgGraphConstraintsRec, *SgGraphConstraintPtr;



#define NODEPTR(widget) ( (widget) ?                                       \
			 ( XtIsWidget(widget)  ?		      \
			  ((SgGraphConstraintsRec *) ((Widget)(widget))->core.constraints)->node   :     \
			  ((SgGraphConstraintsRec *) ((Object)(widget))->object.constraints)->node)  :  \
			 NULL)


#define CONSTRAINTREC(widget) ( (widget) ?                                       \
			       ( XtIsWidget(widget)  ?		      \
				((SgGraphConstraintsRec *) ((Widget)(widget))->core.constraints)   :     \
				((SgGraphConstraintsRec *) ((Object)(widget))->object.constraints))  :  \
			       NULL)



#endif /* _SgGraphP_h  */
/* DON"T ADD ANYTHING AFTER THIS endif */


