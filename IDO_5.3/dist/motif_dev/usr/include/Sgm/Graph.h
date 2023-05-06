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
/************************************<+>*************************************
 ****************************************************************************
 **
 **   File:        Graph.h
 **
 **   Project:     Motif -- OSF X User Environment Widget Set
 **
 **   Description: Public include file for the XmLabel class
 **
 **
 **   (c) Copyright 1989 by Hewlett-Packard Company
 ****************************************************************************
 *************************************<+>************************************/

#ifndef _SgGraph_h
#define _SgGraph_h

#if defined(__cplusplus) || defined(c_plusplus)
extern "C" {
#endif

/* Class record constant */

externalref WidgetClass sgGraphWidgetClass;
typedef struct _SgGraphClassRec 		*SgGraphWidgetClass;
typedef struct _SgGraphRec      		*SgGraphWidget;

#ifndef SgIsGraph
#define SgIsGraph (w)	XtIsSubclass(w, sgGraphWidgetClass)
#endif


/******************* CONVENIENCE FUNCTIONS **********************/

/* creates and returns an instance of the graph widget */

#ifndef _NO_PROTO

extern Widget SgCreateGraph(Widget,String,ArgList,Cardinal);
extern Widget SgCreateManagedGraph (Widget,String,ArgList,Cardinal);
extern Widget SgCreateScrolledGraph(Widget,String,ArgList,Cardinal);
extern Widget SgCreateArc(Widget,String,ArgList,Cardinal);
extern Widget SgCreateAttachedArc(Widget,String,Widget,Widget,ArgList,Cardinal);

extern  void SgGraphCenterAroundWidget(Widget, Widget);
extern  void SgGraphMakeWidgetVisible(Widget, Widget);
extern  void SgGraphDestroyAllArcs(Widget);
extern  void SgGraphDestroyAllNodes(Widget);
extern  void SgGraphDestroySelectedArcsOrNodes(Widget);
extern  void SgGraphGetArcNodes(Widget, Widget, Widget *, Widget *);
extern  WidgetList SgGraphGetArcsBetweenNodes(Widget, Widget, Widget, int *);
extern  WidgetList SgGraphGetArcs(Widget, int *);
extern  WidgetList SgGraphGetNodes(Widget, int *);
extern  void SgGraphGetNodeArcs(Widget, Widget, WidgetList *, WidgetList *, int *, int *);
extern  WidgetList SgGraphGetRoots(Widget, int *);
extern  WidgetList SgGraphGetSelectedArcs(Widget,int *);
extern  WidgetList SgGraphGetSelectedNodes(Widget,int *);
extern  Widget SgGraphInputOverArc(Widget, int, int);
extern  void SgGraphInsertRoots(Widget, WidgetList, int);
extern  Boolean SgGraphIsPointInArc(Widget, int, int);
extern  Boolean SgGraphIsSelectedArc(Widget, Widget);
extern  Boolean SgGraphIsSelectedNode(Widget, Widget);
extern  Boolean SgGraphMoveArc(Widget, Widget, Widget, Widget);
extern  Boolean SgGraphMoveNode(Widget, Widget, Position, Position);
extern  int SgGraphNumArcs(Widget);
extern  int SgGraphNumNodes(Widget);
extern  int SgGraphNumNodeArcs(Widget, Widget, int * , int *);
extern  int SgGraphNumRoots(Widget);
extern  int SgGraphNumSelectedArcs(Widget);
extern  int SgGraphNumSelectedNodes(Widget);
extern  void SgGraphMoveAll(Widget, int, int);
extern  void SgGraphLayout(Widget);
extern  void SgGraphRelaySubgraph(Widget, Widget);
extern  void SgGraphRemoveArcBetweenNodes(Widget, Widget, Widget);
extern  void SgGraphRemoveRoots(Widget, WidgetList, int);
extern  void SgGraphSelectArc(Widget, Widget, Boolean);
extern  void SgGraphSelectArcs(Widget, WidgetList, int, Boolean);
extern  void SgGraphSelectNode(Widget,Widget, Boolean);
extern  void SgGraphSelectNodes(Widget, WidgetList, int, Boolean);
extern  void SgGraphUnselectArc(Widget,Widget, Boolean);
extern  void SgGraphUnselectArcs(Widget, WidgetList, int, Boolean);
extern  void SgGraphUnselectNode(Widget,Widget, Boolean);
extern  void SgGraphUnselectNodes(Widget,WidgetList, int, Boolean);
extern  void SgGraphAddNode(Widget);
extern Widget SgGraphCreateOverView (Widget, Widget);
extern void SgGraphSetButterflyRoot(Widget, Widget);
extern void SgGraphZoom(Widget, float);
extern void SgGraphDontAdjustSize(Widget);
extern void SgGraphAdjustSize(Widget);
extern void SgGraphUnmanageAll(Widget);
extern void SgGraphManageAll(Widget);
extern void SgGraphSwapBuffers(Widget);
extern void SgGraphStartMultiBuffering(Widget);
extern void SgGraphStopMultiBuffering(Widget w);
extern void SgGraphHideCoverWindow(Widget w);
extern void SgGraphShowCoverWindow(Widget w);
extern Boolean SgGraphPrint(Widget graph,  char *fileName);
extern void  SgGraphSelectSubtree(Widget graph, Widget target, Boolean notify);
#else    /* _NO_PROTO */
extern void SgGraphHideCoverWindow();
extern void SgGraphShowCoverWindow();
extern Widget SgCreateGraph();
extern Widget SgCreateManagedGraph ();
extern Widget SgCreateScrolledGraph();
extern Widget SgCreateArc ();
extern Widget SgCreateAttachedArc ();

extern  void SgGraphCenterAroundWidget();
extern  void SgGraphDestroyAllArcs();
extern  void SgGraphDestroyAllNodes();
extern  void SgGraphDestroySelectedArcsOrNodes();
extern  void SgGraphGetArcNodes();
extern  WidgetList SgGraphGetArcsBetweenNodes();
extern  WidgetList SgGraphGetArcs();
extern  WidgetList SgGraphGetNodes();
extern  void SgGraphGetNodeArcs();
extern  WidgetList SgGraphGetRoots();
extern  WidgetList SgGraphGetSelectedArcs();
extern  WidgetList SgGraphGetSelectedNodes();
extern  Widget SgGraphInputOverArc();
extern  void SgGraphInsertRoots();
extern  Boolean SgGraphIsPointInArc();
extern  Boolean SgGraphIsSelectedArc();
extern  Boolean SgGraphIsSelectedNode();
extern  Boolean SgGraphMoveArc();
extern  Boolean SgGraphMoveNode();
extern  int SgGraphNumArcs();
extern  int SgGraphNumNodes();
extern  int SgGraphNumNodeArcs();
extern  int SgGraphNumRoots();
extern  int SgGraphNumSelectedArcs();
extern  int SgGraphNumSelectedNodes();
extern  void SgGraphMoveAll();
extern  void SgGraphLayout();
extern  void SgGraphRelaySubgraph();
extern  void SgGraphRemoveArcBetweenNodes();
extern  void SgGraphRemoveRoots();
extern  void SgGraphSelectArc();
extern void  SgGraphSelectArcs();
extern  void SgGraphSelectNode();
extern  void SgGraphSelectNodes();
extern  void SgGraphUnselectArc();
extern  void SgGraphUnselectArcs();
extern  void SgGraphUnselectNode();
extern  void SgGraphUnselectNodes();
extern void SgGraphAddNode();
extern Widget SgGraphCreateOverView ();
extern void SgGraphSetButterflyRoot();
extern void SgGraphZoom();
extern void SgGraphHideCoverWindow();
extern void SgGraphShowCoverWindow();
extern Boolean SgGraphPrint();
extern void  SgGraphSelectSubtree();
#endif /*_NO_PROTO */


#define XmNtwinsVisible     "twinsVisible"
#define XmCTwinsVisible     "TwinsVisible"

#define XmNarcDrawMode      "arcDrawMode"
#define XmCArcDrawMode      "ArcDrawMode"


#define XmNautoLayoutMode   "autoLayoutMode"
#define XmCAutoLayoutMode   "AutoLayoutMode"

#define XmNlayoutStyle   "layoutStyle"
#define XmRLayoutStyle   "LayoutStyle"
#define XmCLayoutStyle   "LayoutStyle"

#define XmNreLayout	    "reLayout"
#define XmCReLayout	    "ReLayout"

#define XmNreorient         "reorient"
#define XmCReorient         "Reorient"

#define XmNchildSpacing     "childSpacing"
#define XmCChildSpacing     "ChildSpacing" 

#define XmNsiblingSpacing   "siblingSpacing"
#define XmCSiblingSpacing   "SiblingSpacing" 

#define XmNsaveGraph       "saveGraph"
#define XmCSaveGraph       "SaveGraph"

#define XmNsaveFileName     "saveFileName"
#define XmCSaveFileName     "SaveFileName"

#define XmCHighlight     "Highlight"

	/* these have class XmCCallback  */

#define XmNnewArcCallback           "newArcCallback"
#define XmNgraphTooBigCallback      "graphTooBigCallback"
#define XmNuserButtonCallback	    "userButtonCallback"
#define XmNnewNodeCallback	    "newNodeCallback"
#define XmNallowMultipleSelections  "allowMultipleSelections"
#define  XmCAllowMultipleSelections "AllowMultipleSelections"

#define XmNnodeMovedCallback	    "nodeMovedCallback"

#define XmNarcMovedCallback	    "arcMovedCallback"

#define XmNselectNodeCallback	    "selectNodeCallback"

#define XmNselectArcCallback	    "selectArcCallback"

#define XmNdeselectCallback	    "deselectCallback"

#define XmNselectSubgraphCallback   "selectSubgraphCallback"

#define XmNdeleteNodeCallback	    "deleteNodeCallback"

#define XmNdeleteArcCallback	    "deleteArcCallback"

#define XmNgraphChangedCallback	    "graphChangedCallback"

#define XmNdefaultNodeClass "defaultNodeClass"

#define XmCDefaultNodeClass "DefaultNodeClass"

#define XmNdefaultLabel     "defaultLabel"

#define XmRArcDrawMode      "ArcDrawMode"

#define XmRAutoLayoutType     "AutoLayoutType"

#define XmNinteractiveArcDirection     "interactiveArcDirection"

#define XmCInteractiveArcDirection     "InteractiveArcDirection"

#define XmNmovableNodes    "movableNodes"
#define XmCMovableNodes    "MovableNodes"

#define XmNeditableArcs     "editableArcs"
#define XmCEditableArcs     "EditableArcs"
#define XmNselectableArcs     "selectableArcs"
#define XmCSelectableArcs     "SelectableArcs"

#define XmNokToAdd     "okToAdd"
#define XmCOkToAdd     "OkToAdd"

#define XmNshowCrossingArcs  "showCrossingArcs"
#define XmCShowCrossingArcs  "ShowCrossingArcs"

#define XmNptrCursor  "ptrCursor"
#define XmNmotionCursor  "motionCursor"
#define XmNindicateCursor  "indicateCursor"
#define XmNindicateChildCursor  "indicateChildCursor"
#define XmNindicateParentCursor "indicateParentCursor"

#define XmNlayoutProc  "layoutProc"
#define XmCLayoutProc  "LayoutProc"

#define XmNmarkColor  "markColor"
#define XmCMarkColor  "MarkColor"

#define XmNoverviewViewportTopShadowColor  "overviewViewportBottomShadowColor"
#define XmNoverviewViewportBottomShadowColor  "overviewViewportTopShadowColor"
#define XmCOverviewViewportColor  "OverviewViewportColor"

#define XmNoverviewNodeColor  "overviewNodeColor"
#define XmNoverviewLineColor  "overviewLineColor"
#define XmCOverviewColor  "OverviewColor"

#define XmNshowOverviewArcs  "showOverviewArcs"
#define XmCShowOverviewArcs  "ShowOverviewArcs"

#define XmNskewOverviewScale  "skewOverviewScale"
#define XmCSkewOverviewScale  "SkewOverviewScale"

#define XmNlayoutKey  "layoutKey"
#define XmCLayoutKey  "LayoutKey"

#define XmNallowGrandchildInput  "allowGrandchildInput"
#define XmCAllowGrandchildInput  "AllowGrandchildInput"



/****************************************************
 * Callback reasons
 *****************************************************/

#define XmCR_NEW_ARC			41
#define XmCR_NEW_NODE                   42
#define XmCR_NODE_MOVED			43
#define XmCR_ARC_MOVED			44
#define XmCR_SUBGRAPH_MOVED		45
#define XmCR_ARC_EDITED			46
#define XmCR_SELECT_NODE		47
#define XmCR_SELECT_ARC			48
#define XmCR_SELECT_SUBGRAPH		49
#define XmCR_DELETE_NODE		50
#define XmCR_DELETE_ARC			51
#define XmCR_SELECT			52
#define XmCR_RELEASE			53
#define XmCR_NODE_DOUBLE_CLICK		54
#define XmCR_ARC_DOUBLE_CLICK		55
#define XmCR_DOUBLE_CLICK		56
#define XmCR_DESELECT_ARC		57
#define XmCR_DESELECT_NODE		58
#define XmCR_DESELECT		        59
#define XmCR_NODES_MOVED		60
#define XmCR_SELECT_NODES		61
#define XmCR_SELECT_ARCS		62
#define XmCR_SELECT_ARCS_AND_NODES	63
#define XmCR_LAYOUT_CHANGED		64
#define XmCR_GRAPH_SCROLLED		65
#define XmCR_SIZE_CHANGED		66
#define XmCR_OVERVIEW_MARK_CHANGED	67
#define XmCR_BUTTON_OVER_ARC	        68
#define XmCR_BUTTON_OVER_GADGET	        69
#define XmCR_BUTTON_OVER_GRAPH	        70
#define XmCR_BUTTON_OVER_WIDGET	        71

#define XmPOSITION_FIXED                 0
#define XmPOSITION_RELATIVE              1
#define XmPOSITION_PROPORTIONAL          2

typedef struct {
    int            reason;
    XEvent        *event;
    Boolean        interactive;
    WidgetList     selected_widgets;
    int            num_selected_widgets;
    WidgetList     selected_arcs;
    int            num_selected_arcs;
    Widget         widget;
    Widget         old_to;    /* Used for move and edit arc callbacks */
    Widget         old_from;
    Widget         new_to;
    Widget         new_from;
    Boolean        doit;
} SgGraphCallbackStruct;

typedef enum  {XmNEVER, XmALWAYS, XmARCS_ONLY, XmNODES_ONLY, XmPARTIAL} autoLayoutType;

typedef enum  {XmGRAPH, XmBUTTERFLY} layoutStyle;

#if defined(__cplusplus) || defined(c_plusplus)
}
#endif

#endif /* _SgGraph_h */


/* DO NOT ADD ANYTHING AFTER THIS #endif */



