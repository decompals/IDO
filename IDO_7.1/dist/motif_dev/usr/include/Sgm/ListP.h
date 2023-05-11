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
#ifndef _SgListP_h
#define _SgListP_h

#include "List.h"
#include <Xm/PrimitiveP.h>
#include "ScrollBar.h"
#include <Xm/ScrolledW.h>

/*  List struct passed to Convert proc for drag and drop */
typedef struct _SgListDragConvertStruct
{
   Widget     w;
   XmString  *strings;
   int        num_strings;
} SgListDragConvertStruct;

/*  List class structure  */

typedef struct _SgListClassPart
{
   int foo;	/*  No new fields needed  */
#ifdef __sgi /* Allow future extensions without breaking shared library compatibility */
	caddr_t _SG_vendorExtension;
#endif /* __sgi */
} SgListClassPart;


/*  Full class record declaration for List class  */

typedef struct _SgListClassRec
{
   CoreClassPart        core_class;
   XmPrimitiveClassPart primitive_class;
   SgListClassPart     list_class;
} SgListClassRec;

externalref SgListClassRec sgListClassRec;

/****************
 *
 * Internal form of the list elements.
 *
 ****************/
 
typedef	struct {
	_XmString	name;
	Dimension	height;
	Dimension	width;
	Dimension	CumHeight;
	Boolean		selected;
	Boolean		last_selected;
	Boolean		LastTimeDrawn;
	unsigned short	NumLines;
	int		length;
	GC	        gc;
	XmFontList 	font;
	Pixmap 	        icon;
	short           iconWidth, iconHeight;
	short           indent;
} SgListElement, *SgListElementPtr;

/*  The List instance record  */

typedef struct _SgListPart
{
    short           ItemSpacing;
    Dimension       margin_width;
    Dimension    	margin_height;
    XmFontList 	font;
    XmString	*items;
    int		itemCount;
    XmString	*selectedItems;
    int             *selectedIndices;
    int		selectedItemCount;
    int 		visibleItemCount;
    int 		LastSetVizCount;
    unsigned char	SelectionPolicy;
    unsigned char	ScrollBarDisplayPolicy;
    unsigned char	SizePolicy;
    XmStringDirection StrDir;

    Boolean		AutoSelect;
    Boolean		DidSelection;
    Boolean		FromSetSB;
    Boolean		FromSetNewSize;
    Boolean		AddMode;
    unsigned char	LeaveDir;
    unsigned char	HighlightThickness;
    int 		ClickInterval;
    XtIntervalId	DragID;
    XtCallbackList 	SingleCallback;
    XtCallbackList 	MultipleCallback;
    XtCallbackList 	ExtendCallback;
    XtCallbackList 	BrowseCallback;
    XtCallbackList 	DefaultCallback;
    
    GC		NormalGC;	
    GC		InverseGC;
    GC		HighlightGC;
    Pixmap          DashTile;
    SgListElementPtr	*InternalList;
    int		LastItem;
    int		FontHeight;
    int		top_position;
    char		Event;
    int		LastHLItem;
    int		StartItem;
    int		OldStartItem;
    int		EndItem;
    int		OldEndItem;
    Position	BaseX;
    Position	BaseY;
    Boolean		MouseMoved;
    Boolean		AppendInProgress;
    Boolean		Traversing;
    Boolean		KbdSelection;
    short		DownCount;
    Time		DownTime;
    int		CurrentKbdItem;	/* 0 to n-1, -1 means empty list. */
    unsigned char	SelectionType;
    GC		InsensitiveGC;
    
    
    int vmin;		  /*  slider minimum coordiate position     */
    int vmax;		  /*  slider maximum coordiate position     */
    int vOrigin;		  /*  slider edge location                  */
    int vExtent;		  /*  slider size                           */
    
    int hmin;		  /*  Same as above for horizontal bar.     */
    int hmax;
    int hOrigin;
    int hExtent;
    
    Dimension	MaxWidth;
    Dimension	CharWidth;
    Position	XOrigin;
    
    SgScrollBarWidget   	hScrollBar;
    SgScrollBarWidget   	vScrollBar;
    XmScrolledWindowWidget  Mom;
    Dimension	MaxItemHeight;
    
    Boolean      hasIcons;
    Boolean      update;
    short        iconWidth;
    int          indicatedIcon;
    int          *fields;
    int          numFields;
    int          selectedField;
    Pixel          arm_color;
    int          field_margin;
    Dimension	spacing;
    XtCallbackList 	IconActivateCallback;
    XtCallbackList 	IconDisarmCallback;
    XtCallbackList 	IconArmCallback;
    XtCallbackList 	FieldChangedCallback;
    GC		ArmGC;	
    GC		IconGC;	
    Boolean     indentIcons;
    Dimension   defaultIconWidth;
#ifdef __sgi /* Allow future extensions without breaking shared library compatibility */
	caddr_t _SG_vendorExtension;
#endif /* __sgi */
} SgListPart;


/*  Full instance record declaration  */

typedef struct _SgListRec
{
   CorePart	   core;
   XmPrimitivePart primitive;
   SgListPart	   list;
} SgListRec;


/* BEGIN OSF Fix pir 2661 */
/* Access Macros */
#define POS_IsSelected(lw,pos) (((SgListWidget)(lw))->list.InternalList[(pos)]->selected)
/* END OSF Fix pir 2661 */

#endif /* _SgListP_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */
