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

#define XmCreateList 		SgCreateList
#define XmCreateScrolledList 	SgCreateScrolledList
#define XmListAddItem 		SgListAddItem
#define XmListAddItemUnselected SgListAddItemUnselected
#define XmListAddItemsUnselected SgListAddItemsUnselected
#define XmListAddItems 		SgListAddItems
#define XmListDeleteAllItems 	SgListDeleteAllItems
#define XmListDeleteItem 	SgListDeleteItem
#define XmListDeleteItems	SgListDeleteItems
#define XmListDeleteItemsPos 	SgListDeleteItemsPos
#define XmListDeletePositions   SgListDeletePositions
#define XmListDeletePos 	SgListDeletePos
#define XmListReplaceItemsUnselected  SgListReplaceItemsUnselected
#define XmListReplaceItemsPosUnselected  SgListReplaceItemsPosUnselected
#define XmListReplacePositions  SgListReplacePositions
#define XmListGetKbdItemPos  SgListGetKbdItemPos
#define XmListSetKbdItemPos  SgListSetKbdItemPos
#define XmListYToPos  SgListYToPos
#define XmListPosToBounds  SgListPosToBounds
#define XmListUpdateSelectedList  SgListUpdateSelectedList
#define XmListPosSelected  SgListPosSelected
#define XmListDeselectAllItems 	SgListDeselectAllItems
#define XmListDeselectItem 	SgListDeselectItem
#define XmListDeselectPos 	SgListDeselectPos
#define XmListGetMatchPos 	SgListGetMatchPos
#define XmListGetSelectedPos 	SgListGetSelectedPos
#define XmListItemExists 	SgListItemExists
#define XmListItemPos 		SgListItemPos
#define XmListReplaceItems 	SgListReplaceItems
#define XmListReplaceItemsPos 	SgListReplaceItemsPos
#define XmListSelectItem 	SgListSelectItem
#define XmListSelectPos		SgListSelectPos
#define XmListSetAddMode 	SgListSetAddMode
#define XmListSetBottomItem 	SgListSetBottomItem
#define XmListSetBottomPos 	SgListSetBottomPos
#define XmListSetHorizPos 	SgListSetHorizPos
#define XmListSetItem 		SgListSetItem
#define XmListSetItemAttributes SgListSetItemAttributes
#define XmListSetPos 		SgListSetPos
#define XmListSetPositionAttributes SgListSetPositionAttributes
#define XmListWidgetClass 	SgListWidgetClass
#define XmListWidget 		SgListWidget
#define xmListWidgetClass 	sgListWidgetClass
#define _XmListClassRec 	_SgListClassRec
#define _XmListRec      	_SgListRec    
#define XmList SgList
#define XmListSetPositionIcon   SgListSetPositionIcon
#define XmListSetPositionIcons   SgListSetPositionIcons
#define XmListIconStruct   SgListIconStruct
#define XmListSetItemIcon   SgListSetItemIcon
#define XmListCallbackStruct SgListCallbackStruct
#define XmListClassPart SgListClassPart
#define XmListClassRec SgListClassRec
#define xmListClassRec sgListClassRec
#define _XmListPart _SgListPart
#define _XmListClassPart _SgListClassPart
#define XmListPart SgListPart
#define XmListRec SgListRec
#define XmListSetFields SgListSetFields
#define XmListSetTable SgListSetTable
#define XmListSetPositionIndent SgListSetPositionIndent
#define XmListSetItemIndent SgListSetItemIndent
#define XmListSetAttributes SgListSetAttributes

/*  List struct passed to Convert proc for drag and drop */
typedef struct _XmListDragConvertStruct
{
   Widget     w;
   XmString  *strings;
   int        num_strings;
} XmListDragConvertStruct;

/*  List class structure  */

typedef struct _XmListClassPart
{
   int foo;	/*  No new fields needed  */
#ifdef __sgi /* Allow future extensions without breaking shared library compatibility */
	caddr_t _SG_vendorExtension;
#endif /* __sgi */
} XmListClassPart;


/*  Full class record declaration for List class  */

typedef struct _XmListClassRec
{
   CoreClassPart        core_class;
   XmPrimitiveClassPart primitive_class;
   XmListClassPart     list_class;
} XmListClassRec;

externalref XmListClassRec xmListClassRec;

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
} Element, *ElementPtr;

/*  The List instance record  */

typedef struct _XmListPart
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
    ElementPtr	*InternalList;
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
} XmListPart;


/*  Full instance record declaration  */

typedef struct _XmListRec
{
   CorePart	   core;
   XmPrimitivePart primitive;
   XmListPart	   list;
} XmListRec;


/* BEGIN OSF Fix pir 2661 */
/* Access Macros */
#define POS_IsSelected(lw,pos) (((XmListWidget)(lw))->list.InternalList[(pos)]->selected)
/* END OSF Fix pir 2661 */

#endif /* _SgListP_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */
