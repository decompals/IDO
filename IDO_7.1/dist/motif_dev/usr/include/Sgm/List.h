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
/* 
 * (c) Copyright 1989, 1990, 1991, 1992 OPEN SOFTWARE FOUNDATION, INC. 
 * ALL RIGHTS RESERVED 
*/ 
/* 
 * Motif Release 1.2
*/ 
/*   $RCSfile: List.h,v $ $Revision: 1.2 $ $Date: 1993/10/11 19:57:48 $ */
/*
*  (c) Copyright 1987, 1988, 1989, 1990, 1991, 1992 HEWLETT-PACKARD COMPANY */
#ifndef _SgList_h
#define _SgList_h

#include <Xm/Xm.h>

#ifdef __cplusplus
extern "C" {
#endif

externalref WidgetClass sgListWidgetClass;

#ifndef SgIsList
#define SgIsList(w)	XtIsSubclass(w, sgListWidgetClass)
#endif /* SgIsList */

typedef struct _SgListClassRec * SgListWidgetClass;
typedef struct _SgListRec      * SgListWidget;

#define XmINITIAL 	0
#define XmADDITION	1
#define XmMODIFICATION	2

typedef struct
{
    int  item_pos;
    Pixmap icon;
} SgListIconStruct;

typedef struct
{
    int       item_pos;
    Pixel     fg;
    Pixel     bg;
    XmFontList     fl;
} SgListAttributeStruct;

#ifdef _NO_PROTO
extern void SgListAddItem ();
extern void SgListAddItems ();
extern void SgListAddItemUnselected ();
extern void SgListDeleteItem ();
extern void SgListDeleteItems ();
extern void SgListDeletePos ();
extern void SgListDeleteItemsPos ();
extern void SgListDeleteAllItems ();
extern void SgListReplaceItems ();
extern void SgListReplaceItemsPos ();
extern void SgListSelectItem ();
extern void SgListSelectPos ();
extern void SgListDeselectItem ();
extern void SgListDeselectPos ();
extern void SgListDeselectAllItems ();
extern void SgListSetPos ();
extern void SgListSetBottomPos ();
extern void SgListSetItem ();
extern void SgListSetBottomItem ();
extern void SgListSetAddMode ();
extern Boolean SgListItemExists ();
extern int SgListItemPos ();
extern Boolean SgListGetMatchPos ();
extern Boolean SgListGetSelectedPos ();
extern void SgListSetHorizPos ();
extern Widget SgCreateList ();
extern Widget SgCreateScrolledList ();
extern void SgListSetPositionIndent ();
extern void SgListSetItemIndent ();
extern void SgListSetPositionAttributes ();
extern void SgListSetItemAttributes ();
extern void SgListSetPositionIcons ();
extern void SgListSetAttributes ();
extern void SgListSetItemIcon ();
extern void SgListSetPositionIcon ();
extern void SgListSetFields();
extern void SgListSetTable();
#else /* _NO_PROTO */

extern void SgListAddItem (Widget w, XmString item, int pos);
extern void SgListAddItems (Widget w, XmString *items, int item_count, int pos);
extern void SgListAddItemUnselected (Widget w, XmString item, int pos);
extern void SgListDeleteItem (Widget w, XmString item);
extern void SgListDeleteItems (Widget w, XmString *items, int item_count);
extern void SgListDeletePos (Widget w, int pos);
extern void SgListDeleteItemsPos (Widget w, int item_count, int pos);
extern void SgListDeleteAllItems (Widget w);
extern void SgListReplaceItems (Widget w, XmString *old_items, int item_count, XmString *new_items);
extern void SgListReplaceItemsPos (Widget w, XmString *new_items, int item_count, int position);
extern void SgListSelectItem (Widget w, XmString item,  
#if NeedWidePrototypes
int notify
#else
Boolean notify
#endif 
);
extern void SgListSelectPos (Widget w, int pos,  
#if NeedWidePrototypes
int notify
#else
Boolean notify
#endif 
);
extern void SgListDeselectItem (Widget w, XmString item);
extern void SgListDeselectPos (Widget w, int pos);
extern void SgListDeselectAllItems (Widget w);
extern void SgListSetPos (Widget w, int pos);
extern void SgListSetBottomPos (Widget w, int pos);
extern void SgListSetItem (Widget w, XmString item);
extern void SgListSetBottomItem (Widget w, XmString item);
extern void SgListSetAddMode (Widget w,    
#if NeedWidePrototypes
int add_mode
#else
Boolean add_mode
#endif 
);
extern Boolean SgListItemExists (Widget w, XmString item);
extern int SgListItemPos (Widget w, XmString item);
extern Boolean SgListGetMatchPos (Widget w, XmString item, int **pos_list, int *pos_count);
extern Boolean SgListGetSelectedPos (Widget w, int **pos_list, int *pos_count);
extern void SgListSetHorizPos (Widget w, int position);
extern Widget SgCreateList (Widget parent, char *name, ArgList args, Cardinal argCount);
extern Widget SgCreateScrolledList (Widget parent, char *name, ArgList args, Cardinal argCount);

extern void SgListSetPositionIndent (Widget w, int item_pos, int indent);
extern void SgListSetItemIndent (Widget w, XmString, int indent);

extern void SgListSetPositionAttributes (Widget w, int item_pos, XmFontList fl, Pixel fg, Pixel bg);
extern void SgListSetItemAttributes (Widget w, XmString, XmFontList fl, Pixel fg, Pixel bg);
extern void SgListSetPositionIcons (Widget w,   SgListIconStruct *icons, int numIcons);
extern void SgListSetAttributes (Widget w, SgListAttributeStruct *attr, int size);
extern void SgListSetItemIcon (Widget w, XmString item, Pixmap icon);
extern void SgListSetPositionIcon (Widget w, int pos, Pixmap icon);
extern void SgListSetFields(Widget w, int *fields, int numFields);
extern void SgListSetTable(Widget w, String *table, int rows, int cols);
#endif /* _NO_PROTO */
/********    End Public Function Declarations    ********/

#define XmCR_ICON_ARMED          101
#define XmCR_ICON_AREA_SELECTED  102
#define XmCR_ICON_DISARMED       103
#define XmCR_ICON_ACTIVATED      104
#define XmCR_ICON_AREA_ACTIVATED 105

#define XmNfieldMargin "fieldMargin"
#define XmNfieldChangedCallback "fieldChangedCallback"

typedef struct
{
   int 	     reason;
   XEvent    *event;
   XmString  item;
   int       item_length;
   int       item_position;
   XmString  *selected_items;
   int       selected_item_count;
   int       *selected_item_positions;
   char      selection_type;
   int       selected_field;  /* Experimental */
} SgListCallbackStruct;

#define XmCUpdate             "Update"
#define XmNshowIcons          "showIcons"
#define XmCShowIcons          "ShowIcons"
#define XmNindentIcons        "indentIcons"
#define XmCIndentIcons        "IndentIcons"
#define XmNdefaultIconWidth   "defaultIconWidth"
#define XmCDefaultIconWidth   "DefaultIconWidth"

#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif 

/* DON'T ADD ANYTHING AFTER THIS #endif */
