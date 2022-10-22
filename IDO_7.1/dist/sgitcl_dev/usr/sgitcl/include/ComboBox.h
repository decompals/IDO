/*
 * ComboBox.h, Interleaf, 16aug93 2:37pm Version 1.1.
 */

/***********************************************************
Copyright 1993 Interleaf, Inc.

Permission to use, copy, modify, and distribute this software
and its documentation for any purpose without fee is granted,
provided that the above copyright notice appear in all copies
and that both copyright notice and this permission notice appear
in supporting documentation, and that the name of Interleaf not
be used in advertising or publicly pertaining to distribution of
the software without specific written prior permission.

Interleaf makes no representation about the suitability of this
software for any purpose. It is provided "AS IS" without any
express or implied warranty. 
******************************************************************/

/*
 *  (C) Copyright 1991,1992, 1993
 *  Interleaf, Inc.
 *  Nine Hillside Avenue, Waltham, MA  02154
 *
 * ComboBox.h: 
 * 
 * Public header file for DtComboBoxWidget.
 */
#ifndef _ComboBox_h
#define _ComboBox_h

#ifndef AA
#define AA(args) ()
#endif

#include <Xm/Xm.h>

#define XmNarrowSize		"arrowSize"
#define XmNarrowSpacing		"arrowSpacing"
#define XmNarrowType		"arrowType"
#define XmNlist			"list"
#define XmNlistFontList		"listFontList"
#define XmNpoppedUp		"poppedUp"
#define XmNselectedPosition	"selectedPosition"
#define XmNselectedItem		"selectedItem"
#define XmNtextField		"textField"
#define XmNcomboBoxType		"comboBoxType"
#define XmNupdateLabel		"updateLabel"

#define XmCArrowSize		"ArrowSize"
#define XmCArrowSpacing		"ArrowSpacing"
#define XmCArrowType		"ArrowType"
#define XmCHorizontalSpacing	"HorizontalSpacing"
#define XmCList			"List"
#define XmCListFontList		"ListFontList"
#define XmCPoppedUp		"PoppedUp"
#define XmCSelectedPosition	"SelectedPosition"
#define XmCSelectedItem		"SelectedItem"
#define XmCComboBoxType		"ComboBoxType"
#define XmCTextField		"TextField"
#define XmCUpdateLabel		"UpdateLabel"
#define XmCVerticalSpacing	"VerticalSpacing"

#define XmRArrowType		"ArrowType"
#define XmRComboBoxType		"ComboBoxType"

/* XmNorientation defines */
#define XmLEFT		0
#define XmRIGHT		1

/* ArrowType defines */
#define XmMOTIF		0
#define XmWINDOWS	1

/* XmNtype defines */
#define XmDROP_DOWN_LIST_BOX	0
#define XmDROP_DOWN_COMBO_BOX	1

/* ComboBox callback info */
#define XmNselectionCallback	"selectionCallback"
#define XmNmenuPostCallback	"menuPostCallback"
#define XmCR_COMBO_SELECT    128 /* Large #, so no collisions with XM */
#define XmCR_MENU_POST      129 /* Large #, so no collisions with XM */

typedef struct {
   int 	     reason;
   XEvent    *event;
   XmString  item_or_text;
   int       item_position;
} DtComboBoxCallbackStruct;

extern WidgetClass dtComboBoxWidgetClass;

typedef struct _DtComboBoxClassRec *DtComboBoxWidgetClass;
typedef struct _DtComboBoxRec      *DtComboBoxWidget;

/*
 * External functions which manipulate the ComboBox list of items.
 */
extern Widget DtCreateComboBox(Widget parent, char *name,
				   Arg *arglist, int num_args);
extern Widget DtCreateDropDownComboBox(Widget, char *, Arg *, int);
extern Widget DtCreateDropDownList(Widget, char *, Arg *, int);

extern void DtComboBoxAddItem(DtComboBoxWidget combo, XmString item,
				  int pos, Boolean unique);
extern void DtComboBoxDeletePos(DtComboBoxWidget combo, int pos);
extern void DtComboBoxSetItem(DtComboBoxWidget combo, XmString item);
extern void DtComboBoxSelectItem(DtComboBoxWidget combo, XmString item,
				Boolean notify);

#endif	/* _ComboBox_h */
