/*********************************************************
 * menus.c: Simple menu package
  *         From:
  *                   The X Window System, 
  *            Programming and Applications with Xt
  *                   OSF/Motif Edition
  *         by
  *                Douglas Young
  *              Prentice Hall, 1990
  *
  *                 Example described on pages:  104-106
  *
  *
  *  Copyright 1989 by Prentice Hall
  *  All Rights Reserved
  *
  * This code is based on the OSF/Motif widget set and the X Window System
  *
  * Permission to use, copy, modify, and distribute this software for 
  * any purpose and without fee is hereby granted, provided that the above
  * copyright notice appear in all copies and that both the copyright notice
  * and this permission notice appear in supporting documentation.
  *
  * Prentice Hall and the author disclaim all warranties with regard to 
  * this software, including all implied warranties of merchantability and fitness.
  * In no event shall Prentice Hall or the author be liable for any special,
  * indirect or cosequential damages or any damages whatsoever resulting from 
  * loss of use, data or profits, whether in an action of contract, negligence 
  * or other tortious action, arising out of or in connection with the use 
  * or performance of this software.
  *
  * Open Software Foundation is a trademark of The Open Software Foundation, Inc.
  * OSF is a trademark of Open Software Foundation, Inc.
  * OSF/Motif is a trademark of Open Software Foundation, Inc.
  * Motif is a trademark of Open Software Foundation, Inc.
  * DEC is a registered trademark of Digital Equipment Corporation
  * HP is a registered trademark of the Hewlett Packard Company
  * DIGITAL is a registered trademark of Digital Equipment Corporation
  * X Window System is a trademark of the Massachusetts Institute of Technology
  **********************************************************************************/

#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/Separator.h>
#include <Xm/PushB.h>
#include <Xm/CascadeB.h>
#include <Xm/RowColumn.h>
#include <Xm/Label.h>
#include "libXs.h"

xs_create_menu_buttons(title, menu, menulist, nitems)
   char           *title;
   Widget          menu;
   xs_menu_struct *menulist;
   int             nitems;
{
  Arg        wargs[1];
  int        i;
  WidgetList buttons;
  int        separators = 0;
  /*
   * Allocate a widget list to hold all 
   * button widgets.
   */
   buttons = (WidgetList) XtMalloc(nitems * sizeof(Widget));
  /*
   * If a title is given, create Label and Separator widgets.
  */
  if(title){
    XtCreateManagedWidget(title, xmLabelWidgetClass, menu,
                          NULL, 0);
    XtCreateManagedWidget("separator", xmSeparatorWidgetClass,
                          menu, NULL, 0);
  }
  /*
   * Create an entry for each item in the menu.
   */
  for(i=0;i<nitems;i++){
   /* 
    * A NULL name represents a separator.
    */
   if(menulist[i].name == NULL){ 
     XtCreateManagedWidget("separator",
                           xmSeparatorWidgetClass,
                           menu, NULL, 0);
     separators++; /* Count how many entries aren't buttons */
   }
   /*
    * If there is a name and a callback, create a "normal"
    * menu entry and register the callback function.
    */
   else if(menulist[i].func){
     buttons[i-separators] = XtCreateWidget(menulist[i].name,
                                     					xmPushButtonWidgetClass,
                                     menu, NULL, 0);
     XtAddCallback(buttons[i-separators], XmNactivateCallback,
                   menulist[i].func, menulist[i].data);
   }
   /*
    * If there is a name, but no callback function, the entry
    * must be a label, unless there is a submenu.
    */
   else if(!menulist[i].sub_menu)
     buttons[i-separators] = XtCreateWidget(menulist[i].name,
                                           xmLabelWidgetClass,
                                           menu, NULL, 0);
   /*
    * If we got here, the entry must be a submenu.
    * Create a pulldown menu pane and an XmCascadeButton
    * widget. Attach the menu pane and make a recursive call
    * to create the entries in the submenu.
    */
   else{  
    Widget sub_menu;
    sub_menu =XmCreatePulldownMenu(menu,
                                   menulist[i].sub_menu_title,
                                   NULL, 0);
    XtSetArg(wargs[0], XmNsubMenuId, sub_menu);
    buttons[i-separators] =
          XtCreateWidget(menulist[i].name,
                         xmCascadeButtonWidgetClass,
                         menu, wargs, 1);
    xs_create_menu_buttons(menulist[i].sub_menu_title,
                          sub_menu, menulist[i].sub_menu,
                          menulist[i].n_sub_items);
    }
  } 
  /*
   * Manage all button widgets. Menu panes are not managed.
   */
  XtManageChildren(buttons, nitems - separators);
}
