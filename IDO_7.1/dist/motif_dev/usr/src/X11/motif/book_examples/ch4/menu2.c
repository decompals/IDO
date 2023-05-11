/**********************************************************************************
  * menu.c: pop up menu example using libXs menu functions
  *         From:
  *                   The X Window System, 
  *            Programming and Applications with Xt
  *                   OSF/Motif Edition
  *         by
  *                Douglas Young
  *              Prentice Hall, 1990
  *
  *                 Example described on pages:     102-103
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
#include <Xm/RowColumn.h>
#include <Xm/BulletinB.h>
#include "libXs.h"

extern void post_menu_handler ();
extern void do_it();
/* 
 * Describe the list of menu items for a submenu pane.
 */
static xs_menu_struct sub_menu_a[] = {
  {"Item 2 A" , do_it, "Item 2 A"},
  {"Item 2 B" , do_it, "Item 2 B"}
};
/*
 * Describe the main menu pane.
 */
static xs_menu_struct MenuData[] = {
  { "Item 1" , do_it, "Item One",   
     NULL, 0, NULL },
  { "Item 2" , NULL, "Item Two", 
     sub_menu_a, XtNumber(sub_menu_a), "SubMenuA" },
  { "Item 3" , do_it, "Item Three", 
     NULL, 0 , NULL }
};

main(argc, argv)
     int   argc;
     char *argv[];
{
  Widget toplevel, bboard, popup_menu;
 /*
  * Initialize the Intrinsics and create a top level widget.
  */
  toplevel = XtInitialize (argv[0], "MenuTest", NULL, 0, 
                           &argc, argv);
  bboard = XtCreateManagedWidget("bboard", 
                                 xmBulletinBoardWidgetClass, 
                                 toplevel, NULL, 0);
  /*
   * Step 1. Create a popup menu pane and define an event    
   *         handler to pop it up.
   */
  popup_menu = XmCreatePopupMenu(bboard, "menu", NULL, 0);
  XtAddEventHandler(bboard, ButtonPressMask, FALSE,
                    post_menu_handler, popup_menu);
  /*
   * Step 2. Create the menu entries from the description.
   */
  xs_create_menu_buttons("Menu", popup_menu, 
                         MenuData, XtNumber(MenuData));
  XtRealizeWidget (toplevel);
  XtMainLoop();
}

void do_it(w, client_data, call_data)
     Widget   w;
     char    *client_data;
     caddr_t  call_data;
{
  printf("%s selected\n", client_data);
}


void post_menu_handler (w, menu, event)
   Widget  w;
   Widget  menu;
   XEvent *event;
{
  Arg wargs[10];
  int button;
  /*
   * Make sure the button that caused this event was the one
   * the menupane uses for menu item selection.
   */  
  XtSetArg(wargs[0], XmNwhichButton, &button);
  XtGetValues(menu, wargs, 1);
  if (event->xbutton.button == button){
   /*
    * Position the menu over the sprite and post the menu.
    */
    XmMenuPosition(menu, event);
    XtManageChild(menu);
  }
}

