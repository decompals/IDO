/**********************************************************************************
  * menu.c: First pop up menu example
  *         From:
  *                   The X Window System, 
  *            Programming and Applications with Xt
  *                   OSF/Motif Edition
  *         by
  *                Douglas Young
  *              Prentice Hall, 1990
  *
  *                 Example described on pages:     99-101
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


#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <Xm/Xm.h>
#include <Xm/Label.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/Separator.h>
#include <Xm/BulletinB.h>
#include <Xm/CascadeB.h>

extern void post_menu_handler ();

void do_it(w, client_data, call_data)
     Widget   w;
     char    *client_data;
     caddr_t  call_data;
{
  printf("%s selected\n", client_data);
}

main(argc, argv)
     int   argc;
     char *argv[];
{
  Widget toplevel, button, bboard, menu, button1,
         button2, button3, submenu, button2a, button2b;
  Arg wargs[10];
  /*
   * Initialize the Intrinsics and create a BulletinBoard 
   * widget to be the primary widget in the program.
   */  
  toplevel = XtInitialize (argv[0], "MenuTest", NULL, 0, 
                           &argc, argv);
  bboard = XtCreateManagedWidget("bboard", 
                                 xmBulletinBoardWidgetClass, 
                                 toplevel, NULL, 0);
  /*
   * Step 1. Create a popup menu. Add an event handler
   * To the BulletinBoard widget to pop up the menu when
   * a mouse button is pressed.
   */
  menu = XmCreatePopupMenu(bboard, "menu", NULL, 0);
  XtAddEventHandler(bboard, ButtonPressMask, FALSE,
                    post_menu_handler, menu);
  /*
   * Step 2. Add buttons, labels, and separators to the pane.
   * Step 3. Register callbacks to define the action
   *         associated with each menu entry.
   */
  XtCreateManagedWidget("Title", xmLabelWidgetClass, menu,
                         NULL, 0);
  XtCreateManagedWidget("separator", xmSeparatorWidgetClass,
                        menu, NULL, 0);
  button1 = XtCreateManagedWidget("Item1",
                                  xmPushButtonWidgetClass,
                                  menu, NULL, 0);
  XtAddCallback (button1, XmNactivateCallback, 
                 do_it, "Item1");
  /*
   * Add an XmCascadeButton widget to support a submenu.
   */
  button2 = XtCreateManagedWidget("Item2",
                                  xmCascadeButtonWidgetClass, 
                                  menu, NULL, 0);
  button3 = XtCreateManagedWidget("Item3",
                                  xmPushButtonWidgetClass,
                                  menu, NULL, 0);
  XtAddCallback (button3, XmNactivateCallback, 
                 do_it, "Item3");
  /*
   * To Create a cascading menu pane:
   *         Create a pulldown menu pane and attach it to the
   *         XmCascadeButton widget.
   */
  submenu = XmCreatePulldownMenu(menu, "submenu", NULL, 0);
  XtSetArg(wargs[0], XmNsubMenuId, submenu);
  XtSetValues(button2, wargs, 1);
  /*
   * Add buttons the submenu.
   */
  button2a = XtCreateManagedWidget("Item2a",
                                   xmPushButtonWidgetClass,
                                   submenu, NULL, 0);
  XtAddCallback (button2a, XmNactivateCallback, 
                 do_it, "Item2a");  
  button2b = XtCreateManagedWidget("Item2b",
                                   xmPushButtonWidgetClass,
                                   submenu, NULL, 0);
  XtAddCallback (button2b, XmNactivateCallback, 
                 do_it, "Item2b");
  
  XtRealizeWidget (toplevel);
  XtMainLoop();
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

