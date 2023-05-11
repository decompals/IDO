/**********************************************************************************
 * menu3.c: pulldown menu example
  *         From:
  *                   The X Window System, 
  *            Programming and Applications with Xt
  *                   OSF/Motif Edition
  *         by
  *                Douglas Young
  *              Prentice Hall, 1990
  *
  *                 Example described on pages:     107-109
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

extern void do_it();

/*
 * Create the list of menu items for a sub menu pane.
 */
static xs_menu_struct sub_menu_a[] = {
  {"Item 2 A", do_it, "Item 2 A"},
  {"Item 2 B", do_it, "Item 2 B"}
};
/*
 * Describe pulldown pane one.
 */
static xs_menu_struct MenuData[] = {
  {"Item 1", do_it, "Item One",  NULL, 0, NULL},
  {"Item 2", NULL,     "Item Two",  sub_menu_a,
      XtNumber(sub_menu_a), "SubMenuA"},
  {"Item 3", do_it, "Item Three", NULL, 0 , NULL}
};
/*
 * Describe pulldown pane two.
 */
static xs_menu_struct Menu2Data[] = {
  {"Item 2-1", do_it, "Iten 2-One",   NULL, 0, NULL},
  {"Item 2-2", do_it, "Item 2-Two",   NULL, 0, NULL},
  {"Item 2-3", do_it, "Item 2-Three", NULL, 0 ,NULL}
};
/*
 * Describe the menu bar, giving only the names to appear in
 * the menu bar and pointers to each pulldown pane.
 */
static xs_menu_struct PulldownData[] = {
  {"Menu One" , NULL,  NULL, 
    MenuData, XtNumber(MenuData), NULL},
  {"Menu Two" , NULL,  NULL,  
    Menu2Data, XtNumber(Menu2Data), NULL},
};

main(argc, argv)
     int   argc;
     char *argv[];
{
  Widget toplevel, bb, menubar;
  /*
   * Initialize the Intrinsics and create a top level widget.
   */
  toplevel = XtInitialize (argv[0], "MenuTest", NULL, 0, 
                           &argc, argv);
  bb = XtCreateManagedWidget("bulletin", 
			     xmBulletinBoardWidgetClass, 
                             toplevel, NULL, 0);
  /*
   * Steps 1 and 2. Create and manage a menubar.
   */
  menubar = XmCreateMenuBar(bb, "menubar", NULL, 0);
  XtManageChild(menubar); 
  /*
   * Step 3. Create the menu from the description.
   */
  xs_create_menu_buttons(NULL, menubar, PulldownData,
                         XtNumber(PulldownData));
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
