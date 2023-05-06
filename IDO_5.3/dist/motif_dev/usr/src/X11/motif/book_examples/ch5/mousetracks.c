/**********************************************************************************
 * mousetracks.c: Driver to test the mouse tracker module
  *         From:
  *                   The X Window System, 
  *            Programming and Applications with Xt
  *                   OSF/Motif Edition
  *         by
  *                Douglas Young
  *              Prentice Hall, 1990
  *
  *                 Example described on pages:     136-139
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
#include <Xm/PanedW.h>
#include <Xm/RowColumn.h>
#include <Xm/DrawingA.h>
#include <Xm/Label.h>
#include "libXs.h"

main(argc, argv)
   int             argc;
   char           *argv[];
{
   Widget          toplevel, panel, command, target;
  /*
   * Initialize the Intrinsics.
   */
   toplevel = XtInitialize(argv[0], "Mousetracks", NULL, 0,
                           &argc, argv);
   /*
    * Create a vertical paned widget, to hold
    * all the other widgets.
    */
   panel = XtCreateManagedWidget("panel", 
                                 xmPanedWindowWidgetClass,
                                 toplevel, NULL, 0);
   /*
    * Create a command widget to hold both a quit button 
    * and the mouse tracker display.
    */
    command = XtCreateManagedWidget("command", 
                                    xmRowColumnWidgetClass, 
                                    panel, NULL, 0);
    /*
     *  Add a quit button.
     */
    xs_create_quit_button(command);
    /* 
     *  Create the widget in which we track the 
     *  motion of the sprite.
     */
    target = XtCreateManagedWidget("target", 
                                   xmDrawingAreaWidgetClass,
                                   panel, NULL, 0);
    /*
     * Create the mouse tracker.
     */
    create_mouse_tracker(command, target);
    XtRealizeWidget(toplevel);
    XtMainLoop();
}
