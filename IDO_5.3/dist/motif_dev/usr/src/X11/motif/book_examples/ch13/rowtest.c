/**********************************************************************************
 * rowtest.c: Program to test the Row widget
  *         From:
  *                   The X Window System, 
  *            Programming and Applications with Xt
  *                   OSF/Motif Edition
  *         by
  *                Douglas Young
  *              Prentice Hall, 1990
  *
  *                 Example described on pages:  391-397
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
#include <Xm/PushB.h> 
#include "Row.h"

void   grow (); 
void   unmanage(); 
void   manage(); 

char *names[] = {"Button1", "Button2", "Button3", "Button4"};

main(argc, argv)
  int    argc;
  char  *argv[];
{
  Widget toplevel, row, buttons[4];
  Arg    wargs[2];
  int    i;
  /*
   * Initialize the Intrinsics.
  */
  toplevel = XtInitialize(argv[0], "Rowtest", NULL,
                          0, &argc, argv);
  /*
   * Create a Row widget.
   */
  row = XtCreateManagedWidget("row", XsrowWidgetClass,
                               toplevel, NULL, 0);
  /*
   * Add children to the Row widget.
   */
  for(i=0;i<XtNumber(names);i++)
    buttons[i] = XtCreateWidget(names[i],
                                xmPushButtonWidgetClass,
                                row, NULL, 0);

  XtAddCallback(buttons[0], XmNactivateCallback,
                grow , NULL);
  XtAddCallback(buttons[1], XmNactivateCallback,
                unmanage, NULL);
  XtAddCallback(buttons[2], XmNactivateCallback,
                manage, buttons[1]);
  XtAddCallback(buttons[3], XmNactivateCallback,
                grow , NULL);

  XtManageChildren(buttons, XtNumber(buttons));
  XtRealizeWidget(toplevel);
  XtMainLoop(); 
}

void grow (w, button, call_data)
  Widget     w;
  Widget     button;
  caddr_t    call_data;
{
  Arg        wargs[2];
  Dimension  width, height;
  /*
   *  Get the current width and height of the widget.
   */
  XtSetArg(wargs[0], XtNwidth,  &width);
  XtSetArg(wargs[1], XtNheight, &height);
  XtGetValues(w, wargs, 2);
  /*
   * Increment the width and height by 10 pixels before
   * setting the size.
   */
  width  +=10;
  height +=10;
  XtSetArg(wargs[0], XtNwidth, width);
  XtSetArg(wargs[1], XtNheight, height);
  XtSetValues(w, wargs, 2);
  }
void unmanage(w, client_data, call_data)
   Widget     w;
   caddr_t    client_data;
   caddr_t    call_data;
{
  XtUnmanageChild(w);
}

void manage(w, button, call_data)
   Widget     w;
   Widget     button;
   caddr_t    call_data;
{
  XtManageChild(button);
}
